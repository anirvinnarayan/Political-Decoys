# ============================================
# Political Decoys - Decoys for GE and AE v.2
# Using more computationally intensive method to calculate lv scores for all pairs. 
# ============================================
# Date: 06/03/25
# Author: Anirvin Narayan

rm(list = ls())
setwd("/Users/anirvin/Downloads/Political Decoys Data")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  readr,
  lubridate,
  here,
  readr, 
  geosphere,
  sf,
  rnaturalearth, 
  stringr, 
  lwgeom,
  stringdist,
  fixest, 
  modelsummary, 
  writexl, 
  plm, 
  openxlsx, 
  lmtest, 
  etable, 
  clubSandwich, 
  gridExtra,
  stargazer, 
  psych, 
  kableExtra,
  MASS, 
  glmmTMB, 
  survival, 
  sandwich, 
  xtable, 
  fixest, 
  RefManageR, 
  corrplot, 
  Hmisc, 
  patchwork
)

### Loading
all_states_GE <- read.csv("Raw Data/All_States_GE.csv")
all_states_AE <- read.csv("Raw Data/TCPD_AE_All_States_2025-3-6.csv")

str(all_states_AE)
str(all_states_GE)

# how to merge
setdiff(names(all_states_AE), names(all_states_GE))
all_states_GE$Age <- NA
all_states_GE$District_Name <- NA

all_states_elections <- rbind(all_states_AE, all_states_GE)

### Understanding the Data
str(all_states_elections)

how_many_main_min <- all_states_elections %>%
  group_by(Year, Constituency_Name, State_Name, Election_Type) %>%
  summarise(
    main_cand = sum(replace_na(Vote_Share_Percentage, 0) > 10), 
    minor_cand = sum(replace_na(Vote_Share_Percentage, 0) <= 10)
  ) %>%
  ungroup()

main_min_year <- how_many_main_min %>%
  group_by(Year) %>%
  summarise(avg_main_cand = mean(main_cand, na.rm = TRUE),
            avg_minor_cand = mean(minor_cand, na.rm = TRUE)) 

ggplot(main_min_year, aes(x = Year)) +
  geom_line(aes(y = avg_main_cand, color = "Main Candidates"), linewidth = 1) +
  geom_line(aes(y = avg_minor_cand, color = "Minor Candidates"), linewidth = 1) +
  labs(title = "Average Number of Main and Minor Candidates Over Time",
       x = "Year",
       y = "Average Number of Candidates",
       color = "Candidate Type") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

ggsave(
  filename = "Plots/Main_Minor_overtime.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

main_min_state <- how_many_main_min %>%
  group_by(State_Name) %>%
  summarise(avg_main_cand = mean(main_cand, na.rm = TRUE),
            avg_minor_cand = mean(minor_cand, na.rm = TRUE)) 

main_min_state_long <- main_min_state %>%
  gather(key = "Candidate_Type", value = "avg_candidates", avg_main_cand, avg_minor_cand)

ggplot(main_min_state_long, aes(x = State_Name, y = avg_candidates, fill = Candidate_Type)) +
  geom_col(position = "stack") +
  labs(title = "Stacked Bar Chart of Main and Minor Candidates Across States",
       x = "State",
       y = "Average Number of Candidates",
       fill = "Candidate Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 9)
  )

ggsave(
  filename = "Plots/Main_Minor_state.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

unique_elections <- all_states_elections %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type) %>%
  distinct()

normalized_levenshtein_matrix <- function(strings) {
  # get raw distance matrix
  raw_dist <- stringdistmatrix(strings, strings, method = "lv")
  # create a matrix of maximum lengths
  n <- length(strings)
  len_matrix <- matrix(0, nrow = n, ncol = n)
  string_lengths <- nchar(strings)
  for (i in 1:n) {
    for (j in 1:n) {
      len_matrix[i, j] <- max(string_lengths[i], string_lengths[j])
    }
  }
  # normalize the distances and invert (1 - distance/length)
  normalized <- 1 - (raw_dist / len_matrix)
  # set diagonal to 1 (complete similarity to self)
  diag(normalized) <- 1
  rownames(normalized) <- strings
  colnames(normalized) <- strings
  return(normalized)
}

# pre-processing the name string (more could be done here) 
# right now just strip spaces and punctuation
all_states_elections$Candidate_clean <- gsub(pattern = "[[:space:][:punct:]]", replacement = "", all_states_elections$Candidate)

### More complicated approach - following the STATA code more closely - MORE COMPUTATIONALLY INTENSIVE
### Should include because then, it would be easier to tune the thresholds and not take time. 

format_time <- function(seconds) {
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- round(seconds %% 60, 1)
  
  if (hours > 0) {
    return(sprintf("%d hours, %d minutes, %.1f seconds", hours, minutes, secs))
  } else if (minutes > 0) {
    return(sprintf("%d minutes, %.1f seconds", minutes, secs))
  } else {
    return(sprintf("%.1f seconds", secs))
  }
}

unique_elections <- all_states_elections %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  distinct()

candidate_pairs <- data.frame()
result_list <- list()
list_index <- 1

# tracking progress
total_elections <- nrow(unique_elections)
cat("Processing", total_elections, "unique elections\n")

start_time_total <- Sys.time()
last_batch_size <- 100

# loop through each election
for (i in 1:nrow(unique_elections)) {
  # election details
  year <- unique_elections$Year[i]
  state <- unique_elections$State_Name[i]
  constituency <- unique_elections$Constituency_Name[i]
  election_type <- unique_elections$Election_Type[i]
  assembly_no <- unique_elections$Assembly_No[i]
  
  # print progress every 1000 elections
  if (i %% 100 == 0 || i == 1 || i == total_elections) {
    current_time <- Sys.time()
    time_elapsed_total <- as.numeric(difftime(current_time, start_time_total, units = "secs"))
    
    cat("Processing election", i, "of", total_elections, 
        "(", round(i/total_elections*100, 1), "%)\n")
    cat("  Total elapsed time:", format_time(time_elapsed_total), "\n\n")
  }
  
  # filter data for current election
  election_data <- all_states_elections %>%
    filter(Year == year,
           State_Name == state,
           Constituency_Name == constituency,
           Election_Type == election_type,
           Assembly_No == assembly_no)
  
  # skip if fewer than 2 candidates
  if (nrow(election_data) < 2) {
    next
  }
  
  # mark candidates as main or minor
  election_data <- election_data %>%
    mutate(candidate_type = ifelse(Vote_Share_Percentage > 10, "main", "minor"))
  
  # get candidate names and calculate normalized Levenshtein matrix
  candidate_names <- election_data$Candidate_clean
  distance_matrix <- normalized_levenshtein_matrix(candidate_names)
  
  # create all pairs of candidates
  for (j in 1:(nrow(election_data) - 1)) {
    for (k in (j+1):nrow(election_data)) {
      # get candidate details
      candidate1 <- election_data[j, ]
      candidate2 <- election_data[k, ]
      
      # get similarity score from matrix
      similarity_score <- distance_matrix[j, k]
      
      # determine pair type
      pair_type <- paste0(candidate1$candidate_type, "-", candidate2$candidate_type)
      
      # create row for this pair
      pair_row <- data.frame(
        Year = year,
        State_Name = state,
        Constituency_Name = constituency,
        Election_Type = election_type,
        Assembly_No = assembly_no,
        
        Candidate1_Name = candidate1$Candidate_clean,
        Candidate1_PID = candidate1$pid,
        Candidate1_Party = candidate1$Party,
        Candidate1_Votes = candidate1$Votes,
        Candidate1_VoteShare = candidate1$Vote_Share_Percentage,
        Candidate1_Type = candidate1$candidate_type,
        
        Candidate2_Name = candidate2$Candidate_clean,
        Candidate2_PID = candidate2$pid,
        Candidate2_Party = candidate2$Party,
        Candidate2_Votes = candidate2$Votes,
        Candidate2_VoteShare = candidate2$Vote_Share_Percentage,
        Candidate2_Type = candidate2$candidate_type,
        
        Pair_Type = pair_type,
        Levenshtein_Similarity = similarity_score
      )
      
      # add to results
      result_list[[list_index]] <- pair_row
      list_index <- list_index + 1
    }
  }
}

end_time_total <- Sys.time()
total_time <- as.numeric(difftime(end_time_total, start_time_total, units = "secs"))

sample_size <- min(10000, length(result_list))
test_df <- do.call(rbind, result_list[1:sample_size])

candidate_pairs <- do.call(rbind, result_list)

sum(all_states_elections$MyNeta_education == "")
sum(all_states_elections$Party_Type_TCPD == "")
sum(all_states_elections$TCPD_Prof_Main == "")

str(all_states_elections)

# cleaning - should have done this before
# issues like no vote info, 0 votes, negative votes, NOTA in party name
candidate_pairs_1 <- candidate_pairs %>%
  filter(Candidate1_Name != "NOTA" & Candidate2_Name != "NOTA")

what <- candidate_pairs_1 %>%
  filter(Pair_Type == "NA-NA" | Pair_Type == "NA-main" | Pair_Type == "NA-minor" )

candidate_pairs_1 <- candidate_pairs_1 %>%
  filter(!is.na(Candidate1_Votes) & !is.na(Candidate2_Votes))

pair_type_summary <- candidate_pairs_1 %>%
  group_by(Pair_Type) %>%
  dplyr::summarize(
    Count = n(),
    Avg_Similarity = mean(Levenshtein_Similarity),
    High_Similarity_Count = sum(Levenshtein_Similarity > 0.53)
  )

what <- candidate_pairs_1 %>%
  filter(Pair_Type == "NA-NA" | Pair_Type == "NA-main" | Pair_Type == "NA-minor" )

candidate_pairs_1 <- candidate_pairs_1 %>%
  filter(Candidate1_Party != "NOTA" & Candidate2_Party != "NOTA")

pair_type_summary <- candidate_pairs_1 %>%
  group_by(Pair_Type) %>%
  dplyr::summarize(
    Count = n(),
    Avg_Similarity = mean(Levenshtein_Similarity),
    High_Similarity_Count = sum(Levenshtein_Similarity > 0.53)
  )

what <- candidate_pairs_1 %>%
  filter(Pair_Type == "NA-NA" | Pair_Type == "NA-main" | Pair_Type == "NA-minor" )

candidate_pairs_1 <- candidate_pairs_1 %>%
  filter(Pair_Type != "NA-NA" & Pair_Type != "NA-main" & Pair_Type != "NA-minor")

pair_type_summary <- candidate_pairs_1 %>%
  group_by(Pair_Type) %>%
  dplyr::summarize(
    Count = n(),
    Avg_Similarity = mean(Levenshtein_Similarity),
    High_Similarity_Count = sum(Levenshtein_Similarity > 0.53)
  )

what <- candidate_pairs_1 %>%
  filter(Pair_Type == "NA-NA" | Pair_Type == "NA-main" | Pair_Type == "NA-minor" )

candidate_pairs_1 <- candidate_pairs_1 %>%
  mutate(is_decoy = ifelse(Levenshtein_Similarity > 0.53, TRUE, FALSE))

### basic summary stats to understand the results
percentiles <- quantile(candidate_pairs_1$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_1, aes(x = Levenshtein_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Levenshtein Similarity with Percentiles",
       x = "Levenshtein Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold") 
  )

ggsave(
  filename = "Plots/Levenshtein_Kernel.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

candidate_pairs_1_main_minor <- candidate_pairs_1 %>%
  filter(Pair_Type == "main-minor")

candidate_pairs_1_main_main <- candidate_pairs_1 %>%
  filter(Pair_Type == "main-main")

candidate_pairs_1_minor_minor <- candidate_pairs_1 %>%
  filter(Pair_Type == "minor-minor")

percentiles <- quantile(candidate_pairs_1_main_minor$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_1_main_minor, aes(x = Levenshtein_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Levenshtein Similarity with Percentiles (Main-Minor)",
       x = "Levenshtein Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "Plots/Levenshtein_Kernel_main_minor.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

percentiles <- quantile(candidate_pairs_1_main_main$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_1_main_main, aes(x = Levenshtein_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Levenshtein Similarity with Percentiles (Main-Main)",
       x = "Levenshtein Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "Plots/Levenshtein_Kernel_main_main.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

percentiles <- quantile(candidate_pairs_1_minor_minor$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_1_minor_minor, aes(x = Levenshtein_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Levenshtein Similarity with Percentiles (Minor-Minor)",
       x = "Levenshtein Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "Plots/Levenshtein_Kernel_minor_minor.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# summaries at election level
election_decoys <- candidate_pairs_1 %>%
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  summarise(
    main_main_decoys = sum(Pair_Type == "main-main" & is_decoy, na.rm = TRUE),
    main_minor_decoys = sum(Pair_Type == "main-minor" & is_decoy, na.rm = TRUE),
    minor_minor_decoys = sum(Pair_Type == "minor-minor" & is_decoy, na.rm = TRUE),
    .groups = "drop"
  )
candidate_pairs_1$cand

max_decoy_info <- candidate_pairs_1 %>%
  filter(Pair_Type == "main-minor" & is_decoy) %>%
  # who is main and minor
  mutate(
    main_candidate_id = ifelse(Candidate1_Type == "main", Candidate1_PID, Candidate2_PID),
    main_candidate_name = ifelse(Candidate1_Type == "main", Candidate1_Name, Candidate2_Name)
  ) %>%
  # group by election and main candidate
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, main_candidate_id, main_candidate_name) %>%
  summarise(decoy_count = n(), .groups = "drop") %>%
  # for each election, find the candidate with max decoys
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  slice_max(order_by = decoy_count, n = 1, with_ties = FALSE) %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, 
         max_decoys_main_id = main_candidate_id, 
         max_decoys_main_name = main_candidate_name,
         max_decoys_per_main = decoy_count)

final_election_decoys <- election_decoys %>%
  left_join(max_decoy_info, 
            by = c("Year", "State_Name", "Constituency_Name", "Election_Type", "Assembly_No")) %>%
  # handle cases with no decoys
  mutate(
    max_decoys_per_main = replace_na(max_decoys_per_main, 0),
    max_decoys_main_id = if_else(max_decoys_per_main == 0, NA_character_, max_decoys_main_id),
    max_decoys_main_name = if_else(max_decoys_per_main == 0, NA_character_, max_decoys_main_name)
  )

sum(final_election_decoys$main_minor_decoys > 3)/nrow(final_election_decoys)
sum(final_election_decoys$main_minor_decoys > 0)/nrow(final_election_decoys)

thresholds <- 0:10

threshold_counts <- data.frame(
  threshold = thresholds,
  elections_count = sapply(thresholds, function(t) {
    sum(final_election_decoys$main_minor_decoys > t)
  })
)

ggplot(threshold_counts, aes(x = factor(threshold), y = elections_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = elections_count), vjust = -0.5, size = 3.5) +
  labs(
    title = "Number of Elections with Main-Minor Decoys Above Thresholds",
    x = "Number of Main-Minor Decoys (>)",
    y = "Number of Elections"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

threshold_counts$percentage <- threshold_counts$elections_count / nrow(election_decoys) * 100

ggplot(threshold_counts, aes(x = factor(threshold), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Percentage of Elections with Main-Minor Decoys Above Thresholds",
    x = "Number of Main-Minor Decoys (>)",
    y = "Percentage of Elections"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

# Saving
  # note: this is incomplete, i save because R might crash. does not have educ, party_type, margins etc.
write.csv(candidate_pairs_1, "Cleaned Data/candidate_pairs_lv.csv")

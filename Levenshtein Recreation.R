# ============================================
# Political Decoys - Levenshtein Recreation
# ============================================
# Date: 06/03/25
# Author: Anirvin Narayan

##### NOTES: I just realised, the STATA code create a row per candidate pair. which is a lot of rows. 
# which is very computationally intensive. so, i just calculated what i needed to and the scores of the decoys. 
# and the "mains" with which they corresponded. 

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

duplicates_pid <- all_states_GE$pid[duplicated(all_states_GE$pid)]
duplicates_pid_numeric <- as.numeric(duplicates_pid)
sum_duplicates <- sum(duplicates_pid_numeric, na.rm = TRUE)

### Understanding the Data
str(all_states_GE)

how_many_main_min <- all_states_GE %>%
  group_by(Year, Constituency_Name, State_Name) %>%
  summarise(main_cand = sum(Vote_Share_Percentage > 10, na.rm = TRUE), 
            minor_cand = sum(Vote_Share_Percentage < 10, na.rm = TRUE)) %>%
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
  theme_minimal()

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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

unique_elections <- all_states_GE %>%
  dplyr::select(Year, State_Name, Constituency_Name) %>%
  distinct()

### Calculate Similarity Scores
# define fn for levenshtein calculation
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

# init decoy vars
all_states_GE$is_decoy <- FALSE

# pre-processing the name string (more could be done here) 
# right now just strip spaces and punctuation
all_states_GE$Candidate_clean <- gsub(pattern = "[[:space:][:punct:]]", replacement = "", all_states_GE$Candidate)

# for first try - note pre-processing and threshold of 0.53
all_states_GE_1 <- all_states_GE

# list to process the data
result_list <- list()

str(all_states_GE_1)

# process for each election
for (i in 1:nrow(unique_elections)) {
  year <- unique_elections$Year[i]
  constituency <- unique_elections$Constituency_Name[i]
  state <- unique_elections$State_Name[i]
  
  # filter data for current group
  group_data <- all_states_GE_1 %>%
    filter(Year == year, 
           Constituency_Name == constituency, 
           State_Name == state)
  
  # mark main and minor candidates
  group_data <- group_data %>%
    mutate(candidate_type = ifelse(Vote_Share_Percentage > 10, "main", "minor"))
  
  # get main and minor candidates
  main_candidates <- group_data %>% filter(candidate_type == "main")
  minor_candidates <- group_data %>% filter(candidate_type == "minor")
  
  # skip if no main or no minor candidates
  if (nrow(main_candidates) == 0 || nrow(minor_candidates) == 0) {
    next
  }
  
  # extract all candidate names and their corresponding pids
  candidate_names <- group_data$Candidate_clean
  candidate_pids <- group_data$pid
  
  # calculate the normalized Levenshtein matrix for names - access by index
  distance_matrix <- normalized_levenshtein_matrix(candidate_names)
  
  # check for decoys by comparing each main candidate with each minor candidate
  for (main_idx in 1:nrow(main_candidates)) {
    main_candidate_name <- main_candidates$Candidate_clean[main_idx]
    main_candidate_pid <- main_candidates$pid[main_idx]
    main_matrix_idx <- which(group_data$pid == main_candidate_pid)
    
    for (minor_idx in 1:nrow(minor_candidates)) {
      minor_candidate_name <- minor_candidates$Candidate_clean[minor_idx]
      minor_candidate_pid <- minor_candidates$pid[minor_idx]
      minor_matrix_idx <- which(group_data$pid == minor_candidate_pid)
      
      # get the normalized levenshtein similarity using matrix indices
      if (length(main_matrix_idx) == 1 && length(minor_matrix_idx) == 1) {
        lev_similarity <- distance_matrix[main_matrix_idx, minor_matrix_idx]
        
        # check if this is a potential decoy based on similarity threshold
        if (lev_similarity > 0.53) {
          # mark as decoy in the original dataframe using pid as ID
          all_states_GE_1$is_decoy[all_states_GE_1$pid == minor_candidate_pid] <- TRUE
          all_states_GE_1$decoy_for_pid[all_states_GE_1$pid == minor_candidate_pid] <- main_candidate_pid
          all_states_GE_1$decoy_for_name[all_states_GE_1$pid == minor_candidate_pid] <- main_candidate_name
          
          # add to results list for review
          result_list[[length(result_list) + 1]] <- data.frame(
            Year = year,
            Constituency_Name = constituency,
            State_Name = state,
            Main_Candidate_name = main_candidate_name,
            Main_Candidate_pid = main_candidate_pid,
            Main_Party = main_candidates$Party[main_idx],
            Main_Votes = main_candidates$Votes[main_idx],
            Main_Vote_Share = main_candidates$Vote_Share_Percentage[main_idx],
            Minor_Candidate_name = minor_candidate_name,
            Minor_Candidate_pid = minor_candidate_pid,
            Minor_Party = minor_candidates$Party[minor_idx],
            Minor_Votes = minor_candidates$Votes[minor_idx],
            Minor_Vote_Share = minor_candidates$Vote_Share_Percentage[minor_idx],
            Levenshtein_Similarity = lev_similarity
          )
        }
      }
    }
  }
}

# how many decoys?
if (length(result_list) > 0) {
  decoy_results <- do.call(rbind, result_list)
  cat("Found", nrow(decoy_results), "potential decoy candidates\n")
} else {
  cat("No potential decoy candidates found.\n")
}

### Collapse by constituency-state-year
constituency_metrics <- all_states_GE_1 %>%
  group_by(State_Name, Year, Constituency_Name) %>%
  dplyr::summarize(
    total_candidates = n_distinct(pid),
    decoy_candidates = sum(is_decoy, na.rm = TRUE),
    decoy_share = round(sum(is_decoy, na.rm = TRUE) / n_distinct(pid) * 100, 2),
    total_votes = max(Valid_Votes, na.rm = TRUE),
    total_votes_to_decoys = sum(Votes[is_decoy], na.rm = TRUE),
    decoy_vote_share = round(sum(Votes[is_decoy], na.rm = TRUE) / max(Valid_Votes, na.rm = TRUE) * 100, 2),
    winning_margin = min(Margin[Position == 1], na.rm = TRUE),  # Margin of the winner
    winning_margin_percentage = min(Margin_Percentage[Position == 1], na.rm = TRUE),
    winner_party = first(Party[Position == 1]),
    runner_up_party = first(Party[Position == 2]),
    winner_vote_share = max(Vote_Share_Percentage[Position == 1], na.rm = TRUE),
    runner_up_vote_share = max(Vote_Share_Percentage[Position == 2], na.rm = TRUE),
    winner_has_decoys = any(decoy_for_pid[is_decoy] %in% pid[Position == 1], na.rm = TRUE),
    runner_up_has_decoys = any(decoy_for_pid[is_decoy] %in% pid[Position == 2], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    close_race = ifelse(winning_margin_percentage < 5, TRUE, FALSE),
    decoy_impact_potential = ifelse(decoy_vote_share > winning_margin_percentage, TRUE, FALSE)
  ) %>%
  # sort by state, year and constituency
  arrange(State_Name, Year, Constituency_Name)

state_metrics <- constituency_metrics %>%
  group_by(State_Name) %>%
  dplyr::summarize(
    # number of decoys
    mean_num_decoys = mean(decoy_candidates, na.rm = TRUE),
    median_num_decoys = median(decoy_candidates, na.rm = TRUE),
    p90_num_decoys = quantile(decoy_candidates, 0.9, na.rm = TRUE),
    max_num_decoys = max(decoy_candidates, na.rm = TRUE),
    
    # share of decoys
    mean_decoy_share = mean(decoy_share, na.rm = TRUE),
    median_decoy_share = median(decoy_share, na.rm = TRUE),
    p90_decoy_share = quantile(decoy_share, 0.9, na.rm = TRUE),
    max_decoy_share = max(decoy_share, na.rm = TRUE),
    
    # how many constituencies
    constituency_count = n(),
    .groups = "drop"
  ) %>%
  # format w 4 decimal points
  mutate(across(c(starts_with("mean_"), starts_with("median_"), 
                  starts_with("p90_"), starts_with("max_")), 
                ~round(., 4)))

### Placebo Test - to see whether similarity is distributed differently across major and minor cands
# placebo for all_states_GE_1
placebo_results <- list()

constituency_year_counts <- data.frame(
  Year = numeric(),
  Constituency_Name = character(),
  State_Name = character(),
  main_main_total = numeric(),
  main_main_fps = numeric(),
  main_minor_total = numeric(), 
  main_minor_fps = numeric(),
  minor_minor_total = numeric(),
  minor_minor_fps = numeric()
)

for (i in 1:nrow(unique_elections)) {
  year <- unique_elections$Year[i]
  constituency <- unique_elections$Constituency_Name[i]
  state <- unique_elections$State_Name[i]
  
  # filter data for current group
  group_data <- all_states_GE_1 %>%
    filter(Year == year, 
           Constituency_Name == constituency, 
           State_Name == state)
  
  # mark main and minor candidates
  group_data <- group_data %>%
    mutate(candidate_type = ifelse(Vote_Share_Percentage > 10, "main", "minor"))
  
  # get main and minor candidates
  main_candidates <- group_data %>% filter(candidate_type == "main")
  minor_candidates <- group_data %>% filter(candidate_type == "minor")
  
  # skip if no candidates of either type
  if (nrow(main_candidates) == 0 || nrow(minor_candidates) == 0) {
    next
  }
  
  # extract all candidate names and their corresponding pids
  candidate_names <- group_data$Candidate_clean
  candidate_pids <- group_data$pid
  
  # calculate the normalized Levenshtein matrix for names - access by index
  distance_matrix <- normalized_levenshtein_matrix(candidate_names)
  
  # Initialize counters for this constituency-year
  main_main_total <- 0
  main_main_fps <- 0
  main_minor_total <- 0
  main_minor_fps <- 0
  minor_minor_total <- 0
  minor_minor_fps <- 0
  
  # for main-main
  if (nrow(main_candidates) > 1) {
    for (idx1 in 1:(nrow(main_candidates)-1)) {
      for (idx2 in (idx1+1):nrow(main_candidates)) {
        main_main_total <- main_main_total + 1
        
        candidate1_pid <- main_candidates$pid[idx1]
        candidate2_pid <- main_candidates$pid[idx2]
        
        matrix_idx1 <- which(group_data$pid == candidate1_pid)
        matrix_idx2 <- which(group_data$pid == candidate2_pid)
        
        if (length(matrix_idx1) == 1 && length(matrix_idx2) == 1) {
          lev_similarity <- distance_matrix[matrix_idx1, matrix_idx2]
          
          # Check if this would be flagged as a false positive
          if (lev_similarity > 0.53) {
            main_main_fps <- main_main_fps + 1
            
            # Add to placebo results for review
            placebo_results[[length(placebo_results) + 1]] <- data.frame(
              Year = year,
              Constituency_Name = constituency,
              State_Name = state,
              Pair_Type = "main-main",
              Candidate1_name = main_candidates$Candidate_clean[idx1],
              Candidate1_pid = candidate1_pid,
              Candidate1_Party = main_candidates$Party[idx1],
              Candidate1_Votes = main_candidates$Votes[idx1],
              Candidate1_Vote_Share = main_candidates$Vote_Share_Percentage[idx1],
              Candidate2_name = main_candidates$Candidate_clean[idx2],
              Candidate2_pid = candidate2_pid,
              Candidate2_Party = main_candidates$Party[idx2],
              Candidate2_Votes = main_candidates$Votes[idx2],
              Candidate2_Vote_Share = main_candidates$Vote_Share_Percentage[idx2],
              Levenshtein_Similarity = lev_similarity
            )
          }
        }
      }
    }
  }
  
  # for main-minor (what we want)
  for (main_idx in 1:nrow(main_candidates)) {
    for (minor_idx in 1:nrow(minor_candidates)) {
      main_minor_total <- main_minor_total + 1
      
      main_candidate_pid <- main_candidates$pid[main_idx]
      minor_candidate_pid <- minor_candidates$pid[minor_idx]
      
      main_matrix_idx <- which(group_data$pid == main_candidate_pid)
      minor_matrix_idx <- which(group_data$pid == minor_candidate_pid)
      
      if (length(main_matrix_idx) == 1 && length(minor_matrix_idx) == 1) {
        lev_similarity <- distance_matrix[main_matrix_idx, minor_matrix_idx]
        
        # check if this would be flagged
        if (lev_similarity > 0.53) {
          main_minor_fps <- main_minor_fps + 1
          
          # add to placebo results for review
          placebo_results[[length(placebo_results) + 1]] <- data.frame(
            Year = year,
            Constituency_Name = constituency,
            State_Name = state,
            Pair_Type = "main-minor",
            Candidate1_name = main_candidates$Candidate_clean[main_idx],
            Candidate1_pid = main_candidate_pid,
            Candidate1_Party = main_candidates$Party[main_idx],
            Candidate1_Votes = main_candidates$Votes[main_idx],
            Candidate1_Vote_Share = main_candidates$Vote_Share_Percentage[main_idx],
            Candidate2_name = minor_candidates$Candidate_clean[minor_idx],
            Candidate2_pid = minor_candidate_pid,
            Candidate2_Party = minor_candidates$Party[minor_idx],
            Candidate2_Votes = minor_candidates$Votes[minor_idx],
            Candidate2_Vote_Share = minor_candidates$Vote_Share_Percentage[minor_idx],
            Levenshtein_Similarity = lev_similarity
          )
          
          # update the original dataframe markers (same as original code)
          all_states_GE_1$is_decoy[all_states_GE_1$pid == minor_candidate_pid] <- TRUE
          all_states_GE_1$decoy_for_pid[all_states_GE_1$pid == minor_candidate_pid] <- main_candidate_pid
          all_states_GE_1$decoy_for_name[all_states_GE_1$pid == minor_candidate_pid] <- main_candidates$Candidate_clean[main_idx]
        }
      }
    }
  }
  
  # for minor-minor
  if (nrow(minor_candidates) > 1) {
    for (idx1 in 1:(nrow(minor_candidates)-1)) {
      for (idx2 in (idx1+1):nrow(minor_candidates)) {
        minor_minor_total <- minor_minor_total + 1
        
        candidate1_pid <- minor_candidates$pid[idx1]
        candidate2_pid <- minor_candidates$pid[idx2]
        
        matrix_idx1 <- which(group_data$pid == candidate1_pid)
        matrix_idx2 <- which(group_data$pid == candidate2_pid)
        
        if (length(matrix_idx1) == 1 && length(matrix_idx2) == 1) {
          lev_similarity <- distance_matrix[matrix_idx1, matrix_idx2]
          
          # Check if this would be flagged as a false positive
          if (lev_similarity > 0.53) {
            minor_minor_fps <- minor_minor_fps + 1
            
            # Add to placebo results for review
            placebo_results[[length(placebo_results) + 1]] <- data.frame(
              Year = year,
              Constituency_Name = constituency,
              State_Name = state,
              Pair_Type = "minor-minor",
              Candidate1_name = minor_candidates$Candidate_clean[idx1],
              Candidate1_pid = candidate1_pid,
              Candidate1_Party = minor_candidates$Party[idx1],
              Candidate1_Votes = minor_candidates$Votes[idx1],
              Candidate1_Vote_Share = minor_candidates$Vote_Share_Percentage[idx1],
              Candidate2_name = minor_candidates$Candidate_clean[idx2],
              Candidate2_pid = candidate2_pid,
              Candidate2_Party = minor_candidates$Party[idx2],
              Candidate2_Votes = minor_candidates$Votes[idx2],
              Candidate2_Vote_Share = minor_candidates$Vote_Share_Percentage[idx2],
              Levenshtein_Similarity = lev_similarity
            )
          }
        }
      }
    }
  }
  
  # store counts for this constituency-year
  constituency_year_counts <- rbind(constituency_year_counts, data.frame(
    Year = year,
    Constituency_Name = constituency,
    State_Name = state,
    main_main_total = main_main_total,
    main_main_fps = main_main_fps,
    main_minor_total = main_minor_total,
    main_minor_fps = main_minor_fps,
    minor_minor_total = minor_minor_total,
    minor_minor_fps = minor_minor_fps
  ))
}

placebo_results_df <- do.call(rbind, placebo_results)

fp_summary <- data.frame(
  Pair_Type = c("main-main", "main-minor", "minor-minor"),
  Total_Pairs = c(
    sum(constituency_year_counts$main_main_total),
    sum(constituency_year_counts$main_minor_total),
    sum(constituency_year_counts$minor_minor_total)
  ),
  False_Positives = c(
    sum(constituency_year_counts$main_main_fps),
    sum(constituency_year_counts$main_minor_fps),
    sum(constituency_year_counts$minor_minor_fps)
  )
)

fp_summary$False_Positive_Rate <- fp_summary$False_Positives / fp_summary$Total_Pairs * 100

### Some more plots
state_metrics <- state_metrics %>%
  arrange(desc(mean_decoy_share)) %>%
  mutate(State_Name = factor(State_Name, levels = State_Name))

ggplot(state_metrics, aes(x = State_Name, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_decoy_share, ymax = p90_decoy_share), width = 0.2) +
  geom_point(aes(y = max_decoy_share), color = "red", size = 2) +
  labs(
    title = "Average Share of Decoy Candidates by State",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "State",
    y = "Proportion of Decoy Candidates"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

ggplot(state_metrics, aes(x = State_Name, y = mean_num_decoys)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_num_decoys, ymax = p90_num_decoys), width = 0.2) +
  geom_point(aes(y = max_num_decoys), color = "red", size = 2) +
  labs(
    title = "Average Number of Decoy Candidates by State",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "State",
    y = "Number of Decoy Candidates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

state_metrics_long <- state_metrics %>%
  dplyr::select(State_Name, 
         `Average Number` = mean_num_decoys, 
         `Maximum Number` = max_num_decoys,
         `Average Share` = mean_decoy_share, 
         `Maximum Share` = max_decoy_share,
         constituency_count) %>%
  pivot_longer(cols = c(`Average Number`, `Maximum Number`, `Average Share`, `Maximum Share`),
               names_to = "Metric", values_to = "Value")

# faceted
ggplot(state_metrics_long, aes(x = State_Name, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
  # geom_text(aes(label = round(Value, 3)), position = position_dodge(width = 0.9), 
  #           vjust = -0.5, size = 2.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Decoy Candidates Metrics by State",
    subtitle = "States ordered by average share of decoy candidates",
    x = "State",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

constituency_metrics$tota

state_year_metrics <- constituency_metrics %>%
  group_by(State_Name, Year) %>%
  summarise(
    decoy_proportion = mean(decoy_candidates/total_candidates, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(state_year_metrics, aes(x = Year, y = decoy_proportion, color = State_Name, group = State_Name)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Avg Decoy Proportion by State Over Time",
    x = "Election Year",
    y = "Average Decoy Proportion",
    color = "State"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

year_metrics <- state_year_metrics %>%
  group_by(Year) %>%
  dplyr::summarize(
    decoy_proportion = mean(decoy_proportion, na.rm = TRUE), 
    .groups = "drop"
  )

ggplot(year_metrics, aes(x = Year, y = decoy_proportion)) +
  geom_line(color = "red") +
  geom_point(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Avg Decoy Proportion Over Time (EW)",
    x = "Election Year",
    y = "Average Decoy Proportion"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

### Make graphs on how well these candidates did. 


### More complicated approach - following the STATA code more closely

### Margin Notes
# here is how u calc levenshtein
words <- c("ani", "ali", "anirvin", "brian")
lv <- stringdistmatrix(words, words, method = "lv")

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

  # normalize the distances
  normalized <- raw_dist / len_matrix

  # set diagonal to 0
  diag(normalized) <- 0

  rownames(normalized) <- strings
  colnames(normalized) <- strings
  return(normalized)
}

n_lv <- normalized_levenshtein_matrix(words)

test <- constituency_metrics %>%
  filter(constituency_metrics$decoy_candidates >= 3)

test_2 <- all_states_GE_1 %>%
  filter(State_Name == "Andhra_Pradesh" & Constituency_Name == "VIJAYAWADA" & Year == 2014)

sum(constituency_metrics$decoy_candidates >= 3)

# Notes from Mukund's STATA Code:
# using All_States_GE as delimited
# define list of all states (except bihar)
# fn to identify decoys
  # loops thru states
  # create vars to store id, lv_dist, similarity, decoy, main_cand
  # convert voteshare into numeric
  # identify main cand (>10%)
  # sort by const and id
  # compares pair in constituency and election yr
  # calculates similarity score
  # identifies decoys
  # save result for each state in separate file
  # edits: save altogether
  # make a flag to show (within const-year), main candidate, and corresponding decoy. have way to identify multiple mains and decoys of which mains. 
# fn to collapse
  # collapses by const-year (total cand and total decoys)
  # shows decoy share
  # 

# Notes
  # count of decoys
  # robustness to many ways to measuring decoys
  # vote margin
  # describe decoy characteristics
  # Do this for state assembly
  # Look at how decoys have evolved
  # how do we deal with situations like punjab



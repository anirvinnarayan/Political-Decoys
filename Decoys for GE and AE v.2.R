# ============================================
# Political Decoys - Decoys for GE and AE v.2
# Using more computationally intensive method to calculate lv scores for all pairs. 
# ============================================
# Date: 06/03/25
# Author: Anirvin Narayan

### THINGS TO DO
  # how many close elections? plot histogram.

### NEW ISSUES
  # Need to try multiple ways of calculating distance
  # calculating share of decoy candidates by adding up is_decoy will not work if a minor candidate is mistakenly 
  # assigned as a decoy for multiple major candidate. for example, 1967 AMLOH Punjab AE, where EVERYBODY has name like
  # /Initial. /SINGH. 
  # Common name cutoff is probably going to be important. But I feel its a bad way to go about it. 
  # some candidates have a substring in a () in the raw name var, like /name/ (mbbs).

### THINGS TO DO TOMORROW
  # Edit state level chart. Cannot count up is_decoy for number of decoys. 
  # Make Candidate pair df complete by adding education and other characteristics. Most importantly, votes. 
  # Make charts and plots for jw, ngram. 
  # Compare jw, ngram with lv. 
  # Run weighted score. 
  # Make all the same charts as before. 

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
  scales,
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
  patchwork, 
  scales
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
  group_by(Year, Constituency_Name, State_Name, Election_Type, Assembly_No) %>%
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
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  distinct()

# ngram character
create_char_ngrams <- function(text, n = 2) {
  text <- gsub(" ", "_", text)  # replace spaces with underscore to capture word boundaries
  
  if(nchar(text) < n) return(text)
  
  ngrams <- character(nchar(text) - n + 1)
  for(i in 1:(nchar(text) - n + 1)) {
    ngrams[i] <- substr(text, i, i+n-1)
  }
  return(ngrams)
}

# function to calculate n-gram similarity using Jaccard coefficient
calculate_ngram_similarity <- function(str1, str2, n = 2) {
  ngrams1 <- create_char_ngrams(str1, n)
  ngrams2 <- create_char_ngrams(str2, n)
  
  return(calculate_jaccard(ngrams1, ngrams2))
}

# function to calculate normalized lv score
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

# unused but might be cool
weighted_flexible_levenshtein_matrix <- function(strings, common_name_weight = 0.3) {
  n <- length(strings)
  similarity_matrix <- matrix(0, n, n)
  
  # tokenize names
  tokenized_names <- strsplit(strings, "\\s+")
  
  # flatten list of all name parts and count occurrences
  all_names <- unlist(tokenized_names)
  name_freq <- table(all_names)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        similarity_matrix[i, j] <- 1  # same string has perfect similarity
        next
      }
      
      # get tokenized names
      tokens1 <- tokenized_names[[i]]
      tokens2 <- tokenized_names[[j]]
      
      # safety checks for empty or invalid tokens
      if (length(tokens1) == 0 || length(tokens2) == 0 || any(is.na(tokens1)) || any(is.na(tokens2))) {
        similarity_matrix[i, j] <- 0  # No similarity for problematic tokens
        next
      }
      
      # calculate token-level distance matrix
      token_dists <- matrix(NA, length(tokens1), length(tokens2))
      for (k in 1:length(tokens1)) {
        for (l in 1:length(tokens2)) {
          token_dists[k, l] <- stringdist(tokens1[k], tokens2[l], method = "lv")
        }
      }
      
      # normalize token distances to similarities
      token1_lens <- nchar(tokens1)
      token2_lens <- nchar(tokens2)
      
      max_lens <- matrix(NA, length(tokens1), length(tokens2))
      for (k in 1:length(tokens1)) {
        for (l in 1:length(tokens2)) {
          max_lens[k, l] <- max(token1_lens[k], token2_lens[l])
          # Prevent division by zero
          if (max_lens[k, l] == 0) max_lens[k, l] <- 1
        }
      }
      
      # normalize similarity
      normalized_token_similarities <- 1 - (token_dists / max_lens)
      
      # compute similarity with weights
      token_weights <- matrix(1, nrow = length(tokens1), ncol = length(tokens2))
      
      # reduce weight for common names
      for (k in 1:length(tokens1)) {
        for (l in 1:length(tokens2)) {
          if (tokens1[k] %in% names(name_freq) && tokens2[l] %in% names(name_freq)) {
            # only apply reduced weight if both tokens are common
            if (name_freq[tokens1[k]] > 1 && name_freq[tokens2[l]] > 1) {
              token_weights[k, l] <- common_name_weight
            }
          }
        }
      }
      
      # compute weighted similarity
      sum_weights <- sum(token_weights)
      if (sum_weights == 0) sum_weights <- 1  # avoid division by zero
      
      weighted_similarity <- sum(normalized_token_similarities * token_weights) / sum_weights
      
      similarity_matrix[i, j] <- weighted_similarity
    }
  }
  
  # set diagonal to 1 (perfect match)
  diag(similarity_matrix) <- 1
  
  rownames(similarity_matrix) <- strings
  colnames(similarity_matrix) <- strings
  return(similarity_matrix)
}

# function to calculate Jaro-Winkler similarity matrix
jaro_winkler_matrix <- function(strings) {
  n <- length(strings)
  result <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      # calculate Jaro-Winkler similarity (already in [0,1] range)
      jw_sim <- 1 - stringdist(strings[i], strings[j], method = "jw", p = 0.1)
      
      result[i, j] <- jw_sim
      result[j, i] <- jw_sim  # matrix is symmetric
    }
  }
  
  return(result)
}

# function to calculate n-gram similarity matrix
ngram_similarity_matrix <- function(strings, n = 2) {
  len <- length(strings)
  result <- matrix(0, nrow = len, ncol = len)
  
  for (i in 1:(len-1)) {
    for (j in (i+1):len) {
      # calculate n-gram similarity
      ng_sim <- calculate_ngram_similarity(strings[i], strings[j], n)
      
      result[i, j] <- ng_sim
      result[j, i] <- ng_sim  # matrix is symmetric
    }
  }
  
  return(result)
}

# pre-processing the name string
all_states_elections$Candidate_clean <- tolower(all_states_elections$Candidate)
all_states_elections$Candidate_clean <- gsub("(shri|smt|dr|prof|mr|mrs|adv)\\.?\\s*", "", all_states_elections$Candidate_clean, ignore.case = TRUE)
all_states_elections$Candidate_clean <- gsub("(advocate|(advocate))", "", all_states_elections$Candidate_clean)

# remove periods after initials at the start of names
all_states_elections$Candidate_clean <- gsub("([A-Za-z])\\. ?([A-Za-z])\\.", "\\1 \\2", all_states_elections$Candidate_clean)
all_states_elections$Candidate_clean <- gsub("([A-Za-z])\\. ([A-Za-z][a-z]+)", "\\1 \\2", all_states_elections$Candidate_clean)

# remove trailing period at the end of a name
all_states_elections$Candidate_clean <- gsub("\\.$", "", all_states_elections$Candidate_clean)

# add space after single-letter initials fused with names (e.g., "a.raghu" → "a raghu")
all_states_elections$Candidate_clean <- gsub("([A-Za-z])\\.([A-Za-z])", "\\1 \\2", all_states_elections$Candidate_clean)

# remove unnecessary periods that appear at the end of a word but before a space (e.g., "nishidha. m" → "nishidha m")
all_states_elections$Candidate_clean <- gsub("\\. ([A-Za-z])", " \\1", all_states_elections$Candidate_clean)

# remove () and things inside it
all_states_elections$Candidate_clean <- gsub("\\s*\\([^)]*\\)", "", all_states_elections$Candidate_clean)

# pre-processing to drop NOTA, no name, no voteshare
all_states_elections <- all_states_elections %>%
  filter(!is.na(Vote_Share_Percentage) | Vote_Share_Percentage != "") %>%
  filter(!is.na(pid) | pid != "") %>%
  filter(Candidate != "NOTA" | Party != "NOTA")

# testing for a subset first
subset <- all_states_elections[1:1000, ]

unique_elections <- subset %>%
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

  # print progress every 100 elections
  if (i %% 100 == 0 || i == 1 || i == total_elections) {
    current_time <- Sys.time()
    time_elapsed_total <- as.numeric(difftime(current_time, start_time_total, units = "secs"))

    cat("Processing election", i, "of", total_elections,
        "(", round(i/total_elections*100, 1), "%)\n")
    cat("  Total elapsed time:", format_time(time_elapsed_total), "\n\n")
  }

  # filter data for current election
  election_data <- subset %>%
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

  # get candidate names
  candidate_names <- election_data$Candidate_clean

  # calculate all similarity matrices
  levenshtein_matrix <- normalized_levenshtein_matrix(candidate_names)
  jw_matrix <- jaro_winkler_matrix(candidate_names)
  wlv_matrix <- weighted_flexible_levenshtein_matrix(candidate_names)
  ngram_matrix <- ngram_similarity_matrix(candidate_names, n = 2)

  # create all pairs of candidates
  for (j in 1:(nrow(election_data) - 1)) {
    for (k in (j+1):nrow(election_data)) {
      # get candidate details
      candidate1 <- election_data[j, ]
      candidate2 <- election_data[k, ]

      # get similarity scores from matrices
      lv_similarity <- levenshtein_matrix[j, k]
      jw_similarity <- jw_matrix[j, k]
      wlv_similarity <- wlv_matrix[j, k]
      ngram_similarity <- ngram_matrix[j, k]

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
        Levenshtein_Similarity = lv_similarity,
        Jaro_Winkler_Similarity = jw_similarity,
        Weighted_Levenshtein_Similarity = wlv_similarity,
        NGram_Similarity = ngram_similarity
      )

      # add to results
      result_list[[list_index]] <- pair_row
      list_index <- list_index + 1
    }
  }
}

candidate_pairs <- do.call(rbind, result_list)

### Calculating similarity scores for all candidates
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
  
  # print progress every 100 elections
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
  
  # get candidate names
  candidate_names <- election_data$Candidate_clean
  
  # calculate all similarity matrices
  levenshtein_matrix <- normalized_levenshtein_matrix(candidate_names)
  jw_matrix <- jaro_winkler_matrix(candidate_names)
  ngram_matrix <- ngram_similarity_matrix(candidate_names, n = 2)
  
  # create all pairs of candidates
  for (j in 1:(nrow(election_data) - 1)) {
    for (k in (j+1):nrow(election_data)) {
      # get candidate details
      candidate1 <- election_data[j, ]
      candidate2 <- election_data[k, ]
      
      # get similarity scores from matrices
      lv_similarity <- levenshtein_matrix[j, k]
      jw_similarity <- jw_matrix[j, k]
      ngram_similarity <- ngram_matrix[j, k]
      
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
        Levenshtein_Similarity = lv_similarity,
        Jaro_Winkler_Similarity = jw_similarity,
        NGram_Similarity = ngram_similarity
      )
      
      result_list[[list_index]] <- pair_row
      list_index <- list_index + 1
    }
  }
  
  # save progress every 1000 elections
  if (i %% 1000 == 0) {
    saveRDS(result_list, "partial_results_list.rds")
    cat("Saved progress at election", i, "\n")
  }
}

end_time_total <- Sys.time()
total_time <- as.numeric(difftime(end_time_total, start_time_total, units = "secs"))

# Create the final dataframe
candidate_pairs <- bind_rows(result_list)
candidate_pairs <- candidate_pairs %>%
  mutate(Pair_Type = ifelse(Pair_Type == "minor-main", "main-minor", Pair_Type))
write_csv(candidate_pairs, "Cleaned Data/candidate_pairs_lv_jw_ngram.csv")
  # phew this took too long. 

# Final Cleaning Check
repeated_pairs_check <- candidate_pairs %>%
  filter(Year == 2019 & State_Name == "Andhra_Pradesh" & Constituency_Name == "ICHCHAPURAM" & Election_Type == "State Assembly Election (AE)" 
         & Assembly_No == 15)
  # nope! 

candidate_pairs <- candidate_pairs %>%
  filter(Candidate1_Type != "NA" & Candidate2_Type != "NA")

pair_type_summary <- candidate_pairs %>%
  group_by(Pair_Type) %>%
  dplyr::summarize(
    Count = n(),
    Avg_LV = mean(Levenshtein_Similarity),
    Avg_JW = mean(Jaro_Winkler_Similarity), 
    Avg_NG = mean(NGram_Similarity),
    High_LV_Count = sum(Levenshtein_Similarity > 0.53)
  )

candidate_pairs <- candidate_pairs %>%
  mutate(is_decoy_lv = ifelse(Levenshtein_Similarity > 0.53, TRUE, FALSE))

join_keys_1 <- c(
  "Candidate1_PID",
  "Year",
  "State_Name",
  "Constituency_Name",
  "Election_Type",
  "Assembly_No"
)

join_keys_2 <- c(
  "Candidate2_PID",
  "Year",
  "State_Name",
  "Constituency_Name",
  "Election_Type",
  "Assembly_No"
)

# Prepare all_states_elections for joining with Candidate1
candidate_info_1 <- all_states_elections %>%
  rename(Candidate1_PID = pid) %>%
  rename_with(~ paste0("Candidate1_", .), -c(Candidate1_PID, Year, State_Name, Constituency_Name, Election_Type, Assembly_No)) %>%
  dplyr::select(-Candidate1_Votes, -Candidate1_Party)

# Prepare all_states_elections for joining with Candidate2
candidate_info_2 <- all_states_elections %>%
  rename(Candidate2_PID = pid) %>%
  rename_with(~ paste0("Candidate2_", .), -c(Candidate2_PID, Year, State_Name, Constituency_Name, Election_Type, Assembly_No)) %>%
  dplyr::select(-Candidate2_Votes, -Candidate2_Party)

# Perform the join using all the necessary keys for Candidate1
candidate_pairs_merged <- candidate_pairs %>%
  left_join(candidate_info_1, by = join_keys_1) %>%
  left_join(candidate_info_2, by = join_keys_2)

candidate_pairs_merged <- candidate_pairs_merged %>%
  dplyr::select(-is_decoy)

### False positive cause multiple decoys
refine_decoy_detection <- function(candidate_pairs_df) {
  # create a unique identifier for each pair
  candidate_pairs_df <- candidate_pairs_df %>%
    mutate(
      # create a unique pair ID by combining both candidate IDs (in alphabetical order)
      pair_id = ifelse(Candidate1_PID < Candidate2_PID,
                       paste(Candidate1_PID, Candidate2_PID, sep = "-"),
                       paste(Candidate2_PID, Candidate1_PID, sep = "-"))
    )
  
  # process only decoy pairs for efficiency
  decoy_pairs <- candidate_pairs_df %>%
    filter(is_decoy_lv == TRUE)
  
  # for each candidate in each election, find their best pair
  best_pairs_c1 <- decoy_pairs %>%
    group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, Candidate1_PID) %>%
    slice_max(order_by = Levenshtein_Similarity, n = 1, with_ties = FALSE) %>%
    dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, pair_id) %>%
    ungroup()
  
  best_pairs_c2 <- decoy_pairs %>%
    group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, Candidate2_PID) %>%
    slice_max(order_by = Levenshtein_Similarity, n = 1, with_ties = FALSE) %>%
    dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, pair_id) %>%
    ungroup()
  
  # combine the best pairs and get unique pair IDs
  best_pair_ids <- bind_rows(best_pairs_c1, best_pairs_c2) %>%
    distinct() %>%
    pull(pair_id)
  
  # create the refined decoy indicator
  candidate_pairs_df <- candidate_pairs_df %>%
    mutate(
      is_decoy_lv_refined = ifelse(pair_id %in% best_pair_ids, is_decoy_lv, FALSE)
    )
  
  return(candidate_pairs_df)
}

candidate_pairs_merged_2 <- refine_decoy_detection(candidate_pairs_merged)

### False positive by common last name
# first get the unedited names and put it in the pairwise df
add_original_names <- function(pairwise_df, raw_candidates) {
  # join for first candidate
  enhanced_df <- pairwise_df %>%
    left_join(
      raw_candidates %>% 
        dplyr::select(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, 
                      pid, Candidate),
      by = c("Year", "State_Name", "Constituency_Name", "Assembly_No", "Election_Type",
             "Candidate1_PID" = "pid")
    ) %>%
    rename(Candidate1_Original_Name = Candidate)
  
  # Join for second candidate
  enhanced_df <- enhanced_df %>%
    left_join(
      raw_candidates %>% 
        dplyr::select(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, 
                      pid, Candidate),
      by = c("Year", "State_Name", "Constituency_Name", "Assembly_No", "Election_Type",
             "Candidate2_PID" = "pid")
    ) %>%
    rename(Candidate2_Original_Name = Candidate)
  
  return(enhanced_df)
}

all_states_elections <- all_states_elections %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, pid, .keep_all = TRUE)

candidate_pairs_merged_2 <- add_original_names(candidate_pairs_merged_2, all_states_elections)

# identify common name parts
find_common_name_parts <- function(df_with_original_names, min_frequency = 10) {
  # extract all unique candidates with their original names
  all_candidates <- rbind(
    df_with_original_names %>%
      dplyr::select(State_Name, Candidate_Name = Candidate1_Name) %>%
      distinct(),
    df_with_original_names %>%
      dplyr::select(State_Name, Candidate_Name = Candidate2_Name) %>%
      distinct()
  ) %>% 
    filter(!is.na(Candidate_Name)) %>%
    distinct()
  
  # extract all name parts by splitting on spaces
  name_parts <- all_candidates %>%
    rowwise() %>%
    mutate(part = list(strsplit(Candidate_Name, "\\s+")[[1]])) %>%
    unnest(part) %>%
    # remove very short parts (like initials)
    filter(nchar(part) > 2)
  
  # count frequency of each name part by state
  common_parts <- name_parts %>%
    group_by(State_Name, part) %>%
    summarise(frequency = n(), .groups = "drop") %>%
    filter(frequency >= min_frequency) %>%
    arrange(State_Name, desc(frequency))
  
  return(common_parts)
}

common_name_parts <- find_common_name_parts(candidate_pairs_merged_2, min_frequency = 15)
common_name_parts %>% arrange(State_Name, desc(frequency))

how_common <- quantile(common_name_parts$frequency, 
                       probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                       na.rm = TRUE)

print(how_common)

common_name_parts_1 <- common_name_parts %>%
  filter(frequency > 182)

# check the decoy classification
check_decoy_status <- function(df_with_original_names, common_name_parts) {
  # keep a copy of all rows
  all_pairs <- df_with_original_names
  
  # focus only on pairs currently marked as decoys to analyze
  decoy_pairs <- df_with_original_names %>%
    filter(is_decoy_lv == TRUE)
  
  # function to check if names share common parts
  check_common_parts <- function(name1, name2, state, common_parts) {
    # handle NA values
    if (is.na(name1) || is.na(name2)) return(FALSE)
    
    # split names into parts
    parts1 <- strsplit(name1, "\\s+")[[1]]
    parts2 <- strsplit(name2, "\\s+")[[1]]
    
    # get common parts for this state
    state_common_parts <- common_parts %>%
      filter(State_Name == state) %>%
      pull(part)
    
    # check if any parts of the names match common parts in the state
    common_in_name1 <- any(parts1 %in% state_common_parts)
    common_in_name2 <- any(parts2 %in% state_common_parts)
    
    # check if they share any common parts
    shared_parts <- intersect(parts1, parts2)
    
    # if they share parts AND those parts are common in the state, 
    # then similarity might be due to common name parts
    return(length(shared_parts) > 0 && 
             any(shared_parts %in% state_common_parts))
  }
  
  # apply check to each decoy pair
  refined_decoys <- decoy_pairs %>%
    rowwise() %>%
    mutate(
      similarity_due_to_common = check_common_parts(
        Candidate1_Name, 
        Candidate2_Name, 
        State_Name, 
        common_name_parts
      ),
      # if similarity is due to common name parts, unmark as decoy
      should_unmark_decoy = similarity_due_to_common
    ) %>%
    ungroup()
  
  return(refined_decoys)
}

decoy_analysis <- check_decoy_status(candidate_pairs_merged_2, common_name_parts_1)

decoy_analysis <- decoy_analysis %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, Candidate1_PID, Candidate2_PID, .keep_all = TRUE)

candidate_pairs_merged_2 <- candidate_pairs_merged_2 %>%
  left_join(
    decoy_analysis %>% 
      dplyr::select(Year, State_Name, Constituency_Name, Assembly_No, Election_Type,
                    Candidate1_PID, Candidate2_PID, should_unmark_decoy),
    by = c("Year", "State_Name", "Constituency_Name", "Assembly_No", "Election_Type",
           "Candidate1_PID", "Candidate2_PID")
  ) %>%
  # create new decoy status (replacing NAs from the join with FALSE)
  mutate(
    should_unmark_decoy = ifelse(is.na(should_unmark_decoy), FALSE, should_unmark_decoy),
    is_decoy_lv_refined_2 = ifelse(is_decoy_lv_refined & should_unmark_decoy, FALSE, is_decoy_lv_refined)
  )

### Plots
# basic summary stats to understand the results
percentiles <- quantile(candidate_pairs_merged_2$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

lev_mean <- mean(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE)
lev_median <- median(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE)

ggplot(candidate_pairs_merged_2, aes(x = Levenshtein_Similarity)) +
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

plot1 <- ggplot(candidate_pairs_merged_2, aes(x = Levenshtein_Similarity)) +
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

stats_table <- data.frame(
  Statistic = c("Mean", "Median", "10th Percentile", "25th Percentile", 
                "50th Percentile", "75th Percentile", "90th Percentile",
                "95th Percentile", "99th Percentile"),
  Value = round(c(lev_mean, lev_median, percentiles), 3)
)

library(scales)
table_grob <- tableGrob(
  stats_table, 
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 8),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    ),
    colhead = list(
      fg_params = list(fontsize = 9, fontface = "bold"),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    )
  )
)

plot1 + 
  annotation_custom(
    table_grob, 
    xmin = quantile(candidate_pairs_merged_2$Levenshtein_Similarity, 0.95, na.rm = TRUE), 
    xmax = max(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE),
    ymin = max(density(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE)$y) * 0.5, 
    ymax = max(density(candidate_pairs_merged_2$Levenshtein_Similarity, na.rm = TRUE)$y) * 0.7
  )

ggsave(
  filename = "Plots/Levenshtein_Kernel_with_stats.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

candidate_pairs_merged_1_main_minor <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "main-minor")

candidate_pairs_merged_1_main_main <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "main-main")

candidate_pairs_merged_1_minor_minor <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "minor-minor")

percentiles <- quantile(candidate_pairs_merged_1_main_minor$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_merged_1_main_minor, aes(x = Levenshtein_Similarity)) +
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

percentiles <- quantile(candidate_pairs_merged_1_main_main$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_merged_1_main_main, aes(x = Levenshtein_Similarity)) +
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

percentiles <- quantile(candidate_pairs_merged_1_minor_minor$Levenshtein_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ggplot(candidate_pairs_merged_1_minor_minor, aes(x = Levenshtein_Similarity)) +
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

# for JW
percentiles <- quantile(candidate_pairs_merged_2$Jaro_Winkler_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

jw_mean <- mean(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE)
jw_median <- median(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE)


plot1 <- ggplot(candidate_pairs_merged_2, aes(x = Jaro_Winkler_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of Jaro-Winkler Similarity with Percentiles",
       x = "Jaro-Winkler Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold") 
  )

stats_table <- data.frame(
  Statistic = c("Mean", "Median", "10th Percentile", "25th Percentile", 
                "50th Percentile", "75th Percentile", "90th Percentile",
                "95th Percentile", "99th Percentile"),
  Value = round(c(jw_mean, jw_median, percentiles), 3)
)

library(scales)
table_grob <- tableGrob(
  stats_table, 
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 8),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    ),
    colhead = list(
      fg_params = list(fontsize = 9, fontface = "bold"),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    )
  )
)

plot1 + 
  annotation_custom(
    table_grob, 
    xmin = quantile(candidate_pairs_merged_2$Jaro_Winkler_Similarity, 0.95, na.rm = TRUE), 
    xmax = max(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE),
    ymin = max(density(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE)$y) * 0.5, 
    ymax = max(density(candidate_pairs_merged_2$Jaro_Winkler_Similarity, na.rm = TRUE)$y) * 0.7
  )

ggsave(
  filename = "Plots/JW_Kernel_with_stats.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# for n-gram
# for JW
percentiles <- quantile(candidate_pairs_merged_2$NGram_Similarity, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), 
                        na.rm = TRUE)

ng_mean <- mean(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE)
ng_median <- median(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE)


plot1 <- ggplot(candidate_pairs_merged_2, aes(x = NGram_Similarity)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.7) +
  geom_vline(xintercept = percentiles, color = "red", linetype = "dashed") +
  annotate("text", x = percentiles, y = rep(0, 7) + 0.05,
           label = c("10th", "25th", "50th", "75th", "90th", "95th", "99th"),
           color = "red", angle = 90, vjust = -0.5) +
  labs(title = "Kernel Density of NGram Similarity with Percentiles",
       x = "NGram Similarity",
       y = "Density") +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold") 
  )

stats_table <- data.frame(
  Statistic = c("Mean", "Median", "10th Percentile", "25th Percentile", 
                "50th Percentile", "75th Percentile", "90th Percentile",
                "95th Percentile", "99th Percentile"),
  Value = round(c(ng_mean, ng_median, percentiles), 3)
)

library(scales)
table_grob <- tableGrob(
  stats_table, 
  rows = NULL,
  theme = ttheme_minimal(
    core = list(
      fg_params = list(fontsize = 8),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    ),
    colhead = list(
      fg_params = list(fontsize = 9, fontface = "bold"),
      bg_params = list(fill = rgb(1, 1, 1, 0.8), col = "black", lwd = 1)
    )
  )
)

plot1 + 
  annotation_custom(
    table_grob, 
    xmin = quantile(candidate_pairs_merged_2$NGram_Similarity, 0.95, na.rm = TRUE), 
    xmax = max(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE),
    ymin = max(density(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE)$y) * 0.5, 
    ymax = max(density(candidate_pairs_merged_2$NGram_Similarity, na.rm = TRUE)$y) * 0.7
  )

ggsave(
  filename = "Plots/NG_Kernel_with_stats.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# summaries at election level
election_decoys <- candidate_pairs_merged_2 %>%
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  summarise(
    main_main_decoys = sum(Pair_Type == "main-main" & is_decoy_lv_refined, na.rm = TRUE),
    main_minor_decoys = sum(Pair_Type == "main-minor" & is_decoy_lv_refined, na.rm = TRUE),
    minor_minor_decoys = sum(Pair_Type == "minor-minor" & is_decoy_lv_refined, na.rm = TRUE),
    .groups = "drop"
  )

max_decoy_info <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "main-minor" & is_decoy_lv_refined) %>%
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

ggsave(
  filename = "Plots/Decoy_histogram.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# adjust for last name
# summaries at election level
election_decoys_2 <- candidate_pairs_merged_2 %>%
  filter(is_decoy_lv_refined_2) %>%
  distinct(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, Candidate2_PID, Pair_Type) %>%
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  summarise(
    main_main_decoys = sum(Pair_Type == "main-main", na.rm = TRUE),
    main_minor_decoys = sum(Pair_Type == "main-minor", na.rm = TRUE),
    minor_minor_decoys = sum(Pair_Type == "minor-minor", na.rm = TRUE),
    .groups = "drop"
  )

max_decoy_info_2 <- candidate_pairs_merged_2 %>%
  filter(Pair_Type == "main-minor" & is_decoy_lv_refined_2) %>%
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

final_election_decoys_2 <- election_decoys_2 %>%
  left_join(max_decoy_info_2, 
            by = c("Year", "State_Name", "Constituency_Name", "Election_Type", "Assembly_No")) %>%
  # handle cases with no decoys
  mutate(
    max_decoys_per_main = replace_na(max_decoys_per_main, 0),
    max_decoys_main_id = if_else(max_decoys_per_main == 0, NA_character_, max_decoys_main_id),
    max_decoys_main_name = if_else(max_decoys_per_main == 0, NA_character_, max_decoys_main_name)
  )

sum(final_election_decoys_2$main_minor_decoys > 3)/nrow(final_election_decoys_2)
sum(final_election_decoys_2$main_minor_decoys > 0)/nrow(final_election_decoys_2)

thresholds <- 0:10

threshold_counts_2 <- data.frame(
  threshold = thresholds,
  elections_count = sapply(thresholds, function(t) {
    sum(final_election_decoys_2$main_minor_decoys > t)
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

threshold_counts_2$percentage <- threshold_counts_2$elections_count / nrow(election_decoys) * 100

ggplot(threshold_counts_2, aes(x = factor(threshold), y = percentage)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Percentage of Elections with Main-Minor Decoys Above Thresholds",
    subtitle = "Decoy pairs with name sub-string frequency > 182 (95th percentile) omitted",
    x = "Number of Main-Minor Decoys (>)",
    y = "Percentage of Elections"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "Plots/Decoy_histogram_lastname_adjustment.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

# by state
str(candidate_pairs_merged_2)

all_states_elections <- all_states_elections %>%
  mutate(MyNeta_education_numeric = case_when(
    MyNeta_education == "Illiterate" ~ 0,
    MyNeta_education == "Literate" ~ 1,
    MyNeta_education == "5th Pass" ~ 5,
    MyNeta_education == "8th Pass" ~ 8,
    MyNeta_education == "10th Pass" ~ 10,
    MyNeta_education == "12th Pass" ~ 12,
    MyNeta_education == "Graduate" ~ 16,
    MyNeta_education == "Graduate Professional" ~ 16,
    MyNeta_education == "Post Graduate" ~ 20,
    MyNeta_education == "Doctorate" ~ 22,
    MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

all_states_elections <- all_states_elections %>%
  mutate(Party_type_numeric = recode(Party_Type_TCPD,
                                     "Independents" = 0,
                                     "Local Party" = 1, 
                                     "State-based Party" = 2, 
                                     "State-based Party (Other State" = 3, 
                                     "National Party" = 4, 
                                     .default = NA_real_
  ))

election_decoys_2 <- candidate_pairs_merged_2 %>%
  filter(is_decoy_lv_refined_2) %>%
  distinct(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, Candidate2_PID, Pair_Type) %>%
  group_by(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  summarise(
    main_main_decoys = sum(Pair_Type == "main-main", na.rm = TRUE),
    main_minor_decoys = sum(Pair_Type == "main-minor", na.rm = TRUE),
    minor_minor_decoys = sum(Pair_Type == "minor-minor", na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics <- candidate_pairs_merged_2 %>%
  group_by(State_Name, Year, Constituency_Name, Election_Type, Assembly_No) %>%
  dplyr::summarize(
    # basic constituency metrics
    total_candidates = max(Candidate1_N_Cand, na.rm = TRUE),
    decoy_candidates = sum(is_decoy_lv_refined & Pair_Type == "main-minor", na.rm = TRUE),
    decoy_share = round(sum(is_decoy_lv_refined & Pair_Type == "main-minor", na.rm = TRUE) / 
                          max(Candidate1_N_Cand, na.rm = TRUE) * 100, 2),
    
    # vote metrics
    total_votes = max(Candidate1_Valid_Votes, na.rm = TRUE),
    
    # count votes to decoys - adjusted to use is_decoy_lv_refined
    total_votes_to_decoys = sum(
      ifelse(is_decoy_lv_refined & Pair_Type == "main-minor", Candidate2_Votes, 
             ifelse(is_decoy_lv_refined & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
      na.rm = TRUE),
    decoy_vote_share = round(
      sum(ifelse(is_decoy_lv_refined & Pair_Type == "main-minor", Candidate2_Votes, 
                 ifelse(is_decoy_lv_refined & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
          na.rm = TRUE) / max(Candidate1_Valid_Votes, na.rm = TRUE) * 100, 2),
    
    # winner and runner-up metrics
    winning_margin = min(Candidate1_Margin[Candidate1_Position == 1], na.rm = TRUE),
    winning_margin_percentage = min(Candidate1_Margin_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    winner_party = first(Candidate1_Party[Candidate1_Position == 1]),
    runner_up_party = first(Candidate1_Party[Candidate1_Position == 2]),
    winner_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    runner_up_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 2], na.rm = TRUE),
    
    # check for decoys associated with winner/runner-up - using is_decoy_lv_refined
    winner_has_decoys = any((is_decoy_lv_refined & Pair_Type == "main-minor" & Candidate1_Position == 1) | 
                              (is_decoy_lv_refined & Pair_Type == "minor-main" & Candidate2_Position == 1), 
                            na.rm = TRUE),
    runner_up_has_decoys = any((is_decoy_lv_refined & Pair_Type == "main-minor" & Candidate1_Position == 2) | 
                                 (is_decoy_lv_refined & Pair_Type == "minor-main" & Candidate2_Position == 2), 
                               na.rm = TRUE),
    
    # education metrics
    education = mean(c(Candidate1_MyNeta_education_numeric, 
                       Candidate2_MyNeta_education_numeric), na.rm = TRUE),
    
    # educ & party for decoys - using is_decoy_lv_refined
    decoy_education = mean(
      c(Candidate2_MyNeta_education_numeric[is_decoy_lv_refined & Pair_Type == "main-minor"],
        Candidate1_MyNeta_education_numeric[is_decoy_lv_refined & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    decoy_party = mean(
      c(Candidate2_Party_type_numeric[is_decoy_lv_refined & Pair_Type == "main-minor"],
        Candidate1_Party_type_numeric[is_decoy_lv_refined & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    
    # educ & party for main with decoys - using is_decoy_lv_refined
    main_with_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[is_decoy_lv_refined & Pair_Type == "main-minor"],
        Candidate2_MyNeta_education_numeric[is_decoy_lv_refined & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    main_with_decoy_party = mean(
      c(Candidate1_Party_type_numeric[is_decoy_lv_refined & Pair_Type == "main-minor"],
        Candidate2_Party_type_numeric[is_decoy_lv_refined & Pair_Type == "minor-main"]), 
      na.rm = TRUE),
    
    # educ & party for main without decoys
    main_without_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[!is_decoy_lv_refined & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_MyNeta_education_numeric[!is_decoy_lv_refined & (Pair_Type == "minor-main" | Pair_Type == "main-main")]), 
      na.rm = TRUE),
    main_without_decoy_party = mean(
      c(Candidate1_Party_type_numeric[!is_decoy_lv_refined & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_Party_type_numeric[!is_decoy_lv_refined & (Pair_Type == "minor-main" | Pair_Type == "main-main")]), 
      na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    close_race = ifelse(winning_margin_percentage < 5, TRUE, FALSE),
    decoy_impact_potential = ifelse(decoy_vote_share > winning_margin_percentage, TRUE, FALSE)
  ) %>%
  # sort by state, year and constituency
  arrange(State_Name, Year, Constituency_Name, Election_Type, Assembly_No)

state_metrics <- constituency_metrics %>%
  group_by(State_Name) %>%
  dplyr::summarize(
    # how many constituencies
    constituency_count = n(),
    
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
    
    # voteshare stuff
    total_votes = sum(total_votes, na.rm = TRUE),
    total_votes_to_decoys = sum(total_votes_to_decoys, na.rm = TRUE),
    overall_decoy_vote_share = (total_votes_to_decoys / total_votes) * 100,
    
    # decoys affect outcomes?
    races_with_decoys = sum(decoy_candidates > 0, na.rm = TRUE),
    close_races = sum(close_race, na.rm = TRUE),
    decoy_impact_races = sum(decoy_impact_potential, na.rm = TRUE),
    
    # calculate races
    pct_races_with_decoys = (races_with_decoys / constituency_count) * 100,
    pct_close_races = (close_races / constituency_count) * 100,
    pct_decoy_impact = (decoy_impact_races / constituency_count) * 100,
    
    # education and party
    decoy_education = mean(decoy_education, na.rm = TRUE), 
    decoy_party = mean(decoy_party, na.rm = TRUE),
    main_with_decoy_education = mean(main_with_decoy_education, na.rm = TRUE),
    main_with_decoy_party = mean(main_with_decoy_party, na.rm = TRUE),
    main_without_decoy_education = mean(main_without_decoy_education, na.rm = TRUE),
    main_without_decoy_party = mean(main_without_decoy_party, na.rm = TRUE),
    
    decoy_effectiveness = (overall_decoy_vote_share * decoy_impact_races) / constituency_count,
    .groups = "drop"
  )

state_metrics %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_decoy_share)) %>%
  ggplot(aes(x = State_Name, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_decoy_share, ymax = p90_decoy_share), width = 0.2) +
  geom_point(aes(y = max_decoy_share), color = "red", size = 2) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),
    labels = scales::percent_format(scale = 1)
  ) + 
  labs(
    title = "Average Share of Decoy Candidates per Election by State",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "State",
    y = "Proportion of Decoy Candidates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

# by state
election_decoys_2 <- election_decoys_2 %>%
  left_join(
    constituency_metrics %>% 
      dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No, total_candidates),
    by = c("Year", "State_Name", "Constituency_Name", "Assembly_No", "Election_Type")
  ) 

election_decoys_2 <- election_decoys_2 %>%
  mutate(decoy_share = main_minor_decoys/total_candidates)

state_metrics_1 <- election_decoys_2 %>%
  group_by(State_Name) %>%
  dplyr::summarize(
    # how many constituencies
    constituency_count = n(),
    
    # number of decoys
    mean_num_decoys = mean(main_minor_decoys, na.rm = TRUE),
    median_num_decoys = median(main_minor_decoys, na.rm = TRUE),
    p90_num_decoys = quantile(main_minor_decoys, 0.9, na.rm = TRUE),
    max_num_decoys = max(main_minor_decoys, na.rm = TRUE),
    
    # share of decoys
    mean_decoy_share = mean(decoy_share, na.rm = TRUE),
    median_decoy_share = median(decoy_share, na.rm = TRUE),
    p90_decoy_share = quantile(decoy_share, 0.9, na.rm = TRUE),
    max_decoy_share = max(decoy_share, na.rm = TRUE),
    
    .groups = "drop"
  )

state_metrics_1 %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_decoy_share)) %>%
  ggplot(aes(x = State_Name, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_decoy_share, ymax = p90_decoy_share), width = 0.2) +
  geom_point(aes(y = max_decoy_share), color = "red", size = 2) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = percent_format()) + 
  labs(
    title = "Average Share of Decoy Candidates per Election by State",
    subtitle = "With median (bottom of bar), 90th percentile (top of bar), and maximum (red dot)",
    x = "State",
    y = "Proportion of Decoy Candidates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9)
  )

str(election_decoys)
str(constituency_metrics)

# Saving
write_csv(candidate_pairs_merged_2, "Cleaned Data/candidate_pairs_lv_jw_ngram.csv")
write.csv(all_states_elections, "Raw Data/all_states_elections.csv")
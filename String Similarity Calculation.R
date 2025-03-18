# ============================================
# Political Decoys - String Similarity Calculation
# ============================================
# Date: 11/03/25
# Author: Anirvin Narayan

### NOTES
# There are some issues with the cleaning --> some initial are still there like 
  # manirathinem. .k i in kattumannarkoil TN 2016 AE. 

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
  reticulate,
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
  PGRdup,
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
  scales, 
  phonics
)

### Loading
all_states_elections <- read.csv("Raw Data/all_states_elections.csv")
candidate_pairs_old <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram.csv")
py_run_string('
import sys
sys.path.append("masala-merge-master/")
from lev import levenshtein
')

### Understanding the Data
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

unique_elections <- all_states_elections %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  distinct()

### String Similarity Calculation Formula

# 1. Levenshtein
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

# 2. Jaco-Winkler
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

# 3. N-Gram (Bigram)
# jaccard
calculate_jaccard <- function(set1, set2) {
  # get unique elements in each set
  set1 <- unique(set1)
  set2 <- unique(set2)
  
  # find intersection and union
  intersection_size <- length(intersect(set1, set2))
  union_size <- length(union(set1, set2))
  
  # calculate Jaccard coefficient
  if (union_size == 0) return(0)
  
  return(intersection_size / union_size)
}

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

# 4. Masala-Merge
masala_distance <- function(str1, str2, max_distance = 10) {
  # call the Python function
  dist <- py$levenshtein(str1, str2, distance = max_distance)
  
  # apply first letter penalty (as in the original code)
  if (nchar(str1) > 0 && nchar(str2) > 0 && substr(str1, 1, 1) != substr(str2, 1, 1)) {
    dist <- dist + py$levenshtein(substr(str1, 1, 1), substr(str2, 1, 1), distance = max_distance)
  }
  
  return(dist)
}

masala_distance("singapore", "singapura", 2000000)

# function to create a matrix
masala_similarity_matrix <- function(names, max_name_length = 100) {
  n <- length(names)
  sim_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        sim_matrix[i, j] <- 1
      } else if (i < j) {  # only calculate once for each pair
        # calculate Masala-Levenshtein distance
        distance <- masala_distance(names[i], names[j])
        
        # convert to similarity (0-1 scale)
        # use the maximum name length for normalization, with a cap
        max_len <- min(max(nchar(names[i]), nchar(names[j])), max_name_length)
        similarity <- max(0, 1 - (distance / max_len))
        
        sim_matrix[i, j] <- similarity
        sim_matrix[j, i] <- similarity
      }
    }
  }
  
  return(sim_matrix)
}

# 5. Double Metaphone
double_metaphone_matrix <- function(names, max_name_length = 100) {
  mp_codes <- PGRdup::DoubleMetaphone(names)
  n <- length(names)
  
  sim_matrix <- matrix(0, n, n)
  rownames(sim_matrix) <- names
  colnames(sim_matrix) <- names
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        # same name, maximum similarity
        sim_matrix[i, j] <- 1
      } else if (i < j) {  # only calculate once for each pair
        # get primary and alternate codes
        mp_i_primary <- mp_codes$primary[i]
        mp_i_alternate <- mp_codes$alternate[i]
        mp_j_primary <- mp_codes$primary[j]
        mp_j_alternate <- mp_codes$alternate[j]
        
        # calculate Levenshtein distances between all combinations
        distances <- c(
          stringdist::stringdist(mp_i_primary, mp_j_primary, method = "lv")
        )
        
        # add distances for alternate codes if they exist
        if (mp_i_alternate != "" && mp_j_primary != "") {
          distances <- c(distances, 
                         stringdist::stringdist(mp_i_alternate, mp_j_primary, method = "lv"))
        }
        if (mp_i_primary != "" && mp_j_alternate != "") {
          distances <- c(distances, 
                         stringdist::stringdist(mp_i_primary, mp_j_alternate, method = "lv"))
        }
        if (mp_i_alternate != "" && mp_j_alternate != "") {
          distances <- c(distances, 
                         stringdist::stringdist(mp_i_alternate, mp_j_alternate, method = "lv"))
        }
        
        # use the minimum distance
        min_distance <- min(distances)
        
        # calculate maximum possible metaphone length
        max_mp_len <- max(
          nchar(mp_i_primary), 
          nchar(mp_i_alternate), 
          nchar(mp_j_primary), 
          nchar(mp_j_alternate)
        )
        
        # ensure we have a valid divisor (avoid division by zero)
        max_mp_len <- max(max_mp_len, 1)
        
        # convert to similarity (0-1 scale)
        similarity <- max(0, 1 - (min_distance / max_mp_len))
        
        # fill in symmetric matrix
        sim_matrix[i, j] <- similarity
        sim_matrix[j, i] <- similarity
      }
    }
  }
  
  return(sim_matrix)
}


### Cleaning Data for Processing
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

# making sure no duplicates
all_states_elections <- all_states_elections %>%
  distinct(Year, State_Name, Constituency_Name, Assembly_No, Election_Type, pid, .keep_all = TRUE)

unique(all_states_elections$Vote_Share_Percentage)

### Calculating and Storing in a pair-wise df
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

subset <- all_states_elections[1:1000, ]

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
  masala_matrix <- masala_similarity_matrix(candidate_names)
  
  # add phonetic matrices
  metaphone_mat <- double_metaphone_matrix(candidate_names)
  
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
      masala_similarity <- masala_matrix[j, k]
      metaphone_similarity <- metaphone_mat[j, k] 
      
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
        NGram_Similarity = ngram_similarity,
        Masala_Similarity = masala_similarity,
        Metaphone_Similarity = metaphone_similarity
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

# Create the final dataframe and filling it with candidate info
candidate_pairs <- bind_rows(result_list)

pair_type_summary <- candidate_pairs %>%
  group_by(Pair_Type) %>%
  dplyr::summarize(
    Count = n(),
    Avg_LV = mean(Levenshtein_Similarity),
    Avg_JW = mean(Jaro_Winkler_Similarity), 
    Avg_NG = mean(NGram_Similarity),
    Avg_Met = mean(Metaphone_Similarity)
  )

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

# False positive
pair_type_summary <- candidate_pairs_merged_2 %>%
  group_by(Pair_Type) %>%
  dplyr::summarize(
    Count = n(),
    Avg_LV = mean(Levenshtein_Similarity),
    Avg_JW = mean(Jaro_Winkler_Similarity), 
    Avg_NG = mean(NGram_Similarity),
    High_LV_Count = sum(is_decoy_lv_refined),
    fp_rate = High_LV_Count / Count
  )

# Saving
write_csv(candidate_pairs_merged, "Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet.csv")
write.csv(all_states_elections, "Raw Data/all_states_elections.csv")
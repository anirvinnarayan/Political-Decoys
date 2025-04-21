# ============================================
# Political Decoys - Processing CLEA Data and String Similarity Calculation
# ============================================
# Date: 09/04/25
# Author: Anirvin Narayan

### Big Notes: There are some issues
# First of all not all elections were processed. Check which werent and append accordingly. 
# Second, for some reason this shit takes time. Figure out why. 

rm(list = ls())
setwd("/Users/anirvin/Downloads/Political Decoys Data")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  readr,
  lubridate,
  readr, 
  reticulate,
  scales,
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
  phonics,
  haven, 
  naniar, 
  skimr
)

### Loading
clea_data <- read_dta("Raw Data/CLEA/clea_lc_20201216_stata/clea_lc_20201216.dta")
py_run_string('
import sys
sys.path.append("masala-merge-master/")
from lev import levenshtein
')
result_list <- readRDS("partial_results_list.rds")
candidate_pairs_CLEA <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA.csv")

### Cleaning
# what's up with this dataset?
str(clea_data)
dim(clea_data)

get_value_labels <- function(data) {
  result <- list()
  for(col in names(data)) {
    labels <- attr(data[[col]], "labels")
    if(!is.null(labels)) {
      result[[col]] <- labels
    }
  }
  return(result)
}

# Get all value labels
all_labels <- get_value_labels(clea_data)
print(all_labels)

summary(clea_data)

# cool but not useful
skim(clea_data)

# how many elections? 
unique_elections <- clea_data %>%
  dplyr::select(ctr, yr, mn, cst_n, id) %>%
  distinct()

# candidates 
table(clea_data$can, useNA = "always")
class(clea_data$can)

# if candidate is a number, then we remove it
clea_data_filtered <- clea_data %>%
  filter(!str_detect(can, "^-?\\d+$"))

# check if there are any numbers left
remaining_candidates <- clea_data_filtered %>%
  filter(str_detect(can, "^-?\\d+$"))

# remove if constituency name is a number
number_const <- clea_data_filtered %>%
  filter(str_detect(cst_n, "^-?\\d+$"))

number_const_sub <- clea_data_filtered %>%
  filter(if_any(c(sub, cst_n), ~str_detect(., "^-?\\d+$")))

# remove if constituency name is a number
clea_data_filtered <- clea_data_filtered %>%
  filter(!str_detect(cst_n, "^-?\\d+$"))

country_const <- clea_data_filtered %>%
  filter(str_detect(ctr_n, "^-?\\d+$"))

# vote share stuff
sum(is.na(clea_data$cvs1) & is.na(clea_data$cvs2))
sum(is.na(clea_data$cvs1))

# drop if voteshare is missing
clea_data_filtered <- clea_data_filtered %>%
  filter(!(cvs1 %in% c(-990, -992, -994)))

sum(clea_data_filtered$cvs2 == -990 |clea_data_filtered$cvs2 == -992 | clea_data_filtered$cvs2 == -994)

where_da_fk_are_there_second_rounds <- clea_data_filtered %>%
  filter(cvs2 != -990 & cvs2 != -992 & cvs2 != -994)

# drop if voteshare is <0 or >100
clea_data_filtered <- clea_data_filtered %>%
  filter(cvs1 >= 0 & cvs1 <= 1)

range(clea_data_filtered$cvs1, na.rm = TRUE)

clea_data_filtered <- clea_data_filtered %>%
  filter(cvs2 <= 1)

### Understanding the Data
how_many_main_min <- clea_data_filtered %>%
  group_by(ctr, yr, mn, cst_n, id) %>%
  summarise(
    main_cand = sum(replace_na(cvs1, 0) > 0.1), 
    minor_cand = sum(replace_na(cvs1, 0) <= 0.1)
  ) %>%
  ungroup()

main_min_year <- how_many_main_min %>%
  group_by(yr) %>%
  summarise(avg_main_cand = mean(main_cand, na.rm = TRUE),
            avg_minor_cand = mean(minor_cand, na.rm = TRUE)) 

ggplot(main_min_year, aes(x = yr)) +
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

unique_elections <- clea_data_filtered %>%
  dplyr::select(ctr, yr, mn, cst_n, id) %>%
  distinct()

# if the candidate name has a number drop it 
clea_data_filtered <- clea_data_filtered[!grepl("[0-9]", clea_data_filtered$can), ]

# drop if there is only 1 candidate in an election
clea_data_filtered <- clea_data_filtered %>%
  group_by(ctr, yr, mn, cst_n, id) %>%
  filter(n() > 1) %>%
  ungroup()

# what about elections with too many candidates? 
unique_elections <- clea_data_filtered %>%
  dplyr::select(ctr, yr, mn, cst_n, id) %>%
  # summarize and count number of candidates per election
  group_by(ctr, yr, mn, cst_n, id) %>%
  summarise(
    num_candidates = n(),
    .groups = "drop"
  ) %>%
  distinct()

ggplot(unique_elections, aes(x = num_candidates)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal()

quantile(unique_elections$num_candidates, probs = seq(0, 1, 0.01))

sum(unique_elections$num_candidates > 50)

# drop if more than 50 candidates for computational efficiency
clea_data_filtered <- clea_data_filtered %>%
  group_by(ctr, yr, mn, cst_n, id) %>%
  filter(n() < 50) %>%
  ungroup()

# save this
write_csv(clea_data_filtered, "Cleaned Data/CLEA_cleaned.csv")

unique_elections_india <- clea_data_filtered %>%
  filter(ctr_n == "India") %>%
  dplyr::select(ctr, yr, mn, cst_n, id) %>%
  distinct()

### String Similarity Calculation Formulae
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
  # Create empty results if DoubleMetaphone fails
  n <- length(names)
  
  # Try to get metaphone codes with proper error handling
  mp_codes <- tryCatch({
    suppressWarnings(PGRdup::DoubleMetaphone(names))
  }, error = function(e) {
    # If there's an error, create empty placeholder results
    list(primary = rep("", n), alternate = rep("", n))
  })
  
  # Check if mp_codes has the expected structure
  if (!all(c("primary", "alternate") %in% names(mp_codes))) {
    mp_codes <- list(primary = rep("", n), alternate = rep("", n))
  }
  
  # Initialize similarity matrix
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
clea_data_filtered$Candidate_clean <- tolower(clea_data_filtered$can)
clea_data_filtered$Candidate_clean <- gsub("(shri|smt|dr|prof|mr|mrs|adv)\\.?\\s*", "", clea_data_filtered$Candidate_clean, ignore.case = TRUE)
clea_data_filtered$Candidate_clean <- gsub("(advocate|(advocate))", "", clea_data_filtered$Candidate_clean)

# remove periods after initials at the start of names
clea_data_filtered$Candidate_clean <- gsub("([A-Za-z])\\. ?([A-Za-z])\\.", "\\1 \\2", clea_data_filtered$Candidate_clean)
clea_data_filtered$Candidate_clean <- gsub("([A-Za-z])\\. ([A-Za-z][a-z]+)", "\\1 \\2", clea_data_filtered$Candidate_clean)

# remove trailing period at the end of a name
clea_data_filtered$Candidate_clean <- gsub("\\.$", "", clea_data_filtered$Candidate_clean)

# add space after single-letter initials fused with names (e.g., "a.raghu" → "a raghu")
clea_data_filtered$Candidate_clean <- gsub("([A-Za-z])\\.([A-Za-z])", "\\1 \\2", clea_data_filtered$Candidate_clean)

# remove unnecessary periods that appear at the end of a word but before a space (e.g., "nishidha. m" → "nishidha m")
clea_data_filtered$Candidate_clean <- gsub("\\. ([A-Za-z])", " \\1", clea_data_filtered$Candidate_clean)

# remove () and things inside it
clea_data_filtered$Candidate_clean <- gsub("\\s*\\([^)]*\\)", "", clea_data_filtered$Candidate_clean)

# should have removed "," and other punctuation. very stupid. 

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

unique_elections <- clea_data_filtered %>%
  dplyr::select(ctr, yr, mn, cst_n, id) %>%
  # summarize and count number of candidates per election
  group_by(ctr, yr, mn, cst_n, id) %>%
  summarise(
    num_candidates = n(),
    .groups = "drop"
  ) %>%
  distinct()

set.seed(222)

unique_elections_subset <- clea_data_filtered %>%
  distinct(ctr, yr, mn, cst_n, id) %>%
  sample_n(100)

# filter 100 random elections
subset <- clea_data_filtered %>%
  semi_join(unique_elections_subset, by = c("ctr", "yr", "mn", "cst_n", "id"))

# # did subset, seems ok now doing for actual
# candidate_pairs <- data.frame()
# result_list <- list()
# list_index <- 1
# 
# # tracking progress
# total_elections <- nrow(unique_elections)
# cat("Processing", total_elections, "unique elections\n")
# 
# start_time_total <- Sys.time()
# last_batch_size <- 100
# 
# str(clea_data_filtered)
# 
# # loop through each election
# for (i in 1:nrow(unique_elections)) {
#   # election details
#   year <- unique_elections$yr[i]
#   country <- unique_elections$ctr[i]
#   constituency <- unique_elections$cst_n[i]
#   month <- unique_elections$mn[i]
#   id <- unique_elections$id[i]
#   
#   # print progress every 100 elections
#   if (i %% 100 == 0 || i == 1 || i == total_elections) {
#     current_time <- Sys.time()
#     time_elapsed_total <- as.numeric(difftime(current_time, start_time_total, units = "secs"))
#     
#     cat("Processing election", i, "of", total_elections, 
#         "(", round(i/total_elections*100, 1), "%)\n")
#     cat("  Total elapsed time:", format_time(time_elapsed_total), "\n\n")
#   }
#   
#   # filter data for current election
#   election_data <- clea_data_filtered %>%
#     filter(yr == year,
#            ctr == country,
#            cst_n == constituency,
#            mn == month,
#            id == id)
#   
#   # skip if fewer than 2 candidates
#   if (nrow(election_data) < 2) {
#     next
#   }
#   
#   # mark candidates as main or minor
#   election_data <- election_data %>%
#     mutate(candidate_type = ifelse(cvs1 * 100 > 10, "main", "minor"))
#   
#   # get candidate names
#   candidate_names <- election_data$Candidate_clean
#   
#   # calculate all similarity matrices
#   levenshtein_matrix <- normalized_levenshtein_matrix(candidate_names)
#   jw_matrix <- jaro_winkler_matrix(candidate_names)
#   ngram_matrix <- ngram_similarity_matrix(candidate_names, n = 2)
#   masala_matrix <- masala_similarity_matrix(candidate_names)
#   
#   # add phonetic matrices
#   metaphone_mat <- double_metaphone_matrix(candidate_names)
#   
#   # create all pairs of candidates
#   for (j in 1:(nrow(election_data) - 1)) {
#     for (k in (j+1):nrow(election_data)) {
#       # get candidate details
#       candidate1 <- election_data[j, ]
#       candidate2 <- election_data[k, ]
#       
#       # get similarity scores from matrices
#       lv_similarity <- levenshtein_matrix[j, k]
#       jw_similarity <- jw_matrix[j, k]
#       ngram_similarity <- ngram_matrix[j, k]
#       masala_similarity <- masala_matrix[j, k]
#       metaphone_similarity <- metaphone_mat[j, k] 
#       
#       # determine pair type
#       pair_type <- paste0(candidate1$candidate_type, "-", candidate2$candidate_type)
#       
#       # create row for this pair
#       pair_row <- data.frame(
#         Year = year,
#         Country_Code = country,
#         Constituency_Name = constituency,
#         Election_ID = id,
#         Election_Month = month,
#         
#         Candidate1_Name = candidate1$Candidate_clean,
#         Candidate1_Party_Code = candidate1$pty,
#         Candidate1_Party_Name = candidate1$pty_n,
#         Candidate1_Votes = candidate1$cv1,
#         Candidate1_VoteShare = candidate1$cvs1 * 100, # proportion to percentage
#         Candidate1_Type = candidate1$candidate_type,
#         
#         Candidate2_Name = candidate2$Candidate_clean,
#         Candidate2_Party_Code = candidate2$pty,
#         Candidate2_Party_Name = candidate2$pty_n,
#         Candidate2_Votes = candidate2$cv1,
#         Candidate2_VoteShare = candidate2$cvs1 * 100, # proportion to percentage
#         Candidate2_Type = candidate2$candidate_type,
#         
#         Pair_Type = pair_type,
#         Levenshtein_Similarity = lv_similarity,
#         Jaro_Winkler_Similarity = jw_similarity,
#         NGram_Similarity = ngram_similarity,
#         Masala_Similarity = masala_similarity,
#         Metaphone_Similarity = metaphone_similarity
#       )
#       
#       result_list[[list_index]] <- pair_row
#       list_index <- list_index + 1
#     }
#   }
#   
#   # save progress every 1000 elections
#   if (i %% 1000 == 0) {
#     saveRDS(result_list, "partial_results_list.rds")
#     cat("Saved progress at election", i, "\n")
#   }
# }
# 
# end_time_total <- Sys.time()
# total_time <- as.numeric(difftime(end_time_total, start_time_total, units = "secs"))

candidate_pairs <- data.frame()
total_elections <- nrow(unique_elections)

start_time_total <- Sys.time()
last_batch_size <- 100

### just double checking, do we have as many elections in partials list 
# create a function to extract election identifiers
get_election_id <- function(result_list) {
  if(is.data.frame(result_list)) {
    return(paste(result_list$Year, 
                 result_list$Country_Code, 
                 result_list$Constituency_Name, 
                 result_list$Election_Month, 
                 result_list$Election_ID, 
                 sep = "_"))
  } else {
    return(NA)
  }
}

# ext ract unique election identifiers
election_ids <- sapply(result_list, get_election_id)
unique_election_ids <- unique(election_ids[!is.na(election_ids)])

# count of unique elections
num_elections <- length(unique_election_ids)

list_index <- length(result_list) + 1  # continue from where we left off

# get the last item in the result list
last_result <- result_list[[length(result_list)]]

# find the corresponding index in unique_elections
last_processed_i <- which(unique_elections$yr == last_result$Year &
                            unique_elections$ctr == last_result$Country_Code &
                            unique_elections$cst_n == last_result$Constituency_Name &
                            unique_elections$mn == last_result$Election_Month &
                            unique_elections$id == last_result$Election_ID)

starting_i <- last_processed_i + 1
total_elections <- nrow(unique_elections)
start_time_total <- Sys.time()  # reset the timer for the new session
cat("Restarting from election", starting_i, "of", total_elections, "\n")


for (i in starting_i:nrow(unique_elections)) {
  # election details
  year <- unique_elections$yr[i]
  country <- unique_elections$ctr[i]
  constituency <- unique_elections$cst_n[i]
  month <- unique_elections$mn[i]
  id <- unique_elections$id[i]

  # print progress every 100 elections
  if (i %% 100 == 0 || i == starting_i || i == total_elections) {
    current_time <- Sys.time()
    time_elapsed_total <- as.numeric(difftime(current_time, start_time_total, units = "secs"))

    cat("Processing election", i, "of", total_elections,
        "(", round(i/total_elections*100, 1), "%)\n")
    cat("  Total elapsed time:", format_time(time_elapsed_total), "\n\n")
  }

  # filter data for current election
  election_data <- clea_data_filtered %>%
    filter(yr == year,
           ctr == country,
           cst_n == constituency,
           mn == month,
           id == id)

  # skip if fewer than 2 candidates
  if (nrow(election_data) < 2) {
    next
  }

  # mark candidates as main or minor
  election_data <- election_data %>%
    mutate(candidate_type = ifelse(cvs1 * 100 > 10, "main", "minor"))

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
        Country_Code = country,
        Constituency_Name = constituency,
        Election_ID = id,
        Election_Month = month,

        Candidate1_Name = candidate1$Candidate_clean,
        Candidate1_Party_Code = candidate1$pty,
        Candidate1_Party_Name = candidate1$pty_n,
        Candidate1_Votes = candidate1$cv1,
        Candidate1_VoteShare = candidate1$cvs1 * 100, # proportion to percentage
        Candidate1_Type = candidate1$candidate_type,

        Candidate2_Name = candidate2$Candidate_clean,
        Candidate2_Party_Code = candidate2$pty,
        Candidate2_Party_Name = candidate2$pty_n,
        Candidate2_Votes = candidate2$cv1,
        Candidate2_VoteShare = candidate2$cvs1 * 100, # proportion to percentage
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

# Create the final dataframe and filling it with candidate info
candidate_pairs <- bind_rows(result_list)

chunk_size <- 1000
total_chunks <- ceiling(length(result_list) / chunk_size)
final_df <- data.frame()

for (i in 1:total_chunks) {
  start_idx <- ((i-1) * chunk_size) + 1
  end_idx <- min(i * chunk_size, length(result_list))
  
  cat("Processing chunk", i, "of", total_chunks, "(items", start_idx, "to", end_idx, ")\n")
  
  # Process one chunk at a time
  chunk_df <- bind_rows(result_list[start_idx:end_idx])
  
  # Either save each chunk
  chunk_filename <- paste0("candidate_pairs_chunk_", i, ".rds")
  saveRDS(chunk_df, chunk_filename)
  
  # Or append to a growing dataframe if memory permits
  final_df <- bind_rows(final_df, chunk_df)
}

# fpr tje rest
chunk_size <- 1000  # Make sure this is the same as before
last_completed_chunk <- 477

# Continue processing from the next chunk
for (i in (last_completed_chunk + 1):total_chunks) {
  start_idx <- ((i-1) * chunk_size) + 1
  end_idx <- min(i * chunk_size, length(result_list))
  
  cat("Processing chunk", i, "of", total_chunks, "\n")
  
  # Process current chunk and append
  chunk_df <- bind_rows(result_list[start_idx:end_idx])
  final_df <- bind_rows(final_df, chunk_df)
}

pair_type_summary <- final_df %>%
  group_by(Pair_Type) %>%
  dplyr::summarize(
    Count = n(),
    Avg_LV = mean(Levenshtein_Similarity),
    Avg_JW = mean(Jaro_Winkler_Similarity), 
    Avg_NG = mean(NGram_Similarity),
    Avg_Met = mean(Metaphone_Similarity)
  )

# Saving
write_csv(final_df, "Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA.csv")

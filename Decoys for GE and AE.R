# ============================================
# Political Decoys - Decoys for GE and AE
# ============================================
# Date: 06/03/25
# Author: Anirvin Narayan

##### NOTES: I just realised, the STATA code create a row per candidate pair. which is a lot of rows. 
# which is very computationally intensive. so, i just calculated what i needed to and the scores of the decoys. 
# and the "mains" with which they corresponded. 

### PROBLEMS TO SOLVE: 
  # sometimes major candidate is also being marked. 
    # this is happening because same candidate can stand in 2 elections, be major in 1 and be minor in another. 
    # so cannot use unique candidate ID - obv. 
  # Bihar (maybe others) have 2 AEs in same year. So, group additionally by Assembly No. 


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

# for first try - note pre-processing and threshold of 0.53
all_states_elections_1 <- all_states_elections
all_states_elections_1$is_decoy <- FALSE
all_states_elections_1$decoy_for_pid <- NA_character_
all_states_elections_1$decoy_for_name <- NA_character_

# list to process the data
result_list <- list()

str(all_states_elections_1)

### FIRST TEST
tn <- all_states_elections_1 %>%
  filter(State_Name == "Tamil_Nadu")

unique_elections <- tn %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  distinct()

# progress
current_state <- ""
states_completed <- c()
total_states <- length(unique(unique_elections$State_Name))
state_counter <- 0

for (i in 1:nrow(unique_elections)) {
  year <- unique_elections$Year[i]
  constituency <- unique_elections$Constituency_Name[i]
  state <- unique_elections$State_Name[i]
  election_type <- unique_elections$Election_Type[i]
  assembly_no <- unique_elections$Assembly_No[i]

  if (state != current_state) {
    if (current_state != "") {
      state_counter <- state_counter + 1
      cat("\nCompleted state:", current_state, "-", state_counter, "of", total_states,
          "states (", round(state_counter/total_states*100, 1), "%)\n")
      states_completed <- c(states_completed, current_state)
    }
    current_state <- state
    cat("\nStarting new state:", current_state, "\n")
  }

  # filter data for current group
  group_data <- tn %>%
    filter(Year == year,
           Constituency_Name == constituency,
           State_Name == state,
           Election_Type == election_type,
           Assembly_No == assembly_no)

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

      # Skip if comparing the same candidate ID
      if (minor_candidate_pid == main_candidate_pid) {
        next
      }

      minor_matrix_idx <- which(group_data$pid == minor_candidate_pid)

      # get the normalized levenshtein similarity using matrix indices
      if (length(main_matrix_idx) == 1 && length(minor_matrix_idx) == 1) {
        lev_similarity <- distance_matrix[main_matrix_idx, minor_matrix_idx]

        # check if this is a potential decoy based on similarity threshold
        if (lev_similarity > 0.53) {
          # CRITICAL CHANGE: Only update rows for this specific election context
          decoy_rows <- which(tn$pid == minor_candidate_pid &
                                tn$Year == year &
                                tn$Constituency_Name == constituency &
                                tn$State_Name == state &
                                tn$Election_Type == election_type &
                                tn$Assembly_No == assembly_no)

          # mark as decoy in the original dataframe only for this specific election
          if (length(decoy_rows) > 0) {
            tn$is_decoy[decoy_rows] <- TRUE
            tn$decoy_for_pid[decoy_rows] <- main_candidate_pid
            tn$decoy_for_name[decoy_rows] <- main_candidate_name
          }

          # add to results list for review
          result_list[[length(result_list) + 1]] <- data.frame(
            Year = year,
            Constituency_Name = constituency,
            State_Name = state,
            Election_Type = election_type,
            Assembly_No = assembly_no,
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

test2 <- tn %>%
  filter(Year == 1971 & State_Name == "Tamil_Nadu" & Constituency_Name == "PALANI" & Election_Type == "State Assembly Election (AE)")
# FUCK YES.

### Now, for the rest
unique_elections <- all_states_elections %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  distinct()

# empty the list post test
result_list <- list()

# progress
current_state <- ""
states_completed <- c()
total_states <- length(unique(unique_elections$State_Name))
state_counter <- 0

for (i in 1:nrow(unique_elections)) {
  year <- unique_elections$Year[i]
  constituency <- unique_elections$Constituency_Name[i]
  state <- unique_elections$State_Name[i]
  election_type <- unique_elections$Election_Type[i]
  assembly_no <- unique_elections$Assembly_No[i]
  
  if (state != current_state) {
    if (current_state != "") {
      state_counter <- state_counter + 1
      cat("\nCompleted state:", current_state, "-", state_counter, "of", total_states,
          "states (", round(state_counter/total_states*100, 1), "%)\n")
      states_completed <- c(states_completed, current_state)
    }
    current_state <- state
    cat("\nStarting new state:", current_state, "\n")
  }
  
  # filter data for current group
  group_data <- all_states_elections_1 %>%
    filter(Year == year,
           Constituency_Name == constituency,
           State_Name == state,
           Election_Type == election_type, 
           Assembly_No == assembly_no)
  
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
      
      # Skip if comparing the same candidate ID
      if (minor_candidate_pid == main_candidate_pid) {
        next
      }
      
      minor_matrix_idx <- which(group_data$pid == minor_candidate_pid)
      
      # get the normalized levenshtein similarity using matrix indices
      if (length(main_matrix_idx) == 1 && length(minor_matrix_idx) == 1) {
        lev_similarity <- distance_matrix[main_matrix_idx, minor_matrix_idx]
        
        # check if this is a potential decoy based on similarity threshold
        if (lev_similarity > 0.53) {
          # CRITICAL CHANGE: Only update rows for this specific election context
          decoy_rows <- which(all_states_elections_1$pid == minor_candidate_pid & 
                                all_states_elections_1$Year == year &
                                all_states_elections_1$Constituency_Name == constituency &
                                all_states_elections_1$State_Name == state &
                                all_states_elections_1$Election_Type == election_type &
                                all_states_elections_1$Assembly_No == assembly_no)
          
          # mark as decoy in the original dataframe only for this specific election
          if (length(decoy_rows) > 0) {
            all_states_elections_1$is_decoy[decoy_rows] <- TRUE
            all_states_elections_1$decoy_for_pid[decoy_rows] <- main_candidate_pid
            all_states_elections_1$decoy_for_name[decoy_rows] <- main_candidate_name
          }
          
          # add to results list for review
          result_list[[length(result_list) + 1]] <- data.frame(
            Year = year,
            Constituency_Name = constituency,
            State_Name = state,
            Election_Type = election_type,
            Assembly_No = assembly_no,
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

str(all_states_elections_1)
unique(all_states_elections_1$MyNeta_education)
unique(all_states_elections_1$Party_Type_TCPD)

all_states_elections_1 <- all_states_elections_1 %>%
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

all_states_elections_1 <- all_states_elections_1 %>%
  mutate(Party_type_numeric = recode(Party_Type_TCPD,
                                     "Independents" = 0,
                                     "Local Party" = 1, 
                                     "State-based Party" = 2, 
                                     "State-based Party (Other State" = 3, 
                                     "National Party" = 4, 
                                     .default = NA_real_
  ))

### Collapse by constituency-state-year
constituency_metrics <- all_states_elections_1 %>%
  group_by(State_Name, Year, Constituency_Name, Election_Type) %>%
  dplyr::summarize(
    total_candidates = n_distinct(pid),
    decoy_candidates = sum(is_decoy, na.rm = TRUE),
    decoy_share = round(sum(is_decoy, na.rm = TRUE) / n_distinct(pid) * 100, 2),
    total_votes = max(Valid_Votes, na.rm = TRUE),
    total_votes_to_decoys = sum(Votes[is_decoy], na.rm = TRUE),
    decoy_vote_share = round(sum(Votes[is_decoy], na.rm = TRUE) / max(Valid_Votes, na.rm = TRUE) * 100, 2),
    winning_margin = min(Margin[Position == 1], na.rm = TRUE),  # margin of the winner
    winning_margin_percentage = min(Margin_Percentage[Position == 1], na.rm = TRUE),
    winner_party = first(Party[Position == 1]),
    runner_up_party = first(Party[Position == 2]),
    winner_vote_share = max(Vote_Share_Percentage[Position == 1], na.rm = TRUE),
    runner_up_vote_share = max(Vote_Share_Percentage[Position == 2], na.rm = TRUE),
    winner_has_decoys = any(decoy_for_pid[is_decoy] %in% pid[Position == 1], na.rm = TRUE),
    runner_up_has_decoys = any(decoy_for_pid[is_decoy] %in% pid[Position == 2], na.rm = TRUE),
    education = mean(MyNeta_education_numeric),
    
    # education and party for decoys
    decoy_education = mean(MyNeta_education_numeric[is_decoy], na.rm = TRUE),
    decoy_party = mean(Party_type_numeric[is_decoy], na.rm = TRUE),
    
    # education and party for main with decoy
    main_with_decoy_education = mean(MyNeta_education_numeric[pid %in% decoy_for_pid[is_decoy]], na.rm = TRUE),
    main_with_decoy_party = mean(Party_type_numeric[pid %in% decoy_for_pid[is_decoy]], na.rm = TRUE),
    
    # education and party for main without decoy
    main_without_decoy_education = mean(MyNeta_education_numeric[!(pid %in% decoy_for_pid)], na.rm = TRUE),
    main_without_decoy_party = mean(Party_type_numeric[!(pid %in% decoy_for_pid)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    close_race = ifelse(winning_margin_percentage < 5, TRUE, FALSE),
    decoy_impact_potential = ifelse(decoy_vote_share > winning_margin_percentage, TRUE, FALSE)
  ) %>%
  # sort by state, year and constituency
  arrange(State_Name, Year, Constituency_Name, Election_Type)

constituency_metrics_AE <- constituency_metrics %>%
  filter(constituency_metrics$Election_Type == "State Assembly Election (AE)")

constituency_metrics_GE <- constituency_metrics %>%
  filter(constituency_metrics$Election_Type == "Lok Sabha Election (GE)")

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

state_metrics_AE <- constituency_metrics_AE %>%
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

state_metrics_GE <- constituency_metrics_GE %>%
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

### Some more plots
state_metrics <- state_metrics %>%
  arrange(desc(mean_decoy_share)) %>%
  mutate(State_Name = factor(State_Name, levels = State_Name))

state_metrics_AE <- state_metrics_AE %>%
  arrange(desc(mean_decoy_share)) %>%
  mutate(State_Name = factor(State_Name, levels = State_Name))

state_metrics_GE <- state_metrics_GE %>%
  arrange(desc(mean_decoy_share)) %>%
  mutate(State_Name = factor(State_Name, levels = State_Name))

ggplot(state_metrics, aes(x = State_Name, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_decoy_share, ymax = p90_decoy_share), width = 0.2) +
  geom_point(aes(y = max_decoy_share), color = "red", size = 2) +
  scale_y_continuous(breaks = seq(0, 100, by = 25)) + 
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

ggsave(
  filename = "Plots/Decoy_perc_state.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(state_metrics_AE, aes(x = State_Name, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_decoy_share, ymax = p90_decoy_share), width = 0.2) +
  geom_point(aes(y = max_decoy_share), color = "red", size = 2) +
  scale_y_continuous(breaks = seq(0, 100, by = 25)) + 
  labs(
    title = "Average Share of Decoy Candidates per Election by State (AE)",
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

ggsave(
  filename = "Plots/Decoy_perc_state_AE.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(state_metrics_GE, aes(x = State_Name, y = mean_decoy_share)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_decoy_share, ymax = p90_decoy_share), width = 0.2) +
  geom_point(aes(y = max_decoy_share), color = "red", size = 2) +
  scale_y_continuous(breaks = seq(0, 100, by = 25)) + 
  labs(
    title = "Average Share of Decoy Candidates per Election by State (GE)",
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

ggsave(
  filename = "Plots/Decoy_perc_state_GE.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_num_decoys)) %>%
  ggplot(aes(x = State_Name, y = mean_num_decoys)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_num_decoys, ymax = p90_num_decoys), width = 0.2) +
  geom_point(aes(y = max_num_decoys), color = "red", size = 2) +
  labs(
    title = "Average Number of Decoy Candidates per Election by State",
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


ggsave(
  filename = "Plots/Decoy_number_state.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics_AE %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_num_decoys)) %>%
  ggplot(aes(x = State_Name, y = mean_num_decoys)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_num_decoys, ymax = p90_num_decoys), width = 0.2) +
  geom_point(aes(y = max_num_decoys), color = "red", size = 2) +
  labs(
    title = "Average Number of Decoy Candidates per Election by State (AE)",
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

ggsave(
  filename = "Plots/Decoy_number_state_AE.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics_GE %>%
  mutate(State_Name = fct_reorder(State_Name, -mean_num_decoys)) %>%
  ggplot(aes(x = State_Name, y = mean_num_decoys)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_errorbar(aes(ymin = median_num_decoys, ymax = p90_num_decoys), width = 0.2) +
  geom_point(aes(y = max_num_decoys), color = "red", size = 2) +
  labs(
    title = "Average Number of Decoy Candidates per Election by State (GE)",
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

ggsave(
  filename = "Plots/Decoy_number_state_GE.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
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

ggsave(
  filename = "Plots/Decoy_metrics_state.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

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
  scale_y_continuous(labels = scales::percent) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave(
  filename = "Plots/Decoy_perc_overtime.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### Make graphs on how well these candidates did
state_metrics %>%
  # reorder
  mutate(State_Name = fct_reorder(State_Name, overall_decoy_vote_share)) %>%
  ggplot(aes(x = State_Name, y = overall_decoy_vote_share)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = sprintf("%.1f%%", overall_decoy_vote_share)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Votes Captured by Decoy Candidates",
    subtitle = "Percentage of total votes going to decoy candidates by state",
    y = "Decoy Vote Share (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "Plots/Decoy_voteshare_state.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics %>%
  # reorder
  mutate(State_Name = fct_reorder(State_Name, pct_decoy_impact)) %>%
  ggplot(aes(x = State_Name, y = pct_decoy_impact)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  geom_text(aes(label = sprintf("%.1f%%", pct_decoy_impact)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Races Where Decoys Could Have Changed Outcomes",
    subtitle = "Percentage of races where decoy votes exceeded winning margin",
    y = "Percentage of Races (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "Plots/Decoy_voteshare_state_margin.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

state_metrics_for_viz <- state_metrics %>%
  dplyr::select(State_Name, pct_close_races, pct_decoy_impact) %>%
  tidyr::pivot_longer(
    cols = c(pct_close_races, pct_decoy_impact),
    names_to = "metric",
    values_to = "percentage"
  ) %>%
  mutate(
    metric = case_when(
      metric == "pct_close_races" ~ "Close Races (<5% margin)",
      metric == "pct_decoy_impact" ~ "Races Where Decoys Matter",
      TRUE ~ metric
    ),
    State_Name = fct_reorder(State_Name, percentage, .fun = max)
  )

ggplot(state_metrics_for_viz, aes(x = State_Name, y = percentage, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("darkblue", "red")) +
  coord_flip() +
  labs(
    title = "Close Races vs. Decoy Impact Races",
    subtitle = "Comparing races with tight margins to those where decoys could matter",
    y = "Percentage of Total Races (%)",
    x = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "Plots/Decoy_voteshare_state_impact.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### Characteristics of these weirdos
education_data <- state_metrics %>%
  dplyr::select(State_Name, 
         decoy_education, 
         main_with_decoy_education, 
         main_without_decoy_education) %>%
  pivot_longer(
    cols = c(decoy_education, main_with_decoy_education, main_without_decoy_education),
    names_to = "candidate_type",
    values_to = "education_level"
  ) %>%
  mutate(candidate_type = factor(candidate_type, 
                                 levels = c("decoy_education", 
                                            "main_with_decoy_education", 
                                            "main_without_decoy_education"),
                                 labels = c("Decoy Candidates", 
                                            "Main Candidates (with decoys)", 
                                            "Main Candidates (no decoys)")))

party_data <- state_metrics %>%
  dplyr::select(State_Name, 
         decoy_party, 
         main_with_decoy_party, 
         main_without_decoy_party) %>%
  pivot_longer(
    cols = c(decoy_party, main_with_decoy_party, main_without_decoy_party),
    names_to = "candidate_type",
    values_to = "party_affiliation"
  ) %>%
  mutate(candidate_type = factor(candidate_type, 
                                 levels = c("decoy_party", 
                                            "main_with_decoy_party", 
                                            "main_without_decoy_party"),
                                 labels = c("Decoy Candidates", 
                                            "Main Candidates (with decoys)", 
                                            "Main Candidates (no decoys)")))

ggplot(education_data, aes(x = education_level, fill = candidate_type)) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Years of Education by Candidate Type",
    x = "Education Years",
    y = "Density",
    fill = "Candidate Type"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "Plots/Candidate_education.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(party_data, aes(x = party_affiliation, fill = candidate_type)) +
  geom_density(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Party Type by Candidate Type",
    x = "Party Type Score",
    y = "Density",
    fill = "Candidate Type"
  ) +
  scale_fill_brewer(palette = "Set1") +
  # x-axis
  scale_x_continuous(
    breaks = c(0, 1, 2, 3, 4),
    labels = c("Independents", "Local", "State", "State (Out of State)", "National")
  ) +
  theme(legend.position = "bottom") + 
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = "Plots/Candidate_party_type.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(education_data, aes(x = candidate_type, y = education_level, fill = candidate_type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution of Years of Education by Candidate Type",
    x = "",
    y = "Education Years"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "Plots/Candidate_education_box.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

ggplot(party_data, aes(x = candidate_type, y = party_affiliation, fill = candidate_type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution of Party Type by Candidate Type",
    x = "",
    y = "Party Type Score"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "Plots/Candidate_party_type_box.png",
  plot = last_plot(),
  width = 10,
  height = 6,
  dpi = 300,
  device = "png", 
  bg = "white"
)

### Placebo Test - to see whether similarity is distributed differently across major and minor cands
placebo_results <- list()

constituency_year_counts <- data.frame(
  Year = numeric(),
  Constituency_Name = character(),
  State_Name = character(),
  Election_Type = character(),
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
  election_type <- unique_elections$Election_Type[i]
  assembly_no <- unique_elections$Assembly_No[i]
  
  state_election_combo <- paste(state, election_type, sep = " - ")
  
  if (state_election_combo != current_state_election) {
    if (current_state_election != "") {
      state_counter <- state_counter + 1
      cat("\nCompleted:", current_state_election, "-", state_counter, "of", total_state_elections,
          "state-election type combinations (", round(state_counter/total_state_elections*100, 1), "%)\n")
      states_completed <- c(states_completed, current_state_election)
    }
    current_state_election <- state_election_combo
    cat("\nStarting new:", current_state_election, "\n")
  }
  
  # filter data for current group
  group_data <- all_states_elections_1 %>%
    filter(Year == year,
           Constituency_Name == constituency,
           State_Name == state,
           Election_Type == election_type, 
           Assembly_No == assembly_no)
  
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
              Election_Type = election_type,
              Assembly_No == assembly_no,
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
      
      # Skip if comparing the same candidate ID
      if (minor_candidate_pid == main_candidate_pid) {
        next
      }
      
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
            Election_Type = election_type,
            Assembly_No == assembly_no,
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
          }
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
              Election_Type = election_type,
              Assembly_No == assembly_no,
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
    Assembly_No == assembly_no,
    Election_Type = election_type,
    main_main_total = main_main_total,
    main_main_fps = main_main_fps,
    main_minor_total = main_minor_total,
    main_minor_fps = main_minor_fps,
    minor_minor_total = minor_minor_total,
    minor_minor_fps = minor_minor_fps
  ))


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

### Saving
write.csv(all_states_elections_1, "Cleaned Data/all_states_election_decoy.csv")
write.csv(constituency_metrics, "Cleaned Data/constituency_year_state_decoy.csv")
write.csv(state_metrics, "Cleaned Data/state_decoy.csv")
write.csv(year_metrics, "Cleaned Data/year_decoy.csv")

### SANITY CHECKING WEIRD SHIT
test1 <- all_states_elections_1 %>%
  filter(Year == 2005 & State_Name == "Bihar" & Constituency_Name == "CHENARI" & Election_Type == "State Assembly Election (AE)")

test2 <- all_states_elections_1 %>%
  filter(Year == 1971 & State_Name == "Tamil_Nadu" & Constituency_Name == "PALANI" & Election_Type == "State Assembly Election (AE)")

test1 <- test1 %>%
  mutate(candidate_type = ifelse(Vote_Share_Percentage > 10, "main", "minor"))

main_candidates <- test1 %>% filter(candidate_type == "main")
minor_candidates <- test1 %>% filter(candidate_type == "minor")

if (nrow(main_candidates) > 0 && nrow(minor_candidates) > 0) {
  candidate_names <- test1$Candidate_clean
  candidate_pids <- test1$pid
  
  distance_matrix <- normalized_levenshtein_matrix(candidate_names)
  
  if (!exists("result_list")) {
    result_list <- list()
  }
  
  for (main_idx in 1:nrow(main_candidates)) {
    main_candidate_name <- main_candidates$Candidate_clean[main_idx]
    main_candidate_pid <- main_candidates$pid[main_idx]
    main_matrix_idx <- which(test1$pid == main_candidate_pid)
    
    for (minor_idx in 1:nrow(minor_candidates)) {
      minor_candidate_name <- minor_candidates$Candidate_clean[minor_idx]
      minor_candidate_pid <- minor_candidates$pid[minor_idx]
      minor_matrix_idx <- which(test1$pid == minor_candidate_pid)
      
      if (length(main_matrix_idx) == 1 && length(minor_matrix_idx) == 1) {
        lev_similarity <- distance_matrix[main_matrix_idx, minor_matrix_idx]
        
        # check if this is a potential decoy based on similarity threshold
        if (lev_similarity > 0.53) {
          # mark as decoy in the original dataframe using pid as ID
          all_states_elections_1$is_decoy[all_states_elections_1$pid == minor_candidate_pid] <- TRUE
          all_states_elections_1$decoy_for_pid[all_states_elections_1$pid == minor_candidate_pid] <- main_candidate_pid
          all_states_elections_1$decoy_for_name[all_states_elections_1$pid == minor_candidate_pid] <- main_candidate_name
          
          # add to results list for review
          result_list[[length(result_list) + 1]] <- data.frame(
            Year = year,
            Constituency_Name = constituency,
            State_Name = state,
            Election_Type = election_type,
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
  
  # print summary of results
  cat("Processing complete for", constituency, "in", state, "\n")
  cat("Found", length(result_list), "potential decoy candidates\n")
} else {
  cat("No main or minor candidates found for this constituency\n")
}

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

test <- all_states_elections_1 %>%
  filter(State_Name == "Tamil_Nadu" & Constituency_Name == "KALASAPAKKAM" & Year == 2016 & Election_Type == "State Assembly Election (AE)")

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
  # can potentially add less weight to last names
  
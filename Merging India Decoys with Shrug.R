# ============================================
# Political Decoys - Merging with Shrug
# ============================================
# Date: 15/04/25
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
  reticulate,
  sf,
  gt,
  formattable,
  DT,
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
  phonics, 
  flextable, 
  purr
)

### Loading
all_states_elections <- read.csv("Raw Data/all_states_elections.csv")
CLEA_cleaned <- read.csv("Cleaned Data/CLEA_cleaned.csv")
candidate_pairs <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet.csv")

triv <- read_csv("/Users/anirvin/Downloads/Corruption Protest Data/Raw Data/shrug-triv-cand-csv/trivedi_candidates_clean.csv")

shrid_07 <- read_csv("/Users/anirvin/Downloads/Corruption Protest Data/Raw Data/shrug-con-keys-csv/shrid_frag_con07_key.csv")
shrid_08 <- read_csv("/Users/anirvin/Downloads/Corruption Protest Data/Raw Data/shrug-con-keys-csv/shrid_frag_con08_key.csv")

### Shrug data
ac07_name_key <- read.csv("Shrug Data/shrug-con-keys-csv/ac07_name_key.csv")
ac08_name_key <- read.csv("Shrug Data/shrug-con-keys-csv/ac08_name_key.csv")

### Standardize state names
standardize_state_names <- function(state_name) { 
  # Convert state name to lowercase and trim whitespace
  state_name <- trimws(tolower(state_name)) 
  
  # Dictionary mapping variations to standard names from pol_outcomes_no_pre_2011
  state_dict <- list(
    "andhra pradesh" = c("andhara pradesh", "ap", "andhra", "andhra pradesh state", "telangana", "andhra_pradesh"),
    "arunachal pradesh" = c("arunachal", "arunachal pradesh state", "arunanchal pradesh", "arunachal_pradesh"),
    "assam" = c("assam"),
    "bihar" = c("bihar"),
    "chhattisgarh" = c("chattisgarh", "chhattisgarh state", "chhattisgarh"),
    "delhi" = c("delhi", "delhi state"),
    "goa" = c("goa", "goa state"),
    "gujarat" = c("gujarat", "gujrath", "gujarat state", "guj"),
    "haryana" = c("haryana"),
    "himachal pradesh" = c("himachal pradesh", "hp", "himachal pradesh state", "himachal_pradesh"),
    "jammu kashmir" = c("jammu and kashmir", "j&k", "j & k", "jk", "jammu & kashmir", "jammu and kashmir", "jammu_kashmir", "jammu_&_kashmir"),
    "jharkhand" = c("jharkhand", "jharkhand state"),
    "karnataka" = c("karnatka", "karnataka state", "karnataka"),
    "kerala" = c("kerala", "kerala state"),
    "madhya pradesh" = c("mp", "m.p.", "madhya pradesh state", "madhya_pradesh"),
    "maharashtra" = c("maharashtra"),
    "manipur" = c("manipur"),
    "meghalaya" = c("meghalaya", "meghalaya state"),
    "mizoram" = c("mizoram", "mizoram state"),
    "nagaland" = c("nagaland", "nagaland state"),
    "orissa" = c("odisha", "orissa state", "orissa"),
    "pondicherry" = c("puducherry", "pondicherry state", "pondicherry"),
    "punjab" = c("punjab", "pnjb", "punjab state", "pnb"),
    "rajasthan" = c("rajasthan", "rajsthan", "rajsthan state", "raj"),
    "sikkim" = c("sikkim"),
    "tamil nadu" = c("tamilnadu", "tamil nadu state", "tn", "tamil_nadu"),
    "tripura" = c("tripura", "tripura state"),
    "uttar pradesh" = c("up", "u.p.", "uttar pradesh state", "uttar_pradesh"),
    "uttarakhand" = c("uttarkhand", "uttaranchal", "uttrakhand", "uttarakhand state", "uttarakhand"),
    "west bengal" = c("wb", "w.b.", "westbengal", "west_bengal")
  ) 
  
  # Search for a match in the dictionary
  for (standard_name in names(state_dict)) { 
    if (state_name %in% state_dict[[standard_name]] || state_name == standard_name) { 
      return(standard_name) # Return lowercase to match pol_outcomes_no_pre_2011 format
    } 
  } 
  
  # Return original if no match found
  return(state_name)
}

all_states_elections$State_Name <- sapply(all_states_elections$State_Name, standardize_state_names)
ac07_name_key$pc01_state_name <- sapply(ac07_name_key$pc01_state_name, standardize_state_names)
ac08_name_key$pc01_state_name <- sapply(ac08_name_key$pc01_state_name, standardize_state_names)

# are there states in all_states_elections that are not in ac07/ac08
states_elections <- unique(all_states_elections$State_Name)
states_ac07 <- unique(ac07_name_key$pc01_state_name)
states_ac08 <- unique(ac08_name_key$pc01_state_name)

cat("Number of unique states in all_states_elections:", length(states_elections), "\n")
cat("Number of unique states in ac07_name_key:", length(states_ac07), "\n")
cat("Number of unique states in ac08_name_key:", length(states_ac08), "\n")

states_not_in_ac07 <- setdiff(states_elections, states_ac07)
if(length(states_not_in_ac07) > 0) {
  cat("\nStates in all_states_elections but not in ac07_name_key:\n")
  print(states_not_in_ac07)
} else {
  cat("\nAll states in all_states_elections are also in ac07_name_key\n")
}

states_not_in_ac08 <- setdiff(states_elections, states_ac08)
if(length(states_not_in_ac08) > 0) {
  cat("\nStates in all_states_elections but not in ac08_name_key:\n")
  print(states_not_in_ac08)
} else {
  cat("\nAll states in all_states_elections are also in ac08_name_key\n")
}

states_not_in_either <- setdiff(states_elections, union(states_ac07, states_ac08))
if(length(states_not_in_either) > 0) {
  cat("\nStates in all_states_elections but not in either ac07_name_key or ac08_name_key:\n")
  print(states_not_in_either)
} else {
  cat("\nAll states in all_states_elections are in at least one of the other datasets\n")
}

comparison_df <- data.frame(
  State = unique(c(states_elections, states_ac07, states_ac08)),
  In_all_states_elections = NA,
  In_ac07_name_key = NA,
  In_ac08_name_key = NA
)

comparison_df$In_all_states_elections <- comparison_df$State %in% states_elections
comparison_df$In_ac07_name_key <- comparison_df$State %in% states_ac07
comparison_df$In_ac08_name_key <- comparison_df$State %in% states_ac08

# drop rows from all_states_elections if cannot match state with ac07 or ac08
states_to_remove <- c("mysore", "madras", "goa_daman_&_diu", "andaman_&_nicobar_islands", 
                      "chandigarh", "dadra_&_nagar_haveli", "daman_&_diu", "lakshadweep", 
                      "dadra & nagar haveli and daman & diu", "goa,_daman_&_diu")

all_states_elections_filtered <- all_states_elections[!all_states_elections$State_Name %in% states_to_remove, ]

# remove gen assembly elections because cannot match with shrug
all_states_elections_filtered <- all_states_elections_filtered %>%
  filter(Election_Type == "State Assembly Election (AE)")

# remove delimID == 1 and 2 cause i cant reliably match those
all_states_elections_filtered <- all_states_elections_filtered %>%
  filter(DelimID > 2)

all_states_elections_filtered_pre_delim <- all_states_elections_filtered %>%
  filter(DelimID == 3) %>%
  mutate(
    Constituency_Name_lower = tolower(Constituency_Name)
  )

all_states_elections_filtered_post_delim <- all_states_elections_filtered %>%
  filter(DelimID == 4) %>%
  mutate(
    Constituency_Name_lower = tolower(Constituency_Name)
  )

### Merging directly with names
all_states_elections_filtered_pre_delim <- all_states_elections_filtered_pre_delim %>%
  left_join(ac07_name_key %>%
              dplyr::select(ac07_id, ac07_name, pc01_state_name), 
            by = c("State_Name" = "pc01_state_name", "Constituency_Name_lower" = "ac07_name")) %>%
  filter(!is.na(ac07_id))

all_states_elections_filtered_post_delim <- all_states_elections_filtered_post_delim %>%
  left_join(ac08_name_key %>%
              dplyr::select(ac08_id, ac08_name, pc01_state_name), 
            by = c("State_Name" = "pc01_state_name", "Constituency_Name_lower" = "ac08_name")) %>%
  filter(!is.na(ac08_id))

unique_elections_pre_delim <- all_states_elections_filtered_pre_delim %>%
  distinct(State_Name, Year, Constituency_Name, Election_Type, Assembly_No, ac07_id)

unique_elections_post_delim <- all_states_elections_filtered_post_delim %>%
  distinct(State_Name, Year, Constituency_Name, Election_Type, Assembly_No, ac08_id)

### Creating constituency level metrics - separately for ac07 & ac08? or coalesce? 
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

candidate_pairs <- candidate_pairs %>%
  mutate(Candidate1_MyNeta_education_numeric = case_when(
    Candidate1_MyNeta_education == "Illiterate" ~ 0,
    Candidate1_MyNeta_education == "Literate" ~ 1,
    Candidate1_MyNeta_education == "5th Pass" ~ 5,
    Candidate1_MyNeta_education == "8th Pass" ~ 8,
    Candidate1_MyNeta_education == "10th Pass" ~ 10,
    Candidate1_MyNeta_education == "12th Pass" ~ 12,
    Candidate1_MyNeta_education == "Graduate" ~ 16,
    Candidate1_MyNeta_education == "Graduate Professional" ~ 16,
    Candidate1_MyNeta_education == "Post Graduate" ~ 20,
    Candidate1_MyNeta_education == "Doctorate" ~ 22,
    Candidate1_MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

candidate_pairs <- candidate_pairs %>%
  mutate(Candidate2_MyNeta_education_numeric = case_when(
    Candidate2_MyNeta_education == "Illiterate" ~ 0,
    Candidate2_MyNeta_education == "Literate" ~ 2,
    Candidate2_MyNeta_education == "5th Pass" ~ 5,
    Candidate2_MyNeta_education == "8th Pass" ~ 8,
    Candidate2_MyNeta_education == "20th Pass" ~ 20,
    Candidate2_MyNeta_education == "22th Pass" ~ 22,
    Candidate2_MyNeta_education == "Graduate" ~ 26,
    Candidate2_MyNeta_education == "Graduate Professional" ~ 26,
    Candidate2_MyNeta_education == "Post Graduate" ~ 20,
    Candidate2_MyNeta_education == "Doctorate" ~ 22,
    Candidate2_MyNeta_education %in% c("Others", "Not Given Page Missing", "Not Given Not Filled", "") ~ NA_real_,
    TRUE ~ NA_real_))

candidate_pairs <- candidate_pairs %>%
  mutate(Candidate1_Party_type_numeric = recode(Candidate1_Party_Type_TCPD,
                                                "Independents" = 0,
                                                "Local Party" = 1, 
                                                "State-based Party" = 2, 
                                                "State-based Party (Other State" = 3, 
                                                "National Party" = 4, 
                                                .default = NA_real_
  ))

candidate_pairs <- candidate_pairs %>%
  mutate(Candidate2_Party_type_numeric = recode(Candidate2_Party_Type_TCPD,
                                                "Independents" = 0,
                                                "Local Party" = 1, 
                                                "State-based Party" = 2, 
                                                "State-based Party (Other State" = 3, 
                                                "National Party" = 4, 
                                                .default = NA_real_
  ))

filter_out_initial_names <- function(df) {
  # pattern for "initial (with/without period) followed by name"
  initial_pattern <- "^[A-Z]\\s*\\.?\\s*[A-Za-z]+$"
  
  # pattern for just a single name
  single_name_pattern <- "^[A-Za-z]+$"
  
  # combined pattern using OR (|)
  combined_pattern <- paste0("(", initial_pattern, ")|(", single_name_pattern, ")")
  
  # check if either candidate matches either pattern
  matches_pattern1 <- grepl(combined_pattern, df$Candidate1_Name)
  matches_pattern2 <- grepl(combined_pattern, df$Candidate2_Name)
  
  # identify rows to be filtered out (either candidate matches either pattern)
  rows_to_filter <- matches_pattern1 | matches_pattern2
  
  # create filtered dataframe and dataframe of filtered-out rows
  filtered_df <- df[!rows_to_filter, ]
  filtered_out_df <- df[rows_to_filter, ]
  
  # return both dataframes as a list
  return(list(
    kept = filtered_df,
    filtered_out = filtered_out_df
  ))
}

results <- filter_out_initial_names(candidate_pairs)
candidate_pairs_2 <- results$kept
candidate_pairs_out <- results$filtered_out

# making is_decoy
# thresholds
lv_99th <- quantile(candidate_pairs$Levenshtein_Similarity, 0.99, na.rm = TRUE)
jw_99th <- quantile(candidate_pairs$Jaro_Winkler_Similarity, 0.99, na.rm = TRUE)
mp_99th <- quantile(candidate_pairs$Metaphone_Similarity, 0.99, na.rm = TRUE)
mas_99th <- quantile(candidate_pairs$Masala_Similarity, 0.99, na.rm = TRUE)
ng_99th <- quantile(candidate_pairs$NGram_Similarity, 0.99, na.rm = TRUE)

candidate_pairs <- candidate_pairs %>%
  mutate(is_decoy = ifelse(
    Levenshtein_Similarity > lv_99th & 
      Jaro_Winkler_Similarity > jw_99th & 
      Metaphone_Similarity > mp_99th & 
      Masala_Similarity > mas_99th & 
      NGram_Similarity > ng_99th,
    TRUE, 
    FALSE
  ))

constituency_election_metrics <- candidate_pairs %>%
  group_by(State_Name, Year, Constituency_Name, Election_Type, Assembly_No) %>%
  dplyr::summarize(
    # basic constituency metrics
    total_candidates = max(Candidate1_N_Cand, na.rm = TRUE),
    decoy_candidates = sum((is_decoy & Pair_Type == "main-minor") | 
                             (is_decoy & Pair_Type == "minor-main"), na.rm = TRUE),
    decoy_share = round(sum((is_decoy & Pair_Type == "main-minor") | 
                              (is_decoy & Pair_Type == "minor-main"), na.rm = TRUE) /
                          max(Candidate1_N_Cand, na.rm = TRUE) * 100, 2),
    
    # vote metrics
    total_votes = max(Candidate1_Valid_Votes, na.rm = TRUE),
    
    # count votes to decoys - adjusted to use is_decoy
    total_votes_to_decoys = sum(
      ifelse(is_decoy & Pair_Type == "main-minor", Candidate2_Votes, 
             ifelse(is_decoy & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
      na.rm = TRUE),
    decoy_vote_share = round(
      sum(ifelse(is_decoy & Pair_Type == "main-minor", Candidate2_Votes, 
                 ifelse(is_decoy & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
          na.rm = TRUE) / max(Candidate1_Valid_Votes, na.rm = TRUE) * 100, 2),
    
    # winner and runner-up metrics
    winning_margin = min(Candidate1_Margin[Candidate1_Position == 1], na.rm = TRUE),
    winning_margin_percentage = min(Candidate1_Margin_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    winner_party = first(Candidate1_Party[Candidate1_Position == 1]),
    runner_up_party = first(Candidate1_Party[Candidate1_Position == 2]),
    winner_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    runner_up_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 2], na.rm = TRUE),
    
    # check for decoys associated with winner/runner-up - using is_decoy
    winner_has_decoys = any((is_decoy & Pair_Type == "main-minor" & Candidate1_Position == 1) | 
                              (is_decoy & Pair_Type == "minor-main" & Candidate2_Position == 1), 
                            na.rm = TRUE),
    runner_up_has_decoys = any((is_decoy & Pair_Type == "main-minor" & Candidate1_Position == 2) | 
                                 (is_decoy & Pair_Type == "minor-main" & Candidate2_Position == 2), 
                               na.rm = TRUE),
    
    # education metrics
    education = mean(c(Candidate1_MyNeta_education_numeric,
                       Candidate2_MyNeta_education_numeric), na.rm = TRUE),
    
    # educ & party for decoys - using is_decoy
    decoy_education = mean(
      c(Candidate2_MyNeta_education_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate1_MyNeta_education_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    decoy_party = mean(
      c(Candidate2_Party_type_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate1_Party_type_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    
    # educ & party for main with decoys - using is_decoy
    main_with_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate2_MyNeta_education_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    main_with_decoy_party = mean(
      c(Candidate1_Party_type_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate2_Party_type_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    
    # educ & party for main without decoys
    main_without_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[!is_decoy & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_MyNeta_education_numeric[!is_decoy & (Pair_Type == "minor-main" | Pair_Type == "main-main")]),
      na.rm = TRUE),
    main_without_decoy_party = mean(
      c(Candidate1_Party_type_numeric[!is_decoy & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_Party_type_numeric[!is_decoy & (Pair_Type == "minor-main" | Pair_Type == "main-main")]),
      na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    close_race = ifelse(winning_margin_percentage < 5, TRUE, FALSE),
    decoy_impact_potential = ifelse(decoy_vote_share > winning_margin_percentage, TRUE, FALSE)
  ) %>%
  # sort by state, year and constituency
  arrange(State_Name, Year, Constituency_Name, Election_Type, Assembly_No)

# get the overall constituency_metrics df ready
# filter out GE
constituency_election_metrics <- constituency_election_metrics %>%
  filter(Election_Type == "State Assembly Election (AE)")
constituency_election_metrics <- constituency_election_metrics[!constituency_election_metrics$State_Name %in% states_to_remove, ]
constituency_election_metrics$State_Name <- sapply(constituency_election_metrics$State_Name, standardize_state_names)

constituency_election_metrics <- constituency_election_metrics %>%
  left_join(unique_elections_pre_delim, by = c("State_Name", "Year", "Constituency_Name", 
                                               "Assembly_No"))

constituency_election_metrics <- constituency_election_metrics %>%
  left_join(unique_elections_post_delim, by = c("State_Name", "Year", "Constituency_Name", 
                                               "Assembly_No"))

# drop if no ac07 or ac08 id
constituency_election_metrics <- constituency_election_metrics %>%
  filter(!is.na(ac07_id) | !is.na(ac08_id)) %>%
  dplyr::select(-Election_Type.y, -Election_Type.x, -Election_Type)

constituency_election_metrics_ac07 <- constituency_election_metrics %>%
  filter(is.na(ac08_id)) %>%
  dplyr::select(-ac08_id)

constituency_election_metrics_ac08 <- constituency_election_metrics %>%
  filter(is.na(ac07_id)) %>%
  dplyr::select(-ac07_id)

str(constituency_election_metrics)

# how do we combine pre and post delim like we did for corruption?
constituency_crosswalk <- shrid_07 %>%
  dplyr::select(shrid2, ac07_id, fragment_wt_con07) %>%
  inner_join(
    shrid_08 %>% dplyr::select(shrid2, ac08_id, fragment_wt_con08),
    by = "shrid2",
    relationship = "many-to-many"
  ) %>%
  # aggregate to create constituency-to-constituency mapping with weights
  group_by(ac07_id, ac08_id) %>%
  dplyr::summarize(
    # create a weight based on the sum of fragment weights
    weight = sum(fragment_wt_con07 * fragment_wt_con08) / 
      (sum(fragment_wt_con07) * sum(fragment_wt_con08)),
    n_shrids = n()  # no. of shrids connecting constituencies
  ) %>%
  ungroup()

constituency_election_metrics <- constituency_election_metrics %>%
  mutate(post_delimitation = case_when(
    !is.na(ac08_id) ~ 1,
    TRUE ~ 0
  ))

str(constituency_election_metrics)

constituency_election_metrics <- constituency_election_metrics %>%
  mutate(constituency_id = ifelse(is.na(ac08_id), ac07_id, ac08_id))

constituency_metrics <- constituency_election_metrics %>%
  group_by(constituency_id, State_Name, Constituency_Name) %>%
  dplyr::summarize(
    avg_total_candidates = mean(total_candidates, na.rm = TRUE),
    avg_total_votes = mean(total_votes, na.rm = TRUE),
    avg_winning_margin_pct = mean(winning_margin_percentage, na.rm = TRUE),
    years_count = n_distinct(Year),
    avg_decoy_candidates = mean(decoy_candidates, na.rm = TRUE), 
    avg_decoy_share = mean(decoy_share, na.rm = TRUE), 
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics_74_94 <- constituency_election_metrics %>%
  filter(Year >= 1974 & Year <= 1994) %>%
  group_by(constituency_id, State_Name, Constituency_Name) %>%
  dplyr::summarize(
    avg_total_candidates = mean(total_candidates, na.rm = TRUE),
    avg_total_votes = mean(total_votes, na.rm = TRUE),
    avg_winning_margin_pct = mean(winning_margin_percentage, na.rm = TRUE),
    years_count = n_distinct(Year),
    avg_decoy_candidates = mean(decoy_candidates, na.rm = TRUE), 
    avg_decoy_share = mean(decoy_share, na.rm = TRUE), 
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics_95_04 <- constituency_election_metrics %>%
  filter(Year >= 1995 & Year <= 2004) %>%
  group_by(constituency_id, State_Name, Constituency_Name) %>%
  dplyr::summarize(
    avg_total_candidates = mean(total_candidates, na.rm = TRUE),
    avg_total_votes = mean(total_votes, na.rm = TRUE),
    avg_winning_margin_pct = mean(winning_margin_percentage, na.rm = TRUE),
    years_count = n_distinct(Year),
    avg_decoy_candidates = mean(decoy_candidates, na.rm = TRUE), 
    avg_decoy_share = mean(decoy_share, na.rm = TRUE), 
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    .groups = "drop"
  )

constituency_metrics_05_23 <- constituency_election_metrics %>%
  filter(Year >= 2005 & Year <= 2023) %>%
  group_by(constituency_id, State_Name, Constituency_Name) %>%
  dplyr::summarize(
    avg_total_candidates = mean(total_candidates, na.rm = TRUE),
    avg_total_votes = mean(total_votes, na.rm = TRUE),
    avg_winning_margin_pct = mean(winning_margin_percentage, na.rm = TRUE),
    years_count = n_distinct(Year),
    avg_decoy_candidates = mean(decoy_candidates, na.rm = TRUE), 
    avg_decoy_share = mean(decoy_share, na.rm = TRUE), 
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    .groups = "drop"
  )

### do for filtered out names
# making is_decoy
# thresholds
lv_99th <- quantile(candidate_pairs_2$Levenshtein_Similarity, 0.99, na.rm = TRUE)
jw_99th <- quantile(candidate_pairs_2$Jaro_Winkler_Similarity, 0.99, na.rm = TRUE)
mp_99th <- quantile(candidate_pairs_2$Metaphone_Similarity, 0.99, na.rm = TRUE)
mas_99th <- quantile(candidate_pairs_2$Masala_Similarity, 0.99, na.rm = TRUE)
ng_99th <- quantile(candidate_pairs_2$NGram_Similarity, 0.99, na.rm = TRUE)

candidate_pairs_2 <- candidate_pairs_2 %>%
  mutate(is_decoy = ifelse(
    Levenshtein_Similarity > lv_99th & 
      Jaro_Winkler_Similarity > jw_99th & 
      Metaphone_Similarity > mp_99th & 
      Masala_Similarity > mas_99th & 
      NGram_Similarity > ng_99th,
    TRUE, 
    FALSE
  ))

constituency_election_metrics_2 <- candidate_pairs_2 %>%
  group_by(State_Name, Year, Constituency_Name, Election_Type, Assembly_No) %>%
  dplyr::summarize(
    # basic constituency metrics
    total_candidates = max(Candidate1_N_Cand, na.rm = TRUE),
    decoy_candidates = sum((is_decoy & Pair_Type == "main-minor") | 
                             (is_decoy & Pair_Type == "minor-main"), na.rm = TRUE),
    decoy_share = round(sum((is_decoy & Pair_Type == "main-minor") | 
                              (is_decoy & Pair_Type == "minor-main"), na.rm = TRUE) /
                          max(Candidate1_N_Cand, na.rm = TRUE) * 100, 2),
    
    # vote metrics
    total_votes = max(Candidate1_Valid_Votes, na.rm = TRUE),
    
    # count votes to decoys - adjusted to use is_decoy
    total_votes_to_decoys = sum(
      ifelse(is_decoy & Pair_Type == "main-minor", Candidate2_Votes, 
             ifelse(is_decoy & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
      na.rm = TRUE),
    decoy_vote_share = round(
      sum(ifelse(is_decoy & Pair_Type == "main-minor", Candidate2_Votes, 
                 ifelse(is_decoy & Pair_Type == "minor-main", Candidate1_Votes, 0)), 
          na.rm = TRUE) / max(Candidate1_Valid_Votes, na.rm = TRUE) * 100, 2),
    
    # winner and runner-up metrics
    winning_margin = min(Candidate1_Margin[Candidate1_Position == 1], na.rm = TRUE),
    winning_margin_percentage = min(Candidate1_Margin_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    winner_party = first(Candidate1_Party[Candidate1_Position == 1]),
    runner_up_party = first(Candidate1_Party[Candidate1_Position == 2]),
    winner_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 1], na.rm = TRUE),
    runner_up_vote_share = max(Candidate1_Vote_Share_Percentage[Candidate1_Position == 2], na.rm = TRUE),
    
    # check for decoys associated with winner/runner-up - using is_decoy
    winner_has_decoys = any((is_decoy & Pair_Type == "main-minor" & Candidate1_Position == 1) | 
                              (is_decoy & Pair_Type == "minor-main" & Candidate2_Position == 1), 
                            na.rm = TRUE),
    runner_up_has_decoys = any((is_decoy & Pair_Type == "main-minor" & Candidate1_Position == 2) | 
                                 (is_decoy & Pair_Type == "minor-main" & Candidate2_Position == 2), 
                               na.rm = TRUE),
    
    # education metrics
    education = mean(c(Candidate1_MyNeta_education_numeric,
                       Candidate2_MyNeta_education_numeric), na.rm = TRUE),
    
    # educ & party for decoys - using is_decoy
    decoy_education = mean(
      c(Candidate2_MyNeta_education_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate1_MyNeta_education_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    decoy_party = mean(
      c(Candidate2_Party_type_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate1_Party_type_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    
    # educ & party for main with decoys - using is_decoy
    main_with_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate2_MyNeta_education_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    main_with_decoy_party = mean(
      c(Candidate1_Party_type_numeric[is_decoy & Pair_Type == "main-minor"],
        Candidate2_Party_type_numeric[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    
    # educ & party for main without decoys
    main_without_decoy_education = mean(
      c(Candidate1_MyNeta_education_numeric[!is_decoy & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_MyNeta_education_numeric[!is_decoy & (Pair_Type == "minor-main" | Pair_Type == "main-main")]),
      na.rm = TRUE),
    main_without_decoy_party = mean(
      c(Candidate1_Party_type_numeric[!is_decoy & (Pair_Type == "main-minor" | Pair_Type == "main-main")],
        Candidate2_Party_type_numeric[!is_decoy & (Pair_Type == "minor-main" | Pair_Type == "main-main")]),
      na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    close_race = ifelse(winning_margin_percentage < 5, TRUE, FALSE),
    decoy_impact_potential = ifelse(decoy_vote_share > winning_margin_percentage, TRUE, FALSE)
  ) %>%
  # sort by state, year and constituency
  arrange(State_Name, Year, Constituency_Name, Election_Type, Assembly_No)

# get the overall constituency_metrics df ready
# filter out GE
constituency_election_metrics_2 <- constituency_election_metrics_2 %>%
  filter(Election_Type == "State Assembly Election (AE)")
constituency_election_metrics_2 <- constituency_election_metrics_2[!constituency_election_metrics_2$State_Name %in% states_to_remove, ]
constituency_election_metrics_2$State_Name <- sapply(constituency_election_metrics_2$State_Name, standardize_state_names)

constituency_election_metrics_2 <- constituency_election_metrics_2 %>%
  left_join(unique_elections_pre_delim, by = c("State_Name", "Year", "Constituency_Name", 
                                               "Assembly_No"))

constituency_election_metrics_2 <- constituency_election_metrics_2 %>%
  left_join(unique_elections_post_delim, by = c("State_Name", "Year", "Constituency_Name", 
                                                "Assembly_No"))

# drop if no ac07 or ac08 id
constituency_election_metrics_2 <- constituency_election_metrics_2 %>%
  filter(!is.na(ac07_id) | !is.na(ac08_id)) %>%
  dplyr::select(-Election_Type.y, -Election_Type.x, -Election_Type)

constituency_election_metrics_2_ac07 <- constituency_election_metrics_2 %>%
  filter(is.na(ac08_id)) %>%
  dplyr::select(-ac08_id)

constituency_election_metrics_2_ac08 <- constituency_election_metrics_2 %>%
  filter(is.na(ac07_id)) %>%
  dplyr::select(-ac07_id)

str(constituency_election_metrics_2)

# how do we combine pre and post delim like we did for corruption?
constituency_crosswalk <- shrid_07 %>%
  dplyr::select(shrid2, ac07_id, fragment_wt_con07) %>%
  inner_join(
    shrid_08 %>% dplyr::select(shrid2, ac08_id, fragment_wt_con08),
    by = "shrid2",
    relationship = "many-to-many"
  ) %>%
  # aggregate to create constituency-to-constituency mapping with weights
  group_by(ac07_id, ac08_id) %>%
  dplyr::summarize(
    # create a weight based on the sum of fragment weights
    weight = sum(fragment_wt_con07 * fragment_wt_con08) / 
      (sum(fragment_wt_con07) * sum(fragment_wt_con08)),
    n_shrids = n()  # no. of shrids connecting constituencies
  ) %>%
  ungroup()

constituency_election_metrics_2 <- constituency_election_metrics_2 %>%
  mutate(post_delimitation = case_when(
    !is.na(ac08_id) ~ 1,
    TRUE ~ 0
  ))

str(constituency_election_metrics_2)

constituency_election_metrics_2 <- constituency_election_metrics_2 %>%
  mutate(constituency_id = ifelse(is.na(ac08_id), ac07_id, ac08_id))

constituency_metrics_2 <- constituency_election_metrics_2 %>%
  group_by(constituency_id, State_Name, Constituency_Name) %>%
  dplyr::summarize(
    avg_total_candidates = mean(total_candidates, na.rm = TRUE),
    avg_total_votes = mean(total_votes, na.rm = TRUE),
    avg_winning_margin_pct = mean(winning_margin_percentage, na.rm = TRUE),
    years_count = n_distinct(Year),
    avg_decoy_candidates = mean(decoy_candidates, na.rm = TRUE), 
    avg_decoy_share = mean(decoy_share, na.rm = TRUE), 
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    .groups = "drop"
  )

# ### Weighting ac07 to ac08 because geographical correlation for exog vars might exist. 
# # NEED TO FIGURE OUT. 
# str(constituency_metrics)
# str(constituency_crosswalk)
# 
# constituency_metrics_pre <- constituency_metrics %>%
#   filter(startsWith(constituency_id, "2007"))
# 
# weighted_results <- data.frame()
# 
# unique_2008_ids <- unique(constituency_crosswalk$ac08_id)
# 
# constituency_metrics_tn <- constituency_metrics %>%
#   filter(State_Name == "tamil nadu")
# 
# # for each ac08
# for (ac08_id in unique_2008_ids) {
#   # get all matching rows from the crosswalk
#   matches <- constituency_crosswalk %>% 
#     filter(ac08_id == ac08_id)
#   
#   # init aggregated values
#   agg_values <- list()
#   
#   # columns to apply weights to
#   numeric_cols <- c("avg_total_candidates", "avg_total_votes", 
#                     "avg_winning_margin_pct", "avg_decoy_candidates", 
#                     "avg_decoy_share", "decoy_vote_share")
#   
#   # add 0s
#   for (col in numeric_cols) {
#     agg_values[[col]] <- 0
#   }
#   
#   total_weight <- 0
#   weighted_state_names <- character()
#   weighted_constituency_names <- character()
#   max_years_count <- 0
#   
#   # process each matching 2007 constituency
#   for (i in 1:nrow(matches)) {
#     ac07_id <- matches$ac07_id[i]
#     match_weight <- matches$weight[i]
#     
#     # get original constituency data
#     orig_const <- constituency_metrics_tn %>% 
#       filter(constituency_id == ac07_id)
#     
#     if (nrow(orig_const) == 0) next
#     
#     # store state and constituency names with weights for later weighted selection
#     weighted_state_names <- c(weighted_state_names, 
#                               rep(orig_const$State_Name[1], round(match_weight * 100)))
#     
#     weighted_constituency_names <- c(weighted_constituency_names, 
#                                      rep(orig_const$Constituency_Name[1], round(match_weight * 100)))
#     
#     # keep track of max years count - this might be more relevant than weighted average
#     max_years_count <- max(max_years_count, orig_const$years_count[1])
#     
#     # apply weights to numeric columns
#     for (col in numeric_cols) {
#       if (!is.na(orig_const[[col]])) {
#         agg_values[[col]] <- agg_values[[col]] + (orig_const[[col]] * match_weight)
#         # count the weight if the value wasn't NA
#         if (col == numeric_cols[1]) {  # Only count once per row
#           total_weight <- total_weight + match_weight
#         }
#       }
#     }
#   }
#   
#   if (total_weight > 0) {
#     # choose the most frequently weighted state and constituency name
#     state_name <- names(sort(table(weighted_state_names), decreasing = TRUE)[1])
#     constituency_name <- paste0(names(sort(table(weighted_constituency_names), decreasing = TRUE)[1]), 
#                                 " (weighted)")
#     
#     # create a new row with the aggregated values
#     new_row <- data.frame(
#       constituency_id = ac08_id,
#       State_Name = state_name,
#       Constituency_Name = constituency_name,
#       years_count = max_years_count  # use max rather than weighted average
#     )
#     
#     # add weighted num cols
#     for (col in numeric_cols) {
#       # normalize by total weight
#       if (total_weight > 0) {
#         new_row[[col]] <- agg_values[[col]] / total_weight
#       } else {
#         new_row[[col]] <- NA
#       }
#     }
#     
#     # Add to our results
#     weighted_results <- bind_rows(weighted_results, new_row)
#   }
# }
# 
# constituency_metrics_post <- constituency_metrics %>%
#   filter(startsWith(constituency_id, "2008"))
# 
# # then add weighted results
# constituency_metrics_ac08 <- bind_rows(constituency_metrics_post, weighted_results)
# 
# # then aggregate

### Merging with Shrug ExogVars
# Elevation
elev_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con07.csv")
elev_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con08.csv")

elev_1 <- elev_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

elev_2 <- elev_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

elev <- rbind(elev_1, elev_2) %>%
  dplyr::select(constituency_id, elevation_mean)

constituency_metrics <- constituency_metrics %>%
  left_join(elev, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(elevation_mean))

# Ruggedness
rug_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con07.csv")
rug_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con08.csv")

rug_1 <- rug_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

rug_2 <- rug_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

rug <- rbind(rug_1, rug_2) %>%
  dplyr::select(constituency_id, tri_mean)

constituency_metrics <- constituency_metrics %>%
  left_join(rug, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(tri_mean))

# Nightlights
dmsp_1 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con07.csv")
dmsp_2 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con08.csv")

dmsp_1 <- dmsp_1 %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

dmsp_2 <- dmsp_2 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

dmsp <- rbind(dmsp_1, dmsp_2) %>%
  dplyr::select(constituency_id, dmsp_mean_light_cal)

constituency_metrics <- constituency_metrics %>%
  left_join(dmsp, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(dmsp_mean_light_cal))

# EC (2013)
ec13_1 <- read.csv("Shrug Data/ec/shrug-ec13-csv/ec13_con07.csv")
ec13_2 <- read.csv("Shrug Data/ec/shrug-ec13-csv/ec13_con08.csv")

ec13_1 <- ec13_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec13_2 <- ec13_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec13 <- rbind(ec13_1, ec13_2) %>%
  dplyr::select(constituency_id, ec13_emp_all, ec13_emp_manuf, ec13_count_all, ec13_emp_services)

constituency_metrics <- constituency_metrics %>%
  left_join(ec13, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(ec13_emp_all))

# EC (2005)
ec05_1 <- read.csv("Shrug Data/ec/shrug-ec05-csv/ec05_con07.csv")
ec05_2 <- read.csv("Shrug Data/ec/shrug-ec05-csv/ec05_con08.csv")

ec05_1 <- ec05_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec05_2 <- ec05_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec05 <- rbind(ec05_1, ec05_2) %>%
  dplyr::select(constituency_id, ec05_emp_all, ec05_emp_manuf, ec05_count_all, ec05_emp_services)

constituency_metrics <- constituency_metrics %>%
  left_join(ec05, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(ec05_emp_all))

# EC (1998)
ec98_1 <- read.csv("Shrug Data/ec/shrug-ec98-csv/ec98_con07.csv")
ec98_2 <- read.csv("Shrug Data/ec/shrug-ec98-csv/ec98_con08.csv")

ec98_1 <- ec98_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec98_2 <- ec98_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec98 <- rbind(ec98_1, ec98_2) %>%
  dplyr::select(constituency_id, ec98_emp_all, ec98_emp_manuf, ec98_count_all, ec98_emp_services)

constituency_metrics <- constituency_metrics %>%
  left_join(ec98, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(ec98_emp_all))

# EC (1990)
ec90_1 <- read.csv("Shrug Data/ec/shrug-ec90-csv/ec90_con07.csv")
ec90_2 <- read.csv("Shrug Data/ec/shrug-ec90-csv/ec90_con08.csv")

ec90_1 <- ec90_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec90_2 <- ec90_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec90 <- rbind(ec90_1, ec90_2) %>%
  dplyr::select(constituency_id, ec90_emp_all, ec90_emp_manuf, ec90_count_all, ec90_emp_services)

constituency_metrics <- constituency_metrics %>%
  left_join(ec90, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(ec90_emp_all))

# Forest Cover
vcf_1 <- read.csv("Shrug Data/forest cover/shrug-vcf-csv/vcf_con07.csv")
vcf_2 <- read.csv("Shrug Data/forest cover/shrug-vcf-csv/vcf_con08.csv")

vcf_1 <- vcf_1 %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    vcf_mean = mean(vcf_mean, na.rm = TRUE)
  ) %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

vcf_2 <- vcf_2 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    vcf_mean = mean(vcf_mean, na.rm = TRUE)
  ) %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

vcf <- rbind(vcf_1, vcf_2) %>%
  dplyr::select(constituency_id, vcf_mean)

constituency_metrics <- constituency_metrics %>%
  left_join(vcf, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(vcf_mean))

# Area
popwt_1 <- read.csv("Shrug Data/pop weight/shrug-con07-wt-csv/con07_pop_area_key.csv")
popwt_2 <- read.csv("Shrug Data/pop weight/shrug-con08-wt-csv/con08_pop_area_key.csv")

popwt_1 <- popwt_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  rename(
    land_area = con07_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt_2 <- popwt_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  rename(
    land_area = con08_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt <- rbind(popwt_1, popwt_2) %>%
  dplyr::select(constituency_id, land_area)

constituency_metrics <- constituency_metrics %>%
  left_join(popwt, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(land_area))

# PC (2011) Abstract
pc11_1 <- read.csv("Shrug Data/pc/shrug-pca11-csv/pc11_pca_clean_con07.csv")
pc11_2 <- read.csv("Shrug Data/pc/shrug-pca11-csv/pc11_pca_clean_con08.csv")

pc11_1 <- pc11_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

pc11_2 <- pc11_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

pc11 <- rbind(pc11_1, pc11_2) %>%
  dplyr::select(constituency_id, pc11_pca_p_lit, pc11_pca_main_al_p, pc11_pca_tot_p,
                pc11_pca_m_sc, pc11_pca_f_sc, pc11_pca_p_ill, pc11_pca_m_st, pc11_pca_f_st)

constituency_metrics <- constituency_metrics %>%
  left_join(pc11, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(pc11_pca_p_lit))

# PC (2011) Town
td11_1 <- read.csv("Shrug Data/pc/shrug-td11-csv/pc11_td_clean_con07.csv")
td11_2 <- read.csv("Shrug Data/pc/shrug-td11-csv/pc11_td_clean_con08.csv")

td11_1 <- td11_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

td11_2 <- td11_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

td11 <- rbind(td11_1, td11_2) %>%
  dplyr::select(constituency_id, pc11_td_disp, pc11_td_primary_gov, pc11_td_el_dom)

constituency_metrics <- constituency_metrics %>%
  left_join(td11, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(pc11_td_el_dom))

# PC (2001) Abstract
pc01_1 <- read.csv("Shrug Data/pc/shrug-pca01-csv/pc01_pca_clean_con07.csv")
pc01_2 <- read.csv("Shrug Data/pc/shrug-pca01-csv/pc01_pca_clean_con08.csv")

pc01_1 <- pc01_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

pc01_2 <- pc01_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

pc01 <- rbind(pc01_1, pc01_2) %>%
  dplyr::select(constituency_id, pc01_pca_p_lit, pc01_pca_main_al_p, pc01_pca_tot_p,
                pc01_pca_m_sc, pc01_pca_f_sc, pc01_pca_p_ill, pc01_pca_m_st, pc01_pca_f_st)

constituency_metrics <- constituency_metrics %>%
  left_join(pc01, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(pc01_pca_p_lit))

# PC (2001) Town
td01_1 <- read.csv("Shrug Data/pc/shrug-td01-csv/pc01_td_clean_con07.csv")
td01_2 <- read.csv("Shrug Data/pc/shrug-td01-csv/pc01_td_clean_con08.csv")

td01_1 <- td01_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

td01_2 <- td01_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

td01 <- rbind(td01_1, td01_2) %>%
  dplyr::select(constituency_id, pc01_td_health_ctr, pc01_td_primary, pc01_td_el_dom)

constituency_metrics <- constituency_metrics %>%
  left_join(td01, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(pc01_td_el_dom))

# PC (1991) Abstract
pc91_1 <- read.csv("Shrug Data/pc/shrug-pca91-csv/pc91_pca_clean_con07.csv")
pc91_2 <- read.csv("Shrug Data/pc/shrug-pca91-csv/pc91_pca_clean_con08.csv")

pc91_1 <- pc91_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

pc91_2 <- pc91_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

pc91 <- rbind(pc91_1, pc91_2) %>%
  dplyr::select(constituency_id, pc91_pca_p_lit, pc91_pca_main_al_p, pc91_pca_tot_p,
                pc91_pca_m_sc, pc91_pca_f_sc, pc91_pca_m_st, pc91_pca_f_st)

constituency_metrics <- constituency_metrics %>%
  left_join(pc91, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(pc91_pca_p_lit))

# PC (1991) Town
td91_1 <- read.csv("Shrug Data/pc/shrug-td91-csv/pc91_td_clean_con07.csv")
td91_2 <- read.csv("Shrug Data/pc/shrug-td91-csv/pc91_td_clean_con08.csv")

td91_1 <- td91_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

td91_2 <- td91_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

td91 <- rbind(td91_1, td91_2) %>%
  dplyr::select(constituency_id, pc91_td_health_ctr, pc91_td_primary, pc91_td_el_dom)

constituency_metrics <- constituency_metrics %>%
  left_join(td91, by = "constituency_id")

which <- constituency_metrics %>%
  filter(is.na(pc91_td_el_dom))

### Rural simple
unique_ac07_shrid <- shrid_07 %>%
  distinct(ac07_id)

unique_ac08_shrid <- shrid_08 %>%
  distinct(ac08_id)

rural_constituencies <- c(unique_ac07_shrid$ac07_id, unique_ac08_shrid$ac08_id)

constituency_metrics <- constituency_metrics %>%
  mutate(Rural = ifelse(constituency_id %in% rural_constituencies, 1, 0))

### Prepping and cleaning the final dataset
# adding dummy for post_delim
constituency_metrics <- constituency_metrics %>%
  mutate(pre_delim = ifelse(startsWith(constituency_id, "2007"), 1, 0))

constituency_metrics <- constituency_metrics %>%
  mutate(sc_m = pc11_pca_m_sc/pc11_pca_tot_p, 
         sc_f = pc11_pca_f_sc/pc11_pca_tot_p, 
         st_m = pc11_pca_m_st/pc11_pca_tot_p, 
         st_f = pc11_pca_f_st/pc11_pca_tot_p, 
         lit = pc11_pca_p_lit/pc11_pca_tot_p, 
         illit = pc11_pca_p_ill/pc11_pca_tot_p)

### the same for constituency_metrics_2
### Merging with Shrug ExogVars
# Elevation
elev_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con07.csv")
elev_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con08.csv")

elev_1 <- elev_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

elev_2 <- elev_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

elev <- rbind(elev_1, elev_2) %>%
  dplyr::select(constituency_id, elevation_mean)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(elev, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(elevation_mean))

# Ruggedness
rug_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con07.csv")
rug_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con08.csv")

rug_1 <- rug_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

rug_2 <- rug_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

rug <- rbind(rug_1, rug_2) %>%
  dplyr::select(constituency_id, tri_mean)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(rug, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(tri_mean))

# Nightlights
dmsp_1 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con07.csv")
dmsp_2 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con08.csv")

dmsp_1 <- dmsp_1 %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

dmsp_2 <- dmsp_2 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

dmsp <- rbind(dmsp_1, dmsp_2) %>%
  dplyr::select(constituency_id, dmsp_mean_light_cal)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(dmsp, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(dmsp_mean_light_cal))

# EC (2013)
ec13_1 <- read.csv("Shrug Data/ec/shrug-ec13-csv/ec13_con07.csv")
ec13_2 <- read.csv("Shrug Data/ec/shrug-ec13-csv/ec13_con08.csv")

ec13_1 <- ec13_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec13_2 <- ec13_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec13 <- rbind(ec13_1, ec13_2) %>%
  dplyr::select(constituency_id, ec13_emp_all, ec13_emp_manuf, ec13_count_all, ec13_emp_services)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(ec13, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(ec13_emp_all))

# EC (2005)
ec05_1 <- read.csv("Shrug Data/ec/shrug-ec05-csv/ec05_con07.csv")
ec05_2 <- read.csv("Shrug Data/ec/shrug-ec05-csv/ec05_con08.csv")

ec05_1 <- ec05_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec05_2 <- ec05_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec05 <- rbind(ec05_1, ec05_2) %>%
  dplyr::select(constituency_id, ec05_emp_all, ec05_emp_manuf, ec05_count_all, ec05_emp_services)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(ec05, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(ec05_emp_all))

# EC (1998)
ec98_1 <- read.csv("Shrug Data/ec/shrug-ec98-csv/ec98_con07.csv")
ec98_2 <- read.csv("Shrug Data/ec/shrug-ec98-csv/ec98_con08.csv")

ec98_1 <- ec98_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec98_2 <- ec98_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec98 <- rbind(ec98_1, ec98_2) %>%
  dplyr::select(constituency_id, ec98_emp_all, ec98_emp_manuf, ec98_count_all, ec98_emp_services)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(ec98, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(ec98_emp_all))

# EC (1990)
ec90_1 <- read.csv("Shrug Data/ec/shrug-ec90-csv/ec90_con07.csv")
ec90_2 <- read.csv("Shrug Data/ec/shrug-ec90-csv/ec90_con08.csv")

ec90_1 <- ec90_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec90_2 <- ec90_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec90 <- rbind(ec90_1, ec90_2) %>%
  dplyr::select(constituency_id, ec90_emp_all, ec90_emp_manuf, ec90_count_all, ec90_emp_services)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(ec90, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(ec90_emp_all))

# Forest Cover
vcf_1 <- read.csv("Shrug Data/forest cover/shrug-vcf-csv/vcf_con07.csv")
vcf_2 <- read.csv("Shrug Data/forest cover/shrug-vcf-csv/vcf_con08.csv")

vcf_1 <- vcf_1 %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    vcf_mean = mean(vcf_mean, na.rm = TRUE)
  ) %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

vcf_2 <- vcf_2 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    vcf_mean = mean(vcf_mean, na.rm = TRUE)
  ) %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

vcf <- rbind(vcf_1, vcf_2) %>%
  dplyr::select(constituency_id, vcf_mean)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(vcf, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(vcf_mean))

# Area
popwt_1 <- read.csv("Shrug Data/pop weight/shrug-con07-wt-csv/con07_pop_area_key.csv")
popwt_2 <- read.csv("Shrug Data/pop weight/shrug-con08-wt-csv/con08_pop_area_key.csv")

popwt_1 <- popwt_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  rename(
    land_area = con07_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt_2 <- popwt_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  rename(
    land_area = con08_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt <- rbind(popwt_1, popwt_2) %>%
  dplyr::select(constituency_id, land_area)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(popwt, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(land_area))

# PC (2011) Abstract
pc11_1 <- read.csv("Shrug Data/pc/shrug-pca11-csv/pc11_pca_clean_con07.csv")
pc11_2 <- read.csv("Shrug Data/pc/shrug-pca11-csv/pc11_pca_clean_con08.csv")

pc11_1 <- pc11_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

pc11_2 <- pc11_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

pc11 <- rbind(pc11_1, pc11_2) %>%
  dplyr::select(constituency_id, pc11_pca_p_lit, pc11_pca_main_al_p, pc11_pca_tot_p,
                pc11_pca_m_sc, pc11_pca_f_sc, pc11_pca_p_ill, pc11_pca_m_st, pc11_pca_f_st)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(pc11, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(pc11_pca_p_lit))

# PC (2011) Town
td11_1 <- read.csv("Shrug Data/pc/shrug-td11-csv/pc11_td_clean_con07.csv")
td11_2 <- read.csv("Shrug Data/pc/shrug-td11-csv/pc11_td_clean_con08.csv")

td11_1 <- td11_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

td11_2 <- td11_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

td11 <- rbind(td11_1, td11_2) %>%
  dplyr::select(constituency_id, pc11_td_disp, pc11_td_primary_gov, pc11_td_el_dom)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(td11, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(pc11_td_el_dom))

# PC (2001) Abstract
pc01_1 <- read.csv("Shrug Data/pc/shrug-pca01-csv/pc01_pca_clean_con07.csv")
pc01_2 <- read.csv("Shrug Data/pc/shrug-pca01-csv/pc01_pca_clean_con08.csv")

pc01_1 <- pc01_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

pc01_2 <- pc01_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

pc01 <- rbind(pc01_1, pc01_2) %>%
  dplyr::select(constituency_id, pc01_pca_p_lit, pc01_pca_main_al_p, pc01_pca_tot_p,
                pc01_pca_m_sc, pc01_pca_f_sc, pc01_pca_p_ill, pc01_pca_m_st, pc01_pca_f_st)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(pc01, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(pc01_pca_p_lit))

# PC (2001) Town
td01_1 <- read.csv("Shrug Data/pc/shrug-td01-csv/pc01_td_clean_con07.csv")
td01_2 <- read.csv("Shrug Data/pc/shrug-td01-csv/pc01_td_clean_con08.csv")

td01_1 <- td01_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

td01_2 <- td01_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

td01 <- rbind(td01_1, td01_2) %>%
  dplyr::select(constituency_id, pc01_td_health_ctr, pc01_td_primary, pc01_td_el_dom)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(td01, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(pc01_td_el_dom))

# PC (1991) Abstract
pc91_1 <- read.csv("Shrug Data/pc/shrug-pca91-csv/pc91_pca_clean_con07.csv")
pc91_2 <- read.csv("Shrug Data/pc/shrug-pca91-csv/pc91_pca_clean_con08.csv")

pc91_1 <- pc91_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

pc91_2 <- pc91_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

pc91 <- rbind(pc91_1, pc91_2) %>%
  dplyr::select(constituency_id, pc91_pca_p_lit, pc91_pca_main_al_p, pc91_pca_tot_p,
                pc91_pca_m_sc, pc91_pca_f_sc, pc91_pca_m_st, pc91_pca_f_st)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(pc91, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(pc91_pca_p_lit))

# PC (1991) Town
td91_1 <- read.csv("Shrug Data/pc/shrug-td91-csv/pc91_td_clean_con07.csv")
td91_2 <- read.csv("Shrug Data/pc/shrug-td91-csv/pc91_td_clean_con08.csv")

td91_1 <- td91_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

td91_2 <- td91_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

td91 <- rbind(td91_1, td91_2) %>%
  dplyr::select(constituency_id, pc91_td_health_ctr, pc91_td_primary, pc91_td_el_dom)

constituency_metrics_2 <- constituency_metrics_2 %>%
  left_join(td91, by = "constituency_id")

which <- constituency_metrics_2 %>%
  filter(is.na(pc91_td_el_dom))

### Rural simple
unique_ac07_shrid <- shrid_07 %>%
  distinct(ac07_id)

unique_ac08_shrid <- shrid_08 %>%
  distinct(ac08_id)

rural_constituencies <- c(unique_ac07_shrid$ac07_id, unique_ac08_shrid$ac08_id)

constituency_metrics_2 <- constituency_metrics_2 %>%
  mutate(Rural = ifelse(constituency_id %in% rural_constituencies, 1, 0))

### Prepping and cleaning the final dataset
# adding dummy for post_delim
constituency_metrics_2 <- constituency_metrics_2 %>%
  mutate(pre_delim = ifelse(startsWith(constituency_id, "2007"), 1, 0))

constituency_metrics_2 <- constituency_metrics_2 %>%
  mutate(sc_m = pc11_pca_m_sc/pc11_pca_tot_p, 
         sc_f = pc11_pca_f_sc/pc11_pca_tot_p, 
         st_m = pc11_pca_m_st/pc11_pca_tot_p, 
         st_f = pc11_pca_f_st/pc11_pca_tot_p, 
         lit = pc11_pca_p_lit/pc11_pca_tot_p, 
         illit = pc11_pca_p_ill/pc11_pca_tot_p)

### the same for 1974 to 1994 ###
### Merging with Shrug ExogVars
# Elevation
elev_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con07.csv")
elev_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con08.csv")

elev_1 <- elev_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

elev_2 <- elev_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

elev <- rbind(elev_1, elev_2) %>%
  dplyr::select(constituency_id, elevation_mean)

constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  left_join(elev, by = "constituency_id")

which <- constituency_metrics_74_94 %>%
  filter(is.na(elevation_mean))

# Ruggedness
rug_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con07.csv")
rug_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con08.csv")

rug_1 <- rug_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

rug_2 <- rug_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

rug <- rbind(rug_1, rug_2) %>%
  dplyr::select(constituency_id, tri_mean)

constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  left_join(rug, by = "constituency_id")

which <- constituency_metrics_74_94 %>%
  filter(is.na(tri_mean))

# Nightlights
dmsp_1 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con07.csv")
dmsp_2 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con08.csv")

dmsp_1 <- dmsp_1 %>%
  filter(year == 1994) %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

dmsp_2 <- dmsp_2 %>%
  filter(year == 1994) %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

dmsp <- rbind(dmsp_1, dmsp_2) %>%
  dplyr::select(constituency_id, dmsp_mean_light_cal)

constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  left_join(dmsp, by = "constituency_id")

which <- constituency_metrics_74_94 %>%
  filter(is.na(dmsp_mean_light_cal))

# EC (1990)
ec90_1 <- read.csv("Shrug Data/ec/shrug-ec90-csv/ec90_con07.csv")
ec90_2 <- read.csv("Shrug Data/ec/shrug-ec90-csv/ec90_con08.csv")

ec90_1 <- ec90_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec90_2 <- ec90_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec90 <- rbind(ec90_1, ec90_2) %>%
  dplyr::select(constituency_id, ec90_emp_all, ec90_emp_manuf, ec90_count_all, ec90_emp_services)

constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  left_join(ec90, by = "constituency_id")

which <- constituency_metrics_74_94 %>%
  filter(is.na(ec90_emp_all))

# Area
popwt_1 <- read.csv("Shrug Data/pop weight/shrug-con07-wt-csv/con07_pop_area_key.csv")
popwt_2 <- read.csv("Shrug Data/pop weight/shrug-con08-wt-csv/con08_pop_area_key.csv")

popwt_1 <- popwt_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  rename(
    land_area = con07_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt_2 <- popwt_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  rename(
    land_area = con08_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt <- rbind(popwt_1, popwt_2) %>%
  dplyr::select(constituency_id, land_area)

constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  left_join(popwt, by = "constituency_id")

which <- constituency_metrics_74_94 %>%
  filter(is.na(land_area))

# PC (1991) Abstract
pc91_1 <- read.csv("Shrug Data/pc/shrug-pca91-csv/pc91_pca_clean_con07.csv")
pc91_2 <- read.csv("Shrug Data/pc/shrug-pca91-csv/pc91_pca_clean_con08.csv")

pc91_1 <- pc91_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

pc91_2 <- pc91_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

pc91 <- rbind(pc91_1, pc91_2) %>%
  dplyr::select(constituency_id, pc91_pca_p_lit, pc91_pca_main_al_p, pc91_pca_tot_p,
                pc91_pca_m_sc, pc91_pca_f_sc, pc91_pca_m_st, pc91_pca_f_st)

constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  left_join(pc91, by = "constituency_id")

which <- constituency_metrics_74_94 %>%
  filter(is.na(pc91_pca_p_lit))

# PC (1991) Town
td91_1 <- read.csv("Shrug Data/pc/shrug-td91-csv/pc91_td_clean_con07.csv")
td91_2 <- read.csv("Shrug Data/pc/shrug-td91-csv/pc91_td_clean_con08.csv")

td91_1 <- td91_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

td91_2 <- td91_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

td91 <- rbind(td91_1, td91_2) %>%
  dplyr::select(constituency_id, pc91_td_health_ctr, pc91_td_primary, pc91_td_el_dom)

constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  left_join(td91, by = "constituency_id")

which <- constituency_metrics_74_94 %>%
  filter(is.na(pc91_td_el_dom))

### Rural simple
unique_ac07_shrid <- shrid_07 %>%
  distinct(ac07_id)

unique_ac08_shrid <- shrid_08 %>%
  distinct(ac08_id)

rural_constituencies <- c(unique_ac07_shrid$ac07_id, unique_ac08_shrid$ac08_id)

constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  mutate(Rural = ifelse(constituency_id %in% rural_constituencies, 1, 0))

### Prepping and cleaning the final dataset
# adding dummy for post_delim
constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  mutate(pre_delim = ifelse(startsWith(constituency_id, "2007"), 1, 0))

constituency_metrics_74_94 <- constituency_metrics_74_94 %>%
  mutate(sc_m = pc91_pca_m_sc/pc91_pca_tot_p, 
         sc_f = pc91_pca_f_sc/pc91_pca_tot_p, 
         st_m = pc91_pca_m_st/pc91_pca_tot_p, 
         st_f = pc91_pca_f_st/pc91_pca_tot_p, 
         lit = pc91_pca_p_lit/pc91_pca_tot_p, 
         lit = pc91_pca_p_lit/pc91_pca_tot_p)

### same for 1995 to 2004
### Merging with Shrug ExogVars
# Elevation
elev_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con07.csv")
elev_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con08.csv")

elev_1 <- elev_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

elev_2 <- elev_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

elev <- rbind(elev_1, elev_2) %>%
  dplyr::select(constituency_id, elevation_mean)

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  left_join(elev, by = "constituency_id")

which <- constituency_metrics_95_04 %>%
  filter(is.na(elevation_mean))

# Ruggedness
rug_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con07.csv")
rug_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con08.csv")

rug_1 <- rug_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

rug_2 <- rug_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

rug <- rbind(rug_1, rug_2) %>%
  dplyr::select(constituency_id, tri_mean)

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  left_join(rug, by = "constituency_id")

which <- constituency_metrics_95_04 %>%
  filter(is.na(tri_mean))

# Nightlights
dmsp_1 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con07.csv")
dmsp_2 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con08.csv")

dmsp_1 <- dmsp_1 %>%
  filter(year >= 1994 & year <= 2004) %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

dmsp_2 <- dmsp_2 %>%
  filter(year >= 1994 & year <= 2004) %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

dmsp <- rbind(dmsp_1, dmsp_2) %>%
  dplyr::select(constituency_id, dmsp_mean_light_cal)

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  left_join(dmsp, by = "constituency_id")

which <- constituency_metrics_95_04 %>%
  filter(is.na(dmsp_mean_light_cal))

# EC (1998)
ec98_1 <- read.csv("Shrug Data/ec/shrug-ec98-csv/ec98_con07.csv")
ec98_2 <- read.csv("Shrug Data/ec/shrug-ec98-csv/ec98_con08.csv")

ec98_1 <- ec98_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec98_2 <- ec98_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec98 <- rbind(ec98_1, ec98_2) %>%
  dplyr::select(constituency_id, ec98_emp_all, ec98_emp_manuf, ec98_count_all, ec98_emp_services)

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  left_join(ec98, by = "constituency_id")

which <- constituency_metrics_95_04 %>%
  filter(is.na(ec98_emp_all))

# Forest Cover
vcf_1 <- read.csv("Shrug Data/forest cover/shrug-vcf-csv/vcf_con07.csv")
vcf_2 <- read.csv("Shrug Data/forest cover/shrug-vcf-csv/vcf_con08.csv")

vcf_1 <- vcf_1 %>%
  filter(year >= 1995 & year <= 2004) %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    vcf_mean = mean(vcf_mean, na.rm = TRUE)
  ) %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

vcf_2 <- vcf_2 %>%
  filter(year >= 1995 & year <= 2004) %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    vcf_mean = mean(vcf_mean, na.rm = TRUE)
  ) %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

vcf <- rbind(vcf_1, vcf_2) %>%
  dplyr::select(constituency_id, vcf_mean)

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  left_join(vcf, by = "constituency_id")

which <- constituency_metrics_95_04 %>%
  filter(is.na(vcf_mean))

# Area
popwt_1 <- read.csv("Shrug Data/pop weight/shrug-con07-wt-csv/con07_pop_area_key.csv")
popwt_2 <- read.csv("Shrug Data/pop weight/shrug-con08-wt-csv/con08_pop_area_key.csv")

popwt_1 <- popwt_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  rename(
    land_area = con07_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt_2 <- popwt_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  rename(
    land_area = con08_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt <- rbind(popwt_1, popwt_2) %>%
  dplyr::select(constituency_id, land_area)

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  left_join(popwt, by = "constituency_id")

which <- constituency_metrics_95_04 %>%
  filter(is.na(land_area))

# PC (2001) Abstract
pc01_1 <- read.csv("Shrug Data/pc/shrug-pca01-csv/pc01_pca_clean_con07.csv")
pc01_2 <- read.csv("Shrug Data/pc/shrug-pca01-csv/pc01_pca_clean_con08.csv")

pc01_1 <- pc01_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

pc01_2 <- pc01_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

pc01 <- rbind(pc01_1, pc01_2) %>%
  dplyr::select(constituency_id, pc01_pca_p_lit, pc01_pca_main_al_p, pc01_pca_tot_p,
                pc01_pca_m_sc, pc01_pca_f_sc, pc01_pca_p_ill, pc01_pca_m_st, pc01_pca_f_st)

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  left_join(pc01, by = "constituency_id")

which <- constituency_metrics_95_04 %>%
  filter(is.na(pc01_pca_p_lit))

# PC (2001) Town
td01_1 <- read.csv("Shrug Data/pc/shrug-td01-csv/pc01_td_clean_con07.csv")
td01_2 <- read.csv("Shrug Data/pc/shrug-td01-csv/pc01_td_clean_con08.csv")

td01_1 <- td01_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

td01_2 <- td01_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

td01 <- rbind(td01_1, td01_2) %>%
  dplyr::select(constituency_id, pc01_td_health_ctr, pc01_td_primary, pc01_td_el_dom)

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  left_join(td01, by = "constituency_id")

which <- constituency_metrics_95_04 %>%
  filter(is.na(pc01_td_el_dom))

### Rural simple
unique_ac07_shrid <- shrid_07 %>%
  distinct(ac07_id)

unique_ac08_shrid <- shrid_08 %>%
  distinct(ac08_id)

rural_constituencies <- c(unique_ac07_shrid$ac07_id, unique_ac08_shrid$ac08_id)

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  mutate(Rural = ifelse(constituency_id %in% rural_constituencies, 1, 0))

### Prepping and cleaning the final dataset
# adding dummy for post_delim
constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  mutate(pre_delim = ifelse(startsWith(constituency_id, "2007"), 1, 0))

constituency_metrics_95_04 <- constituency_metrics_95_04 %>%
  mutate(sc_m = pc01_pca_m_sc/pc01_pca_tot_p, 
         sc_f = pc01_pca_f_sc/pc01_pca_tot_p, 
         st_m = pc01_pca_m_st/pc01_pca_tot_p, 
         st_f = pc01_pca_f_st/pc01_pca_tot_p, 
         lit = pc01_pca_p_lit/pc01_pca_tot_p, 
         illit = pc01_pca_p_ill/pc01_pca_tot_p)

### same for 2005 to 2023
### Merging with Shrug ExogVars
# Elevation
elev_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con07.csv")
elev_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-elevation-csv/elevation_con08.csv")

elev_1 <- elev_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

elev_2 <- elev_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

elev <- rbind(elev_1, elev_2) %>%
  dplyr::select(constituency_id, elevation_mean)

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  left_join(elev, by = "constituency_id")

which <- constituency_metrics_05_23 %>%
  filter(is.na(elevation_mean))

# Ruggedness
rug_1 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con07.csv")
rug_2 <- read.csv("Shrug Data/elevation ruggedness/shrug-rugged-csv/tri_con08.csv")

rug_1 <- rug_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

rug_2 <- rug_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

rug <- rbind(rug_1, rug_2) %>%
  dplyr::select(constituency_id, tri_mean)

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  left_join(rug, by = "constituency_id")

which <- constituency_metrics_05_23 %>%
  filter(is.na(tri_mean))

# Nightlights
dmsp_1 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con07.csv")
dmsp_2 <- read.csv("Shrug Data/night lights/shrug-dmsp-csv/dmsp_con08.csv")

dmsp_1 <- dmsp_1 %>%
  filter(year >= 2005 & year <= 2023) %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

dmsp_2 <- dmsp_2 %>%
  filter(year >= 2005 & year <= 2023) %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    dmsp_mean_light_cal = mean(dmsp_mean_light_cal, na.rm = TRUE)
  ) %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

dmsp <- rbind(dmsp_1, dmsp_2) %>%
  dplyr::select(constituency_id, dmsp_mean_light_cal)

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  left_join(dmsp, by = "constituency_id")

which <- constituency_metrics_05_23 %>%
  filter(is.na(dmsp_mean_light_cal))

# EC (2013)
ec13_1 <- read.csv("Shrug Data/ec/shrug-ec13-csv/ec13_con07.csv")
ec13_2 <- read.csv("Shrug Data/ec/shrug-ec13-csv/ec13_con08.csv")

ec13_1 <- ec13_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

ec13_2 <- ec13_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

ec13 <- rbind(ec13_1, ec13_2) %>%
  dplyr::select(constituency_id, ec13_emp_all, ec13_emp_manuf, ec13_count_all, ec13_emp_services)

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  left_join(ec13, by = "constituency_id")

which <- constituency_metrics_05_23 %>%
  filter(is.na(ec13_emp_all))

# Forest Cover
vcf_1 <- read.csv("Shrug Data/forest cover/shrug-vcf-csv/vcf_con07.csv")
vcf_2 <- read.csv("Shrug Data/forest cover/shrug-vcf-csv/vcf_con08.csv")

vcf_1 <- vcf_1 %>%
  filter(year >= 2005 & year <= 2023) %>%
  group_by(ac07_id) %>%
  dplyr::summarize(
    vcf_mean = mean(vcf_mean, na.rm = TRUE)
  ) %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

vcf_2 <- vcf_2 %>%
  filter(year >= 2005 & year <= 2023) %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    vcf_mean = mean(vcf_mean, na.rm = TRUE)
  ) %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

vcf <- rbind(vcf_1, vcf_2) %>%
  dplyr::select(constituency_id, vcf_mean)

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  left_join(vcf, by = "constituency_id")

which <- constituency_metrics_05_23 %>%
  filter(is.na(vcf_mean))

# Area
popwt_1 <- read.csv("Shrug Data/pop weight/shrug-con07-wt-csv/con07_pop_area_key.csv")
popwt_2 <- read.csv("Shrug Data/pop weight/shrug-con08-wt-csv/con08_pop_area_key.csv")

popwt_1 <- popwt_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  rename(
    land_area = con07_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt_2 <- popwt_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  rename(
    land_area = con08_pc01_pca_tot_p
  ) %>%
  dplyr::select(constituency_id, land_area)

popwt <- rbind(popwt_1, popwt_2) %>%
  dplyr::select(constituency_id, land_area)

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  left_join(popwt, by = "constituency_id")

which <- constituency_metrics_05_23 %>%
  filter(is.na(land_area))

# PC (2011) Abstract
pc11_1 <- read.csv("Shrug Data/pc/shrug-pca11-csv/pc11_pca_clean_con07.csv")
pc11_2 <- read.csv("Shrug Data/pc/shrug-pca11-csv/pc11_pca_clean_con08.csv")

pc11_1 <- pc11_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

pc11_2 <- pc11_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

pc11 <- rbind(pc11_1, pc11_2) %>%
  dplyr::select(constituency_id, pc11_pca_p_lit, pc11_pca_main_al_p, pc11_pca_tot_p,
                pc11_pca_m_sc, pc11_pca_f_sc, pc11_pca_p_ill, pc11_pca_m_st, pc11_pca_f_st)

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  left_join(pc11, by = "constituency_id")

which <- constituency_metrics_05_23 %>%
  filter(is.na(pc11_pca_p_lit))

# PC (2011) Town
td11_1 <- read.csv("Shrug Data/pc/shrug-td11-csv/pc11_td_clean_con07.csv")
td11_2 <- read.csv("Shrug Data/pc/shrug-td11-csv/pc11_td_clean_con08.csv")

td11_1 <- td11_1 %>%
  left_join(ac07_name_key, by = "ac07_id") %>%
  rename(
    constituency_id = ac07_id
  ) %>%
  dplyr::select(-ac07_name)

td11_2 <- td11_2 %>%
  left_join(ac08_name_key, by = "ac08_id") %>%
  rename(
    constituency_id = ac08_id
  ) %>%
  dplyr::select(-ac08_name)

td11 <- rbind(td11_1, td11_2) %>%
  dplyr::select(constituency_id, pc11_td_disp, pc11_td_primary_gov, pc11_td_el_dom)

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  left_join(td11, by = "constituency_id")

which <- constituency_metrics_05_23 %>%
  filter(is.na(pc11_td_el_dom))

### Rural simple
unique_ac07_shrid <- shrid_07 %>%
  distinct(ac07_id)

unique_ac08_shrid <- shrid_08 %>%
  distinct(ac08_id)

rural_constituencies <- c(unique_ac07_shrid$ac07_id, unique_ac08_shrid$ac08_id)

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  mutate(Rural = ifelse(constituency_id %in% rural_constituencies, 1, 0))

### Prepping and cleaning the final dataset
# adding dummy for post_delim
constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  mutate(pre_delim = ifelse(startsWith(constituency_id, "2007"), 1, 0))

constituency_metrics_05_23 <- constituency_metrics_05_23 %>%
  mutate(sc_m = pc11_pca_m_sc/pc11_pca_tot_p, 
         sc_f = pc11_pca_f_sc/pc11_pca_tot_p, 
         st_m = pc11_pca_m_st/pc11_pca_tot_p, 
         st_f = pc11_pca_f_st/pc11_pca_tot_p, 
         lit = pc11_pca_p_lit/pc11_pca_tot_p, 
         illit = pc11_pca_p_ill/pc11_pca_tot_p)

### Saving
write.csv(constituency_metrics, "Cleaned Data/constituency_metrics_w_exogvars.csv")
write.csv(constituency_metrics_2, "Cleaned Data/constituency_metrics_w_exogvars_filtered.csv")
write.csv(constituency_metrics_74_94, "Cleaned Data/constituency_metrics_w_exogvars_74_94.csv")
write.csv(constituency_metrics_95_04, "Cleaned Data/constituency_metrics_w_exogvars_95_04.csv")
write.csv(constituency_metrics_05_23, "Cleaned Data/constituency_metrics_w_exogvars_05_23.csv")

### Trial Coef Plot
# constituency_metrics_2_post_delim
constituency_metrics_2_post <- constituency_metrics_2 %>%
  filter(startsWith(constituency_id, "2008"))

constituency_metrics_2_pre <- constituency_metrics_2 %>%
  filter(startsWith(constituency_id, "2007"))

regressors <- c(
  "elevation_mean", "tri_mean", "dmsp_mean_light_cal",
  "vcf_mean", "land_area",
  "sc_m", "sc_f", "st_m", "st_f", "illit", 
  "ec13_emp_all", "ec13_count_all", "ec13_emp_manuf", "ec13_emp_services",
  "pc11_td_disp", "pc11_td_primary_gov", "pc11_td_el_dom", "Rural"
)

regressor_labels <- c(
  "elevation_mean" = "Mean Elevation",
  "tri_mean" = "Mean Ruggedness",
  "dmsp_mean_light_cal" = "Mean Night Lights (< 2011)",
  "land_area" = "Land Area",
  "vcf_mean" = "Vegetation Cover (2010)",
  "sc_m" = "SC Population (Male) % (2011) ",
  "sc_f" = "SC Population (Female) % (2011)",
  "st_m" = "ST Population (Male) % (2011)",
  "st_f" = "ST Population (Female) % (2011)",
  "illit" = "Illiterate Population % (2011)",
  "ec13_emp_all" = "Total Employment (2013)",
  "ec13_count_all" = "Total Firms (2013)",
  "ec13_emp_manuf" = "Manufacturing Employment (2013)",
  "ec13_emp_services" = "Services Employment (2013)",
  "pc11_td_disp" = "Dispensaries (2011)",
  "pc11_td_primary_gov" = "Primary Govt Schools (2011)",
  "pc11_td_el_dom" = "Hours of Power Supply during Summer (2011)", 
  "Rural" = "Rural Constituency Dummy Acc. shrid Coverage"
)

outcomes <- c("avg_decoy_candidates", "avg_decoy_share", "decoy_vote_share")  # Replace with your actual outcome variables
outcome_labels <- c(
  "avg_decoy_candidates" = "Average No. Decoys Per Election",
  "avg_decoy_share" = "Average % of Decoys Per Election ",
  "decoy_vote_share" = "Average Voteshare for Decoys"
)

# create standardized versions of outcome variables
for(outcome in outcomes) {
  constituency_metrics_2[[paste0("z_", outcome)]] <- scale(constituency_metrics_2[[outcome]])
}

# run separate regressions for each regressor and outcome
model_results <- list()

for(outcome in outcomes) {
  z_outcome <- paste0("z_", outcome)
  
  # run a separate regression for each regressor
  for(reg in regressors) {
    # create formula for individual regressor
    formula_str <- paste(z_outcome, "~", reg, "+ pre_delim")
    
    # Run the model
    model <- feols(
      as.formula(formula_str),
      data = constituency_metrics_2,
      cluster = "constituency_id"
    )
    
    # extract coefficient and standard error
    if(reg %in% names(coef(model))) {
      coef_val <- coef(model)[reg]
      se_val <- sqrt(vcov(model)[reg, reg])
      
      # store the results
      model_results[[paste0(outcome, "_", reg)]] <- data.frame(
        outcome = outcome,
        regressor = reg,
        estimate = coef_val,
        std.error = se_val,
        conf_low_90 = coef_val - 1.645 * se_val,
        conf_high_90 = coef_val + 1.645 * se_val,
        conf_low_95 = coef_val - 1.96 * se_val,
        conf_high_95 = coef_val + 1.96 * se_val,
        conf_low_99 = coef_val - 2.576 * se_val,
        conf_high_99 = coef_val + 2.576 * se_val
      )
    }
  }
}

# combine all results
plot_data <- do.call(rbind, model_results)

# add labels
plot_data$outcome_label <- outcome_labels[plot_data$outcome]
plot_data$regressor_label <- regressor_labels[plot_data$regressor]

# convert to factors for ordering
plot_data$outcome_label <- factor(plot_data$outcome_label,
                                  levels = outcome_labels,
                                  ordered = TRUE)
plot_data$regressor_label <- factor(plot_data$regressor_label,
                                    levels = rev(regressor_labels[regressors]),
                                    ordered = TRUE)

# show all outcomes on same chart
ggplot(plot_data, aes(y = regressor_label, x = estimate, color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.5)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coeff Plot: Decoy Vars on Constituency Characteristics") +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# group the regressors
regressor_groups <- list(
  "Geographic" = c("Mean Elevation", "Mean Ruggedness", "Vegetation Cover (2010)", "Land Area"),
  "Demographics" = c("SC Population (Male) % (2011) ", "SC Population (Female) % (2011)",
                     "ST Population (Male) % (2011)", "ST Population (Female) % (2011)",
                     "Illiterate Population % (2011)", "Rural Constituency Dummy Acc. shrid Coverage"),
  "Economy" = c("Total Employment (2013)", "Total Firms (2013)", 
                "Manufacturing Employment (2013)", "Services Employment (2013)"),
  "Infrastructure" = c("Dispensaries (2011)", "Primary Govt Schools (2011)", 
                       "Hours of Power Supply during Summer (2011)", "Mean Night Lights (< 2011)")
)

# add group column to plot_data
plot_data$group <- NA
for (group_name in names(regressor_groups)) {
  plot_data$group[plot_data$regressor_label %in% regressor_groups[[group_name]]] <- group_name
}

# convert group to factor with specific order
plot_data$group <- factor(plot_data$group, 
                          levels = names(regressor_groups))

# now plot with groups
ggplot(plot_data, aes(y = regressor_label, x = estimate, 
                      color = outcome_label)) +
  # add reference line
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  
  # add CIs for 95%
  geom_linerange(aes(xmin = conf_low_95, xmax = conf_high_95),
                 linewidth = 1, alpha = 0.7,
                 position = position_dodge(width = 0.7)) +
  
  # add point estimates
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  
  # grp by category
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  
  # format
  theme_bw() +
  labs(x = "Standardized Coefficient (Standard Deviations)",
       y = NULL,
       color = "Outcome",
       shape = "Outcome",
       title = "Coefficient Plot by Regressor Category") +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 10, r = 0, b = 0, l = 0),
    legend.box.margin = margin(t = 0, r = 0, b = 10, l = 0),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 3)) +  # force legend to use 2 rows
  scale_color_brewer(palette = "Set1")




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
candidate_pairs_CLEA <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA.csv")
candidate_pairs_CLEA_the_rest <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA_the_rest.csv")

triv <- read_csv("/Users/anirvin/Downloads/Corruption Protest Data/Raw Data/shrug-triv-cand-csv/trivedi_candidates_clean.csv")

shrid_07 <- read_csv("/Users/anirvin/Downloads/Corruption Protest Data/Raw Data/shrug-con-keys-csv/shrid_frag_con07_key.csv")
shrid_08 <- read_csv("/Users/anirvin/Downloads/Corruption Protest Data/Raw Data/shrug-con-keys-csv/shrid_frag_con08_key.csv")

### Combining the candidate_pairs_CLEA dfs
candidate_pairs_CLEA_full <- rbind(candidate_pairs_CLEA, candidate_pairs_CLEA_the_rest)

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

### Matching constituency names in all_states_electionss to ac07_id and ac08_id
str(all_states_elections)
str(ac07_name_key)
str(ac08_name_key)

# mapping state id to name
state_id_map_ac07 <- unique(ac07_name_key[, c("pc01_state_name", "pc01_state_id")])
colnames(state_id_map_ac07) <- c("state_name", "state_id_ac07")

state_id_map_ac08 <- unique(ac08_name_key[, c("pc01_state_name", "pc01_state_id")])
colnames(state_id_map_ac08) <- c("state_name", "state_id_ac08")

# which elections in 2008?
all_states_elections_filtered_2008 <- all_states_elections_filtered %>%
  filter(Year == "2008")

# filter all_states_elections for before 2008
all_states_elections_filtered_pre_08 <- all_states_elections_filtered %>%
  filter(Year < "2008")

# for every constituency, create both ac07 and ac08 id
format_state_id <- function(id) {
  sprintf("%02d", id)
}

format_constituency_id <- function(id) {
  sprintf("%03d", id)
}

unique(all_states_elections$State_Name)

# add ac07_id col
all_states_elections_filtered$ac07_id <- NA

# go through rows to create ac07_id
for (i in 1:nrow(all_states_elections_filtered)) {
  state_name <- all_states_elections_filtered$State_Name[i]
  
  # find the state_id from the state_map for this state name
  state_id_row <- state_id_map_ac07[state_id_map_ac07$state_name == state_name, ]
  
  if (nrow(state_id_row) > 0 && !is.na(state_id_row$state_id_ac07)) {
    state_id_07 <- state_id_row$state_id_ac07
    
    # get constituency_id from Constituency_No column
    constituency_id <- all_states_elections_filtered$Constituency_No[i]
    
    # format state_id and constituency_id
    formatted_state_id <- format_state_id(state_id_07)
    formatted_constituency_id <- format_constituency_id(constituency_id)
    
    # create the ac07_id as 2007-state_id-constituency_id
    all_states_elections_filtered$ac07_id[i] <- paste0("2007-", formatted_state_id, "-", formatted_constituency_id)
  }
}

# add ac08_id
all_states_elections_filtered$ac08_id <- NA

# go through rows to create ac08_id
for (i in 1:nrow(all_states_elections_filtered)) {
  state_name <- all_states_elections_filtered$State_Name[i]
  
  # find the state_id from the state_map for this state name
  state_id_row <- state_id_map_ac08[state_id_map_ac08$state_name == state_name, ]
  
  if (nrow(state_id_row) > 0 && !is.na(state_id_row$state_id_ac08)) {
    state_id_08 <- state_id_row$state_id_ac08
    
    # get constituency_id from Constituency_No column
    constituency_id <- all_states_elections_filtered$Constituency_No[i]
    
    # format state_id and constituency_id
    formatted_state_id <- format_state_id(state_id_08)
    formatted_constituency_id <- format_constituency_id(constituency_id)
    
    # create the ac08_id as 2008-state_id-constituency_id
    all_states_elections_filtered$ac08_id[i] <- paste0("2008-", formatted_state_id, "-", formatted_constituency_id)
  }
}

# check the first few rows to see if its all good
head(all_states_elections_filtered[, c("State_Name", "Constituency_No", "ac07_id", "ac08_id")])

# check how many ac07_id values from all_states_elections match with ac07_name_key
matching_ac07_ids <- all_states_elections_filtered$ac07_id %in% ac07_name_key$ac07_id
match_count_ac07 <- sum(matching_ac07_ids, na.rm = TRUE)
total_count_ac07 <- sum(!is.na(all_states_elections_filtered$ac07_id))

all_states_elections_filtered$ac07_id_has_match <- all_states_elections_filtered$ac07_id %in% ac07_name_key$ac07_id

# do the same check for ac08_id 
matching_ac08_ids <- all_states_elections_filtered$ac08_id %in% ac08_name_key$ac08_id
match_count_ac08 <- sum(matching_ac08_ids, na.rm = TRUE)
total_count_ac08 <- sum(!is.na(all_states_elections_filtered$ac08_id))

all_states_elections_filtered$ac08_id_has_match <- all_states_elections_filtered$ac08_id %in% ac08_name_key$ac08_id

all_states_elections_filtered_weird_matches <- all_states_elections_filtered %>%
  filter(ac08_id_has_match == TRUE & ac07_id_has_match == FALSE | 
           ac07_id_has_match == FALSE & ac08_id_has_match == TRUE | 
           ac07_id_has_match == FALSE & ac08_id_has_match == FALSE) %>%
  dplyr::select(State_Name, Constituency_Name, Constituency_No, Year, ac07_id, ac07_id_has_match, ac08_id, ac08_id_has_match)

# drop all of these
all_states_elections_final <- all_states_elections_filtered %>%
  filter(ac08_id_has_match == TRUE & ac07_id_has_match == TRUE)

# what is happening with the ids
what_is_happening <- all_states_elections_final %>%
  dplyr::select(Year, Constituency_No, Constituency_Name, ac07_id, ac08_id, 
                ac07_id_has_match, ac08_id_has_match)
  # AH I SEE. the identifier for a constituency in pre-2007, based on post-2008 constituency number, 
  # may actually identify a DIFFERENT constituency pre-2007. for e.g. achampet ac08_id is 2008-28-082. 
  # but, since we build both 2007 and 2008 ids, the constructed 2007 id of 2007-28-082 corresponds to a 
  # diff constituency. 
  # So ... we need to use years. 

### ANI!!! THERE IS A FUCKING DELIMID! 
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

constituency_metrics <- candidate_pairs %>%
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
constituency_metrics <- constituency_metrics %>%
  filter(Election_Type == "State Assembly Election (AE)")

constituency_metrics$State_Name <- sapply(constituency_metrics$State_Name, standardize_state_names)

constituency_metrics <- constituency_metrics %>%
  left_join(unique_elections_pre_delim, by = c("State_Name", "Year", "Constituency_Name", 
                                               "Assembly_No"))

constituency_metrics <- constituency_metrics %>%
  left_join(unique_elections_post_delim, by = c("State_Name", "Year", "Constituency_Name", 
                                               "Assembly_No"))

# drop if no ac07 or ac08 id
constituency_metrics <- constituency_metrics %>%
  filter(!is.na(ac07_id) | !is.na(ac08_id)) %>%
  dplyr::select(-Election_Type.y, -Election_Type.x, -Election_Type)

constituency_metrics_ac07 <- constituency_metrics %>%
  filter(is.na(ac08_id)) %>%
  dplyr::select(-ac08_id)

constituency_metrics_ac08 <- constituency_metrics %>%
  filter(is.na(ac07_id)) %>%
  dplyr::select(-ac07_id)

str(constituency_metrics)




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

# get top match for each 2007 constituency (for simple mapping)
ac07_to_ac08_mapping <- constituency_crosswalk %>%
  group_by(ac07_id) %>%
  arrange(desc(weight)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(ac07_id, ac08_id, weight)

# get top match for each 2008 constituency (for reverse mapping)
ac08_to_ac07_mapping <- constituency_crosswalk %>%
  group_by(ac08_id) %>%
  arrange(desc(weight)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(ac08_id, ac07_id, weight)

# handle ac07_id using crosswalk
# join pre2011 with crosswalk mapping
temp_constituency_metrics <- constituency_metrics_ac07 %>%
  left_join(constituency_crosswalk, by = "ac07_id") %>%
  # filter to keep only valid mappings
  filter(!is.na(ac08_id))

# weight by crosswalk weight
# weight represents what proportion of the ac07 constituency maps to each ac08 constituency
str(temp_constituency_metrics)

weighted_07_08 <- temp_constituency_metrics %>%
  mutate(across(
    c(total_candidates, decoy_candidates, decoy_share, total_votes, 
      total_votes_to_decoys, decoy_vote_share, winning_margin, 
      winning_margin_percentage, winner_vote_share, runner_up_vote_share, 
      main_without_decoy_party, weight),  # optionally include weight itself
    ~ . * weight
  ))

# agg by ac08_id to get weighted averages
# so this correctly distributes values across post-delimitation constituencies
agg_07_08 <- weighted_07_08 %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    total_candidates = mean(total_candidates, na.rm = TRUE),
    decoy_candidates = mean(decoy_candidates, na.rm = TRUE),
    decoy_share = mean(decoy_share, na.rm = TRUE),
    total_votes = mean(total_votes, na.rm = TRUE),
    total_votes_to_decoys = mean(total_votes_to_decoys, na.rm = TRUE),
    decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    winning_margin = mean(winning_margin, na.rm = TRUE),
    winning_margin_percentage = mean(winning_margin_percentage, na.rm = TRUE),
    winner_vote_share = mean(winner_vote_share, na.rm = TRUE),
    runner_up_vote_share = mean(runner_up_vote_share, na.rm = TRUE)
  ) %>%
  ungroup()

# join the weighted 07 data to our final dataset
final_dataset <- final_dataset %>%
  left_join(
    agg_07_08,
    by = "ac08_id",
    suffix = c("", "_07")
  )

final_dataset <- final_dataset %>%
  mutate(
    used_only_07_data = !is.na(turnout_percentage_pre_07) & is.na(turnout_percentage_pre),
    used_07_data = is.na(turnout_percentage_pre)
  )

# coalesce the columns
final_dataset <- final_dataset %>%
  mutate(
    # do manually lah
    turnout_percentage_pre = coalesce(turnout_percentage_pre, turnout_percentage_pre_07),
    n_cand_pre = coalesce(n_cand_pre, n_cand_pre_07),
    total_cand_pre = coalesce(total_cand_pre, total_cand_pre_07),
    n_elections_pre = coalesce(n_elections_pre, n_elections_pre_07),
    total_criminals_pre = coalesce(total_criminals_pre, total_criminals_pre_07),
    total_num_crim_pre = coalesce(total_num_crim_pre, total_num_crim_pre_07),
    total_major_criminals_pre = coalesce(total_major_criminals_pre, total_major_criminals_pre_07),
    mean_ed_pre = coalesce(mean_ed_pre, mean_ed_pre_07),
    cong_vote_share_pre = coalesce(cong_vote_share_pre, cong_vote_share_pre_07),
    bjp_vote_share_pre = coalesce(bjp_vote_share_pre, bjp_vote_share_pre_07),
    bsp_vote_share_pre = coalesce(bsp_vote_share_pre, bsp_vote_share_pre_07),
    crimes_per_politician_pre = coalesce(crimes_per_politician_pre, crimes_per_politician_pre_07),
    candidates_per_election_pre = coalesce(candidates_per_election_pre, candidates_per_election_pre_07),
    crimes_per_election_pre = coalesce(crimes_per_election_pre, crimes_per_election_pre_07),
    criminals_per_election_pre = coalesce(criminals_per_election_pre, criminals_per_election_pre_07),
    big_party_vote_share_pre = coalesce(big_party_vote_share_pre, big_party_vote_share_pre_07)
  )

final_dataset <- final_dataset %>%
  dplyr::select(-ends_with("_07"))

# additional metadata about crosswalk
final_dataset <- final_dataset %>%
  left_join(
    constituency_crosswalk %>%
      group_by(ac08_id) %>%
      dplyr::summarize(
        n_source_constituencies = n_distinct(ac07_id),
        avg_crosswalk_weight = mean(weight, na.rm = TRUE),
        max_crosswalk_weight = max(weight, na.rm = TRUE),
        n_shrids_in_crosswalk = sum(n_shrids, na.rm = TRUE)
      ),
    by = "ac08_id"
  )

### Merging with Shrug ExogVars

### Prepping and cleaning the final dataset

### Saving



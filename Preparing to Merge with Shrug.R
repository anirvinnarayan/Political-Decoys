# ============================================
# Political Decoys - Preparing to Merge with Shrug
# ============================================
# Date: 20/05/25
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
  purrr
)

py_run_string('
import sys
import os
sys.path.append(os.path.abspath("masala-merge-master/"))
from lev import levenshtein
')

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

#### Loading ####
all_states_elections <- read.csv("Raw Data/all_states_elections.csv")
candidate_pairs <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet.csv")

triv <- read_csv("Raw Data/Shrug Data/shrug-triv-cand-csv/trivedi_candidates_clean.csv")

shrid_07 <- read_csv("Raw Data/Shrug Data/shrug-con-keys-csv/shrid_frag_con07_key.csv")
shrid_08 <- read_csv("Raw Data/Shrug Data/shrug-con-keys-csv/shrid_frag_con08_key.csv")

constituency_crosswalk <- read_csv("/Users/anirvin/Downloads/Corruption Protest Data/Cleaned Data/ac08_ac07_constituency_crosswalk.csv")

# Shrug data
ac07_name_key <- read.csv("Raw Data/Shrug Data/shrug-con-keys-csv/ac07_name_key.csv")
ac08_name_key <- read.csv("Raw Data/Shrug Data/shrug-con-keys-csv/ac08_name_key.csv")

#### Prepare Merging Guide ####
all_states_elections$State_Name <- sapply(all_states_elections$State_Name, standardize_state_names)
ac07_name_key$pc01_state_name <- sapply(ac07_name_key$pc01_state_name, standardize_state_names)
ac08_name_key$pc01_state_name <- sapply(ac08_name_key$pc01_state_name, standardize_state_names)
triv$eci_state_name <- sapply(triv$eci_state_name, standardize_state_names)

# are there states in all_states_elections that are not in ac07/ac08
states_elections <- unique(all_states_elections$State_Name)
states_triv <- unique(triv$eci_state_name)
states_ac07 <- unique(ac07_name_key$pc01_state_name)
states_ac08 <- unique(ac08_name_key$pc01_state_name)

states_not_in_triv <- setdiff(states_elections, states_triv)
if(length(states_not_in_triv) > 0) {
  cat("\nStates in all_states_elections but not in triv:\n")
  print(states_not_in_triv)
} else {
  cat("\nAll states in all_states_elections are also in triv\n")
}

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

# drop all notas, and all lok sabha elections
all_states_elections_filtered <- all_states_elections_filtered %>%
  filter(Party != "NOTA" & Candidate != "nota" & Election_Type == "State Assembly Election (AE)")

# filter for post 1991
all_states_elections_filtered <- all_states_elections_filtered %>%
  filter(Year >= 1991)

triv <- triv %>%
  filter(year >= 1991)

#### Constituency Merging Map ####
  # note: because district names are missing for tr quite frequently, we will use state and constituency only. 
  # duplicates will be dropped. 
clean_names <- function(df) {
  df %>%
    mutate(
      State = trimws(tolower(State)),
      Constituency = trimws(tolower(Constituency))
    )
}

all_states_elections_filtered <- all_states_elections_filtered %>%
  mutate(State = State_Name, 
         Constituency = Constituency_Name, 
         Month = month)

triv <- triv %>%
  rename(State = eci_state_name, 
         Constituency = tr_ac_name, 
         Year = year, 
         Month = month)

all_states_elections_filtered <- clean_names(all_states_elections_filtered)
triv <- clean_names(triv)

all_states_elections_filtered <- all_states_elections_filtered %>%
  mutate(Constituency = str_remove_all(Constituency, "\\s*\\([^\\)]*\\)\\s*"))

triv <- triv %>%
  mutate(Constituency = str_remove_all(Constituency, "\\s*\\([^\\)]*\\)\\s*"))

# function for fuzzy matching constituency names
const_match_simple <- function() {
  # create empty results dataframe
  results <- data.frame(
    State = character(),
    Year = numeric(),
    Month = numeric(),  # Added Month column
    Constituency_Original = character(),
    Constituency_Target = character(),
    dist = numeric(),
    stringsAsFactors = FALSE
  )
  
  # find common state-const pairs
  common_state_year_month_constituencies <- inner_join(
    const_df1 %>% distinct(State, Year, Month),  # Added Month
    const_df2 %>% distinct(State, Year, Month),  # Added Month
    by = c("State", "Year", "Month")  # Added Month to join criteria
  )
  
  # for each common state, match const
  for (i in 1:nrow(common_state_year_month_constituencies)) {
    state <- common_state_year_month_constituencies$State[i]
    year <- common_state_year_month_constituencies$Year[i]
    month <- common_state_year_month_constituencies$Month[i]  # Added month
    
    # filter both dataframes by state, year, and month
    df1_const <- const_df1 %>% 
      filter(State == state, Year == year, Month == month) %>%  # Added Month filter
      pull(Constituency)
    
    df2_const <- const_df2 %>% 
      filter(State == state, Year == year, Month == month) %>%  # Added Month filter
      pull(Constituency)
    
    # for each const in df1, find potential matches in df2
    for (const1 in df1_const) {
      for (const2 in df2_const) {
        # calc lv distance
        dist <- py$levenshtein(const1, const2)
        max_len <- max(nchar(const1), nchar(const2))
        norm_dist <- dist / max_len
        
        # if distance is below threshold, add to results
        if (norm_dist <= 0.3) {
          new_row <- data.frame(
            State = state,
            Year = year,
            Month = month,  # Added Month to results
            Constituency_Original = const1,
            Constituency_Target = const2,
            dist = norm_dist,
            stringsAsFactors = FALSE
          )
          results <- rbind(results, new_row)
        }
      }
    }
  }
  
  # sort by distance
  results <- results %>% arrange(dist)
  
  return(results)
}

# unique constituencies in both dfs
const_df1 <- all_states_elections_filtered %>%
  distinct(State, Year, Constituency, Month)

const_df2 <- triv %>%
  distinct(State, Year, Constituency, Month)

# standardize some instances
int_to_roman <- function(num) {
  roman_numerals <- c("i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x")
  if (num < 1 || num > 10) {
    return(as.character(num))  # return as is if outside our range
  }
  return(roman_numerals[num])
}

# process constituency names
process_constituency <- function(const_df1) {
  # create a copy of the data frame
  result_df <- const_df1
  
  result_df$Constituency <- sapply(const_df1$Constituency, function(name) {
    # Case 1: dash followed by number (e.g., "something-3")
    if (grepl("-\\d+$", name)) {
      base_name <- sub("-\\d+$", "", name)
      num <- as.integer(sub(".*-(\\d+)$", "\\1", name))
      return(paste0(base_name, " ", int_to_roman(num)))
    }
    
    # Case 2: space dash space followed by number (e.g., "something - 3")
    else if (grepl(" - \\d+$", name)) {
      base_name <- sub(" - \\d+$", "", name)
      num <- as.integer(sub(".* - (\\d+)$", "\\1", name))
      return(paste0(base_name, " ", int_to_roman(num)))
    }
    
    # Case 3: dash followed by roman numeral (e.g., "something-ii")
    else if (grepl("-[ivx]+$", name, ignore.case = TRUE)) {
      base_name <- sub("-[ivx]+$", "", name, ignore.case = TRUE)
      roman <- tolower(sub(".*-([ivx]+)$", "\\1", name, ignore.case = TRUE))
      return(paste0(base_name, " ", roman))
    }
    
    # Case 4: space dash space followed by roman numeral (e.g., "something - ii")
    else if (grepl(" - [ivx]+$", name, ignore.case = TRUE)) {
      base_name <- sub(" - [ivx]+$", "", name, ignore.case = TRUE)
      roman <- tolower(sub(".* - ([ivx]+)$", "\\1", name, ignore.case = TRUE))
      return(paste0(base_name, " ", roman))
    }
    
    # Case 5: space followed by number (e.g., "something 3")
    else if (grepl(" \\d+$", name)) {
      base_name <- sub(" \\d+$", "", name)
      num <- as.integer(sub(".* (\\d+)$", "\\1", name))
      return(paste0(base_name, " ", int_to_roman(num)))
    }
    
    # Case 6: space followed by roman numeral (e.g., "something ii")
    else if (grepl(" [ivx]+$", name, ignore.case = TRUE)) {
      base_name <- sub(" [ivx]+$", "", name, ignore.case = TRUE)
      roman <- tolower(sub(".* ([ivx]+)$", "\\1", name, ignore.case = TRUE))
      return(paste0(base_name, " ", roman))
    }
    
    # Case 7: dash followed by space and number (e.g., "something- 3")
    else if (grepl("- \\d+$", name)) {
      base_name <- sub("- \\d+$", "", name)
      num <- as.integer(sub(".*- (\\d+)$", "\\1", name))
      return(paste0(base_name, " ", int_to_roman(num)))
    }
    
    # Case 8: space dash followed by number (e.g., "something -3")
    else if (grepl(" -\\d+$", name)) {
      base_name <- sub(" -\\d+$", "", name)
      num <- as.integer(sub(".* -(\\d+)$", "\\1", name))
      return(paste0(base_name, " ", int_to_roman(num)))
    }
    
    # Case 9: dash followed by space and roman numeral (e.g., "something- i")
    else if (grepl("- [ivx]+$", name, ignore.case = TRUE)) {
      base_name <- sub("- [ivx]+$", "", name, ignore.case = TRUE)
      roman <- tolower(sub(".*- ([ivx]+)$", "\\1", name, ignore.case = TRUE))
      return(paste0(base_name, " ", roman))
    }
    
    # Case 10: space dash followed by roman numeral (e.g., "something -i")
    else if (grepl(" -[ivx]+$", name, ignore.case = TRUE)) {
      base_name <- sub(" -[ivx]+$", "", name, ignore.case = TRUE)
      roman <- tolower(sub(".* -([ivx]+)$", "\\1", name, ignore.case = TRUE))
      return(paste0(base_name, " ", roman))
    }
    
    else {
      return(name)
    }
  })
  
  return(result_df)
}

const_df1 <- process_constituency(const_df1)
const_df2 <- process_constituency(const_df2)

const_matches <- const_match_simple()

write_csv(const_matches, "Raw Data/all_elections_triv_const_match_2.csv")
const_matches <- read_csv("Raw Data/all_elections_triv_const_match_2.csv")

best_const_matches <- const_matches %>%
  filter(dist <= 0.2) %>%
  group_by(State, Constituency_Original) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup()

# find cases where there are multiple targets for the same source
duplicate_matches <- best_const_matches %>%
  group_by(State, Year, Month, Constituency_Original) %>%
  dplyr::summarize(
    num_targets = n_distinct(Constituency_Target),
    target_list = paste(unique(Constituency_Target), collapse = ", ")
  ) %>%
  filter(num_targets > 1) %>%
  arrange(desc(num_targets), State, Year, Month)

# manually delete these
rows_to_delete <- best_const_matches %>%
  filter(
    (State == "arunachal pradesh" & Year == 1999 & Month == 10 & 
       Constituency_Original == "damporijo" & Constituency_Target == "dumporijo" & 
       abs(dist - 0.1111111) < 0.0000001) |
      
      (State == "arunachal pradesh" & Year == 1995 & Month == 3 & 
         Constituency_Original == "damporijo" & Constituency_Target == "dumporijo" & 
         abs(dist - 0.1111111) < 0.0000001) |
      
      (State == "maharashtra" & Year == 2004 & Month == 10 & 
         Constituency_Original == "alvan" & Constituency_Target == "kalvan" & 
         abs(dist - 0.1666667) < 0.0000001)
  )

best_const_matches <- best_const_matches %>%
  anti_join(rows_to_delete, by = c("State", "Year", "Month", 
                                   "Constituency_Original", "Constituency_Target", "dist"))

triv_acid <- triv %>%
  distinct(State, Year, Constituency, Month, ac_id)

all_states_elections_unique_const <- const_df1 %>%
  left_join(best_const_matches, 
            by = c("Constituency" = "Constituency_Original", "State", "Year", "Month"))

# drop these
sum(is.na(all_states_elections_unique_const$dist))/nrow(all_states_elections_unique_const) # 7%; ~2000 unmatched elections

all_states_elections_unique_const <- all_states_elections_unique_const %>%
  filter(!is.na(dist))

all_states_elections_acid <- all_states_elections_unique_const %>%
  left_join(triv_acid, 
            by = c("Constituency_Target" = "Constituency", "State", "Year", "Month"))

duplicates_in_y <- triv_acid %>%
  group_by(Constituency, State, Year, Month) %>%
  dplyr::summarize(count = n(), .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

duplicates_in_x <- all_states_elections_unique_const %>%
  group_by(Constituency_Target, State, Year, Month) %>%
  dplyr::summarize(count = n(), .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

# delete duplicates 
triv_acid <- triv_acid %>%
  anti_join(duplicates_in_y, 
            by = c("Constituency", "State", "Year", "Month"))

all_states_elections_unique_const <- all_states_elections_unique_const %>%
  anti_join(duplicates_in_x, 
            by = c("Constituency_Target", "State", "Year", "Month"))

all_states_elections_acid <- all_states_elections_unique_const %>%
  left_join(triv_acid, 
            by = c("Constituency_Target" = "Constituency", "State", "Year", "Month"))

sum(is.na(all_states_elections_acid$ac_id))/nrow(all_states_elections_acid) # 99%

# drop these
all_states_elections_acid <- all_states_elections_acid %>%
  filter(!is.na(ac_id))

#### Add Election Level Variables ####
# add binary for use of photos in elections
all_states_elections_acid <- all_states_elections_acid %>%
  mutate(photo = ifelse((Year > 2015 | (Year == 2015 & Month > 5)), 1, 0))

test_2015 <- all_states_elections_acid %>%
  filter(Year == 2015)

# add binary for EVM usage

# add ballot order to merge into candidate pairs
str(all_states_elections_filtered)
unique(all_states_elections_filtered$Party_Type_TCPD)

all_states_elections_filtered_2 <- all_states_elections[!all_states_elections$State_Name %in% states_to_remove, ]

all_states_elections_filtered_2 <- all_states_elections_filtered_2 %>%
  filter(Party != "NOTA" & Candidate != "nota" & Party_Type_TCPD != "NOTA")

all_states_elections_filtered_2 <- all_states_elections_filtered_2 %>%
  mutate(State = State_Name, 
         Constituency = Constituency_Name, 
         Month = month)

all_states_elections_filtered_2 <- clean_names(all_states_elections_filtered_2)

all_states_elections_filtered_2 <- all_states_elections_filtered_2 %>%
  mutate(Constituency = str_remove_all(Constituency, "\\s*\\([^\\)]*\\)\\s*"))

all_states_elections_filtered_2 <- all_states_elections_filtered_2 %>%
  group_by(Year, State, Constituency, Month) %>%
  mutate(
    # Create a party category variable based on Party_Type_TCPD
    party_category = case_when(
      Party_Type_TCPD %in% c("National Party", "State-based Party") ~ 1,
      Party_Type_TCPD %in% c("Local Party", "State-based Party (Other State)") ~ 2,
      Party_Type_TCPD %in% c("Independents", "") ~ 3,
      TRUE ~ NA_real_
    ),
    # Calculate ballot order: first by party category, then alphabetically by candidate name
    ballot_order = rank(
      interaction(party_category, Candidate_clean, lex.order = TRUE), 
      ties.method = "first"
    )
  ) %>%
  ungroup()

# creating ballot order df by election and pid
candidate_ballot_order <- all_states_elections_filtered_2 %>%
  dplyr::select(Year, State, Constituency, Month, Constituency_Name, pid, ballot_order)

#### Prepare Decoy at Constituency Level ####
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

# to add: elections with decoys (level and share), average decoy ballot order
sum(candidate_pairs$Candidate1_month != candidate_pairs$Candidate2_month, na.rm=TRUE)
sum(is.na(candidate_pairs$Candidate1_month))

candidate_pairs$State <- sapply(candidate_pairs$State_Name, standardize_state_names)
str(candidate_ballot_order)
str(candidate_pairs)

duplicate_ballot_orders <- candidate_ballot_order %>%
  group_by(Year, State, Constituency_Name, Month, pid) %>%
  dplyr::summarize(count = n(), .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

# remove duplicates
candidate_ballot_order <- candidate_ballot_order %>%
  anti_join(duplicate_ballot_orders, 
            by = c("Year", "State", "Constituency_Name", "Month", "pid"))

# first, merge ballot order for Candidate1
candidate_pairs <- candidate_pairs %>%
  left_join(
    candidate_ballot_order %>% 
      dplyr::select(Year, State, Constituency_Name, Month, pid, ballot_order),
    by = c(
      "Year" = "Year",
      "State" = "State",
      "Constituency_Name" = "Constituency_Name",
      "Candidate1_month" = "Month",
      "Candidate1_PID" = "pid"
    ),
    suffix = c("", "_candidate1")
  ) %>%
  rename(Candidate1_ballot_order = ballot_order)

# then, merge ballot order for Candidate2
candidate_pairs <- candidate_pairs %>%
  left_join(
    candidate_ballot_order %>% 
      dplyr::select(Year, State, Constituency_Name, Month, pid, ballot_order),
    by = c(
      "Year" = "Year",
      "State" = "State",
      "Constituency_Name" = "Constituency_Name",
      "Candidate2_month" = "Month",
      "Candidate2_PID" = "pid"
    )
  ) %>%
  rename(Candidate2_ballot_order = ballot_order)

mean(candidate_pairs$Candidate1_ballot_order, na.rm = TRUE)
sum(is.na(candidate_pairs$Candidate1_ballot_order))
sum(is.na(candidate_pairs$Candidate1_Party_Type_TCPD))

candidate_pairs <- candidate_pairs %>%
  mutate(Month = Candidate1_month)

constituency_election_metrics <- candidate_pairs %>%
  group_by(State, Year, Month, Constituency_Name, Election_Type, Assembly_No) %>%
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
    
    # add decoy ballot order metrics
    avg_decoy_ballot_order = mean(
      c(Candidate2_ballot_order[is_decoy & Pair_Type == "main-minor"],
        Candidate1_ballot_order[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    min_decoy_ballot_order = min(
      c(Candidate2_ballot_order[is_decoy & Pair_Type == "main-minor"],
        Candidate1_ballot_order[is_decoy & Pair_Type == "minor-main"]),
      na.rm = TRUE),
    max_decoy_ballot_order = max(
      c(Candidate2_ballot_order[is_decoy & Pair_Type == "main-minor"],
        Candidate1_ballot_order[is_decoy & Pair_Type == "minor-main"]),
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
    
    # if constituency has at least one decoy
    has_at_least_one_decoy = any(is_decoy, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    close_race = ifelse(winning_margin_percentage < 5, TRUE, FALSE),
    decoy_impact_potential = ifelse(decoy_vote_share > winning_margin_percentage, TRUE, FALSE)
  ) %>%
  # sort by state, year and constituency
  arrange(State, Year, Constituency_Name, Election_Type, Assembly_No)

# cleaning constituency name to match
clean_names <- function(df) {
  df %>%
    mutate(
      State = trimws(tolower(State)),
      Constituency = trimws(tolower(Constituency))
    )
}

constituency_election_metrics <- constituency_election_metrics %>%
  mutate(Constituency = Constituency_Name)

constituency_election_metrics <- clean_names(constituency_election_metrics)

constituency_election_metrics <- constituency_election_metrics %>%
  mutate(Constituency = str_remove_all(Constituency, "\\s*\\([^\\)]*\\)\\s*"))

constituency_election_metrics <- constituency_election_metrics %>%
  left_join(all_states_elections_acid %>%
              dplyr::select(State, Constituency, Year, Month, photo, ac_id), 
            by = c("State" = "State", 
                   "Constituency" = "Constituency", 
                   "Year" = "Year", "Month" = "Month"))

constituency_election_metrics <- constituency_election_metrics %>%
  mutate(photo = ifelse(Year > 2015, 1, 0))

# filtering for post 1991
constituency_election_metrics_final <- constituency_election_metrics %>%
  filter(Year >= 1991 & Election_Type == "State Assembly Election (AE)" & !is.na(ac_id))

# save this 
write_csv(constituency_election_metrics, "Cleaned Data/constituency_election_photo.csv")
write_csv(constituency_election_metrics_final, "Cleaned Data/constituency_election_final.csv")

# now to collapse by constituency
# collapse at pre-delim level
constituency_election_pre_delim <- constituency_election_metrics %>%
  filter(str_starts(ac_id, "2007"))

str(constituency_election_pre_delim)

constituency_pre_delim <- constituency_election_pre_delim %>%
  group_by(ac_id) %>%
  dplyr::summarize(
    # location and identity variables
    State = first(State),
    Constituency_Name = first(Constituency_Name),
    Election_Type = first(Election_Type),
    
    # time spans
    first_year = min(Year, na.rm = TRUE),
    last_year = max(Year, na.rm = TRUE),
    n_elections = n(),
    
    # candidate metrics
    avg_total_candidates = mean(total_candidates, na.rm = TRUE),

    # decoy metrics
    avg_decoy_candidates = mean(decoy_candidates, na.rm = TRUE),
    avg_decoy_share = mean(decoy_share, na.rm = TRUE),

    # vote metrics
    avg_total_votes = mean(total_votes, na.rm = TRUE),
    avg_votes_to_decoys = mean(total_votes_to_decoys, na.rm = TRUE),
    avg_decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),

    # margin metrics
    avg_winning_margin = mean(winning_margin, na.rm = TRUE),
    avg_winning_margin_pct = mean(winning_margin_percentage, na.rm = TRUE),
    
    # decoy presence indicators
    n_winner_has_decoys = sum(winner_has_decoys, na.rm = TRUE),
    pct_winner_has_decoys = mean(winner_has_decoys, na.rm = TRUE),
    
    # ballor order metrics - taking means of non-NA/Inf values
    avg_decoy_ballot_order = mean(avg_decoy_ballot_order[!is.nan(avg_decoy_ballot_order) & 
                                                           is.finite(avg_decoy_ballot_order)], na.rm = TRUE),
    
    # education metrics - taking means of non-NA values
    avg_education = mean(education[!is.nan(education)], na.rm = TRUE),
    avg_decoy_education = mean(decoy_education[!is.nan(decoy_education)], na.rm = TRUE),
    avg_decoy_party = mean(decoy_party[!is.nan(decoy_party)], na.rm = TRUE),
    avg_main_with_decoy_education = mean(main_with_decoy_education[!is.nan(main_with_decoy_education)], na.rm = TRUE),
    avg_main_with_decoy_party = mean(main_with_decoy_party[!is.nan(main_with_decoy_party)], na.rm = TRUE),
    avg_main_without_decoy_education = mean(main_without_decoy_education[!is.nan(main_without_decoy_education)], na.rm = TRUE),
    avg_main_without_decoy_party = mean(main_without_decoy_party[!is.nan(main_without_decoy_party)], na.rm = TRUE),
    
    # aggregate logical variables
    ever_had_decoy = any(has_at_least_one_decoy, na.rm = TRUE),
    n_elections_with_decoys = sum(has_at_least_one_decoy, na.rm = TRUE),
    pct_elections_with_decoys = mean(has_at_least_one_decoy, na.rm = TRUE),
  ) %>%
  # Replace NaN values with NA for cleaner output
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

constituency_election_post_delim <- constituency_election_metrics %>%
  filter(str_starts(ac_id, "2008"))

constituency_post_delim <- constituency_election_post_delim %>%
  group_by(ac_id) %>%
  dplyr::summarize(
    # location and identity variables
    State = first(State),
    Constituency_Name = first(Constituency_Name),
    Election_Type = first(Election_Type),
    
    # time spans
    first_year = min(Year, na.rm = TRUE),
    last_year = max(Year, na.rm = TRUE),
    n_elections = n(),
    
    # candidate metrics
    avg_total_candidates = mean(total_candidates, na.rm = TRUE),
    
    # decoy metrics
    avg_decoy_candidates = mean(decoy_candidates, na.rm = TRUE),
    avg_decoy_share = mean(decoy_share, na.rm = TRUE),
    
    # vote metrics
    avg_total_votes = mean(total_votes, na.rm = TRUE),
    avg_votes_to_decoys = mean(total_votes_to_decoys, na.rm = TRUE),
    avg_decoy_vote_share = mean(decoy_vote_share, na.rm = TRUE),
    
    # margin metrics
    avg_winning_margin = mean(winning_margin, na.rm = TRUE),
    avg_winning_margin_pct = mean(winning_margin_percentage, na.rm = TRUE),
    
    # decoy postsence indicators
    n_winner_has_decoys = sum(winner_has_decoys, na.rm = TRUE),
    pct_winner_has_decoys = mean(winner_has_decoys, na.rm = TRUE),
    
    # ballor order metrics - taking means of non-NA/Inf values
    avg_decoy_ballot_order = mean(avg_decoy_ballot_order[!is.nan(avg_decoy_ballot_order) & 
                                                           is.finite(avg_decoy_ballot_order)], na.rm = TRUE),
    
    # education metrics - taking means of non-NA values
    avg_education = mean(education[!is.nan(education)], na.rm = TRUE),
    avg_decoy_education = mean(decoy_education[!is.nan(decoy_education)], na.rm = TRUE),
    avg_decoy_party = mean(decoy_party[!is.nan(decoy_party)], na.rm = TRUE),
    avg_main_with_decoy_education = mean(main_with_decoy_education[!is.nan(main_with_decoy_education)], na.rm = TRUE),
    avg_main_with_decoy_party = mean(main_with_decoy_party[!is.nan(main_with_decoy_party)], na.rm = TRUE),
    avg_main_without_decoy_education = mean(main_without_decoy_education[!is.nan(main_without_decoy_education)], na.rm = TRUE),
    avg_main_without_decoy_party = mean(main_without_decoy_party[!is.nan(main_without_decoy_party)], na.rm = TRUE),
    
    # aggregate logical variables
    ever_had_decoy = any(has_at_least_one_decoy, na.rm = TRUE),
    n_elections_with_decoys = sum(has_at_least_one_decoy, na.rm = TRUE),
    pct_elections_with_decoys = mean(has_at_least_one_decoy, na.rm = TRUE),
  ) %>%
  # Replace NaN values with NA for cleaner output
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

# collapsing by crosswalk
str(constituency_crosswalk)
str(constituency_election_metrics)

mapping <- constituency_crosswalk %>%
  dplyr::select(ac07_id, ac08_id, weight)

pre_delim_with_mapping <- constituency_pre_delim %>%
  inner_join(constituency_crosswalk, by = c("ac_id" = "ac07_id"))

pre_delim_weighted <- pre_delim_with_mapping %>%
  mutate(
    # Apply weights to all numeric variables
    across(where(is.numeric) & !c(weight, n_shrids), ~ . * weight)
  )

mapped_post_delim <- pre_delim_weighted %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    # Location and identity (take most common)
    State = names(which.max(table(State))),
    Constituency_Name = names(which.max(table(Constituency_Name))),
    Election_Type = names(which.max(table(Election_Type))),
    
    # For numeric variables, sum the weighted values then divide by sum of weights
    across(c(avg_total_candidates, avg_decoy_candidates, avg_decoy_share, 
             avg_total_votes, avg_votes_to_decoys, avg_decoy_vote_share,
             avg_winning_margin, avg_winning_margin_pct, pct_winner_has_decoys,
             avg_decoy_ballot_order, avg_education, avg_decoy_education,
             avg_decoy_party, avg_main_with_decoy_education, avg_main_with_decoy_party,
             avg_main_without_decoy_education, avg_main_without_decoy_party,
             pct_elections_with_decoys),
           ~ sum(. * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)),
    
    # For count variables, just sum them
    n_elections = sum(n_elections, na.rm = TRUE),
    n_winner_has_decoys = sum(n_winner_has_decoys, na.rm = TRUE),
    n_elections_with_decoys = sum(n_elections_with_decoys, na.rm = TRUE),
    
    # For time spans, take the min/max across all constituent parts
    first_year = min(first_year, na.rm = TRUE),
    last_year = max(last_year, na.rm = TRUE),
    
    # For boolean variables that should be any=TRUE
    ever_had_decoy = any(ever_had_decoy, na.rm = TRUE),
    
    # Metadata about the mapping
    n_source_constituencies = n(),
    total_weight = sum(weight, na.rm = TRUE)
  ) %>%
  # Rename to match your original format
  rename(ac_id = ac08_id)

mapped_post_delim <- pre_delim_weighted %>%
  group_by(ac08_id) %>%
  dplyr::summarize(
    # Location and identity (take most common)
    State = names(which.max(table(State))),
    Constituency_Name = names(which.max(table(Constituency_Name))),
    Election_Type = names(which.max(table(Election_Type))),
    
    # For numeric variables, sum the weighted values then divide by sum of weights
    across(c(avg_total_candidates, avg_decoy_candidates, avg_decoy_share, 
             avg_total_votes, avg_votes_to_decoys, avg_decoy_vote_share,
             avg_winning_margin, avg_winning_margin_pct, pct_winner_has_decoys,
             avg_decoy_ballot_order, avg_education, avg_decoy_education,
             avg_decoy_party, avg_main_with_decoy_education, avg_main_with_decoy_party,
             avg_main_without_decoy_education, avg_main_without_decoy_party,
             pct_elections_with_decoys),
           ~ sum(. * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)),
    
    # For count variables, just sum them
    n_elections = sum(n_elections, na.rm = TRUE),
    n_winner_has_decoys = sum(n_winner_has_decoys, na.rm = TRUE),
    n_elections_with_decoys = sum(n_elections_with_decoys, na.rm = TRUE),
    
    # For time spans, take the min/max across all constituent parts
    first_year = min(first_year, na.rm = TRUE),
    last_year = max(last_year, na.rm = TRUE),
    
    # For boolean variables that should be any=TRUE
    ever_had_decoy = any(ever_had_decoy, na.rm = TRUE),
    
    # Metadata about the mapping
    n_source_constituencies = n(),
    total_weight = sum(weight, na.rm = TRUE)
  ) %>%
  # Rename to match your original format
  rename(ac_id = ac08_id)

final_post_delim <- bind_rows(
  constituency_post_delim,
  mapped_post_delim
) %>%
  # handle duplicates by keeping the mapped version if it exists, 
  # otherwise keep the existing one
  group_by(ac_id) %>%
  slice(1) %>%
  ungroup()

# save all the constituency level datasets
write_csv(final_post_delim, "Cleaned Data/constituency_metrics_crosswalk.csv")
write_csv(constituency_post_delim, "Cleaned Data/constituency_metrics_post_delim.csv")
write_csv(constituency_pre_delim, "Cleaned Data/constituency_metrics_pre_delim.csv")

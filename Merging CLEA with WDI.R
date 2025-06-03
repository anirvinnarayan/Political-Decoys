# ============================================
# Political Decoys - Merging WDI with
# CLEA canddiate and candidate-pair level data
# ============================================
# Date: 21/04/25
# Author: Anirvin Narayan

rm(list = ls())
setwd("/Users/anirvin/Downloads/Political Decoys Data")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  readr,
  lubridate,
  gt,
  formattable,
  DT,
  scales,
  stringr, 
  stringdist,
  fixest, 
  modelsummary, 
  writexl, 
  plm, 
  PGRdup,
  openxlsx, 
  lmtest, 
  clubSandwich, 
  gridExtra,
  stargazer, 
  psych, 
  kableExtra,
  xtable, 
  corrplot, 
  Hmisc, 
  patchwork, 
  scales, 
  flextable, 
  purrr, 
  lme4
)

### Loading
CLEA_cleaned <- read.csv("Cleaned Data/CLEA_cleaned.csv")
candidate_pairs_CLEA <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA.csv")
candidate_pairs_CLEA_the_rest <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet_CLEA_the_rest.csv")
candidate_pairs_CLEA_full <- rbind(candidate_pairs_CLEA, candidate_pairs_CLEA_the_rest)

WDI_extract_1 <- read.csv("Raw Data/WDI_extract_1/689cec2b-e2e3-4fc5-8a30-b5304de46c11_Data.csv")

### Preppring WDI data
unique_clea_countries <- CLEA_cleaned %>%
  distinct(ctr_n)

unique_WDI_countries <- WDI_extract_1 %>%
  distinct(Country.Name)

# get the list of countries that are in CLEA but not in WDI_extract_1
missing_countries <- unique_clea_countries %>%
  anti_join(unique_WDI_countries, by = c("ctr_n" = "Country.Name"))

if (nrow(missing_countries) == 0) {
  print("All CLEA countries are present in WDI countries.")
} else {
  print("The following CLEA countries are missing from WDI countries:")
  print(missing_countries)
}

### Since it is not that many, manually edit names
country_mapping <- c(
  "United States" = "US",
  "United Kingdom" = "UK",
  "St. Lucia" = "Saint Lucia",
  "Venezuela, RB" = "Venezuela",
  "Gambia, The" = "Gambia",
  "Micronesia, Fed. Sts." = "Micronesia",
  "Cote d'Ivoire" = "Ivory Coast",
  "Montserrat" = "Montserrat", # # not in WDI
  "St. Kitts and Nevis" = "Saint Kitts and Nevis",
  "Taiwan" = "Taiwan", # not in WDI
  "Anguilla" = "Anguilla", # not in WDI
  "Bahamas" = "Bahamas, The"
)

# edit names in WDI extract
WDI_extract_1 <- WDI_extract_1 %>%
  mutate(country_standardized = case_when(
    Country.Name %in% names(country_mapping) ~ country_mapping[Country.Name],
    TRUE ~ Country.Name  # keep original name if not in mapping
  ))

str(WDI_extract_1)

# first, pivot WDI_extract_1 to long
WDI_long <- WDI_extract_1 %>%
  dplyr::select(Country.Name, Country.Code, Series.Name, Series.Code, starts_with("X")) %>%
  # convert from wide to long
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  # clean up the Year column (remove the X and ..YR part)
  mutate(Year = as.numeric(str_extract(Year, "\\d{4}")))

### Preparing CLEA data
# first filtering out electoral fusion from the US
unique_US_sub <- CLEA_cleaned %>%
  filter(ctr_n == "US") %>%
  distinct(ctr_n, sub)

CLEA_US <- CLEA_cleaned %>%
  filter(ctr_n == "US" & yr >= 1930) %>%
  filter(sub != "new york" & sub != "New York" & 
           sub != "connecticut" & sub != "Connecticut")

country_codes <- CLEA_cleaned %>%
  distinct(ctr_n, ctr) %>%
  rename(
    Country_Name = ctr_n, 
    Country_Code = ctr
  )

# elections are identified as 
# distinct(Country_Code, Year, Election_Month, Constituency_Name, Election_ID)
elections_with_sub <- CLEA_cleaned %>%
  distinct(ctr_n, ctr, yr, mn, cst_n, cst, id, sub) %>%
  rename(
    Country_Name = ctr_n, 
    Country_Code = ctr, 
    Year = yr, 
    Election_Month = mn, 
    Constituency_Name = cst_n, 
    Constituency_Code = cst, 
    Election_ID = id, 
    State = sub
  )

candidate_pairs_CLEA_full <- candidate_pairs_CLEA_full %>%
  left_join(country_codes, 
            by = c("Country_Code")) %>%
  left_join(
    elections_with_sub %>%
      dplyr::select(Country_Code, Year, Election_Month, Constituency_Name, Election_ID, State),
    by = c("Country_Code", "Year", "Election_Month", "Constituency_Name", "Election_ID")
  )

# remove all countries < 1920, and NY and CT
candidate_pairs_CLEA_cleaned <- candidate_pairs_CLEA_full %>%
  filter(!(Country_Name == "US" & Year < 1960)) %>%
  filter(!(Country_Name == "US" & Year < 2012 & (State == "delaware"|State == "Delaware"))) %>%
  filter(!(Country_Name == "US" & Year < 2023 & (State == "south carolina"|State == "South Carolina"))) %>%
  filter(!(Country_Name == "US" & Year < 2000 & (State == "south dakota"|State == "South Dakota"))) %>%
  filter(State != "new york" & State != "New York" & 
           State != "connecticut" & State != "Connecticut")

### Collapsing at candidate level
candidate_pairs_CLEA_cleaned <- candidate_pairs_CLEA_cleaned %>%
  mutate(
    Candidate1_Election_ID = paste(Year, Country_Code, Constituency_Name, Candidate1_Party_Name, Candidate1_Name, Candidate1_Votes, sep = "_"),
    Candidate2_Election_ID = paste(Year, Country_Code, Constituency_Name, Candidate2_Party_Name, Candidate2_Name, Candidate2_Votes, sep = "_")
  )

str(candidate_pairs_CLEA_cleaned)

# creating is_decoy
# thresholds
lv_99th <- quantile(candidate_pairs_CLEA_cleaned$Levenshtein_Similarity, 0.99, na.rm = TRUE)
jw_99th <- quantile(candidate_pairs_CLEA_cleaned$Jaro_Winkler_Similarity, 0.99, na.rm = TRUE)
mp_99th <- quantile(candidate_pairs_CLEA_cleaned$Metaphone_Similarity, 0.99, na.rm = TRUE)
mas_99th <- quantile(candidate_pairs_CLEA_cleaned$Masala_Similarity, 0.99, na.rm = TRUE)
ng_99th <- quantile(candidate_pairs_CLEA_cleaned$NGram_Similarity, 0.99, na.rm = TRUE)

candidate_pairs_CLEA_cleaned <- candidate_pairs_CLEA_cleaned %>%
  mutate(is_decoy = ifelse(
    Levenshtein_Similarity > lv_99th & 
      Jaro_Winkler_Similarity > jw_99th & 
      Metaphone_Similarity > mp_99th & 
      Masala_Similarity > mas_99th & 
      NGram_Similarity > ng_99th,
    TRUE, 
    FALSE
  ))

str(candidate_pairs_CLEA_cleaned)

# create two separate dataframes for candidate1 and candidate2, getting unique candidates from each side
candidates1 <- candidate_pairs_CLEA_cleaned %>%
  dplyr::select(Year, Country_Code, Country_Name, 
         Candidate_Name = Candidate1_Name, 
         Candidate_Party_Code = Candidate1_Party_Code,
         Candidate_Party_Name = Candidate1_Party_Name,
         Candidate_Votes = Candidate1_Votes,
         Candidate_VoteShare = Candidate1_VoteShare,
         Candidate_Type = Candidate1_Type,
         Candidate_Election_ID = Candidate1_Election_ID) %>%
  distinct()

candidates2 <- candidate_pairs_CLEA_cleaned %>%
  dplyr::select(Year, Country_Code, Country_Name, 
         Candidate_Name = Candidate2_Name, 
         Candidate_Party_Code = Candidate2_Party_Code,
         Candidate_Party_Name = Candidate2_Party_Name,
         Candidate_Votes = Candidate2_Votes,
         Candidate_VoteShare = Candidate2_VoteShare,
         Candidate_Type = Candidate2_Type,
         Candidate_Election_ID = Candidate2_Election_ID) %>%
  distinct()

# first, identify all decoy pairs
decoy_pairs <- candidate_pairs_CLEA_cleaned %>%
  filter(is_decoy == TRUE) %>%
  dplyr::select(
    candidate1_id = Candidate1_Election_ID,
    candidate2_id = Candidate2_Election_ID,
    decoy_relationship = Pair_Type
  )

# extract all unique candidates
all_candidates <- bind_rows(
  candidate_pairs_CLEA_cleaned %>%
    dplyr::select(Year, Country_Code, Country_Name, 
           Candidate_Name = Candidate1_Name, 
           Candidate_Party_Code = Candidate1_Party_Code,
           Candidate_Party_Name = Candidate1_Party_Name,
           Candidate_Votes = Candidate1_Votes,
           Candidate_VoteShare = Candidate1_VoteShare,
           Candidate_Type = Candidate1_Type,
           Candidate_Election_ID = Candidate1_Election_ID, 
           State = State, 
           Constituency_Name = Constituency_Name),
  candidate_pairs_CLEA_cleaned %>%
    dplyr::select(Year, Country_Code, Country_Name, 
           Candidate_Name = Candidate2_Name, 
           Candidate_Party_Code = Candidate2_Party_Code,
           Candidate_Party_Name = Candidate2_Party_Name,
           Candidate_Votes = Candidate2_Votes,
           Candidate_VoteShare = Candidate2_VoteShare,
           Candidate_Type = Candidate2_Type,
           Candidate_Election_ID = Candidate2_Election_ID, 
           State = State, 
           Constituency_Name = Constituency_Name)
) %>% distinct()

# mark candidates as decoys if they appear in either side of a decoy pair
decoy_candidates1 <- decoy_pairs %>%
  dplyr::select(Candidate_Election_ID = candidate1_id, 
         decoy_for_id = candidate2_id,
         decoy_relationship)

decoy_candidates2 <- decoy_pairs %>%
  dplyr::select(Candidate_Election_ID = candidate2_id, 
         decoy_for_id = candidate1_id,
         decoy_relationship)

# combine both sides of decoy pairs
all_decoy_candidates <- bind_rows(decoy_candidates1, decoy_candidates2)

# get the names of candidates who are decoys for
decoy_for_names <- all_candidates %>%
  dplyr::select(Candidate_Election_ID, decoy_for_name = Candidate_Name)

# join decoy information with all candidates
candidate_level <- all_candidates %>%
  left_join(all_decoy_candidates, by = "Candidate_Election_ID") %>%
  left_join(decoy_for_names, by = c("decoy_for_id" = "Candidate_Election_ID")) %>%
  mutate(
    is_decoy = !is.na(decoy_for_id),
    # clean up decoy relationship info for non-decoys
    decoy_for_name = ifelse(is_decoy, decoy_for_name, ""),
    decoy_relationship = ifelse(is_decoy, decoy_relationship, "")
  ) %>%
  # group by candidate to combine multiple decoy relationships if they exist
  group_by(Candidate_Election_ID) %>%
  dplyr::summarize(
    Year = first(Year),
    Country_Code = first(Country_Code),
    State = first(State), 
    Constituency_Name = first(Constituency_Name),
    Country_Name = first(Country_Name),
    Candidate_Name = first(Candidate_Name),
    Candidate_Party_Code = first(Candidate_Party_Code),
    Candidate_Party_Name = first(Candidate_Party_Name),
    Candidate_Votes = first(Candidate_Votes),
    Candidate_VoteShare = first(Candidate_VoteShare),
    Candidate_Type = first(Candidate_Type),
    is_decoy = any(is_decoy),
    decoy_for_candidates = paste(unique(decoy_for_name[is_decoy]), collapse = "; "),
    decoy_relationships = paste(unique(decoy_relationship[is_decoy]), collapse = "; ")
  ) %>%
  ungroup()

### Merging with WDI
str(WDI_long)

indicators <- WDI_extract_1 %>%
  distinct(Series.Code, Series.Name)

# make WDI_wide with Series.Code as column
WDI_wide <- WDI_long %>%
  # filter out rows with empty Series.Code
  filter(Series.Code != "") %>%
  # replace missing values ".." with NA
  mutate(Value = ifelse(Value == "..", NA, Value)) %>%
  # convert Value to numeric
  mutate(Value = as.numeric(Value)) %>%
  # handle duplicates by taking the mean (or another appropriate summary function)
  group_by(Country.Name, Country.Code, Year, Series.Code) %>%
  dplyr::summarize(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  # pivot to wide format
  pivot_wider(
    id_cols = c(Country.Name, Country.Code, Year),
    names_from = Series.Code,
    values_from = Value
  ) %>%
  dplyr::select(
    Country.Name, Country.Code, Year,
    # gov
    GE.PER.RNK, RL.PER.RNK, PV.PER.RNK,
    # social/economic
    NY.GDP.PCAP.KD, 
    SP.RUR.TOTL.ZS, SL.TLF.ACTI.1524.ZS, NY.GDP.DEFL.KD.ZG,
    MS.MIL.XPND.ZS, GC.TAX.TOTL.GD.ZS,
    # human capital and education
    SE.XPD.TOTL.GD.ZS,
    # infra
    EG.ELC.ACCS.ZS
  )

candidate_level_with_WDI <- candidate_level %>%
  # join with WDI data
  left_join(
    WDI_wide,
    by = c("Country_Name" = "Country.Name", "Year" = "Year")
  ) %>%
  filter(!is.na(Country.Code))

# convert all NaNs to NA in the dataset
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

# count NAs by column and arrange in descending order
na_counts <- candidate_level_with_WDI %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), 
               names_to = "Column", 
               values_to = "NA_Count") %>%
  arrange(desc(NA_Count)) %>%
  mutate(Percentage = NA_Count / nrow(candidate_level_with_WDI) * 100)

# Print the results
print(na_counts)

# creating name-country FE factor
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  mutate(candidate_country = paste(Candidate_Name, Country_Name, sep = "_"))

### Adding alternatives to name FE
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  mutate(name_length = nchar(Candidate_Name))

# country level name lengths
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  group_by(Country_Name) %>%
  mutate(
    country_avg_name_length = mean(name_length),
    name_length_z_score = (name_length - country_avg_name_length) / sd(name_length)
  ) %>%
  ungroup()

# common substrings
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  mutate(
    # first 5 characters of name
    name_prefix = str_to_lower(substr(Candidate_Name, 1, 6)),
    # last 5 characters of name
    name_suffix = str_to_lower(substr(Candidate_Name, 
                                      pmax(1, nchar(Candidate_Name) - 5), 
                                      nchar(Candidate_Name)))
  )

# calculate frequency of prefixes and suffixes within countries
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  group_by(Country_Name, name_prefix) %>%
  mutate(prefix_frequency = n()) %>%
  ungroup() %>%
  group_by(Country_Name, name_suffix) %>%
  mutate(suffix_frequency = n()) %>%
  ungroup() %>%
  # normalize by country size
  group_by(Country_Name) %>%
  mutate(
    country_candidates = n(),
    prefix_freq_normalized = prefix_frequency / country_candidates,
    suffix_freq_normalized = suffix_frequency / country_candidates
  ) %>%
  ungroup()

# vowel ratio to indicate linguistic patterns
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  mutate(
    vowel_count = str_count(str_to_lower(Candidate_Name), "[aeiou]"),
    vowel_ratio = vowel_count / name_length
  )

# name complexity index (based on character distribution)
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  mutate(
    unique_chars = sapply(str_to_lower(Candidate_Name), function(x) length(unique(strsplit(x, "")[[1]]))),
    char_diversity = unique_chars / name_length  # higher means more diverse characters
  )

# finding common sub string
extract_ngrams <- function(name, n=6) {
  name <- str_to_lower(name)
  if(nchar(name) < n) return(character(0))
  
  ngrams <- character(nchar(name) - n + 1)
  for(i in 1:(nchar(name) - n + 1)) {
    ngrams[i] <- substr(name, i, i + n - 1)
  }
  return(ngrams)
}

# identify common n-grams within each country
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  group_by(Country_Name) %>%
  mutate(
    # create temporary column with all n-grams for each name
    name_ngrams = lapply(Candidate_Name, extract_ngrams, n=6)
  ) 

# flatten all n-grams by country to find common ones
country_ngram_counts <- candidate_level_with_WDI %>%
  group_by(Country_Name) %>%
  dplyr::summarize(
    all_ngrams = list(unlist(name_ngrams)),
    ngram_counts = list(table(unlist(name_ngrams)))
  )

# join the common n-grams back to the main data
# candidate_level_with_WDI <- candidate_level_with_WDI %>%
#   left_join(
#     country_ngram_counts %>%
#       dplyr::select(Country_Name, ngram_counts),
#     by = "Country_Name"
#   ) %>%
#   rowwise() %>%
#   mutate(
#     # calculate the average frequency of n-grams in this name
#     common_substring_score = mean(sapply(name_ngrams, function(ng) {
#       if(length(ng) == 0) return(0)
#       mean(ngram_counts[[1]][ng], na.rm = TRUE)
#     })),
#     # calculate percentage of n-grams that are "common" (appear more than once)
#     pct_common_substrings = mean(sapply(name_ngrams, function(ng) {
#       if(length(ng) == 0) return(0)
#       mean(ngram_counts[[1]][ng] > 1, na.rm = TRUE)
#     }))
#   ) %>%
#   ungroup() %>%
#  dplyr::select(-name_ngrams, -ngram_counts)

# doing the same thing, but leveraging already existing name separators
# split names by both space and comma separators
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  mutate(
    # create a list of name components for each candidate
    name_components = strsplit(str_to_lower(Candidate_Name), "[, ]+")
  )

# calculate frequency of each name component within each country
country_component_frequencies <- candidate_level_with_WDI %>%
  dplyr::select(Country_Name, name_components) %>%
  unnest(name_components) %>%
  # filter out very short components which might be initials or titles
  filter(nchar(name_components) > 1) %>%
  group_by(Country_Name, name_components) %>%
  dplyr::summarize(component_frequency = n(), .groups = "drop") %>%
  # calculate relative frequency within country
  group_by(Country_Name) %>%
  mutate(
    country_total_components = sum(component_frequency),
    relative_frequency = component_frequency / country_total_components
  )

# Collapse country_component_frequencies by country - average relative frequency
country_summary <- country_component_frequencies %>%
  group_by(Country_Name) %>%
  dplyr::summarize(
    avg_relative_frequency = mean(relative_frequency),
    .groups = 'drop'
  )

common_threshold <- 0.1

common_components <- country_component_frequencies %>%
  filter(relative_frequency >= common_threshold) %>%
  dplyr::select(Country_Name, name_components)

# create a lookup table by joining components to their frequencies once
name_component_lookup <- candidate_level_with_WDI %>%
  dplyr::select(Candidate_Election_ID, Country_Name, name_components) %>%
  unnest(name_components) %>%
  filter(nchar(name_components) > 1) %>%
  # join with frequency information
  left_join(
    country_component_frequencies,
    by = c("Country_Name", "name_components")
  ) %>%
  # join with common components flag
  left_join(
    common_components %>% 
      mutate(is_common = TRUE),
    by = c("Country_Name", "name_components")
  ) %>%
  mutate(is_common = ifelse(is.na(is_common), FALSE, TRUE))

# summarize by candidate
component_summary <- name_component_lookup %>%
  group_by(Candidate_Election_ID) %>%
  dplyr::summarize(
    has_common_component = any(is_common),
    common_component_score = mean(component_frequency, na.rm = TRUE),
    pct_common_components = mean(component_frequency > 1, na.rm = TRUE)
  )

# join back to main dataset
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  left_join(component_summary, by = "Candidate_Election_ID") %>%
  dplyr::select(-name_components)

### Final Preps
# constituency name for clustering
candidate_level_with_WDI <- candidate_level_with_WDI %>%
  mutate(constituency_state_country = paste(Constituency_Name, State, Country_Name, sep = "_"))

# only consider minor candidates
candidate_level_with_WDI_minor <- candidate_level_with_WDI %>%
  filter(Candidate_Type == "minor")

# only consider minor candidates that are decoys for mains
candidate_level_with_WDI_minor_decoy_main <- candidate_level_with_WDI %>%
  filter(
    Candidate_Type == "minor" & 
      (grepl("minor-main", decoy_relationships) | grepl("main-minor", decoy_relationships))
  )

### Saving
write_csv(candidate_level_with_WDI, "Cleaned Data/CLEA_candidate_level_w_WDI.csv")

### Trying Regressions
## Logit
# No FE
reg_log_0 <- feglm(
  is_decoy ~
    GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + 
    # Name features
    name_length +
    prefix_freq_normalized + suffix_freq_normalized +
    vowel_ratio + char_diversity,
  family = "logit",
  data = candidate_level_with_WDI_minor
)

# Country FE
reg_log_1 <- feglm(
  is_decoy ~
    GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + 
    # Name features
    name_length +
    prefix_freq_normalized + suffix_freq_normalized +
    vowel_ratio + char_diversity | Country_Name,
  family = "logit",
  data = candidate_level_with_WDI_minor, 
  cluster = ~ constituency_state_country
)

# Candidate Name FE
reg_log_2 <- feglm(
  is_decoy ~
    GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS | Candidate_Name,
  family = "logit",
  data = candidate_level_with_WDI_minor
)

# Year FE
reg_log_3 <- feglm(
  is_decoy ~
    GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS | Year,
  family = "logit",
  data = candidate_level_with_WDI_minor
)

# Country * Candidate Name FE
reg_log_4 <- feglm(
  is_decoy ~
    GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS | Country_Name^Candidate_Name,
  family = "logit",
  data = candidate_level_with_WDI_minor
)

# Country + Year FE
reg_log_5 <- feglm(
  is_decoy ~
    GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + 
    # Name features
    name_length +
    prefix_freq_normalized + suffix_freq_normalized +
    vowel_ratio + char_diversity | Country_Name + Year,
  family = "logit",
  data = candidate_level_with_WDI_minor
)

# results
summary(reg_log_0)
summary(reg_log_1)
summary(reg_log_2)
summary(reg_log_3)
summary(reg_log_4)
summary(reg_log_5)

## how does LPM fare? 
reg_lpm <- feols(
  is_decoy ~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + 
    # Name features
    name_length +
    has_common_component + pct_common_components | Country_Name + Year,
  data = candidate_level_with_WDI_minor
)

summary(reg_lpm)

# hierarchical model? 
reg_hier <- glmer(
  is_decoy ~ GE.PER.RNK + RL.PER.RNK + PV.PER.RNK + NY.GDP.PCAP.KD + 
    SP.RUR.TOTL.ZS + SL.TLF.ACTI.1524.ZS + NY.GDP.DEFL.KD.ZG +
    MS.MIL.XPND.ZS + GC.TAX.TOTL.GD.ZS + SE.XPD.TOTL.GD.ZS +
    EG.ELC.ACCS.ZS + 
    # Name features
    name_length +
    prefix_freq_normalized + suffix_freq_normalized +
    vowel_ratio + char_diversity + (1|Country_Name/Year),
  family = binomial,
  data = candidate_level_with_WDI
)

### what if we try just for india? 
candidate_level_with_WDI_ind <- candidate_level_with_WDI %>%
  filter(Country_Name == "India")

reg_lpm <- feols(
  is_decoy ~ 
    # Name features
    name_length +
    prefix_freq_normalized + suffix_freq_normalized +
    vowel_ratio + char_diversity | State + Year,
  data = candidate_level_with_WDI_ind
)
summary(reg_lpm)

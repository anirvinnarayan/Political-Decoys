# ============================================
# Political Decoys - String Similarity Calculation
# Testing Space
# ============================================
# Date: 08/04/25
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
candidate_pairs <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram_masala_dblmet.csv")

# candidate_pairs_old <- read.csv("Cleaned Data/candidate_pairs_lv_jw_ngram.csv")
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

### Testing new stuff!
# 1. LLM
library(httr)
library(jsonlite)

# OpenAI API key: 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

detect_decoy_openai <- function(string1, string2, api_key, model = "gpt-4o", temperature = 0.2) {
  system_message <- "You are an assistant specialized in identifying decoy items in data. A decoy is an item that appears to be a variant of another item but is actually a separate entity. Respond only with 'DECOY' or 'NOT_DECOY'."
  
  user_message <- paste0(
    "Analyze these two names and determine if they likely represent the same person (DECOY) or different people (NOT a decoy). Return only 'DECOY' or 'NOT_DECOY' without explanation.\n\n",
    "String 1: ", string1, "\n",
    "String 2: ", string2
  )
  
  # set up API call for OpenAI
  api_url <- "https://api.openai.com/v1/chat/completions"
  
  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_message),
      list(role = "user", content = user_message)
    ),
    max_tokens = 10,
    temperature = temperature
  )
  
  # make the API call
  response <- POST(
    url = api_url,
    add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
    ),
    body = toJSON(body, auto_unbox = TRUE)
  )
  
  # parse response
  if (status_code(response) == 200) {
    result <- content(response, "parsed")
    decision <- result$choices[[1]]$message$content
    return(trimws(decision))
  } else {
    warning("API call failed: ", content(response, "text"))
    return(NA)
  }
}

# function to batch process candidate pairs
batch_detect_decoys_openai <- function(candidate_pairs_df, string1_col, string2_col, api_key, model = "gpt-4o") {
  results <- vector("character", nrow(candidate_pairs_df))
  
  for (i in 1:nrow(candidate_pairs_df)) {
    string1 <- candidate_pairs_df[i, string1_col]
    string2 <- candidate_pairs_df[i, string2_col]
    
    # add brief delay to avoid hitting rate limits
    if (i > 1) Sys.sleep(0.5)
    
    results[i] <- detect_decoy_openai(string1, string2, api_key, model)
  }
  
  # add results to the dataframe
  candidate_pairs_df$llm_decision <- results
  return(candidate_pairs_df)
}

testing_for_llm <- all_states_elections %>%
  filter(State_Name == "Tamil_Nadu" & Constituency_Name == "KATTUMANNARKOIL" & Election_Type == "State Assembly Election (AE)" & Assembly_No == 11)

api_key <- "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"

testing_for_llm <- testing_for_llm %>%
  mutate(llm_decision = detect_decoy_openai(Candidate_clean, Candidate_clean, api_key))

unique_elections <- testing_for_llm %>%
  dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
  distinct()

candidate_pairs_test_llm <- data.frame()
result_list <- list()
list_index <- 1

# loop through each election
for (i in 1:nrow(unique_elections)) {
  # election details
  year <- unique_elections$Year[i]
  state <- unique_elections$State_Name[i]
  constituency <- unique_elections$Constituency_Name[i]
  election_type <- unique_elections$Election_Type[i]
  assembly_no <- unique_elections$Assembly_No[i]
  
  # filter data for current election
  election_data <- all_states_elections %>%
    filter(Year == year,
           State_Name == state,
           Constituency_Name == constituency,
           Election_Type == election_type,
           Assembly_No == assembly_no)
  
  # mark candidates as main or minor
  election_data <- election_data %>%
    mutate(candidate_type = ifelse(Vote_Share_Percentage > 10, "main", "minor"))
  
  # get candidate names
  candidate_names <- election_data$Candidate_clean

  # create all pairs of candidates
  for (j in 1:(nrow(election_data) - 1)) {
    for (k in (j+1):nrow(election_data)) {
      candidate1 <- election_data[j, ]
      candidate2 <- election_data[k, ]
      
      # run LLM comparison
      llm_decision <- detect_decoy_openai(
        candidate1$Candidate_clean,
        candidate2$Candidate_clean,
        api_key
      )
      
      # determine pair type
      pair_type <- paste0(candidate1$candidate_type, "-", candidate2$candidate_type)
      
      # create row
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
        LLM_Decision = llm_decision,
        stringsAsFactors = FALSE
      )
      
      result_list[[list_index]] <- pair_row
      list_index <- list_index + 1
      
      # prevent hitting API rate limit
      Sys.sleep(0.5)
    }
  }
}

# Create the final dataframe and filling it with candidate info
candidate_pairs_test_llm <- bind_rows(result_list)

# how does this compare to our original candidate_pairs
kattumannarkoil_TN_2016_AE <- candidate_pairs %>%
  filter(Year == "2016" & State_Name == "Tamil_Nadu" & Constituency_Name == "KATTUMANNARKOIL" & Election_Type == "State Assembly Election (AE)")

# 2. NLP shit --> DOES NOT WORK YET! 

### first, we need a state to lang match
str(candidate_pairs)
unique(candidate_pairs$State_Name)

# Andhra_Pradesh = Telugu
# Bihar = Hindi
# Meghalaya = English
# Madhya Pradesh = Hindi
# Mizoram = English
# Tamil_Nadu = Tamil
# Manipur = English
# Mysore = Kannada
# Kerala = Malayalam
# Jharkhand = Hindi
# Tripura = English
# Karnataka = Kannada
# Assam = Assamese
# Delhi = Hindi
# West_Bengal = Bengali
# Himachal_Pradesh = Hindi
# Maharahstra = Marathi
# Uttarakhand = Hindi
# Punjab = Punjabi
# Sikkim = English
# Nagaland = English
# Jammu_&_Kashmir = Hindi
# Arunachal_Pradesh = English
# Haryana = Hindi
# Chattisgarh = Hindi
# Puducherry = Tamil
# Uttar_Pradesh = Hindi
# Rajasthan = Hindi
# Madras = Tamil
# Goa_Daman_Diu = Hindi
# Goa = Konkani
# Odisha = Odia
# Gujarat = Gujarati
# Telangana = Telugu
# Andaman_&_Nicobar_Islands = English
# Chandigarh = Punjabi
# Dadra_&_Nagar_Haveli = Hindi
# Daman_&_Diu = Hindi
# Lakshwadeep = Malayalam
# Dadra & Nagar Haveli And Daman & Diu = Hindi
# Goa,_Daman_&_Diu = Hindi

# create a virtual python environment for indic nlp access (i think this is easier --> 
# as opposed to bringing all of the functions into R like i did with masala merge)
virtualenv_create("indic_nlp_env")
use_virtualenv("indic_nlp_env")

# install required Python packages
py_require("numpy")
py_require("scipy")
  # this is required for the indic nlp library - 
  # used for morphological analysis, breaking words down 
py_require("morfessor")
# install indic_nlp
py_require("git+https://github.com/anoopkunchukuttan/indic_nlp_library.git")

# create a directory for resources in our folder
dir.create("indic_nlp_resources", showWarnings = FALSE)

# download and extract resources (using R's download.file and unzip functions)
resources_url <- "https://github.com/anoopkunchukuttan/indic_nlp_resources/archive/refs/heads/master.zip"
download_path <- "indic_nlp_resources.zip"

# download the resources
download.file(resources_url, download_path)

# extract the resources
unzip(download_path, exdir = ".")

# move the extracted folder to the resources folder
file.rename("indic_nlp_resources-master", "indic_nlp_resources")

# clean up the zip file
file.remove(download_path)

# initialize the library using pythons script
init_indic_nlp <- function() {
  # python script to initialize the library
  init_script <- "
import sys
import os
  # get current directory
current_dir = os.getcwd()
  # set resources path
INDIC_NLP_RESOURCES = os.path.join(current_dir, 'indic_nlp_resources')
  # set resources path for the library
from indicnlp import common
common.set_resources_path(INDIC_NLP_RESOURCES)
  # init
from indicnlp import loader
loader.load()
  # import necessary modules
from indicnlp.script import indic_scripts as isc
from indicnlp.transliterate.unicode_transliterate import UnicodeIndicTransliterator
"
  
  # execute the initialization script
  py_run_string(init_script)
  
  # return TRUE if successful
  return(TRUE)
}

# fn to transliterate latin to tamil script
transliterate_latin_to_tamil <- function(latin_string) {
  # is library initialized?
  init_indic_nlp()
  
  # python code to transliterate
  py_run_string(paste0("
latin_string = '", latin_string, "'
tamil_string = UnicodeIndicTransliterator.transliterate(latin_string, 'en', 'ta')
result = tamil_string
  "))
  
  # return the transliterated string
  return(py$result)
}

# fn to compare two strings and get similarity score
compare_strings <- function(string1, string2) {
  init_indic_nlp()
  
  # python code to calculate LCSR - which is what the indic nlp library uses
  # https://nbviewer.org/url/anoopkunchukuttan.github.io/indic_nlp_library/doc/indic_nlp_examples.ipynb 
  # lexical NOT phonetic similarity
  py_run_string(paste0("
string1 = '", string1, "'
string2 = '", string2, "'
lcsr, len1, len2 = isc.lcsr_indic(string1, string2, 'ta', 'ta')
result = lcsr
  "))
  
  # return the similarity score
  return(py$result)
}

# fn to detect if two candidates are likely the same person
detect_tamil_decoys <- function(election_data, threshold = 0.7) {
  
  # again is lib initialized?
  init_indic_nlp()
  
  # create a list to store results
  result_list <- list()
  list_index <- 1
  
  # get unique election details
  unique_elections <- election_data %>%
    dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
    distinct()
  
  # loop through each election
  for (i in 1:nrow(unique_elections)) {
    # election details
    year <- unique_elections$Year[i]
    state <- unique_elections$State_Name[i]
    constituency <- unique_elections$Constituency_Name[i]
    election_type <- unique_elections$Election_Type[i]
    assembly_no <- unique_elections$Assembly_No[i]
    
    # filter data for current election
    current_election_data <- election_data %>%
      filter(Year == year,
             State_Name == state,
             Constituency_Name == constituency,
             Election_Type == election_type,
             Assembly_No == assembly_no)
    
    # mark candidates as main or minor
    current_election_data <- current_election_data %>%
      mutate(candidate_type = ifelse(Vote_Share_Percentage > 10, "main", "minor"))
    
    # create all pairs of candidates
    for (j in 1:(nrow(current_election_data) - 1)) {
      for (k in (j+1):nrow(current_election_data)) {
        candidate1 <- current_election_data[j, ]
        candidate2 <- current_election_data[k, ]
        
        # Step 1: Transliterate names if they're not already in Tamil
        # Check if the name is in Latin script and needs transliteration
        needs_transliteration1 <- !grepl("[\u0B80-\u0BFF]", candidate1$Candidate_clean)
        needs_transliteration2 <- !grepl("[\u0B80-\u0BFF]", candidate2$Candidate_clean)
        
        cand1_name <- candidate1$Candidate_clean
        cand2_name <- candidate2$Candidate_clean
        
        if (needs_transliteration1) {
          cand1_name <- transliterate_latin_to_tamil(cand1_name)
        }
        
        if (needs_transliteration2) {
          cand2_name <- transliterate_latin_to_tamil(cand2_name)
        }
        
        # Step 2: Compare similarity between the two names
        similarity_score <- compare_strings(cand1_name, cand2_name)
        
        # Step 3: Determine if they are likely the same person
        is_decoy <- similarity_score >= threshold
        
        # pair type
        pair_type <- paste0(candidate1$candidate_type, "-", candidate2$candidate_type)
        
        # create row with all the required information
        # Create and setup virtual environment for Indic NLP
        virtualenv_create("indic_nlp_env")
        use_virtualenv("indic_nlp_env")
        
        # Install required Python packages
        py_require("numpy")
        py_require("scipy")
        py_require("morfessor")
        py_require("git+https://github.com/anoopkunchukuttan/indic_nlp_library.git")
        
        # Create a directory for resources
        dir.create("indic_nlp_resources", showWarnings = FALSE)
        
        # Download and extract resources
        resources_url <- "https://github.com/anoopkunchukuttan/indic_nlp_resources/archive/refs/heads/master.zip"
        download_path <- "indic_nlp_resources.zip"
        download.file(resources_url, download_path)
        unzip(download_path, exdir = ".")
        file.rename("indic_nlp_resources-master", "indic_nlp_resources")
        file.remove(download_path)
        
        # Initialize the indic_nlp_library
        init_indic_nlp <- function() {
          # Python script to initialize the library
          init_script <- "
import sys
import os
# Get current directory
current_dir = os.getcwd()
# Set resources path
INDIC_NLP_RESOURCES = os.path.join(current_dir, 'indic_nlp_resources')
# Set resources path for the library
from indicnlp import common
common.set_resources_path(INDIC_NLP_RESOURCES)
# Initialize the library
from indicnlp import loader
loader.load()
# Import necessary modules
from indicnlp.script import indic_scripts as isc
from indicnlp.transliterate.unicode_transliterate import UnicodeIndicTransliterator
"
          
          # Execute the initialization script
          py_run_string(init_script)
          
          # Return TRUE if successful
          return(TRUE)
        }
        
        # Function to transliterate Latin to Tamil script
        transliterate_latin_to_tamil <- function(latin_string) {
          # Make sure the library is initialized
          init_indic_nlp()
          
          # Python code to transliterate
          py_run_string(paste0("
latin_string = '", latin_string, "'
tamil_string = UnicodeIndicTransliterator.transliterate(latin_string, 'en', 'ta')
result = tamil_string
  "))
          
          # Return the transliterated string
          return(py$result)
        }
        
        # Function to compare two strings and get similarity score
        compare_strings <- function(string1, string2) {
          # Make sure the library is initialized
          init_indic_nlp()
          
          # Python code to calculate LCSR
          py_run_string(paste0("
string1 = '", string1, "'
string2 = '", string2, "'
lcsr, len1, len2 = isc.lcsr_indic(string1, string2, 'ta', 'ta')
result = lcsr
  "))
          
          # Return the similarity score
          return(py$result)
        }
        
        # Function to detect if two candidates are likely the same person
        detect_tamil_decoys <- function(election_data, threshold = 0.7) {
          # Initialize the library if not already done
          init_indic_nlp()
          
          # Create a list to store results
          result_list <- list()
          list_index <- 1
          
          # Get unique election details
          unique_elections <- election_data %>%
            dplyr::select(Year, State_Name, Constituency_Name, Election_Type, Assembly_No) %>%
            distinct()
          
          # Loop through each election
          for (i in 1:nrow(unique_elections)) {
            # Election details
            year <- unique_elections$Year[i]
            state <- unique_elections$State_Name[i]
            constituency <- unique_elections$Constituency_Name[i]
            election_type <- unique_elections$Election_Type[i]
            assembly_no <- unique_elections$Assembly_No[i]
            
            # Filter data for current election
            current_election_data <- election_data %>%
              filter(Year == year,
                     State_Name == state,
                     Constituency_Name == constituency,
                     Election_Type == election_type,
                     Assembly_No == assembly_no)
            
            # Mark candidates as main or minor
            current_election_data <- current_election_data %>%
              mutate(candidate_type = ifelse(Vote_Share_Percentage > 10, "main", "minor"))
            
            # Create all pairs of candidates
            for (j in 1:(nrow(current_election_data) - 1)) {
              for (k in (j+1):nrow(current_election_data)) {
                candidate1 <- current_election_data[j, ]
                candidate2 <- current_election_data[k, ]
                
                # Step 1: Transliterate names if they're not already in Tamil
                # Check if the name is in Latin script and needs transliteration
                # This is a simplistic check - adjust as needed for your data
                needs_transliteration1 <- !grepl("[\u0B80-\u0BFF]", candidate1$Candidate_clean)
                needs_transliteration2 <- !grepl("[\u0B80-\u0BFF]", candidate2$Candidate_clean)
                
                # Store original names
                cand1_original <- candidate1$Candidate_clean
                cand2_original <- candidate2$Candidate_clean
                
                # Initialize Tamil names with originals
                cand1_name <- cand1_original
                cand2_name <- cand2_original
                
                # Store transliterated versions
                cand1_transliterated <- NA
                cand2_transliterated <- NA
                
                if (needs_transliteration1) {
                  cand1_transliterated <- transliterate_latin_to_tamil(cand1_original)
                  cand1_name <- cand1_transliterated
                }
                
                if (needs_transliteration2) {
                  cand2_transliterated <- transliterate_latin_to_tamil(cand2_original)
                  cand2_name <- cand2_transliterated
                }
                
                # Step 2: Compare similarity between the two names
                similarity_score <- compare_strings(cand1_name, cand2_name)
                
                # Step 3: Determine if they are likely the same person
                is_decoy <- similarity_score >= threshold
                
                # Determine pair type
                pair_type <- paste0(candidate1$candidate_type, "-", candidate2$candidate_type)
                
                # Create row with all the required information
                pair_row <- data.frame(
                  Year = year,
                  State_Name = state,
                  Constituency_Name = constituency,
                  Election_Type = election_type,
                  Assembly_No = assembly_no,
                  
                  Candidate1_Name = cand1_original,
                  Candidate1_Name_Tamil = cand1_name,
                  Candidate1_Transliterated = if(is.na(cand1_transliterated)) NA else cand1_transliterated,
                  Candidate1_Was_Transliterated = needs_transliteration1,
                  Candidate1_PID = candidate1$pid,
                  Candidate1_Party = candidate1$Party,
                  Candidate1_Votes = candidate1$Votes,
                  Candidate1_VoteShare = candidate1$Vote_Share_Percentage,
                  Candidate1_Type = candidate1$candidate_type,
                  
                  Candidate2_Name = cand2_original,
                  Candidate2_Name_Tamil = cand2_name, 
                  Candidate2_Transliterated = if(is.na(cand2_transliterated)) NA else cand2_transliterated,
                  Candidate2_Was_Transliterated = needs_transliteration2,
                  Candidate2_PID = candidate2$pid,
                  Candidate2_Party = candidate2$Party,
                  Candidate2_Votes = candidate2$Votes,
                  Candidate2_VoteShare = candidate2$Vote_Share_Percentage,
                  Candidate2_Type = candidate2$candidate_type,
                  
                  Pair_Type = pair_type,
                  Similarity_Score = similarity_score,
                  Is_Decoy = is_decoy,
                  stringsAsFactors = FALSE
                )
        
        # add to result list
        result_list[[list_index]] <- pair_row
        list_index <- list_index + 1
      }
    }
  }
  
  # combine all results into a single dataframe
  candidate_pairs <- bind_rows(result_list)
  
  return(candidate_pairs)
}

candidate_pairs_test_trans <- detect_tamil_decoys(testing_for_llm, threshold = 0.7)

hindi_test <- "पिछले दिनों हम लोगों ने कई उत्सव मनाये. कल, हिन्दुस्तान भर में श्री कृष्ण जन्म-महोत्सव मनाया गया."
py_run_string(paste0("
test_hindi = '", hindi_test, "'
test_latin = UnicodeIndicTransliterator.transliterate(test_hindi, 'hi', 'en')
print('Hindi to Latin:', test_latin)
"))

# how many decoy pairs
decoy_pairs <- candidate_pairs %>%
  filter(is_decoy == TRUE)


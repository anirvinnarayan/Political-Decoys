# Dbl Metaphone fn
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

# how to create a random sample
set.seed(123)

df_sample <- df %>%
  sample_n(min(15, nrow(.)))
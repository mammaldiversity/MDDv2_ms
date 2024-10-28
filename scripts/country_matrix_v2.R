####################################################################
#R-Script country_matrix_v2.R Madeleine Becker
#
#This script generates .txt file with a presence matrix (0=absence, 1=presence, 2=potential presence)
#from on an MDD-formatted 2-column csv input file of species and country distributions (separated by pipes and no spaces).
#It also generates a .txt file with total number of species by country (strict, lenient, and endemics only)
#Replace file names with full paths before running script
#
#This can also be customized for other columns in MDD such as continents or biogeographic realms, if formatted properly. N.B. that totals may be less applicable (e.g. strict vs. lenient)
#
#
#Updated from a previous 2022 script by MAB 10/28/24
# Implemented in R.4.3.2
####################################################################

# Load necessary libraries
library(tidyverse)

# Define input and output files (& path)
country_csv_path <- "MDD_v2_final/MDD/MDD_v2.0_6759species.csv"
country_matrix_path <- "MDD_v2_final/v2_country_matrix.txt"
country_totals_path <- "MDD_v2_final/v2_country_totals.txt"

# Read in country_csv
init <- read_csv(country_csv_path) %>% 
  select(sciName,countryDistribution)

# Create a vector of unique country names
countries <- init %>%
  pull(2) %>%
  str_split("\\|") %>%
  unlist() %>%
  str_replace_all("\\?$", "") %>%
  unique() %>% 
  sort()

# Create a vector of species names
species <- init %>% 
  pull(1)

# Instantiate final matrix and add species as row names and country names as column names
m <- matrix(0, 
            nrow = nrow(init), 
            ncol = length(countries), 
            dimnames = list(species, countries))

# Create a list of country vectors for each species
vector_list <- init %>%
  pull(2) %>%
  str_split("\\|")

# Fill in the final matrix
for (i in seq_along(vector_list)) {
  for (j in seq_along(countries)) {
    if (countries[j] %in% vector_list[[i]]) {
      m[i, j] <- 1
    } else if (paste0(countries[j], "?") %in% vector_list[[i]]) {
      m[i, j] <- 2
    }
  }
}

# Convert matrix to dataframe
m_df <- as.data.frame(m)
m_df <- tibble::rownames_to_column(m_df, var = "species")

# Write distribution matrix to csv
write_csv(m_df, country_matrix_path)

# Make endemics matrix
species_totals <- rowSums(m == 1, na.rm = TRUE)
endemics_only <- names(species_totals[species_totals == 1])
endemics_m <- m[endemics_only, , drop = FALSE]

# Calculate totals
total_strict <- colSums(m == 1, na.rm = TRUE)
total_lenient <- total_strict + colSums(m == 2, na.rm = TRUE)
total_endemics <- colSums(endemics_m == 1, na.rm = TRUE)

# Make totals table
totals_table <- tibble(
  country = colnames(m),
  total_strict = total_strict,
  total_lenient = total_lenient,
  total_endemics = total_endemics
)

# Write totals matrix to csv
write_csv(totals_table, country_totals_path)

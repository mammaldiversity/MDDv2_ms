# This is a work space for all figures and supplementary tables
# Made by Connor J Burgin for the Mammal Diversity Database v2.0 manuscript
# Created in R version 4.3.3

# Set working directory (should be the only thing you need to alter to run script)
setwd("G:\\My Drive\\Projects\\MDD_v2.0_manuscript\\final_mddv2_stats_figures")

######Loading Packages and Base Data######

# Install and load packages for all sections

#install.packages(c('readxl','readr','dplyr','tidyr','stringr','purrr','gt','patchwork','webshot2','rnaturalearth','rnaturalearthdata','sf','ggplot2','zoo','grid','scales','usmap'))
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(gt)
library(patchwork)
library(webshot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(zoo)
library(grid)
library(scales)
library(usmap)

# Loading base files for all sections, must have the species, synonym, geographic metadata, and museum metadata excel files to run this code
species_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\MDD_v2.0_6759species.csv")
synonyms_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\Species_Syn_v2.0.csv")
geographic_metadata <- read_excel("base_files\\mdd_geographic_terms_metadata.xlsx")

# Cleaning synonym sheet
synonyms_df_cleaned <- synonyms_df %>%
  mutate(across(everything(), ~na_if(str_trim(.), ""))) %>%  # Convert empty strings to NA in all columns
  mutate(
    # Clean latitude and longitude by removing anything that's not a number, period, or minus sign
    MDD_type_latitude = str_trim(str_replace_all(MDD_type_latitude, "[^0-9.-]", "")),
    MDD_type_longitude = str_trim(str_replace_all(MDD_type_longitude, "[^0-9.-]", "")),
    
    # Convert cleaned latitude and longitude to numeric, treating empty strings as NA
    MDD_type_latitude = as.numeric(MDD_type_latitude),
    MDD_type_longitude = as.numeric(MDD_type_longitude),
    
    # Clean other fields for non-printable characters and trim spaces
    MDD_authority_citation = str_trim(str_replace_all(MDD_authority_citation, "[^[:print:]]", "")),
    MDD_unchecked_authority_citation = str_trim(str_replace_all(MDD_unchecked_authority_citation, "[^[:print:]]", "")),
    MDD_original_type_locality = str_trim(str_replace_all(MDD_original_type_locality, "[^[:print:]]", "")),
    MDD_unchecked_type_locality = str_trim(str_replace_all(MDD_unchecked_type_locality, "[^[:print:]]", "")),
    MDD_author = str_trim(str_replace_all(MDD_author, "[^[:print:]]", "")),
    MDD_original_combination = str_trim(str_replace_all(MDD_original_combination, "[^[:print:]]", "")),
    MDD_original_rank = str_trim(str_replace_all(MDD_original_rank, "[^[:print:]]", "")),
    MDD_type_kind = str_trim(str_replace_all(MDD_type_kind, "[^[:print:]]", "")),
    MDD_type_specimen_link = str_trim(str_replace_all(MDD_type_specimen_link, "[^[:print:]]", ""))
  )


#NOT FOR THIS PUB
#museums_metadata <- read_excel("base_files\\museums_metadata.xlsx")

# Saving all base files as CSV files to be included in the supplemental material
write.csv(species_df, "supplementary_files\\mdd_v2_species_sheet.csv", row.names = FALSE)
write.csv(synonyms_df_cleaned, "supplementary_files\\mdd_v2_synonym_sheet.csv", row.names = FALSE)
write.csv(geographic_metadata, "supplementary_files\\mdd_v2_geography_list_sheet.csv", row.names = FALSE)

#NOT FOR THIS PUB
#write.csv(museums_metadata, "supplementary_files\\mdd_v2_museum_list_sheet.csv", row.names = FALSE)

######taxonomic_summary_df Supplement Table######

# Summarizing the data from the species and synonym sheet by taxonomic group for a supplemental table

# Load libraries
#library(readxl)
#library(dplyr)
#library(tidyr)
#library(stringr)

# Load the species and synonym data from excel files
#species_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\MDD_v2.0_6759species.csv")
#synonyms_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\Species_Syn_v2.0.csv")

# Helper functions to calculate living and extinct genera and families
calculate_living_extinct_genera <- function(df) {
  genus_summary <- df %>%
    group_by(genus) %>%
    summarise(living_species = sum(!extinct), total_species = n()) %>%
    mutate(living_genus = ifelse(living_species > 0, 1, 0),
           extinct_genus = ifelse(living_species == 0, 1, 0)) %>%
    summarise(living_genera = sum(living_genus), extinct_genera = sum(extinct_genus))
  return(genus_summary)
}

calculate_living_extinct_families <- function(df) {
  family_summary <- df %>%
    group_by(family) %>%
    summarise(living_species = sum(!extinct), total_species = n()) %>%
    mutate(living_family = ifelse(living_species > 0, 1, 0),
           extinct_family = ifelse(living_species == 0, 1, 0)) %>%
    summarise(living_families = sum(living_family), extinct_families = sum(extinct_family))
  return(family_summary)
}

# Summarize the number of species per order and family
species_summary <- species_df %>%
  group_by(order, family) %>%
  summarise(
    species_count = n(),
    living_species = sum(!extinct),
    wild_species = sum(!extinct & !domestic),
    extinct_species = sum(extinct),
    domestic_species = sum(domestic),
    genera = n_distinct(genus),
    new_since_MSW3 = sum(diffSinceMSW3 == 1),
    new_descriptions_since_MSW3 = sum(diffSinceMSW3 == 1 & authoritySpeciesYear >= 2004),
    IUCN_LC_species = sum(str_detect(iucnStatus, "LC")),
    IUCN_NT_species = sum(str_detect(iucnStatus, "NT")),
    IUCN_VU_species = sum(str_detect(iucnStatus, "VU")),
    IUCN_EN_species = sum(str_detect(iucnStatus, "EN")),
    IUCN_CR_species = sum(str_detect(iucnStatus, "CR")),
    IUCN_EW_species = sum(str_detect(iucnStatus, "EW")),
    IUCN_EX_species = sum(str_detect(iucnStatus, "EX")),
    IUCN_DD_species = sum(str_detect(iucnStatus, "DD")),
    IUCN_NE_species = sum(str_detect(iucnStatus, "NE")),
    IUCN_threatened_total = sum(str_detect(iucnStatus, "VU|EN|CR|EW")),
    IUCN_understudied_total = sum(str_detect(iucnStatus, "DD|NE"))
  ) %>%
  mutate(split_since_MSW3 = new_since_MSW3 - new_descriptions_since_MSW3) %>%
  left_join(species_df %>%
              group_by(order, family, genus) %>%
              summarise(living_species = sum(!extinct)) %>%
              mutate(living_genus = ifelse(living_species > 0, 1, 0),
                     extinct_genus = ifelse(living_species == 0, 1, 0)) %>%
              summarise(living_genera = sum(living_genus), extinct_genera = sum(extinct_genus), .groups = 'drop'), by = c("order", "family")) %>%
  ungroup()

# Define the available statuses of interest for synonyms
statuses_of_interest <- c("available", "as_emended", "preoccupied", "nomen_novum", "partially_suppressed", "fully_suppressed")

# Summarize the available synonyms per order and family
synonyms_summary <- synonyms_df_cleaned %>%
  filter(MDD_nomenclature_status %in% statuses_of_interest) %>%
  group_by(MDD_order, MDD_family) %>%
  summarise(available_names = n()) %>%
  ungroup() %>%
  rename(order = MDD_order, family = MDD_family)

# Combine the summaries into a single dataframe
combined_taxon_summary <- species_summary %>%
  full_join(synonyms_summary, by = c("order", "family")) %>%
  replace_na(list(species_count = 0, available_names = 0, genera = 0, living_species = 0, wild_species = 0, extinct_species = 0, domestic_species = 0, living_genera = 0, extinct_genera = 0, new_since_MSW3 = 0, new_descriptions_since_MSW3 = 0, split_since_MSW3 = 0, IUCN_LC_species = 0, IUCN_NT_species = 0, IUCN_VU_species = 0, IUCN_EN_species = 0, IUCN_CR_species = 0, IUCN_EW_species = 0, IUCN_EX_species = 0)) %>%
  mutate(rank = "family")

# Summarize totals for each order
order_totals <- species_df %>%
  group_by(order) %>%
  summarise(
    species_count = n(),
    living_species = sum(!extinct),
    wild_species = sum(!extinct & !domestic),
    extinct_species = sum(extinct),
    domestic_species = sum(domestic),
    genera = n_distinct(genus),
    available_names = sum(synonyms_summary$available_names[synonyms_summary$order == unique(order)], na.rm = TRUE),
    families = n_distinct(family),
    new_since_MSW3 = sum(diffSinceMSW3 == 1),
    new_descriptions_since_MSW3 = sum(diffSinceMSW3 == 1 & authoritySpeciesYear >= 2004),
    IUCN_LC_species = sum(str_detect(iucnStatus, "LC")),
    IUCN_NT_species = sum(str_detect(iucnStatus, "NT")),
    IUCN_VU_species = sum(str_detect(iucnStatus, "VU")),
    IUCN_EN_species = sum(str_detect(iucnStatus, "EN")),
    IUCN_CR_species = sum(str_detect(iucnStatus, "CR")),
    IUCN_EW_species = sum(str_detect(iucnStatus, "EW")),
    IUCN_EX_species = sum(str_detect(iucnStatus, "EX")),
    IUCN_DD_species = sum(str_detect(iucnStatus, "DD")),
    IUCN_NE_species = sum(str_detect(iucnStatus, "NE")),
    IUCN_threatened_total = sum(str_detect(iucnStatus, "VU|EN|CR|EW")),
    IUCN_understudied_total = sum(str_detect(iucnStatus, "DD|NE"))
  ) %>%
  mutate(split_since_MSW3 = new_since_MSW3 - new_descriptions_since_MSW3) %>%
  left_join(species_df %>%
              group_by(order, genus) %>%
              summarise(living_species = sum(!extinct)) %>%
              mutate(living_genus = ifelse(living_species > 0, 1, 0),
                     extinct_genus = ifelse(living_species == 0, 1, 0)) %>%
              summarise(living_genera = sum(living_genus), extinct_genera = sum(extinct_genus), .groups = 'drop'), by = "order") %>%
  left_join(species_df %>%
              group_by(order, family) %>%
              summarise(living_species = sum(!extinct)) %>%
              mutate(living_family = ifelse(living_species > 0, 1, 0),
                     extinct_family = ifelse(living_species == 0, 1, 0)) %>%
              summarise(living_families = sum(living_family), extinct_families = sum(extinct_family), .groups = 'drop'), by = "order") %>%
  mutate(family = NA, rank = "order")

# Summarize global totals
global_totals <- species_df %>%
  summarise(
    species_count = n(),
    living_species = sum(!extinct),
    wild_species = sum(!extinct & !domestic),
    extinct_species = sum(extinct),
    domestic_species = sum(domestic),
    genera = n_distinct(genus),
    available_names = sum(synonyms_summary$available_names, na.rm = TRUE),
    families = n_distinct(family),
    new_since_MSW3 = sum(diffSinceMSW3 == 1),
    new_descriptions_since_MSW3 = sum(diffSinceMSW3 == 1 & authoritySpeciesYear >= 2004),
    IUCN_LC_species = sum(str_detect(iucnStatus, "LC")),
    IUCN_NT_species = sum(str_detect(iucnStatus, "NT")),
    IUCN_VU_species = sum(str_detect(iucnStatus, "VU")),
    IUCN_EN_species = sum(str_detect(iucnStatus, "EN")),
    IUCN_CR_species = sum(str_detect(iucnStatus, "CR")),
    IUCN_EW_species = sum(str_detect(iucnStatus, "EW")),
    IUCN_EX_species = sum(str_detect(iucnStatus, "EX")),
    IUCN_DD_species = sum(str_detect(iucnStatus, "DD")),
    IUCN_NE_species = sum(str_detect(iucnStatus, "NE")),
    IUCN_threatened_total = sum(str_detect(iucnStatus, "VU|EN|CR|EW")),
    IUCN_understudied_total = sum(str_detect(iucnStatus, "DD|NE"))
  ) %>%
  mutate(split_since_MSW3 = new_since_MSW3 - new_descriptions_since_MSW3) %>%
  left_join(species_df %>%
              group_by(genus) %>%
              summarise(living_species = sum(!extinct)) %>%
              mutate(living_genus = ifelse(living_species > 0, 1, 0),
                     extinct_genus = ifelse(living_species == 0, 1, 0)) %>%
              summarise(living_genera = sum(living_genus), extinct_genera = sum(extinct_genus), .groups = 'drop'), by = character()) %>%
  left_join(species_df %>%
              group_by(family) %>%
              summarise(living_species = sum(!extinct)) %>%
              mutate(living_family = ifelse(living_species > 0, 1, 0),
                     extinct_family = ifelse(living_species == 0, 1, 0)) %>%
              summarise(living_families = sum(living_family), extinct_families = sum(extinct_family), .groups = 'drop'), by = character()) %>%
  mutate(order = "Global", family = NA, rank = "global")

# Combine all summaries
taxonomic_summary_df <- combined_taxon_summary %>%
  bind_rows(order_totals) %>%
  bind_rows(global_totals) %>%
  select(rank, order, family, available_names, species_count, living_species, wild_species, extinct_species, domestic_species, genera, living_genera, extinct_genera, families, living_families, extinct_families, new_since_MSW3, new_descriptions_since_MSW3, split_since_MSW3, IUCN_LC_species, IUCN_NT_species, IUCN_VU_species, IUCN_EN_species, IUCN_CR_species, IUCN_EW_species, IUCN_EX_species, IUCN_DD_species, IUCN_NE_species, IUCN_threatened_total, IUCN_understudied_total)

# Display the final summary
print(taxonomic_summary_df)

# Saving the taxonomic summary as a CSV file
write.csv(taxonomic_summary_df, "supplementary_files\\taxonomy_data_summary.csv", row.names = FALSE)


######geography_summary_df Supplement Table######

# Summarizing country data from the species and synonym sheets for a supplemental table

# Load packages
#library(readxl)
#library(dplyr)
#library(stringr)
#library(tidyr)

# Load the species, synonym (cleaned), and geographic metadata sheets
#species_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\MDD_v2.0_6759species.csv")
#geographic_metadata <- read_excel("G:\\My Drive\\Projects\\R Data/R-Data\\mdd_thesis_work\\mdd_geographic_terms_metadata.xlsx")

# Create a new dataframe from the geographic metadata sheet
geographic_data_df <- geographic_metadata %>%
  select(Category, Region, Governing_country, Higher_continent, Higher_biorealm) %>%
  rename(region_category = Category,
         region_name = Region,
         higher_country = Governing_country,
         higher_continent = Higher_continent,
         higher_biogeographic_realm = Higher_biorealm)

# Create a look-up table for ANSI codes to state names
state_lookup <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California",
  CO = "Colorado", CT = "Connecticut", DE = "Delaware", DC = "District of Columbia", FL = "Florida", GA = "Georgia (United States)",
  HI = "Hawaii", ID = "Idaho", IL = "Illinois", IN = "Indiana", IA = "Iowa",
  KS = "Kansas", KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland",
  MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi", MO = "Missouri",
  MT = "Montana", NE = "Nebraska", NV = "Nevada", NH = "New Hampshire", NJ = "New Jersey",
  NM = "New Mexico", NY = "New York", NC = "North Carolina", ND = "North Dakota", OH = "Ohio",
  OK = "Oklahoma", OR = "Oregon", PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina",
  SD = "South Dakota", TN = "Tennessee", TX = "Texas", UT = "Utah", VT = "Vermont",
  VA = "Virginia", WA = "Washington", WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming"
)

# Combine the other species distribution columns, keeping relevant columns for later summaries
expanded_species_distribution <- species_df %>%
  pivot_longer(cols = c(countryDistribution, continentDistribution, biogeographicRealm),
               names_to = "distribution_type", values_to = "regions") %>%
  filter(!is.na(regions)) %>%
  separate_rows(regions, sep = "\\|") %>%
  mutate(
    possible = str_detect(regions, "\\?$"),
    regions_clean = str_trim(str_remove(regions, "\\?$")),  # Clean up regions, removing any question marks
    sciName = sciName,
    extinct = extinct,
    diffSinceMSW3 = diffSinceMSW3,
    authoritySpeciesYear = authoritySpeciesYear,
    iucnStatus = iucnStatus
  ) %>%
  select(sciName, regions_clean, distribution_type, possible, extinct, diffSinceMSW3, authoritySpeciesYear, iucnStatus)

# Process subregionDistribution for US states, ensuring species are also endemic to the United States
subregion_df <- species_df %>%
  filter(!is.na(subregionDistribution) & !is.na(countryDistribution)) %>%
  mutate(
    # Check if the species is only found in the United States at the country level
    endemic_to_US = str_detect(countryDistribution, "United States") & nchar(gsub("[^|]", "", countryDistribution)) == 0, 
    states_list = str_extract(subregionDistribution, "(?<=USA\\().*(?=\\))")
  ) %>%
  separate_rows(states_list, sep = ",") %>%
  mutate(states_list = str_trim(states_list)) %>%
  mutate(
    regions_clean = state_lookup[str_replace(states_list, "\\?$", "")],  # Convert ANSI codes to state names
    possible = str_detect(states_list, "\\?$"),  # Identify possible occurrences
    sciName = sciName,  # Retain the species name for later grouping
    extinct = extinct,  # Include the extinct column
    diffSinceMSW3 = diffSinceMSW3,  # Include the diffSinceMSW3 column
    authoritySpeciesYear = authoritySpeciesYear,  # Include authoritySpeciesYear
    iucnStatus = iucnStatus  # Include iucnStatus
  ) %>%
  mutate(distribution_type = "subregionDistribution") %>%
  select(sciName, regions_clean, endemic_to_US, distribution_type, possible, extinct, diffSinceMSW3, authoritySpeciesYear, iucnStatus)

# Add the processed subregion data to the expanded_species_distribution
expanded_species_distribution <- bind_rows(expanded_species_distribution, subregion_df)

# Calculate endemism at the country level correctly
endemic_species_df <- expanded_species_distribution %>%
  filter(!possible) %>%
  group_by(sciName, distribution_type) %>%
  # Filter for species endemic to a single region within each distribution type (country, subregion, etc.)
  filter(if_else(distribution_type == "countryDistribution", 
                 n_distinct(regions_clean) == 1,  # Check that species is found in exactly one country
                 if_else(distribution_type == "subregionDistribution", 
                         endemic_to_US & n_distinct(regions_clean) == 1, TRUE))) %>%
  ungroup() %>%
  group_by(regions_clean) %>%
  summarise(endemic_species = n()) %>%
  rename(region_name = regions_clean)

# Calculate the possible species count separately (distribution records followed by a question  mark)
possible_species_counts <- expanded_species_distribution %>%
  filter(possible) %>%
  group_by(regions_clean) %>%
  summarise(possible_species_count = n()) %>%
  rename(region_name = regions_clean)

# Calculate the confirmed counts for all other metrics, excluding possible species records
confirmed_species_counts <- expanded_species_distribution %>%
  filter(!possible) %>%
  group_by(regions_clean) %>%
  summarise(
    species_count = n(),
    extinct_species_count = sum(extinct == 1, na.rm = TRUE),
    living_species_count = n() - sum(extinct == 1, na.rm = TRUE),
    new_since_MSW3 = sum(diffSinceMSW3 == 1, na.rm = TRUE),
    new_descriptions_since_MSW3 = sum(diffSinceMSW3 == 1 & authoritySpeciesYear >= 2004, na.rm = TRUE),
    IUCN_LC_species = sum(str_detect(iucnStatus, "^LC"), na.rm = TRUE),
    IUCN_NT_species = sum(str_detect(iucnStatus, "^NT"), na.rm = TRUE),
    IUCN_VU_species = sum(str_detect(iucnStatus, "^VU"), na.rm = TRUE),
    IUCN_EN_species = sum(str_detect(iucnStatus, "^EN"), na.rm = TRUE),
    IUCN_CR_species = sum(str_detect(iucnStatus, "^CR"), na.rm = TRUE),
    IUCN_EW_species = sum(str_detect(iucnStatus, "^EW"), na.rm = TRUE),
    IUCN_EX_species = sum(str_detect(iucnStatus, "^EX"), na.rm = TRUE),
    IUCN_DD_species = sum(str_detect(iucnStatus, "^DD"), na.rm = TRUE),
    IUCN_NE_species = sum(str_detect(iucnStatus, "^NE"), na.rm = TRUE),
    IUCN_threatened_total = sum(str_detect(iucnStatus, "^VU|^EN|^CR|^EW"), na.rm = TRUE),
    IUCN_understudied_total = sum(str_detect(iucnStatus, "^DD|^NE"), na.rm = TRUE)
  ) %>%
  rename(region_name = regions_clean)

# Merge endemic species counts into confirmed species counts
confirmed_species_counts <- confirmed_species_counts %>%
  left_join(endemic_species_df, by = "region_name") %>%
  mutate(endemic_species = replace_na(endemic_species, 0))

# Merge possible species counts with confirmed species counts
species_counts <- confirmed_species_counts %>%
  left_join(possible_species_counts, by = "region_name") %>%
  replace_na(list(possible_species_count = 0))

# Summarize available names for MDD_type_country with distinct counts
available_names_country <- synonyms_df_cleaned %>%
  separate_rows(MDD_nomenclature_status, sep = "\\|") %>%   # Separate values by pipe
  filter(MDD_nomenclature_status %in% c("available", "as_emended", "preoccupied", "nomen_novum", "partially_suppressed", "fully_suppressed")) %>%
  distinct(MDD_syn_ID, MDD_type_country) %>%  # Ensure unique counts by ID and country
  group_by(MDD_type_country) %>%
  summarise(available_names_country = n()) %>%
  rename(region_name = MDD_type_country)

# Summarize available names for MDD_type_subregion with distinct counts
available_names_subregion <- synonyms_df_cleaned %>%
  separate_rows(MDD_nomenclature_status, sep = "\\|") %>%   # Separate values by pipe
  filter(MDD_nomenclature_status %in% c("available", "as_emended", "preoccupied", "nomen_novum", "partially_suppressed", "fully_suppressed")) %>%
  distinct(MDD_syn_ID, MDD_type_subregion) %>%  # Ensure unique counts by ID and subregion
  group_by(MDD_type_subregion) %>%
  summarise(available_names_subregion = n()) %>%
  rename(region_name = MDD_type_subregion)

# Merge the available names from country directly into the geographic data
geographic_data_df <- geographic_data_df %>%
  left_join(available_names_country, by = "region_name") %>%
  replace_na(list(available_names_country = 0))

# Merge the available names from subregion directly into the geographic data
geographic_data_df <- geographic_data_df %>%
  left_join(available_names_subregion, by = "region_name") %>%
  replace_na(list(available_names_subregion = 0))

# Summarize the total available names
geographic_data_df <- geographic_data_df %>%
  mutate(available_names = available_names_country + available_names_subregion) %>%
  select(-available_names_country, -available_names_subregion)

# Merge final counts into the geographic data, placing the available_names column before species_count
geography_summary_df <- geographic_data_df %>%
  left_join(species_counts, by = "region_name") %>%
  select(region_category, region_name, higher_country, higher_continent, higher_biogeographic_realm,
         available_names, species_count, possible_species_count, extinct_species_count, living_species_count,
         new_since_MSW3, new_descriptions_since_MSW3, endemic_species,
         IUCN_LC_species, IUCN_NT_species, IUCN_VU_species, IUCN_EN_species,
         IUCN_CR_species, IUCN_EW_species, IUCN_EX_species, IUCN_DD_species, IUCN_NE_species, 
         IUCN_threatened_total, IUCN_understudied_total)

# Revised function for calculating available names for each biogeographic realm with special subregion cases
update_available_names_for_realm <- function(realm_name, geography_summary_df) {
  # Define multi-realm countries and specific subregions
  multi_realm_countries <- c("China", "Indonesia", "Mexico")
  special_subregions <- tibble(
    region_name = c("Crozet", "Kerguelen", "Christmas Island", "Cocos Islands", "Hawaii"),
    higher_country = c("France", "France", "Australia", "Australia", "United States"),
    target_realm = c("Antarctic", "Antarctic", "Indomalaya", "Indomalaya", "Oceania (Biorealm)")
  )
  
  # Step 1: Filter for the relevant region categories for this realm, excluding special subregions in their parent realm
  filtered_df <- geography_summary_df %>%
    filter(
      region_category %in% c("Continent Subregion [type localities only]", "Country", "Offshore Region", "Offshore Region [type localities only]", "Continent"),
      higher_biogeographic_realm == realm_name,
      !region_name %in% special_subregions$region_name  # Exclude special subregions from the parent realm
    )
  
  # Step 2: Exclude country-level counts for multi-realm countries
  country_level_df <- filtered_df %>%
    filter(!(region_category == "Country" & region_name %in% multi_realm_countries))
  
  # Step 3: Add subregion counts for multi-realm countries within the specific realm
  subregion_df <- geography_summary_df %>%
    filter(
      region_category == "Country Subregion",
      higher_country %in% multi_realm_countries,
      higher_biogeographic_realm == realm_name
    )
  
  # Step 4: Include counts for special subregions if they belong to this specific realm
  special_subregion_df <- geography_summary_df %>%
    inner_join(special_subregions, by = "region_name") %>%
    filter(target_realm == realm_name) %>%
    select(region_name, available_names)
  
  # Step 5: Combine all data for final sum
  combined_df <- bind_rows(country_level_df, subregion_df, special_subregion_df) %>%
    distinct(region_name, available_names) %>%
    summarise(available_names_sum = sum(available_names, na.rm = TRUE))
  
  # Step 6: Update the available_names for the specified biogeographic realm
  geography_summary_df <- geography_summary_df %>%
    mutate(
      available_names = ifelse(
        region_category == "Biogeographic Realm" & region_name == realm_name,
        combined_df$available_names_sum,
        available_names
      )
    )
  
  return(geography_summary_df)
}

# Revised function for calculating available names for each continent with special subregion cases
update_available_names_for_continent <- function(continent_name, geography_summary_df) {
  # Define multi-continent countries and specific subregions
  multi_continent_countries <- c("Russia", "Indonesia")
  special_subregions <- tibble(
    region_name = c("Crozet", "Kerguelen", "Christmas Island", "Cocos Islands", "Hawaii"),
    higher_country = c("France", "France", "Australia", "Australia", "United States"),
    target_continent = c("Antarctica", "Antarctica", "Asia", "Asia", "Oceania (Continent)")
  )
  
  # Step 1: Filter for the relevant region categories for this continent, excluding special subregions in their parent continent
  filtered_df <- geography_summary_df %>%
    filter(
      region_category %in% c("Continent Subregion [type localities only]", "Country", "Offshore Region", "Offshore Region [type localities only]", "Continent"),
      higher_continent == continent_name,
      !region_name %in% special_subregions$region_name  # Exclude special subregions from the parent continent
    )
  
  # Step 2: Exclude country-level counts for multi-continent countries
  country_level_df <- filtered_df %>%
    filter(!(region_category == "Country" & region_name %in% multi_continent_countries))
  
  # Step 3: Add subregion counts for multi-continent countries within the specific continent
  subregion_df <- geography_summary_df %>%
    filter(
      region_category == "Country Subregion",
      higher_country %in% multi_continent_countries,
      higher_continent == continent_name
    )
  
  # Step 4: Include counts for special subregions if they belong to this specific continent
  special_subregion_df <- geography_summary_df %>%
    inner_join(special_subregions, by = "region_name") %>%
    filter(target_continent == continent_name) %>%
    select(region_name, available_names)
  
  # Step 5: Combine all data for final sum
  combined_df <- bind_rows(country_level_df, subregion_df, special_subregion_df) %>%
    distinct(region_name, available_names) %>%
    summarise(available_names_sum = sum(available_names, na.rm = TRUE))
  
  # Step 6: Update the available_names for the specified continent
  geography_summary_df <- geography_summary_df %>%
    mutate(
      available_names = ifelse(
        region_category == "Continent" & region_name == continent_name,
        combined_df$available_names_sum,
        available_names
      )
    )
  
  return(geography_summary_df)
}


# Update available name counts for each biogeographic realm without additional filters
geography_summary_df <- update_available_names_for_realm("Afrotropic", geography_summary_df)
geography_summary_df <- update_available_names_for_realm("Antarctic", geography_summary_df)
geography_summary_df <- update_available_names_for_realm("Australasia", geography_summary_df)
geography_summary_df <- update_available_names_for_realm("Indomalaya", geography_summary_df)
geography_summary_df <- update_available_names_for_realm("Nearctic", geography_summary_df)
geography_summary_df <- update_available_names_for_realm("Neotropic", geography_summary_df)
geography_summary_df <- update_available_names_for_realm("Oceania (Biorealm)", geography_summary_df)
geography_summary_df <- update_available_names_for_realm("Palearctic", geography_summary_df)

# Update available name counts for each continent without additional filters
geography_summary_df <- update_available_names_for_continent("Africa", geography_summary_df)
geography_summary_df <- update_available_names_for_continent("Antarctica", geography_summary_df)
geography_summary_df <- update_available_names_for_continent("Asia", geography_summary_df)
geography_summary_df <- update_available_names_for_continent("Europe", geography_summary_df)
geography_summary_df <- update_available_names_for_continent("North America", geography_summary_df)
geography_summary_df <- update_available_names_for_continent("Oceania (Continent)", geography_summary_df)
geography_summary_df <- update_available_names_for_continent("South America", geography_summary_df)

# View the geography_summary_df
print(geography_summary_df)

# Saving the geographic data summary as a CSV file
write.csv(geography_summary_df, "supplementary_files\\geographic_data_summary.csv", row.names = FALSE)


######nomenclature_summary_df Supplement Table######

# Summarizing the nomenclature data from the synonym sheet for a supplemental table

# Load packages
#library(dplyr)
#library(readxl)
#library(stringr)

# Use cleaned synonym sheet from earlier

# Create flagged_synonyms_df using the cleaned data and retain MDD_syn_ID
flagged_synonyms_df <- synonyms_df_cleaned %>%
  mutate(
    type_coordinates_flag = if_else(!is.na(MDD_type_latitude), TRUE, FALSE),
    original_type_locality_flag = if_else(!is.na(MDD_original_type_locality) & MDD_original_type_locality != "", TRUE, FALSE),
    authority_citation_flag = if_else(!is.na(MDD_authority_citation) & MDD_authority_citation != "" |
                                        !is.na(MDD_unchecked_authority_citation) & MDD_unchecked_authority_citation != "", TRUE, FALSE),
    type_locality_flag = if_else(!is.na(MDD_original_type_locality) & MDD_original_type_locality != "" |
                                   !is.na(MDD_unchecked_type_locality) & MDD_unchecked_type_locality != "", TRUE, FALSE),
    authority_author_flag = if_else(!is.na(MDD_author) & MDD_author != "", TRUE, FALSE),
    authority_year_flag = if_else(!is.na(MDD_year), TRUE, FALSE),
    original_combination_flag = if_else(!is.na(MDD_original_combination) & MDD_original_combination != "", TRUE, FALSE),
    original_rank_flag = if_else(!is.na(MDD_original_rank) & MDD_original_rank != "", TRUE, FALSE),
    verified_authority_citation_flag = if_else(!is.na(MDD_authority_citation) & MDD_authority_citation != "", TRUE, FALSE),
    authority_page_flag = if_else(!is.na(MDD_authority_page) & MDD_authority_page != "", TRUE, FALSE),
    authority_link_flag = if_else(!is.na(MDD_authority_link) & MDD_authority_link != "", TRUE, FALSE),
    authority_page_link_flag = if_else(!is.na(MDD_authority_page_link) & MDD_authority_page_link != "", TRUE, FALSE),
    type_specimen_flag = if_else(!is.na(MDD_type_kind) & MDD_type_kind != "", TRUE, FALSE),
    type_specimen_link_flag = if_else(!is.na(MDD_type_specimen_link) & MDD_type_specimen_link != "", TRUE, FALSE)
  )

# Separate rows based on multiple values in MDD_nomenclature_status, but retain the original MDD_syn_ID
synonyms_df_separated <- synonyms_df_cleaned %>%
  separate_rows(MDD_nomenclature_status, sep = "\\|") %>%
  mutate(MDD_nomenclature_status = str_trim(MDD_nomenclature_status))

# Create flagged_synonyms_df using the separated data and retain MDD_syn_ID
flagged_sep_synonyms_df <- synonyms_df_separated %>%
  mutate(
    type_coordinates_flag = if_else(!is.na(MDD_type_latitude), TRUE, FALSE),
    original_type_locality_flag = if_else(!is.na(MDD_original_type_locality) & MDD_original_type_locality != "", TRUE, FALSE),
    authority_citation_flag = if_else(!is.na(MDD_authority_citation) & MDD_authority_citation != "" |
                                        !is.na(MDD_unchecked_authority_citation) & MDD_unchecked_authority_citation != "", TRUE, FALSE),
    type_locality_flag = if_else(!is.na(MDD_original_type_locality) & MDD_original_type_locality != "" |
                                   !is.na(MDD_unchecked_type_locality) & MDD_unchecked_type_locality != "", TRUE, FALSE),
    authority_author_flag = if_else(!is.na(MDD_author) & MDD_author != "", TRUE, FALSE),
    authority_year_flag = if_else(!is.na(MDD_year), TRUE, FALSE),
    original_combination_flag = if_else(!is.na(MDD_original_combination) & MDD_original_combination != "", TRUE, FALSE),
    original_rank_flag = if_else(!is.na(MDD_original_rank) & MDD_original_rank != "", TRUE, FALSE),
    verified_authority_citation_flag = if_else(!is.na(MDD_authority_citation) & MDD_authority_citation != "", TRUE, FALSE),
    authority_page_flag = if_else(!is.na(MDD_authority_page) & MDD_authority_page != "", TRUE, FALSE),
    authority_link_flag = if_else(!is.na(MDD_authority_link) & MDD_authority_link != "", TRUE, FALSE),
    authority_page_link_flag = if_else(!is.na(MDD_authority_page_link) & MDD_authority_page_link != "", TRUE, FALSE),
    type_specimen_flag = if_else(!is.na(MDD_type_kind) & MDD_type_kind != "", TRUE, FALSE),
    type_specimen_link_flag = if_else(!is.na(MDD_type_specimen_link) & MDD_type_specimen_link != "", TRUE, FALSE)
  )

# Summarizing the data by status_type and status
intermediate_status_df <- flagged_sep_synonyms_df %>%
  select(MDD_validity, MDD_nomenclature_status) %>%
  pivot_longer(
    cols = c(MDD_validity, MDD_nomenclature_status),
    names_to = "status_type",
    values_to = "status"
  ) %>%
  mutate(status_type = case_when(
    status_type == "MDD_validity" ~ "Validity Status",
    status_type == "MDD_nomenclature_status" ~ "Nomenclature Status"
  )) %>%
  distinct(status_type, status)  # Only keep distinct combinations

# Helper function to calculate counts with exact match for non-year columns
calculate_count <- function(flag_col, status, validity_df, nomenclature_df) {
  # Validity calculation with exact match
  validity_count <- sum((validity_df[[flag_col]] == TRUE) &
                          (validity_df$MDD_validity == status), na.rm = TRUE)
  
  # Nomenclature calculation with exact match
  nomenclature_count <- sum((nomenclature_df[[flag_col]] == TRUE) &
                              (nomenclature_df$MDD_nomenclature_status == status), na.rm = TRUE)
  
  # Total count is the sum of the validity and nomenclature counts
  validity_count + nomenclature_count
}

# Main mutation block with exact match and separate calculation for year_since_2000_count
val_nom_summary_df <- intermediate_status_df %>%
  rowwise() %>%
  mutate(
    # Calculate total_names using exact match
    total_names = sum(flagged_synonyms_df$MDD_validity == status, na.rm = TRUE) +
      sum(flagged_sep_synonyms_df$MDD_nomenclature_status == status, na.rm = TRUE),
    
    # Custom calculation for year_since_2000_count
    year_since_2000_count = sum((flagged_synonyms_df$MDD_year >= 2000) & 
                                  (flagged_synonyms_df$MDD_validity == status), na.rm = TRUE) +
      sum((flagged_sep_synonyms_df$MDD_year >= 2000) & 
            (flagged_sep_synonyms_df$MDD_nomenclature_status == status), na.rm = TRUE),
    
    # Use calculate_count function for each flag column except year_since_2000_count
    authority_author_count = calculate_count("authority_author_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    authority_year_count = calculate_count("authority_year_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    original_combination_total = calculate_count("original_combination_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    original_rank_count = calculate_count("original_rank_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    authority_citation_count = calculate_count("authority_citation_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    verified_authority_citation_count = calculate_count("verified_authority_citation_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    authority_page_count = calculate_count("authority_page_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    authority_link_count = calculate_count("authority_link_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    authority_page_link_count = calculate_count("authority_page_link_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    type_locality_count = calculate_count("type_locality_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    original_type_locality_count = calculate_count("original_type_locality_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    type_coordinates_count = calculate_count("type_coordinates_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    type_specimen_count = calculate_count("type_specimen_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df),
    type_specimen_link_count = calculate_count("type_specimen_link_flag", status, flagged_synonyms_df, flagged_sep_synonyms_df)
  ) %>%
  ungroup()

# Filter the rows with 'Validity Status' in the 'status_type' column
validity_status_rows <- val_nom_summary_df %>%
  filter(status_type == "Validity Status")

# Now calculate the sum for each numeric column (which are your flag counts)
total_synonyms <- validity_status_rows %>%
  summarise(
    status_type = "Total",
    status = "total_synonyms",
    total_names = sum(total_names, na.rm = TRUE),  # Summing the number of names
    
    # Summing the flag counts
    authority_author_count = sum(authority_author_count, na.rm = TRUE),
    authority_year_count = sum(authority_year_count, na.rm = TRUE),
    year_since_2000_count = sum(year_since_2000_count, na.rm = TRUE),
    original_combination_total = sum(original_combination_total, na.rm = TRUE),
    original_rank_count = sum(original_rank_count, na.rm = TRUE),
    authority_citation_count = sum(authority_citation_count, na.rm = TRUE),
    verified_authority_citation_count = sum(verified_authority_citation_count, na.rm = TRUE),
    authority_page_count = sum(authority_page_count, na.rm = TRUE),
    authority_link_count = sum(authority_link_count, na.rm = TRUE),
    authority_page_link_count = sum(authority_page_link_count, na.rm = TRUE),
    type_locality_count = sum(type_locality_count, na.rm = TRUE),
    original_type_locality_count = sum(original_type_locality_count, na.rm = TRUE),
    type_coordinates_count = sum(type_coordinates_count, na.rm = TRUE),
    type_specimen_count = sum(type_specimen_count, na.rm = TRUE),
    type_specimen_link_count = sum(type_specimen_link_count, na.rm = TRUE)
  )

# Combine the summaries (per status) and the new total_synonyms row into one dataframe
nomenclature_summary_df <- bind_rows(val_nom_summary_df, total_synonyms)

# Sort the dataframe alphabetically by status_type and status, keeping the Total row at the bottom
nomenclature_summary_df <- nomenclature_summary_df %>%
  arrange(status_type, status) %>%
  arrange(factor(status, levels = c(setdiff(unique(status), "total_synonyms"), "total_synonyms")))

# View final nomenclature summary dataframe
print(nomenclature_summary_df)

# Save the final CSV
write.csv(nomenclature_summary_df, "supplementary_files\\nomenclature_data_summary.csv", row.names = FALSE)


######Nomenclature In Text Table######

# Creating a table to visualize the nomenclatural data from the synonym sheet

# Load necessary libraries
#library(dplyr)
#library(tidyr)
#library(gt)

# Create the mapping dataframe based on your provided list
mapping_df <- tibble::tribble(
  ~MDD_nomenclature_status,                      ~Primary_Category,             ~Secondary_Category,        ~Tertiary_Category,
  "as_emended",                                  "Total Available Names",       NA,                         NA,
  "available",                                   "Total Available Names",       NA,                         NA,
  "before_1758",                                 "Total Unavailable Names",     NA,                         NA,
  "conditional",                                 "Total Unavailable Names",     NA,                         NA,
  "fully_suppressed",                            "Total Available Names",       "Suppressed Names",         NA,
  "fully_suppressed | nomen_novum",              "Total Available Names",       "Suppressed Names",         "Nomena Nova",
  "fully_suppressed | partially_suppressed | preoccupied", "Total Available Names", "Suppressed Names",    "Preoccupied Names",
  "hybrid_as_such",                              "Total Unavailable Names",     NA,                         NA,
  "hypothetical_concept",                        "Total Unavailable Names",     NA,                         NA,
  "inconsistently_binominal",                    "Total Unavailable Names",     NA,                         NA,
  "inconsistently_binominal | incorrect_subsequent_spelling", "Total Unavailable Names", NA,                 NA,
  "inconsistently_binominal | nomen_novum",      "Total Unavailable Names",     NA,                         NA,
  "incorrect_original_spelling",                 "Spelling Variants",           NA,                         NA,
  "incorrect_original_spelling | unpublished_electronic", "Spelling Variants",   NA,                         NA,
  "incorrect_original_spelling | unpublished_thesis", "Spelling Variants",      NA,                         NA,
  "incorrect_subsequent_spelling",               "Spelling Variants",           NA,                         NA,
  "incorrect_subsequent_spelling | not_used_as_valid", "Spelling Variants",      NA,                         NA,
  "incorrect_subsequent_spelling | preoccupied", "Spelling Variants",           NA,                         NA,
  "incorrect_subsequent_spelling | subsequent_usage", "Spelling Variants",      NA,                         NA,
  "incorrect_subsequent_spelling | unjustified_emendation", "Spelling Variants", NA,                         NA,
  "incorrect_subsequent_spelling | unpublished_electronic", "Spelling Variants", NA,                         NA,
  "incorrect_subsequent_spelling | variety_or_form", "Spelling Variants",       NA,                         NA,
  "infrasubspecific",                            "Total Unavailable Names",     NA,                         NA,
  "infrasubspecific | preoccupied",              "Total Unavailable Names",     NA,                         NA,
  "justified_emendation",                        "Spelling Variants",           NA,                         NA,
  "mandatory_change",                            "Spelling Variants",           NA,                         NA,
  "misidentification",                           "Other Subsequent Usages",     NA,                         NA,
  "misidentification | not_used_as_valid",       "Other Subsequent Usages",     NA,                         NA,
  "misidentification | unjustified_emendation",  "Other Subsequent Usages",     NA,                         NA,
  "name_combination",                            "Name Combinations",           NA,                         NA,
  "name_combination | incorrect_subsequent_spelling", "Name Combinations",       "Spelling Variants",       NA,
  "name_combination | incorrect_subsequent_spelling | unjustified_emendation", "Name Combinations", "Spelling Variants", NA,
  "name_combination | incorrect_subsequent_spelling | unpublished_electronic", "Name Combinations", "Spelling Variants", NA,
  "name_combination | incorrect_subsequent_spelling | unpublished_thesis", "Name Combinations", "Spelling Variants", NA,
  "name_combination | incorrect_subsequent_spelling | variety_or_form", "Name Combinations", "Spelling Variants", NA,
  "name_combination | infrasubspecific",         "Name Combinations",           NA,                         NA,
  "name_combination | justified_emendation",     "Name Combinations",           "Spelling Variants",       NA,
  "name_combination | preoccupied",              "Name Combinations",           NA,                         NA,
  "name_combination | unjustified_emendation",   "Name Combinations",           "Spelling Variants",       NA,
  "name_combination | unpublished_electronic",   "Name Combinations",           NA,                         NA,
  "name_combination | unpublished_thesis",       "Name Combinations",           NA,                         NA,
  "name_combination | variety_or_form",          "Name Combinations",           NA,                         NA,
  "no_type_specified",                           "Total Unavailable Names",     NA,                         NA,
  "nomen_novum",                                 "Total Available Names",       "Nomena Nova",              NA,
  "nomen_novum | as_emended",                    "Total Available Names",       "Nomena Nova",              NA,
  "nomen_novum | preoccupied",                   "Total Available Names",       "Nomena Nova",              "Preoccupied Names",
  "nomen_nudum",                                 "Total Unavailable Names",     NA,                         NA,
  "nomen_nudum | infrasubspecific",              "Total Unavailable Names",     NA,                         NA,
  "nomen_nudum | not_used_as_valid",             "Total Unavailable Names",     NA,                         NA,
  "nomen_nudum | preoccupied",                   "Total Unavailable Names",     NA,                         NA,
  "not_explicitly_new",                          "Total Unavailable Names",     NA,                         NA,
  "not_intended_as_a_scientific_name",           "Total Unavailable Names",     NA,                         NA,
  "not_intended_as_a_scientific_name | infrasubspecific", "Total Unavailable Names", NA,                   NA,
  "not_published_with_a_generic_name",           "Total Unavailable Names",     NA,                         NA,
  "not_used_as_valid",                           "Total Unavailable Names",     NA,                         NA,
  "not_used_as_valid | nomen_novum",             "Total Unavailable Names",     NA,                         NA,
  "not_used_as_valid | preoccupied",             "Total Unavailable Names",     NA,                         NA,
  "partially_suppressed",                        "Total Available Names",       "Suppressed Names",         NA,
  "partially_suppressed | preoccupied",          "Total Available Names",       "Suppressed Names",         "Preoccupied Names",
  "partially_suppressed | subsequent_usage | preoccupied", "Total Available Names", "Suppressed Names", "Preoccupied Names",
  "preoccupied",                                 "Total Available Names",       "Preoccupied Names",        NA,
  "rejected_by_fiat",                            "Total Unavailable Names",     NA,                         NA,
  "subsequent_usage",                            "Other Subsequent Usages",     NA,                         NA,
  "subsequent_usage | nomen_novum",              "Other Subsequent Usages",     NA,                         NA,
  "subsequent_usage | preoccupied",              "Other Subsequent Usages",     NA,                         NA,
  "unjustified_emendation",                      "Spelling Variants",           NA,                         NA,
  "unjustified_emendation | preoccupied",        "Spelling Variants",           NA,                         NA,
  "unpublished",                                 "Total Unavailable Names",     NA,                         NA,
  "unpublished_electronic",                      "Total Unavailable Names",     NA,                         NA,
  "unpublished_pending",                         "Total Unavailable Names",     NA,                         NA,
  "unpublished_supplement",                      "Total Unavailable Names",     NA,                         NA,
  "unpublished_thesis",                          "Total Unavailable Names",     NA,                         NA,
  "variant",                                     "Spelling Variants",           NA,                         NA,
  "variant | preoccupied",                       "Spelling Variants",           NA,                         NA,
  "variety_or_form",                             "Total Unavailable Names",     NA,                         NA,
  "variety_or_form | nomen_novum",               "Total Unavailable Names",     NA,                         NA,
  "variety_or_form | preoccupied",               "Total Unavailable Names",     NA,                         NA
)

# Function to calculate counts based on the flag columns
calculate_count <- function(flag_col, status, validity_df, nomenclature_df) {
  validity_status_count <- sum((validity_df[[flag_col]] == TRUE) & (validity_df$MDD_validity == status), na.rm = TRUE)
  nomenclature_status_count <- sum((nomenclature_df[[flag_col]] == TRUE) & (nomenclature_df$MDD_nomenclature_status == status), na.rm = TRUE)
  validity_status_count + nomenclature_status_count
}

# Merge with `mapping_df` to assign primary, secondary, and tertiary categories
synonyms_with_categories <- flagged_synonyms_df %>%
  left_join(mapping_df, by = "MDD_nomenclature_status")

# Summarize counts for nomenclature statuses with corrected logic
nomenclature_status_counts <- synonyms_with_categories %>%
  # Pivot to create a 'status' column from the three categories for grouping purposes
  pivot_longer(cols = c(Primary_Category, Secondary_Category, Tertiary_Category),
               names_to = "category_type", values_to = "status") %>%
  filter(!is.na(status)) %>%
  group_by(status) %>%
  summarise(
    total_names = n(),
    authority_author_count = sum(authority_author_flag == TRUE, na.rm = TRUE),
    authority_year_count = sum(authority_year_flag == TRUE, na.rm = TRUE),
    year_since_2000_count = sum((MDD_year >= 2000), na.rm = TRUE),
    original_combination_total = sum(original_combination_flag == TRUE, na.rm = TRUE),
    original_rank_count = sum(original_rank_flag == TRUE, na.rm = TRUE),
    authority_citation_count = sum(authority_citation_flag == TRUE, na.rm = TRUE),
    verified_authority_citation_count = sum(verified_authority_citation_flag == TRUE, na.rm = TRUE),
    authority_page_count = sum(authority_page_flag == TRUE, na.rm = TRUE),
    authority_link_count = sum(authority_link_flag == TRUE, na.rm = TRUE),
    authority_page_link_count = sum(authority_page_link_flag == TRUE, na.rm = TRUE),
    type_locality_count = sum(type_locality_flag == TRUE, na.rm = TRUE),
    original_type_locality_count = sum(original_type_locality_flag == TRUE, na.rm = TRUE),
    type_coordinates_count = sum(type_coordinates_flag == TRUE, na.rm = TRUE),
    type_specimen_count = sum(type_specimen_flag == TRUE, na.rm = TRUE),
    type_specimen_link_count = sum(type_specimen_link_flag == TRUE, na.rm = TRUE)
  ) %>%
  mutate(status_type = "Nomenclature Status") %>%
  ungroup()

# Summarize counts for validity statuses with formatted labels
validity_status_counts <- flagged_synonyms_df %>%
  group_by(MDD_validity) %>%
  summarise(
    total_names = n(),
    authority_author_count = sum(authority_author_flag, na.rm = TRUE),
    authority_year_count = sum(authority_year_flag, na.rm = TRUE),
    year_since_2000_count = sum(MDD_year >= 2000, na.rm = TRUE),
    original_combination_total = sum(original_combination_flag, na.rm = TRUE),
    original_rank_count = sum(original_rank_flag, na.rm = TRUE),
    authority_citation_count = sum(authority_citation_flag, na.rm = TRUE),
    verified_authority_citation_count = sum(verified_authority_citation_flag, na.rm = TRUE),
    authority_page_count = sum(authority_page_flag, na.rm = TRUE),
    authority_link_count = sum(authority_link_flag, na.rm = TRUE),
    authority_page_link_count = sum(authority_page_link_flag, na.rm = TRUE),
    type_locality_count = sum(type_locality_flag, na.rm = TRUE),
    original_type_locality_count = sum(original_type_locality_flag, na.rm = TRUE),
    type_coordinates_count = sum(type_coordinates_flag, na.rm = TRUE),
    type_specimen_count = sum(type_specimen_flag, na.rm = TRUE),
    type_specimen_link_count = sum(type_specimen_link_flag, na.rm = TRUE)
  ) %>%
  rename(status = MDD_validity) %>%
  mutate(
    status_type = "Validity Status",
    status = case_when(
      status == "composite" ~ "Composite Type Material",
      status == "hybrid" ~ "Hybrid Type Material",
      status == "nomen_dubium" ~ "Nomena Dubia",
      status == "species" ~ "Valid Species",
      status == "species_inquirenda" ~ "Species Inquirendae",
      status == "synonym" ~ "Synonymous Names",
      TRUE ~ status  # Keep other status titles as they are
    )
  )

# Combine all rows into final summary
nomenclature_summary_for_table <- bind_rows(nomenclature_status_counts, validity_status_counts, total_synonyms)

# Define the desired order of rows
status_order <- c(
  "Total Available Names", "Nomena Nova", "Preoccupied Names", "Suppressed Names", 
  "Total Unavailable Names", "Spelling Variants", "Name Combinations", 
  "Other Subsequent Usages", "Synonymous Names", "Valid Species", 
  "Species Inquirendae", "Nomena Dubia", "Composite Type Material", 
  "Hybrid Type Material", "Total Synonyms"
)

# Convert 'status' to a factor with the specified order
nomenclature_summary_for_table <- nomenclature_summary_for_table %>%
  mutate(status = factor(status, levels = status_order)) %>%
  arrange(status)

# Format the final table in `gt`
nomenclature_summary_gt <- nomenclature_summary_for_table %>%
  rename(
    'Status Type' = status_type,
    'Status' = status,
    'Total Names' = total_names,
    'Authority Author Count' = authority_author_count,
    'Authority Year Count' = authority_year_count,
    'Names Since 2000' = year_since_2000_count,
    'Original Combination Total' = original_combination_total,
    'Original Rank Count' = original_rank_count,
    'Authority Citation Count' = authority_citation_count,
    'Verified Authority Citation Count' = verified_authority_citation_count,
    'Authority Page Count' = authority_page_count,
    'Authority Link Count' = authority_link_count,
    'Authority Page Link Count' = authority_page_link_count,
    'Type Locality Count' = type_locality_count,
    'Original Type Locality Count' = original_type_locality_count,
    'Type Coordinates Count' = type_coordinates_count,
    'Type Specimen Count' = type_specimen_count,
    'Type Specimen Link Count' = type_specimen_link_count
  ) %>%
  gt(groupname_col = "Status Type") %>%
  fmt_number(
    columns = vars(`Total Names`, `Authority Author Count`, `Authority Year Count`, `Names Since 2000`, 
                   `Original Combination Total`, `Original Rank Count`, `Authority Citation Count`, 
                   `Verified Authority Citation Count`, `Authority Page Count`, `Authority Link Count`, 
                   `Authority Page Link Count`, `Type Locality Count`, `Original Type Locality Count`, 
                   `Type Coordinates Count`, `Type Specimen Count`, `Type Specimen Link Count`),
    decimals = 0
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_body(columns = "Status")
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    table.font.size = 12
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_width(
    everything() ~ px(120)
  )

# View the final table
print(nomenclature_summary_gt)

# Save the final table as an HTML file
gtsave(nomenclature_summary_gt, "tables/nomenclature_summary_table.html")


######MDD Version Comparisons ######

# Summarizing the differences between the various MDD versions
# Must already have the taxonomic_summary_df supplement table loaded

# Load packages
#library(readr)
#library(dplyr)
#library(gt)
#library(webshot2)

# Define a list of MDD versions with their download links and column names
mdd_versions <- list(
  "MDD1.0" = read_csv("https://zenodo.org/records/4139723/files/MDD_v1_6495species_JMamm.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.1" = read_csv("https://zenodo.org/records/4139788/files/MDD_v1.1_6526species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.2" = read_csv("https://zenodo.org/records/4139818/files/MDD_v1.2_6485species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.3" = read_csv("https://zenodo.org/records/4397179/files/MDD_v1.3_6513species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.3.1" = read_csv("https://zenodo.org/records/4429371/files/MDD_v1.31_6513species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.4" = read_csv("https://zenodo.org/records/4679816/files/MDD_v1.4_6533species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.5" = read_csv("https://zenodo.org/records/4926590/files/MDD_v1.5_6554species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.6" = read_csv("https://zenodo.org/records/5175993/files/MDD_v1.6_6557species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.7" = read_csv("https://zenodo.org/records/5651212/files/MDD_v1.7_6567species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.8" = read_csv("https://zenodo.org/records/5945626/files/MDD_v1.8_6591species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.9" = read_csv("https://zenodo.org/records/6407053/files/MDD_v1.9_6596species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.9.1" = read_csv("https://zenodo.org/records/7358650/files/MDD_v1.9.1_6596species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.10" = read_csv("https://zenodo.org/records/7394529/files/MDD_v1.10_6615species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.11" = read_csv("https://zenodo.org/records/7830771/files/MDD_v1.11_6649species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.12" = read_csv("https://zenodo.org/records/10463715/files/MDD_v1.12_6718species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.12.1" = read_csv("https://zenodo.org/records/10595931/files/MDD_v1.12.1_6718species.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.13" = read_csv("https://zenodo.org/records/12738010/files/MDD_v1.13_6753species.csv?download=1", locale = locale(encoding = "UTF-8"))
)

# Define a list of diff files for each MDD version that has a diff file
diff_files <- list(
  "MDD1.1" = read_csv("base_files/Diff_v1.0-v1.1.csv"),
  "MDD1.2" = read_csv("base_files/Diff_v1.1-v1.2.csv"),
  "MDD1.3" = read_csv("base_files/Diff_v1.2-v1.3.1.csv"),
  "MDD1.4" = read_csv("https://zenodo.org/records/4679816/files/Diff_v1.31-v1.4.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.5" = read_csv("https://zenodo.org/records/4926590/files/Diff_v1.4-v1.5.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.6" = read_csv("https://zenodo.org/records/5175993/files/Diff_v1.5-v1.6.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.7" = read_csv("https://zenodo.org/records/5651212/files/Diff_v1.6-v1.7.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.8" = read_csv("https://zenodo.org/records/5945626/files/Diff_v1.7-v1.8.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.9" = read_csv("https://zenodo.org/records/6407053/files/Diff_v1.8-v1.9.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.9.1" = read_csv("https://zenodo.org/records/7358650/files/Diff_v1.9-v1.9.1.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.10" = read_csv("https://zenodo.org/records/7394529/files/Diff_v1.9-v1.10.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.11" = read_csv("https://zenodo.org/records/7830771/files/Diff_v1.10-v1.11.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.12" = read_csv("https://zenodo.org/records/10463715/files/Diff_v1.11-v1.12.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.12.1" = read_csv("https://zenodo.org/records/10595931/files/Diff_v1.12-v1.12.1.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD1.13" = read_csv("https://zenodo.org/records/12738010/files/Diff_v1.12.1-v1.13.csv?download=1", locale = locale(encoding = "UTF-8")),
  "MDD2.0" = read_csv("base_files\\MDD_v2.0\\MDD_v2.0\\Diff_v1.13-v2.0.csv", locale = locale(encoding = "UTF-8"))
)

# Saving the versions and diff files for supplementary file use

# Define the paths for the new folders inside the base folder
all_mdd_versions <- file.path("supplementary_files//all_mdd_versions")
all_mdd_diff_files <- file.path("supplementary_files//all_mdd_diff_files")

# Create folders if they don't exist
if (!dir.exists(all_mdd_versions)) {
  dir.create(all_mdd_versions, recursive = TRUE)
}
if (!dir.exists(all_mdd_diff_files)) {
  dir.create(all_mdd_diff_files, recursive = TRUE)
}

# Save files in 'earlier_mdd_versions' folder with '_species_list' suffix
for (version in names(mdd_versions)) {
  file_path <- file.path(all_mdd_versions, paste0(version, "_species_list.csv"))
  write.csv(mdd_versions[[version]], file = file_path, row.names = FALSE)
}

# Save files in 'mdd_versions_diff_files' folder with '_diff_file' suffix
for (diff_version in names(diff_files)) {
  file_path <- file.path(all_mdd_diff_files, paste0(diff_version, "_diff_file.csv"))
  write.csv(diff_files[[diff_version]], file = file_path, row.names = FALSE)
}

# Creating the Graph

# Define a function to summarize various counts for each MDD version
summarize_mdd_version <- function(data, version) {
  sci_name_col <- if ("SciName" %in% colnames(data)) "SciName" else "sciName"
  genus_col <- if ("Genus" %in% colnames(data)) "Genus" else "genus"
  family_col <- if ("Family" %in% colnames(data)) "Family" else "family"
  extinct_col <- if ("extinct?" %in% colnames(data)) "extinct?" else "extinct"
  domestic_col <- if ("domestic?" %in% colnames(data)) "domestic?" else "domestic"
  
  total_species <- length(unique(data[[sci_name_col]]))
  extinct_species <- sum(data[[extinct_col]] == 1, na.rm = TRUE)
  domestic_species <- if (version == "MDD1.0") 17 else sum(data[[domestic_col]] == 1, na.rm = TRUE)
  
  wild_extant_species <- total_species - extinct_species - domestic_species
  total_genera <- length(unique(data[[genus_col]]))
  total_families <- length(unique(data[[family_col]]))
  
  # Calculate the De Novo, Split, and Lump categories from diff files if available
  if (version %in% names(diff_files)) {
    if ("Category" %in% colnames(diff_files[[version]])) {
      de_novo <- sum(diff_files[[version]]$Category == "de novo", na.rm = TRUE)
      split <- sum(diff_files[[version]]$Category == "split", na.rm = TRUE)
      lump <- sum(diff_files[[version]]$Category %in% c("lump", "synonymization"), na.rm = TRUE)
    } else if ("Comment" %in% colnames(diff_files[[version]])) {
      de_novo <- sum(grepl("^recently described", diff_files[[version]]$Comment, ignore.case = TRUE), na.rm = TRUE)
      split <- sum(grepl("^split", diff_files[[version]]$Comment, ignore.case = TRUE), na.rm = TRUE)
      lump <- sum(grepl("^now a synonym", diff_files[[version]]$Comment, ignore.case = TRUE), na.rm = TRUE)
    } else {
      de_novo <- NA
      split <- NA
      lump <- NA
    }
  } else {
    # If no diff file is available, set the values to NA
    de_novo <- NA
    split <- NA
    lump <- NA
  }
  
  return(c(total_species, wild_extant_species, extinct_species, domestic_species, de_novo, split, lump, total_genera, total_families))
}

# Initialize an empty dataframe to store the summaries
version_comparison_df <- data.frame(
  Taxa = c(
    "Species", 
    "  Wild Extant",  
    "  Recently Extinct",  
    "  Domestic",  
    "  De Novo",
    "  Split",
    "  Lump",
    "Genera", 
    "Families"
  )
)

# Loop through each MDD version and summarize the data
for (version in names(mdd_versions)) {
  version_summary_values <- summarize_mdd_version(mdd_versions[[version]], version)
  version_comparison_df[[version]] <- version_summary_values
}

# Initialize an empty list for diff counts
calculated_diff_counts <- list()

# Manually fill in the counts for version 1.3.1 (which has no taxonomic differences)
calculated_diff_counts[["MDD1.3.1"]] <- c(de_novo = 0, split = 0, lump = 0)

# Add these counts to the version comparison dataframe
version_comparison_df[["MDD1.3.1"]][5:7] <- calculated_diff_counts[["MDD1.3.1"]]

# Add the data from the taxonomic_summary_df for the MDD 2.0 column
version_comparison_df[["MDD2.0"]] <- c(
  taxonomic_summary_df %>% filter(rank == "global") %>% pull(species_count),
  taxonomic_summary_df %>% filter(rank == "global") %>% pull(wild_species),
  taxonomic_summary_df %>% filter(rank == "global") %>% pull(extinct_species),
  taxonomic_summary_df %>% filter(rank == "global") %>% pull(domestic_species),
  sum(grepl("^de novo", diff_files[["MDD2.0"]]$Category, ignore.case = TRUE), na.rm = TRUE),
  sum(grepl("^split", diff_files[["MDD2.0"]]$Category, ignore.case = TRUE), na.rm = TRUE),
  sum(grepl("^lump|^synonymization", diff_files[["MDD2.0"]]$Category, ignore.case = TRUE), na.rm = TRUE),
  taxonomic_summary_df %>% filter(rank == "global") %>% pull(genera),
  taxonomic_summary_df %>% filter(rank == "global") %>% pull(families)
)

# Create a vector of the publication dates for each version
publication_dates <- c(
  "Feb 2018", "Mar 2019", "Sep 2020", "Dec 2020", "Jan 2021", "Apr 2021",
  "Jun 2021", "Aug 2021", "Nov 2021", "Feb 2022", "Apr 2022", "Jun 2022",
  "Dec 2022", "Apr 2023", "Jan 2024", "Jan 2024", "Jul 2024", "Aug 2024"
)

# Modify the MDD2.0 values for De Novo, Split, and Lump
mdd_2.0_values <- version_comparison_df[5:7, "MDD2.0"]

# Add parentheses to these values
mdd_2.0_values[1] <- paste0(mdd_2.0_values[1], " (297)")  # For De Novo
mdd_2.0_values[2] <- paste0(mdd_2.0_values[2], " (320)")  # For Split
mdd_2.0_values[3] <- paste0(mdd_2.0_values[3], " (357)")  # For Lump

# Update the version_comparison_df with these modified values
version_comparison_df[5:7, "MDD2.0"] <- mdd_2.0_values

# Update the column labels in the gt table to include both the version and the publication date
mdd_version_comparison_table <- version_comparison_df %>%
  gt() %>%
  fmt_number(
    columns = everything(),
    decimals = 0
  ) %>%
  # Create custom column labels with dates
  cols_label(
    MDD1.0 = md("**MDD1.0**<br>Feb 2018"),
    MDD1.1 = md("**MDD1.1**<br>Mar 2019"),
    MDD1.2 = md("**MDD1.2**<br>Sep 2020"),
    MDD1.3 = md("**MDD1.3**<br>Dec 2020"),
    MDD1.3.1 = md("**MDD1.3.1**<br>Jan 2021"),
    MDD1.4 = md("**MDD1.4**<br>Apr 2021"),
    MDD1.5 = md("**MDD1.5**<br>Jun 2021"),
    MDD1.6 = md("**MDD1.6**<br>Aug 2021"),
    MDD1.7 = md("**MDD1.7**<br>Nov 2021"),
    MDD1.8 = md("**MDD1.8**<br>Feb 2022"),
    MDD1.9 = md("**MDD1.9**<br>Apr 2022"),
    MDD1.9.1 = md("**MDD1.9.1**<br>Jun 2022"),
    MDD1.10 = md("**MDD1.10**<br>Dec 2022"),
    MDD1.11 = md("**MDD1.11**<br>Apr 2023"),
    MDD1.12 = md("**MDD1.12**<br>Jan 2024"),
    MDD1.12.1 = md("**MDD1.12.1**<br>Jan 2024"),
    MDD1.13 = md("**MDD1.13**<br>Jul 2024"),
    MDD2.0 = md("**MDD2.0**<br>Aug 2024")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Italicize and bold the Taxa column values
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_body(columns = Taxa)
  ) %>%
  # Align the Taxa column to the left and indent specific row titles
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = c(Taxa),
      rows = Taxa %in% c("  Wild Extant", "  Recently Extinct", "  Domestic", "  De Novo", "  Split", "  Lump")
    )
  ) %>%
  # Align all other columns to the left without affecting values
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  cols_width(
    Taxa ~ px(160),
    everything() ~ px(90)
  ) %>%
  # Set the font for the entire table to Helvetica
  tab_options(
    table.font.names = "Helvetica"
  )

# Print the table
mdd_version_comparison_table

# Save the gt table as an HTML file
gtsave(mdd_version_comparison_table, "tables\\mdd_version_comparison_table.html")

#Breaking the table into two chunks to make it manageable in the paper

# Split the columns of the dataframe into two parts
first_half_versions <- version_comparison_df %>%
  select(Taxa, MDD1.0:MDD1.7)

second_half_versions <- version_comparison_df %>%
  select(Taxa, MDD1.8:MDD2.0)

# Correctly reference the modified column name for MDD2.0

# Create the first half of the table with footnote
mdd_version_comparison_table_part1 <- first_half_versions %>%
  gt() %>%
  fmt_number(
    columns = everything(),
    decimals = 0
  ) %>%
  # Add the version labels with dates
  cols_label(
    MDD1.0 = md("**MDD1.0**<br>Feb 2018"),
    MDD1.1 = md("**MDD1.1**<br>Mar 2019"),
    MDD1.2 = md("**MDD1.2**<br>Sep 2020"),
    MDD1.3 = md("**MDD1.3**<br>Dec 2020"),
    MDD1.3.1 = md("**MDD1.3.1**<br>Jan 2021"),
    MDD1.4 = md("**MDD1.4**<br>Apr 2021"),
    MDD1.5 = md("**MDD1.5**<br>Jun 2021"),
    MDD1.6 = md("**MDD1.6**<br>Aug 2021"),
    MDD1.7 = md("**MDD1.7**<br>Nov 2021")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_body(columns = Taxa)
  ) %>%
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  cols_width(
    Taxa ~ px(180),
    everything() ~ px(100)
  ) %>%
  tab_footnote(
    footnote = "Total differences between MDD1 and MDD2",
    locations = cells_body(columns = contains("MDD2.0"), rows = c(5, 6, 7))  # Use contains() to match the column
  )

# Create the second half of the table with footnote
mdd_version_comparison_table_part2 <- second_half_versions %>%
  gt() %>%
  fmt_number(
    columns = everything(),
    decimals = 0
  ) %>%
  # Add the version labels with dates
  cols_label(
    MDD1.8 = md("**MDD1.8**<br>Feb 2022"),
    MDD1.9 = md("**MDD1.9**<br>Apr 2022"),
    MDD1.9.1 = md("**MDD1.9.1**<br>Jun 2022"),
    MDD1.10 = md("**MDD1.10**<br>Dec 2022"),
    MDD1.11 = md("**MDD1.11**<br>Apr 2023"),
    MDD1.12 = md("**MDD1.12**<br>Jan 2024"),
    MDD1.12.1 = md("**MDD1.12.1**<br>Jan 2024"),
    MDD1.13 = md("**MDD1.13**<br>Jul 2024"),
    MDD2.0 = md("**MDD2.0**<br>Aug 2024")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_body(columns = Taxa)
  ) %>%
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  cols_width(
    Taxa ~ px(180),
    everything() ~ px(100)
  ) %>%
  tab_footnote(
    footnote = "Total differences between MDD1 and MDD2",
    locations = cells_body(columns = contains("MDD2.0"), rows = c(5, 6, 7))  # Use contains() to match the column
  )

# Save both parts as HTML
gtsave(mdd_version_comparison_table_part1, "tables\\mdd_version_comparison_table_part1.html")
gtsave(mdd_version_comparison_table_part2, "tables\\mdd_version_comparison_table_part2.html")


######Compendia Comparisons######

# Making a table to summarize the difference between major mammal compendia and the MDD
# This section builds from the Table 1 section

# Load packages
#library(dplyr)
#library(gt)

# Define the data for the compendia comparison table
comparison_table_2 <- data.frame(
  Taxa = c(
    "Species",
    "  Wild Extant",
    "  Recently Extinct",
    "  Domestic",
    "Genera",
    "Families",
    "Orders"
  ),
  Corbet_Hill_1980 = c(
    4007, NA, NA, NA, 1014, 129, 21 # Corbet and Hill 1980 values
  ),
  MSW1_1982 = c(
    4170, NA, NA, NA, 1033, 135, 20  # MSW1 values
  ),
  Corbet_Hill_1986 = c(
    4231, NA, NA, NA, 1055, 132, 20 # Corbet and Hill 1980 values
  ),
  Corbet_Hill_1991 = c(
    4336, NA, NA, NA, 1066, 131, 21 # Corbet and Hill 1991 values
  ),
  MSW2_1993 = c(
    4631, NA, NA, NA, 1135, 132, 26  # MSW2 values
  ),
  MSW3_2005 = c(
    5416, 5338, 75, 3, 1230, 153, 29  # MSW3 values
  ),
  IUCN_2008 = c(
    5489, 5410, 79, 0, 1241, 156, 27 #IUCN 2008 values taken from Schipper et al. 2008
  ),
  MDD1_0 = c(
    6495, 6382, 96, 17, 1316, 166, 27  # MDD1.0 values taken from the first table
  ),
  CMW_2020 = c(
    6554, 6451, 103, 20, 1343, 167, 27  # CMW 2020 values
  ),
  IUCN_2024 = c(
    5983, 5899, 84, 0, 1308, 164, 27 # IUCN 2024 values from IUCN version 2024-1
  ),
  MDD2_0 = c(
    6753, 6629, 113, 17, 1353, 167, 27  # MDD2.0 values taken from the first table
  )
)

# Create the gt table
mdd_compendia_table <- comparison_table_2 %>%
  gt() %>%
  fmt_number(
    columns = everything(),
    decimals = 0
  ) %>%
  # Create custom column labels with years
  cols_label(
    Corbet_Hill_1980 = md("**C&H1**<br>1980"),
    MSW1_1982 = md("**MSW1**<br>1982"),
    Corbet_Hill_1986 = md("**C&H2**<br>1986"),
    Corbet_Hill_1991 = md("**C&H3**<br>1991"),
    MSW2_1993 = md("**MSW2**<br>1993"),
    MSW3_2005 = md("**MSW3**<br>2005"),
    IUCN_2008 = md("**IUCN**<br>2008"),
    MDD1_0 = md("**MDD1.0**<br>2018"),
    CMW_2020 = md("**CMW**<br>2020"),
    IUCN_2024 = md("**IUCN**<br>2024"),
    MDD2_0 = md("**MDD2.0**<br>2024")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Italicize and bold the Taxa column values
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_body(columns = Taxa)
  ) %>%
  # Align the Taxa column to the left and indent specific row titles
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = c(Taxa),
      rows = Taxa %in% c("  Wild Extant", "  Recently Extinct", "  Domestic")
    )
  ) %>%
  # Align all other columns to the left without affecting values
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  cols_width(
    Taxa ~ px(160),
    everything() ~ px(90)
  ) %>%
  # Set the font for the entire table to Helvetica
  tab_options(
    table.font.names = "Helvetica"
  )

# Add footnotes sequentially to avoid conflicts
mdd_compendia_table <- mdd_compendia_table %>%
  # Footnote 1: for species total in MSW2
  tab_footnote(
    footnote = "Corrected total per Solari and Baker (2007)",
    locations = cells_body(
      columns = MSW2_1993,
      rows = Taxa == "Species"
    )
  ) %>%
  # Footnote 2: for IUCN wild species totals
  tab_footnote(
    footnote = "Includes 1 species classified as Extinct in the Wild and does not include humans",
    locations = cells_body(
      columns = IUCN_2024,
      rows = Taxa == "  Wild Extant"
    )
  ) %>%
  # Footnote 3: for IUCN domestic species totals
  tab_footnote(
    footnote = "The IUCN does not assess domestic species",
    locations = cells_body(
      columns = IUCN_2024,
      rows = Taxa == "  Domestic"
    )
  )

# Print the table
mdd_compendia_table

# Save the gt table as an HTML file
gtsave(mdd_compendia_table, "tables\\mdd_compendia_table.html")


######Taxonomic Change Graph######

# Creating a histogram of descriptions of available names and species since 1758
# And overlaying trendlines of taxonomic trends with 10 year running means

# Load packages
#library(dplyr)
#library(ggplot2)
#library(zoo)
#library(scales)
#library(patchwork)



# Load the synonym data from excel files
#synonyms_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\Species_Syn_v2.0.csv")

# Filter the dataframe for relevant nomenclature statuses and years after 1757
synonym_since_1758_data <- synonyms_df_cleaned %>%
  filter(MDD_year > 1757, 
         MDD_nomenclature_status %in% c("available", "as_emended", "preoccupied", "nomen_novum", "fully_suppressed", "partially_suppressed")) %>%
  mutate(MDD_year = as.numeric(MDD_year))

# Create a complete sequence of years from 1758 to the most recent year in your dataset
complete_years <- data.frame(MDD_year = seq(1758, max(synonym_since_1758_data$MDD_year)))

# Create a subset for available names where nomenclature status is one of the relevant statuses
available_names_per_year_data <- synonym_since_1758_data %>%
  group_by(MDD_year) %>%
  summarise(total_names = n())
# Create a subset of the filtered data where validity is exactly "species"
species_year_data <- synonym_since_1758_data %>%
  filter(MDD_validity == "species") %>%
  mutate(MDD_year = as.numeric(MDD_year))

# Create a subset for species_per_year where original rank is *exactly* 'species'
species_per_year_data <- synonym_since_1758_data %>%
  filter(MDD_original_rank == "species") %>%
  group_by(MDD_year) %>%
  summarise(species_per_year = n())

# Create a subset for 'Lumps': originally species but now synonyms
lumps_data <- synonym_since_1758_data %>%
  filter(MDD_original_rank == "species", MDD_validity == "synonym")

# Create a subset for 'Splits': originally synonym/subspecies/form/variety but now species
splits_data <- synonym_since_1758_data %>%
  filter(MDD_original_rank %in% c("synonym", "subspecies", "form", "variety"), MDD_validity == "species")

# Create a subset for 'Described as Subspecies': originally subspecies 
subspecies_data <- synonym_since_1758_data %>%
  filter(MDD_original_rank %in% c("subspecies", "form", "variety"))

# Create a subset for valid species where validity is *exactly* 'species'
valid_species_per_year_data <- synonym_since_1758_data %>%
  filter(MDD_validity == "species") %>%
  group_by(MDD_year) %>%
  summarise(valid_species_totals = n())

# Calculate counts of subspecies descriptions per year
subspecies_per_year <- subspecies_data %>%
  group_by(MDD_year) %>%
  summarise(subspecies_count = n())

# Calculate counts of lumps and splits per year
lumps_per_year <- lumps_data %>%
  group_by(MDD_year) %>%
  summarise(lumps_count = n())

splits_per_year <- splits_data %>%
  group_by(MDD_year) %>%
  summarise(splits_count = n())

# Join this sequence with the datasets and fill missing values with 0
names_per_year <- complete_years %>%
  left_join(available_names_per_year_data, by = "MDD_year") %>%
  mutate(total_names = coalesce(total_names, 0))

species_per_year <- complete_years %>%
  left_join(species_per_year_data, by = "MDD_year") %>%
  mutate(species_per_year = coalesce(species_per_year, 0))

valid_species_per_year <- complete_years %>%
  left_join(valid_species_per_year_data, by = "MDD_year") %>%
  mutate(valid_species_totals = coalesce(valid_species_totals, 0))

lumps_per_year <- complete_years %>%
  left_join(lumps_per_year, by = "MDD_year") %>%
  mutate(lumps_count = coalesce(lumps_count, 0))

splits_per_year <- complete_years %>%
  left_join(splits_per_year, by = "MDD_year") %>%
  mutate(splits_count = coalesce(splits_count, 0))

subspecies_per_year <- complete_years %>%
  left_join(subspecies_per_year, by = "MDD_year") %>%
  mutate(subspecies_count = coalesce(subspecies_count, 0))

# Now ensure the percent_species_per_year dataset uses the complete set of years
percent_species_per_year <- names_per_year %>%
  left_join(species_per_year, by = "MDD_year") %>%
  left_join(valid_species_per_year, by = "MDD_year") %>%
  left_join(lumps_per_year, by = "MDD_year") %>%
  left_join(splits_per_year, by = "MDD_year") %>%
  left_join(subspecies_per_year, by = "MDD_year") %>%
  mutate(
    lumps_count = coalesce(lumps_count, 0), 
    splits_count = coalesce(splits_count, 0),
    subspecies_count = coalesce(subspecies_count, 0),
    percent_species = (species_per_year / total_names) * 100,
    percent_lumps = (lumps_count / total_names) * 100,
    percent_splits = (splits_count / total_names) * 100,
    percent_subspecies = (subspecies_count / total_names) * 100
  )

# Adjust the rolling mean to ignore NA values and include all years up to 2024
percent_species_per_year <- percent_species_per_year %>%
  arrange(MDD_year) %>%
  mutate(
    smoothed_percent_species = rollapply(percent_species, width = 10, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "right", partial = FALSE),
    smoothed_percent_lumps = rollapply(percent_lumps, width = 10, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "right", partial = FALSE),
    smoothed_percent_splits = rollapply(percent_splits, width = 10, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "right", partial = FALSE),
    smoothed_percent_subspecies = rollapply(percent_subspecies, width = 10, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, align = "right", partial = FALSE)
  )


# Fix the range to include the full x-axis (1750 to 2025)
x_axis_ticks <- seq(1750, 2025, by = 25)

# Data for publication year points
year_annotations <- data.frame(
  descriptor = c("Storer", "Morris", "C&H1", "MSW2", "MSW3", "MDD1", "MDD2"),
  total = c(15000, 4237, 4007, 4631, 5416, 6495, 6759), # Totals for recognized species
  year = c(1943, 1965, 1980, 1993, 2005, 2018, 2024)    # Corresponding years
)

# Calculate scaling factor
primary_max <- max(names_per_year$total_names)
secondary_max <- max(year_annotations$total)
scaling_factor <- primary_max / secondary_max# Adjust label positions for specific points

# Adjust label positions and formatting for specific points
year_annotations <- year_annotations %>%
  mutate(
    formatted_total = case_when(
      descriptor == "Storer" ~ paste0("~", comma(total)), # Add '~' for Storer
      TRUE ~ comma(total) # Add commas for other totals
    ),
    vjust = case_when(
      descriptor == "MDD2" ~ -1.5,  # Move MDD2 above
      descriptor == "Storer" ~ 0.5, # Align Storer vertically with the point
      TRUE ~ ifelse(year >= 2020, 1.5, -0.5) # Adjust others near edges
    ),
    hjust = case_when(
      descriptor == "Storer" ~ -0.2, # Move Storer slightly further to the right
      TRUE ~ 0.5 # Center others horizontally
    ),
    x_adjust = case_when(
      descriptor == "MDD1" ~ -1.5,  # Move MDD1 slightly to the left
      TRUE ~ 0  # No adjustment for other labels
    )
  )

# Adjust the histogram plot to include dynamically placed labels
histogram_plot <- ggplot() +
  geom_histogram(data = synonym_since_1758_data, aes(x = MDD_year, fill = "Available Names"), 
                 color = "black", binwidth = 1, alpha = 0.6) +
  geom_histogram(data = species_year_data, aes(x = MDD_year, fill = "Currently Valid Species"), 
                 color = "black", binwidth = 1, alpha = 0.6) +
  geom_point(data = year_annotations, aes(x = year, y = total * scaling_factor), color = "red", size = 3) + # Points scaled
  geom_text(data = year_annotations, 
            aes(x = year + x_adjust, y = (total * scaling_factor) + 10, 
                label = paste0(descriptor, "\n", formatted_total), vjust = vjust, hjust = hjust),
            color = "black", size = 4, fontface = "bold") + # Labels bold, formatted, and larger
  scale_x_continuous(limits = c(1750, 2025), expand = c(0, 0)) + # Align with x-axis
  scale_y_continuous(
    name = "Total Names Described", 
    breaks = seq(0, primary_max, by = 100),
    sec.axis = sec_axis(~ . / scaling_factor, 
                        name = "Recognized Species Estimates") # Secondary y-axis styled
  ) +
  scale_fill_manual(values = c("Available Names" = "gray70", "Currently Valid Species" = "gray30"),
                    breaks = c("Available Names", "Currently Valid Species")) +
  guides(fill = guide_legend(title = NULL)) + # Remove 'fill' title
  coord_cartesian(clip = "off") + # Prevent clipping of labels
  theme_minimal(base_size = 15 * 1.1) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),  # Remove default x-axis text
    axis.ticks.x = element_blank(), # Remove default x-axis ticks
    axis.title.y = element_text(color = "black"),  # Y-axis label remains black
    axis.text.y = element_text(color = "black"),   # Y-axis text remains black
    axis.title.y.right = element_text(size = 15 * 1.1, color = "black", hjust = 0.5), # Styled secondary y-axis
    legend.position = c(0.02, 0.98), # Move legend to top-left inside the plot
    legend.justification = c(0, 1),  # Anchor legend to top-left corner
    legend.direction = "vertical",   # Stack legend items vertically
    legend.spacing.y = unit(1.0, "cm"), # Increased spacing between legend items
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA), # Translucent legend background
    plot.margin = margin(t = 10, r = 20, b = 5, l = 10) # Adjust margins for right-side text
  )

# Adjust the line plot legend and line width in the graph
line_plot <- ggplot(data = percent_species_per_year) +
  # Actual lines with doubled width
  geom_line(aes(x = MDD_year, y = smoothed_percent_species, color = "Names Described as Species"), size = 2) +
  geom_line(aes(x = MDD_year, y = smoothed_percent_lumps, color = "Species Lumped Since Description"), size = 2) +
  geom_line(aes(x = MDD_year, y = smoothed_percent_splits, color = "Species Split Since Description"), size = 2) +
  geom_line(aes(x = MDD_year, y = smoothed_percent_subspecies, color = "Names Described as Subspecies"), size = 2) +
  
  # Adjust scales and legends
  scale_x_continuous(limits = c(1750, 2025), expand = c(0, 0)) + # Align with x-axis
  scale_y_continuous(name = "Percentage of Names\n(10-Year Running Means)") +
  scale_color_manual(
    values = c(
      "Names Described as Species" = "#0072b2",
      "Names Described as Subspecies" = "#d55e00",
      "Species Split Since Description" = "#009e73",
      "Species Lumped Since Description" = "#cc79a7"
    )
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        size = 2  # Match legend line width to the plot lines
      )
    )
  ) +
  theme_minimal(base_size = 15 * 1.1) +
  theme(
    axis.title.x = element_blank(), # Remove x-axis for now
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(color = "black"),  # Y-axis label remains black
    axis.text.y = element_text(color = "black"),   # Y-axis text remains black
    legend.position = "bottom",    # Legend at the bottom
    legend.title = element_blank(),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10) # Adjust margins
  )

x_axis_plot <- ggplot(data = percent_species_per_year, aes(x = MDD_year)) +
  # Simple median line
  geom_segment(aes(x = 1750, xend = 2025, y = 0, yend = 0), 
               color = "black", size = 0.8) +
  # Tick marks
  geom_segment(aes(x = x, xend = x, y = 0.03, yend = -0.03),
               color = "black", size = 0.5, 
               data = data.frame(x = seq(1750, 2025, by = 25))) +
  # Alternating labels above and below ticks
  geom_text(aes(x = x, y = ifelse(x %% 50 == 0, 0.05, -0.05), label = x),
            size = 3.5, vjust = ifelse(seq(1750, 2025, by = 25) %% 50 == 0, 0, 1),
            color = "black",
            data = data.frame(x = seq(1750, 2025, by = 25))) +
  scale_x_continuous(limits = c(1750, 2025), expand = c(0, 0)) + # Ensure alignment
  coord_cartesian(xlim = c(1750, 2025), clip = "off") + # Clip to bounds
  theme_minimal(base_size = 15 * 1.1) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.text.x = element_blank(),   # Remove default x-axis text
    axis.ticks.x = element_blank(),  # Remove default x-axis ticks
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    panel.grid = element_blank(),    # Remove grid lines
    panel.background = element_rect(fill = alpha("white", 0.3), color = NA), # Translucent background
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10) # Adjust spacing
  )

# Combine the plots using patchwork
names_over_time_plot <- histogram_plot / x_axis_plot / line_plot +
  plot_layout(heights = c(2, 0.15, 1)) # Adjust height ratios

# Save the combined plot
ggsave("graphs\\names_over_time_graph.jpeg", 
       plot = names_over_time_plot, 
       device = "jpeg", 
       width = 14, 
       height = 10, 
       units = "in", 
       dpi = 900)

# Final summary dataframe with raw totals and smoothed values
names_per_year_summary_df <- percent_species_per_year %>%
  mutate(
    valid_species_totals = valid_species_totals,  # Based on 'MDD_validity == species'
    available_name_totals = total_names,          # Total available names
    species_per_year = species_per_year,          # Based on 'MDD_original_rank == species'
    lumps_per_year = lumps_count,                 # Add raw lumps count
    splits_per_year = splits_count,               # Add raw splits count
    subspecies_per_year = subspecies_count,       # Add raw subspecies count
    percent_species_per_year = percent_species,
    percent_lumps_per_year = percent_lumps,
    percent_subspecies_per_year = percent_subspecies,
    percent_splits_per_year = percent_splits,
    smoothed_10_year_percent_species = smoothed_percent_species,
    smoothed_10_year_percent_lumps = smoothed_percent_lumps,
    smoothed_10_year_percent_splits = smoothed_percent_splits,
    smoothed_10_year_percent_subspecies = smoothed_percent_subspecies
  ) %>%
  rename(year = MDD_year) %>%
  select(year, valid_species_totals, available_name_totals,
         species_per_year, percent_species_per_year, smoothed_10_year_percent_species,
         lumps_per_year, percent_lumps_per_year, smoothed_10_year_percent_lumps,
         splits_per_year, percent_splits_per_year, smoothed_10_year_percent_splits,
         subspecies_per_year, percent_subspecies_per_year, smoothed_10_year_percent_subspecies)

# Save the updated summary dataframe as CSV
write.csv(names_per_year_summary_df, "supplementary_files/names_per_year_summary.csv", row.names = FALSE)


######Species by Major Region Graphs######

# Making a graph of species and available names per continent and biorealm
# Need to have the geography_summary_df loaded 
# A summary table is also made near the end of this section (Table 3)

# Load packages
#library(ggplot2)
#library(dplyr)

# Separate region_category by '|' and create a long format dataframe for easier parsing
geography_summary_long <- geography_summary_df %>%
  separate_rows(region_category, sep = "\\|") %>%
  mutate(split_species_since_MSW3 = new_since_MSW3 - new_descriptions_since_MSW3)  # Correct calculation for split species

# Summarize for Biogeographic Realms
biogeographic_realm_summary <- geography_summary_long %>%
  filter(region_category == "Biogeographic Realm") %>%
  group_by(region_name) %>%
  summarise(
    species_count = sum(species_count, na.rm = TRUE),
    available_names = sum(available_names, na.rm = TRUE),
    new_descriptions = sum(new_descriptions_since_MSW3, na.rm = TRUE),
    split_species = sum(split_species_since_MSW3, na.rm = TRUE)
  )

# Summarize for Continents
continent_summary <- geography_summary_long %>%
  filter(region_category == "Continent") %>%
  group_by(region_name) %>%
  summarise(
    species_count = sum(species_count, na.rm = TRUE),
    available_names = sum(available_names, na.rm = TRUE),
    new_descriptions = sum(new_descriptions_since_MSW3, na.rm = TRUE),
    split_species = sum(split_species_since_MSW3, na.rm = TRUE)
  )

# Land area for biogeographic realms (in million km^2), based on data sourced from Wikipedia (there sourced from WWF), except Antarctic, which I used the area of Antarctica for
biogeographic_realm_areas <- c(
  "Palearctic" = 54.1,
  "Nearctic" = 22.9,
  "Afrotropic" = 22.1,
  "Neotropic" = 19.0,
  "Australasia" = 7.6,
  "Indomalaya" = 7.5,
  "Oceania (Biorealm)" = 1.0,
  "Antarctic" = 14.2
)

# Land area for continents (in million km^2), based on data sourced from Wikipedia (sourced themselves from Encylcopedia Britanica)
continent_areas <- c(
  "Asia" = 44.6,
  "Africa" = 30.4,
  "North America" = 24.2,
  "South America" = 17.8,
  "Antarctica" = 14.2,
  "Europe" = 10.0,
  "Oceania (Continent)" = 8.5
)

# Add species density to the biogeographic realm summary
biogeographic_realm_summary <- biogeographic_realm_summary %>%
  mutate(area = biogeographic_realm_areas[region_name],  # Match area for each region
         species_density = species_count / area)  # Calculate species density

# Add species density to the continent summary
continent_summary <- continent_summary %>%
  mutate(area = continent_areas[region_name],  # Match area for each continent
         species_density = species_count / area)  # Calculate species density

# Compute max species_density and species_count values for scaling
max_species_count_continent <- max(continent_summary$species_count)

# Adjust the scaling so that species density aligns with species count, and ensure the desired axis limits (0-150)
scaling_factor_continent <- max_species_count_continent / 150

# Calculate scaling_factor based on max species_density and species_count
max_species_density <- max(biogeographic_realm_summary$species_density, na.rm = TRUE)
max_species_count <- max(biogeographic_realm_summary$species_count, na.rm = TRUE)
scaling_factor <- max_species_count / max_species_density

# Plot for Species Count by Biogeographic Realm
species_biorealm_plot <- ggplot(biogeographic_realm_summary, aes(x = reorder(region_name, -species_count))) +
  geom_bar(aes(y = species_count, fill = "Total Species"), stat = "identity", color = "black", width = 0.7, position = "stack") +
  geom_bar(aes(y = split_species + new_descriptions, fill = "Split Species"), stat = "identity", color = "black", width = 0.7, position = "stack") +
  geom_bar(aes(y = new_descriptions, fill = "New Species Descriptions"), stat = "identity", color = "black", width = 0.7, position = "stack") +
  geom_text(aes(y = species_count, label = species_count), vjust = -0.5, size = 5) +
  geom_point(aes(y = species_density * scaling_factor), color = "red", size = 5) +
  scale_y_continuous(
    name = "Species Count",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Species Density (per million km^2)", 
                        breaks = seq(0, 150, by = 30))
  ) +
  labs(x = "Terrestrial Biogeographic Realm") +
  scale_fill_manual(values = c("Total Species" = "lightgray", 
                               "Split Species" = "darkgray", 
                               "New Species Descriptions" = "black"),
                    breaks = c("Total Species", "Split Species", "New Species Descriptions")) +
  guides(fill = guide_legend(direction = "vertical")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key.size = unit(2, "lines"),
        legend.title = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0), color = NA),
        legend.box.background = element_rect(fill = alpha("white", 0), color = NA),
        plot.title = element_blank())

# Plot for Species Count by Continent with Fixed Secondary Axis
species_continent_plot <- ggplot(continent_summary, aes(x = reorder(region_name, -species_count))) +
  geom_bar(aes(y = species_count, fill = "Total Species"), stat = "identity", color = "black", width = 0.7) +
  geom_bar(aes(y = split_species + new_descriptions, fill = "Split Species"), stat = "identity", color = "black", width = 0.7) +
  geom_bar(aes(y = new_descriptions, fill = "New Species Descriptions"), stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(y = species_count, label = species_count), vjust = -0.5, size = 5) +
  geom_point(aes(y = species_density * scaling_factor_continent), color = "red", size = 5) +
  scale_y_continuous(
    name = "Species Count",
    sec.axis = sec_axis(~ . / scaling_factor_continent, name = "Species Density (per million km^2)", 
                        breaks = seq(0, 150, by = 30))
  ) +
  labs(x = "Continent") +
  scale_fill_manual(values = c("Total Species" = "lightgray", 
                               "Split Species" = "darkgray", 
                               "New Species Descriptions" = "black"),
                    breaks = c("Total Species", "Split Species", "New Species Descriptions")) +
  guides(fill = guide_legend(direction = "vertical")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key.size = unit(2, "lines"),
        legend.title = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0), color = NA),
        legend.box.background = element_rect(fill = alpha("white", 0), color = NA),
        plot.title = element_blank())

# Plot for Available Names by Biogeographic Realm
name_biorealm_plot <- ggplot(biogeographic_realm_summary, aes(x = reorder(region_name, -available_names), y = available_names)) +
  geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.7) +  
  geom_text(aes(label = available_names), vjust = -0.5, size = 5) +
  labs(x = "Terrestrial Biogeographic Realm", y = "Available Names") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_blank())

# Plot for Available Names by Continent
name_continent_plot <- ggplot(continent_summary, aes(x = reorder(region_name, -available_names), y = available_names)) +
  geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.7) +  
  geom_text(aes(label = available_names), vjust = -0.5, size = 5) +
  labs(x = "Continent", y = "Available Names") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.title = element_blank())

# Load the graphs
species_biorealm_plot
species_continent_plot
name_biorealm_plot
name_continent_plot

# Save the graphs as JPEG files
ggsave("graphs/species_biorealm_plot.jpeg", 
       plot = species_biorealm_plot,
       device = "jpeg", 
       width = 14, 
       height = 8, 
       units = "in", 
       dpi = 900)

ggsave("graphs/species_continent_plot.jpeg", 
       plot = species_continent_plot,
       device = "jpeg", 
       width = 14, 
       height = 8, 
       units = "in", 
       dpi = 900)

ggsave("graphs/name_biorealm_plot.jpeg", 
       plot = name_biorealm_plot,
       device = "jpeg", 
       width = 14, 
       height = 7, 
       units = "in", 
       dpi = 900)

ggsave("graphs/name_continent_plot.jpeg", 
       plot = name_continent_plot,
       device = "jpeg", 
       width = 14, 
       height = 7, 
       units = "in", 
       dpi = 900)

# Combining the four graphs into a single figure.

#library(patchwork)

# Modify the top-left graph (species_biorealm_plot)
species_biorealm_plot_modified <- species_biorealm_plot + 
  scale_y_continuous(
    name = "Species Count",
    sec.axis = sec_axis(~ . / scaling_factor, name = NULL)
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y.right = element_blank(),
    legend.position = "none"
  )

# Modify the top-right graph (species_continent_plot) to display only the right-side y-axis
species_continent_plot_modified <- species_continent_plot + 
  scale_y_continuous(
    name = NULL,
    sec.axis = sec_axis(~ . / scaling_factor_continent, name = "Species Density (per million km^2)")  # Add right-side y-axis label
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_text(size = 16),
    axis.ticks.y.left = element_line(),
    axis.text.y.right = element_text(size = 16),
    axis.ticks.y.right = element_line()
  )

# Modify the bottom-left graph to include both x and y axes
name_biorealm_plot_modified <- name_biorealm_plot + 
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

# Modify the bottom-right graph to keep only the x-axis label
name_continent_plot_modified <- name_continent_plot + 
  theme(
    axis.title.y = element_blank()
  )

# Combine the 2x2 arrangement with labels A), B), C), D)
labeled_combined_plot <- (species_biorealm_plot_modified | species_continent_plot_modified) / 
  (name_biorealm_plot_modified | name_continent_plot_modified)

# Adjust tag size and add the labels A, B, C, D with larger size
labeled_combined_plot <- labeled_combined_plot + 
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 24, face = "bold")
  )

# Save the labeled combined plot as a single JPEG file
ggsave("graphs/combined_region_plots.jpeg", 
       plot = labeled_combined_plot,
       device = "jpeg", 
       width = 14, 
       height = 16,
       units = "in", 
       dpi = 900)

# Creating a table to summarize the data presented in the above graphs

#library(dplyr)
#library(gt)

# Combine Continent and Biogeographic Realm summaries into one table
combined_summary_table <- bind_rows(
  continent_summary %>%
    mutate(region_type = "Continent"),  # Add a column to indicate 'Continent'
  biogeographic_realm_summary %>%
    mutate(region_type = "Biogeographic Realm")  # Add a column to indicate 'Biogeographic Realm'
)

# Reorder columns to have a cleaner layout
combined_summary_table <- combined_summary_table %>%
  select(region_type, region_name, species_count, available_names, new_descriptions, split_species, area, species_density)

# Arrange the rows so that Continents come first, followed by Biogeographic Realms
combined_summary_table <- combined_summary_table %>%
  arrange(region_type, desc(species_count))

# Rename the columns to be more publication-friendly
colnames(combined_summary_table) <- c("Region Type", "Region Name", "Total Species", "Available Names", 
                                      "New Species Descriptions", "Split Species", "Land Area (million km²)", 
                                      "Species Density (per million km²)")

# Create the gt table with region type as a header row
combined_summary_gt <- combined_summary_table %>%
  gt(groupname_col = "Region Type") %>%  # Use 'Region Type' as a header row
  fmt_number(
    columns = c(`Land Area (million km²)`, `Species Density (per million km²)`),
    decimals = 2
  ) %>% 
  fmt_number(
    columns = c(`Total Species`, `Available Names`, `New Species Descriptions`, `Split Species`),
    decimals = 0
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_body(columns = c("Region Name"))
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    table.font.size = 12
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_width(
    everything() ~ px(120)
  ) %>%
  tab_footnote(
    footnote = "Species Density calculated as Total Species / Land Area",
    locations = cells_column_labels(c(`Species Density (per million km²)`))
  ) %>%
  tab_footnote(
    footnote = "Total Species includes recently extinct species but not domesticated or introduced species",
    locations = cells_column_labels(c(`Total Species`))
  )

# Print the table
combined_summary_gt

# Saving the table as an HTML
gtsave(combined_summary_gt, "tables/region_summary_table.html")


######Mapping Setup######

# Setting up the world map to be used for all map figures
# These maps require the various supplemental tables to be loaded

# Load packages
#library(rnaturalearth)
#library(rnaturalearthdata)
#library(sf)
#library(dplyr)
#library(ggplot2)
#library(grid)

# Load world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Separating French Guiana from France in the world map
# Create a point representing the location of French Guiana
french_guiana_point <- st_sfc(st_point(c(-53.1258, 4.8604)), crs = st_crs(world_map))

# Perform a spatial join to find the polygon containing this point
france_geom <- world_map %>%
  filter(name_long == "France") %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON")

french_guiana_geom <- france_geom[st_intersects(france_geom, french_guiana_point, sparse = FALSE), ]

# Create a new entry for French Guiana in the world map data
french_guiana_entry <- world_map %>%
  filter(name_long == "France") %>%
  slice(1) %>%
  mutate(
    name_long = "French Guiana",
    iso_a3 = "GUF",
    geometry = st_geometry(french_guiana_geom)
  )

# Remove French Guiana from the France entry
france_without_french_guiana <- france_geom[!st_intersects(france_geom, french_guiana_point, sparse = FALSE), ] %>%
  st_union() %>%
  st_sf(data.frame(name_long = "France", iso_a3 = "FRA", geometry = .))

# Combine Somaliland with Somalia in the world map
somalia_geom <- world_map %>%
  filter(name_long %in% c("Somalia", "Somaliland")) %>%
  st_union() %>%
  st_sf(data.frame(name_long = "Somalia", iso_a3 = "SOM", geometry = .))

# Combine Northern Cyprus with Cyprus in the world map
cyprus_geom <- world_map %>%
  filter(name_long %in% c("Cyprus", "Northern Cyprus")) %>%
  st_union() %>%
  st_sf(data.frame(name_long = "Cyprus", iso_a3 = "CYP", geometry = .))

# Combine Western Sahara with Morocco in the world map
morocco_geom <- world_map %>%
  filter(name_long %in% c("Morocco", "Western Sahara")) %>%
  st_union() %>%
  st_sf(data.frame(name_long = "Morocco", iso_a3 = "MAR", geometry = .))

# Update the world map
world_map_updated <- world_map %>%
  filter(!name_long %in% c("France", "Somaliland", "Somalia", "Cyprus", "Northern Cyprus", "Morocco", "Western Sahara")) %>%
  bind_rows(france_without_french_guiana, french_guiana_entry, somalia_geom, cyprus_geom, morocco_geom)


######General Map Figures######

# Creating a set of general maps in the same format for a full page map figure 
# of maps, including total species, new species since MSW3, threatened species, 
# understudied species (data deficient and not evaluated on IUCN), and available names

# Load packages
# library(rnaturalearth)
# library(sf)
# library(dplyr)
# library(ggplot2)
# library(grid)

# Create the mapping_data dataframe by joining the geographic_metadata and geography_summary_df
mapping_data <- geography_summary_df %>%
  left_join(
    geographic_metadata %>%
      select(region_name = Region, iso3_code = `ISSO_3166-1_A-3_Code`) %>%
      mutate(iso3_upper = toupper(iso3_code)),
    by = "region_name"
  )

# Match the data using ISO3 codes
world_with_data <- world_map_updated %>%
  left_join(mapping_data, by = c("iso_a3" = "iso3_upper"))

# Identify unmatched regions
unmatched_data <- mapping_data %>%
  filter(!iso3_upper %in% world_with_data$iso_a3)

# Match unmatched regions using country names
world_with_data <- world_with_data %>%
  left_join(
    unmatched_data %>%
      select(region_name, species_count, new_since_MSW3, IUCN_VU_species, IUCN_EN_species, IUCN_CR_species, IUCN_EW_species, IUCN_DD_species, IUCN_NE_species, available_names, endemic_species),
    by = c("name_long" = "region_name"),
    suffix = c("", ".name")
  ) %>%
  mutate(
    species_count = coalesce(species_count, species_count.name),
    new_since_MSW3 = coalesce(new_since_MSW3, 0),
    threatened_species = IUCN_VU_species + IUCN_EN_species + IUCN_CR_species + IUCN_EW_species,
    data_deficient_species = IUCN_DD_species + IUCN_NE_species,
    available_names = coalesce(available_names, 0),
    endemic_species = coalesce(endemic_species, 0),
    threatened_percent = 100 * threatened_species / species_count,
    data_deficient_percent = 100 * data_deficient_species / species_count,
    endemic_percent = 100 * endemic_species / species_count
  ) %>%
  select(-species_count.name)

# Function to create individual maps with the original formatting
create_map <- function(map_data, fill_var, fill_label, low_color, high_color, show_top_10_list = TRUE) {
  top_10_countries <- map_data %>%
    arrange(desc(!!sym(fill_var))) %>%
    slice_head(n = 10) %>%
    mutate(rank = row_number(),
           name_label = ifelse(name_long == "Democratic Republic of the Congo", "DR Congo", name_long))
  
  country_labels <- top_10_countries %>%
    arrange(rank) %>%
    mutate(label = paste0(rank, ". ", !!sym(fill_var), " ", name_label))
  
  label_grob <- textGrob(
    label = paste(country_labels$label, collapse = "\n"),
    x = unit(0.15, "npc"),
    y = unit(0.25, "npc"),
    just = "left", gp = gpar(fontsize = 12, fontface = "bold", col = "black")
  )
  
  map_plot <- ggplot(map_data) +
    geom_sf(aes_string(fill = fill_var), color = "black") +
    scale_fill_gradient(low = low_color, high = high_color, na.value = "gray90") +
    coord_sf(crs = "+proj=robin", expand = FALSE) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray30", size = 0.5, linetype = "dashed"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 24),
      legend.key.width = unit(15, "cm"),
      legend.key.height = unit(0.5, "cm"),
      plot.margin = margin(15, 15, 15, 15),
      legend.box = "horizontal",
      legend.margin = margin(t = -10),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    guides(
      fill = guide_colorbar(
        title = fill_label,
        title.position = "top",
        title.hjust = 0,
        title.theme = element_text(size = 36, face = "bold"),
        barwidth = unit(35, "cm"),
        barheight = unit(1.5, "cm")
      )
    )
  
  if (show_top_10_list) {
    map_plot <- map_plot + annotation_custom(label_grob)
  }
  
  return(map_plot)
}

# Create and display each map
map_total_species <- create_map(world_with_data, "species_count", "Total Species", "beige", "dodgerblue3", show_top_10_list = FALSE)
map_new_species <- create_map(world_with_data, "new_since_MSW3", "Newly Recognized Species Since MSW3", "beige", "dodgerblue3", show_top_10_list = FALSE)
map_threatened_species <- create_map(world_with_data, "threatened_percent", "Percentage of Threatened Species (VU, EN, CR, EW)", "beige", "dodgerblue3", show_top_10_list = FALSE)
map_data_deficient <- create_map(world_with_data, "data_deficient_percent", "Percentage of Understudied Species (DD, NE)", "beige", "dodgerblue3", show_top_10_list = FALSE)
map_endemic_species <- create_map(world_with_data, "endemic_percent", "Percentage of Endemic Species", "beige", "dodgerblue3", show_top_10_list = FALSE)
map_available_names <- create_map(world_with_data, "available_names", "Available Name Type Localities", "beige", "dodgerblue3", show_top_10_list = FALSE)

# Print the individual maps
print(map_total_species)
print(map_new_species)
print(map_threatened_species)
print(map_data_deficient)
print(map_available_names)
print(map_endemic_species)

# Saving the maps as JPEG files
ggsave(
  filename = paste0("map_figures\\total_species_map.jpg"),
  plot = map_total_species,
  device = "jpeg",
  width = 15, height = 10, units = "in",
  dpi = 300
)

ggsave(
  filename = paste0("map_figures\\new_species_MSW3_map.jpg"),
  plot = map_new_species,
  device = "jpeg",
  width = 15, height = 10, units = "in",
  dpi = 300
)

ggsave(
  filename = paste0("map_figures\\threatened_species_map.jpg"),
  plot = map_threatened_species,
  device = "jpeg",
  width = 15, height = 10, units = "in",
  dpi = 300
)

ggsave(
  filename = paste0("map_figures\\understudied_species_map.jpg"),
  plot = map_data_deficient,
  device = "jpeg",
  width = 15, height = 10, units = "in",
  dpi = 300
)

ggsave(
  filename = paste0("map_figures\\endemic_species_map.jpg"),
  plot = map_endemic_species,
  device = "jpeg",
  width = 15, height = 10, units = "in",  # Set the dimensions
  dpi = 300  # Set the resolution
)

ggsave(
  filename = paste0("map_figures\\available_names_map.jpg"),
  plot = map_available_names,
  device = "jpeg",
  width = 15, height = 10, units = "in",
  dpi = 300
)

# Creating a figure with all 6 maps together.

# Load patchwork for combining plots
library(patchwork)

# Adjust the individual map plots and apply changes
map_total_species_modified <- map_total_species + 
  theme(
    legend.position = "bottom",
    legend.key.width = unit(10, "cm"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 32)
  )

map_new_species_modified <- map_new_species + 
  theme(
    legend.position = "bottom",
    legend.key.width = unit(10, "cm"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 32)
  )

map_available_names_modified <- map_available_names + 
  theme(
    legend.position = "bottom",
    legend.key.width = unit(10, "cm"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 32)
  )

map_threatened_species_modified <- map_threatened_species + 
  theme(
    legend.position = "bottom",
    legend.key.width = unit(10, "cm"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 32)
  )

map_data_deficient_modified <- map_data_deficient + 
  theme(
    legend.position = "bottom",
    legend.key.width = unit(10, "cm"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 32)
  )

map_endemic_species_modified <- map_endemic_species + 
  theme(
    legend.position = "bottom",
    legend.key.width = unit(10, "cm"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 32)
  )

# Combine the six maps into a single figure with 3 rows and 2 columns
# Order: total species, new species, available names, threatened species, data deficient, endemic species
combined_maps <- (map_total_species_modified + map_new_species_modified) / 
  (map_available_names_modified + map_threatened_species_modified) / 
  (map_data_deficient_modified + map_endemic_species_modified)

# Adjust the layout and labels, and position labels in the top-left corner, but slightly to the right by 1/16
# Using `tag_position` to control the exact label position
combined_maps <- combined_maps + 
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.tag = element_text(size = 48, face = "bold"),
    plot.tag.position = c(0.06, 0.95),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10)
  )

# Save the combined figure as a single JPEG file
ggsave(
  filename = paste0("map_figures\\combined_maps_figure.jpg"),
  plot = combined_maps,
  device = "jpeg",
  width = 40, height = 30, units = "in",
  dpi = 600
)


######New Since 2000 Type Locality Map######

# Making a map with the new names described point data 
# overlaying a gradient map of valid species described since 2000

# Load packages
#library(rnaturalearth)
#library(sf)
#library(dplyr)
#library(ggplot2)
#library(grid)

# Filter the synonyms_df dataframe for valid species described since 2000
new_species_since_2000 <- synonyms_df_cleaned %>%
  filter(MDD_validity == "species" & MDD_year >= 2000)

# Filter for synonyms or subspecies described after 2000
non_species_since_2000 <- synonyms_df_cleaned %>%
  filter(MDD_validity != "species" & MDD_year >= 2000)

# Summarize the MDD_type_country column and get ISO3 codes for matching
new_2000_country_summary <- new_species_since_2000 %>%
  group_by(MDD_type_country) %>%
  summarise(new_species_count = n()) %>%
  rename(region_name = MDD_type_country) %>%
  left_join(
    geographic_metadata %>%
      select(region_name = Region, iso3_code = `ISSO_3166-1_A-3_Code`) %>%
      mutate(iso3_upper = toupper(iso3_code)),
    by = "region_name"
  )

# Match the summarized data to the world map using ISO3 codes first
world_with_new_species <- world_map_updated %>%
  left_join(new_2000_country_summary, by = c("iso_a3" = "iso3_upper"))

# For any unmatched countries, attempt to match by country name using name_long
unmatched_countries <- new_2000_country_summary %>%
  filter(!iso3_upper %in% world_with_new_species$iso_a3)

world_with_new_species <- world_with_new_species %>%
  left_join(
    unmatched_countries %>%
      select(region_name, new_species_count),
    by = c("name_long" = "region_name"),
    suffix = c("", ".name")
  ) %>%
  mutate(new_species_count = coalesce(new_species_count, new_species_count.name)) %>%
  select(-new_species_count.name)

# Extract latitude and longitude points for species-level type localities
species_type_locality_points <- new_species_since_2000 %>%
  mutate(
    MDD_type_latitude = as.numeric(MDD_type_latitude),
    MDD_type_longitude = as.numeric(MDD_type_longitude),
    shape_label = "Valid Species"
  ) %>%
  filter(!is.na(MDD_type_latitude) & !is.na(MDD_type_longitude)) %>%  # Remove rows with NA values
  filter(between(MDD_type_latitude, -90, 90) & between(MDD_type_longitude, -180, 180)) %>%  # Keep only valid coordinate ranges
  st_as_sf(coords = c("MDD_type_longitude", "MDD_type_latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(world_with_new_species))

# Extract latitude and longitude points for non-species-level type localities
non_species_type_locality_points <- non_species_since_2000 %>%
  mutate(
    MDD_type_latitude = as.numeric(MDD_type_latitude),
    MDD_type_longitude = as.numeric(MDD_type_longitude),
    shape_label = "Synonyms/Subspecies"
  ) %>%
  filter(!is.na(MDD_type_latitude) & !is.na(MDD_type_longitude)) %>%  # Remove rows with NA values
  filter(between(MDD_type_latitude, -90, 90) & between(MDD_type_longitude, -180, 180)) %>%  # Keep only valid coordinate ranges
  st_as_sf(coords = c("MDD_type_longitude", "MDD_type_latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(world_with_new_species))

# Combine the point data into a single dataset to ensure consistent shapes
combined_points <- bind_rows(
  species_type_locality_points,
  non_species_type_locality_points
)

# Create the base map with the gradient scale legend positioned below the map
map_base <- ggplot(world_with_new_species) +
  # Fill layer for all countries
  geom_sf(aes(fill = new_species_count), color = "black") +
  # Add points for non-species-level type localities (plotted first)
  geom_sf(data = non_species_type_locality_points, color = "blue", size = 1.7, alpha = 1, shape = 18) +
  # Add points for species-level type localities (plotted last to overlay)
  geom_sf(data = species_type_locality_points, color = "red", size = 1.7, alpha = 1.5, shape = 19) +
  scale_fill_gradient(low = "gray85", high = "gray30", na.value = "white") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray40", size = 0.5, linetype = "dashed"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.key.width = unit(15, "cm"),
    legend.key.height = unit(0.75, "cm"),
    plot.margin = margin(15, 15, 15, 15),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      title = "New Currently Valid Species Described Since 2000",
      title.position = "top",
      title.hjust = 0,
      title.theme = element_text(size = 18, face = "bold"),
      barwidth = unit(35, "cm"),
      barheight = unit(0.75, "cm")
    )
  )

# Create the point legend separately using grobs with an aligned white background
point_legend_grob <- grobTree(
  # Add a white rectangle as the background, shifted slightly up and to the right
  rectGrob(
    x = unit(0.44, "npc"), y = unit(0.30, "npc"),  # Adjusted x and y for fine-tuning
    width = unit(0.8, "npc"), height = unit(0.4, "npc"),  # Keep dimensions the same
    gp = gpar(fill = "white", col = "black", lwd = 0.5)  # Optional black border for clarity
  ),
  # Add the legend text and points
  textGrob("Type Localities", x = unit(0.1, "npc"), y = unit(0.4, "npc"), hjust = 0, gp = gpar(fontsize = 18, fontface = "bold")),
  pointsGrob(x = unit(0.1, "npc"), y = unit(0.3, "npc"), pch = 19, gp = gpar(col = "red", fontsize = 14)),
  textGrob("Valid Species", x = unit(0.2, "npc"), y = unit(0.3, "npc"), hjust = 0, gp = gpar(fontsize = 14)),
  pointsGrob(x = unit(0.1, "npc"), y = unit(0.2, "npc"), pch = 18, gp = gpar(col = "blue", fontsize = 14)),
  textGrob("Synonyms/Subspecies", x = unit(0.2, "npc"), y = unit(0.2, "npc"), hjust = 0, gp = gpar(fontsize = 14))
)

# Combine the map and point legend using annotation_custom
type_locality_map <- map_base +
  annotation_custom(point_legend_grob, xmin = -14000000, xmax = -6000000, ymin = -1000000, ymax = -6000000)

# Print the final type locality map
print(type_locality_map)

# Saving the type locality map as a JPEG file
ggsave(
  filename = paste0("map_figures\\type_locality_map.jpg"),
  plot = type_locality_map,
  device = "jpeg",
  width = 15, height = 10, units = "in",
  dpi = 300
)

# Get top 5 countries with the most new species type localities
top_5_countries <- new_species_since_2000 %>%
  group_by(MDD_type_country) %>%
  summarise(new_species_count = n()) %>%
  arrange(desc(new_species_count)) %>%
  slice_head(n = 5)

# Print the result
print(top_5_countries)

######US States Species Diversity Map######

# Creating a map of species diversity by US state

# Load packages
library(usmap)
library(ggplot2)
library(dplyr)

# Create a copy of fips_info and modify the name for Georgia
fips_info_modified <- usmap::fips_info() %>%
  mutate(full = ifelse(full == "Georgia", "Georgia (United States)", full))

# Create a new dataframe for plotting by matching state names with abbreviations
plot_data <- geography_summary_df %>%
  mutate(state = fips_info_modified$abbr[match(region_name, fips_info_modified$full)]) %>%
  filter(!is.na(state))

# Plot the US state species totals map using usmap
map_us_states_species <- plot_usmap(data = plot_data, values = "species_count", regions = "states") +
  scale_fill_gradient(
    low = "beige", 
    high = "dodgerblue3", 
    name = "Total Species", 
    na.value = "grey80"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 16, face = "bold", hjust = 0),
    legend.text = element_text(size = 16),
    legend.key.width = unit(15, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.margin = margin(15, 15, 25, 15),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0,
      title.theme = element_text(size = 18, face = "bold"),
      barwidth = unit(35, "cm"),
      barheight = unit(1.5, "cm")
    )
  )

# Print the US state species map
map_us_states_species

# Saving the US state species map as a JPEG file
ggsave(
  filename = paste0("map_figures\\us_states_species_map.jpg"),
  plot = map_us_states_species,
  device = "jpeg",
  width = 15, height = 10, units = "in",
  dpi = 300
)
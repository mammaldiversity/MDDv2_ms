# This is a work space for all figures and supplementary tables
# Made by Connor J Burgin for the Mammal Diversity Database v2.0 manuscript
# Created in R version 4.3.2

# Set working directory (should be the only thing you need to alter to run script)
setwd("G:\\My Drive\\Projects\\MDD_v2.0_manuscript\\final_mddv2_stats_figures")


######Loading Packages and Base Data######

# Install and load packages for all sections

#install.packages(c('readxl','readr','dplyr','tidyr','stringr','purrr','gt','webshot2','rnaturalearth','sf','ggplot2','zoo','grid','scales','usmap'))
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(gt)
library(webshot2)
library(rnaturalearth)
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

#NOT FOR THIS PUB
#museums_metadata <- read_excel("base_files\\museums_metadata.xlsx")

# Saving all base files as CSV files to be included in the supplemental material
write.csv(species_df, "supplementary_files\\mdd_v2_species_sheet.csv", row.names = FALSE)
write.csv(synonyms_df, "supplementary_files\\mdd_v2_synonym_sheet.csv", row.names = FALSE)
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
    IUCN_NE_species = sum(str_detect(iucnStatus, "NE"))
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
synonyms_summary <- synonyms_df %>%
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
    IUCN_NE_species = sum(str_detect(iucnStatus, "NE"))
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
    IUCN_NE_species = sum(str_detect(iucnStatus, "NE"))
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
  select(rank, order, family, available_names, species_count, living_species, wild_species, extinct_species, domestic_species, genera, living_genera, extinct_genera, families, living_families, extinct_families, new_since_MSW3, new_descriptions_since_MSW3, split_since_MSW3, IUCN_LC_species, IUCN_NT_species, IUCN_VU_species, IUCN_EN_species, IUCN_CR_species, IUCN_EW_species, IUCN_EX_species)

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

# Load the species, synonym, and geographic metadata sheets
#species_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\MDD_v2.0_6759species.csv")
#synonyms_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\Species_Syn_v2.0.csv")
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

# Process subregionDistribution for US states
subregion_df <- species_df %>%
  filter(!is.na(subregionDistribution)) %>%
  mutate(states_list = str_extract(subregionDistribution, "(?<=USA\\().*(?=\\))")) %>%
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
  select(sciName, regions_clean, distribution_type, possible, extinct, diffSinceMSW3, authoritySpeciesYear, iucnStatus)

# Combine the other species distribution columns, keeping relevant columns for later summaries
expanded_species_distribution <- species_df %>%
  pivot_longer(cols = c(countryDistribution, continentDistribution, biogeographicRealm),
               names_to = "distribution_type", values_to = "regions") %>%
  filter(!is.na(regions)) %>%
  separate_rows(regions, sep = "\\|") %>%
  mutate(
    possible = str_detect(regions, "\\?$"),
    regions_clean = str_trim(str_remove(regions, "\\?$")),
    sciName = sciName,
    extinct = extinct,
    diffSinceMSW3 = diffSinceMSW3,
    authoritySpeciesYear = authoritySpeciesYear,
    iucnStatus = iucnStatus
  ) %>%
  select(sciName, regions_clean, distribution_type, possible, extinct, diffSinceMSW3, authoritySpeciesYear, iucnStatus)

# Add the processed subregion data to the expanded_species_distribution
expanded_species_distribution <- bind_rows(expanded_species_distribution, subregion_df)

# Identify and count endemic species per region
endemic_species_df <- expanded_species_distribution %>%
  filter(!possible) %>%
  group_by(sciName, distribution_type) %>%
  filter(n_distinct(regions_clean) == 1) %>%
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
    IUCN_NE_species = sum(str_detect(iucnStatus, "^NE"), na.rm = TRUE)
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

# Summarize available names for MDD_type_country
available_names_country <- synonyms_df %>%
  filter(MDD_nomenclature_status %in% c("available", "as_emended", "preoccupied", "nomen_novum", "partially_suppressed", "fully_suppressed")) %>%
  group_by(MDD_type_country) %>%
  summarise(available_names_country = n()) %>%
  rename(region_name = MDD_type_country)

# Summarize available names for MDD_type_subregion
available_names_subregion <- synonyms_df %>%
  filter(MDD_nomenclature_status %in% c("available", "as_emended", "preoccupied", "nomen_novum", "partially_suppressed", "fully_suppressed")) %>%
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
         IUCN_CR_species, IUCN_EW_species, IUCN_EX_species, IUCN_DD_species, IUCN_NE_species)

# Function to calculate and update available names for a specific biogeographic realm
update_available_names_for_realm <- function(realm_name, result_df, additional_filters = NULL) {
  # Filter the dataframe to include relevant regions for the specified realm
  filtered_df <- geography_summary_df %>%
    filter(
      (str_detect(region_category, "Country|Offshore Region|Continent Subregion") &
         higher_biogeographic_realm == realm_name) |
        (!is.null(additional_filters) & 
           (higher_country %in% additional_filters$country_values &
              higher_biogeographic_realm == realm_name))
    )
  
  # Sum the available names for these filtered regions
  available_names_sum <- sum(filtered_df$available_names, na.rm = TRUE)
  
  # Update the available_names for the specified biogeographic realm in the geography_summary_df
  geography_summary_df <- geography_summary_df %>%
    mutate(
      available_names = ifelse(
        region_category == "Biogeographic Realm" & region_name == realm_name,
        available_names_sum,
        available_names
      )
    )
  
  return(geography_summary_df)
}

# Calculate and upadate the available name counts for each biogeographic realm
# Afrotropic available name counts
geography_summary_df <- update_available_names_for_realm("Afrotropic", geography_summary_df)

# Antarctic available name counts
geography_summary_df <- update_available_names_for_realm("Antarctic", geography_summary_df)

# Australasia (including Indonesian subregions in Australasia) available name counts
geography_summary_df <- update_available_names_for_realm("Australasia", geography_summary_df,
                                              additional_filters = list(country_values = c("Indonesia")))

# Indomalaya (including Indonesian and Chinese subregions in Indomalaya) available name counts
geography_summary_df <- update_available_names_for_realm("Indomalaya", geography_summary_df,
                                              additional_filters = list(country_values = c("Indonesia", "China")))

# Nearctic (including Mexican subregions in the Nearctic) available name counts
geography_summary_df <- update_available_names_for_realm("Nearctic", geography_summary_df,
                                              additional_filters = list(country_values = c("Mexico")))

# Neotropic (including Mexican subregions in the Neotropics) available name counts
geography_summary_df <- update_available_names_for_realm("Neotropic", geography_summary_df,
                                              additional_filters = list(country_values = c("Mexico")))

# Oceania (Biorealm) available name counts
geography_summary_df <- update_available_names_for_realm("Oceania (Biorealm)", geography_summary_df)

# Palearctic (including Chinese subregions in the Palearctic) available name counts
geography_summary_df <- update_available_names_for_realm("Palearctic", geography_summary_df,
                                              additional_filters = list(country_values = c("China")))

# Function to calculate and update available names for each continent
update_available_names_for_continent <- function(continent_name, geography_summary_df, additional_filters = NULL) {
  # Filter the dataframe to include relevant regions for the specified continent
  filtered_df <- geography_summary_df %>%
    filter(
      (str_detect(region_category, "Country|Offshore Region|Continent Subregion") &
         higher_continent == continent_name) |
        (!is.null(additional_filters) & 
           (higher_country %in% additional_filters$country_values &
              higher_continent == continent_name))
    )
  
  # Sum the available names for these filtered regions
  available_names_sum <- sum(filtered_df$available_names, na.rm = TRUE)
  
  # Update the available_names for the specified continent in the geography_summary_df
  geography_summary_df <- geography_summary_df %>%
    mutate(
      available_names = ifelse(
        region_category == "Continent" & region_name == continent_name,
        available_names_sum,
        available_names
      )
    )
  
  return(geography_summary_df)
}

# Calculate and update the available names for each continent
# Africa available name counts
geography_summary_df <- update_available_names_for_continent("Africa", geography_summary_df)

# Antarctica available name counts
geography_summary_df <- update_available_names_for_continent("Antarctica", geography_summary_df)

# Asia (including Indonesian and Russian subregions in Asia and all of Turkey)
geography_summary_df <- update_available_names_for_continent("Asia", geography_summary_df,
                                                  additional_filters = list(country_values = c("Indonesia", "Russia", "Turkey")))

# Europe (including Russian subregions in Europe) available name counts
geography_summary_df <- update_available_names_for_continent("Europe", geography_summary_df,
                                                  additional_filters = list(country_values = c("Russia")))

# North America available name counts
geography_summary_df <- update_available_names_for_continent("North America", geography_summary_df)

# Oceania (Continent) (including Indonesian subregions in Oceania) available name counts
geography_summary_df <- update_available_names_for_continent("Oceania (Continent)", geography_summary_df,
                                                  additional_filters = list(country_values = c("Indonesia")))

# South America available name counts
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

# Load the synonym sheet from an excel file
#synonyms_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\Species_Syn_v2.0.csv")

# Summarize the name totals for the MDD_validity column
validity_summary <- synonyms_df %>%
  group_by(MDD_validity) %>%
  summarise(total_names = n()) %>%
  ungroup() %>%
  mutate(status_type = "Validity Status") %>%
  rename(status = MDD_validity)

# Process the MDD_nomenclature_status column to handle multiple values separated by pipes
nomenclature_split <- synonyms_df %>%
  separate_rows(MDD_nomenclature_status, sep = "\\|") %>%
  mutate(MDD_nomenclature_status = str_trim(MDD_nomenclature_status)) %>%
  group_by(MDD_nomenclature_status) %>%
  summarise(total_names = n()) %>%
  ungroup() %>%
  mutate(status_type = "Nomenclature Status") %>%
  rename(status = MDD_nomenclature_status)

# Combine the validity and nomenclature summaries
val_nom_summary_df <- bind_rows(validity_summary, nomenclature_split)

# Create logical flags for each relevant column
flagged_synonyms_df <- synonyms_df %>%
  mutate(
    authority_citation_flag = !is.na(MDD_authority_citation) | !is.na(MDD_unchecked_authority_citation),
    type_locality_flag = !is.na(MDD_original_type_locality) | !is.na(MDD_unchecked_type_locality),
    authority_author_flag = !is.na(MDD_author),
    authority_year_flag = !is.na(MDD_year),
    original_combination_flag = !is.na(MDD_original_combination),
    original_rank_flag = !is.na(MDD_original_rank),
    verified_authority_citation_flag = !is.na(MDD_authority_citation),
    authority_page_flag = !is.na(MDD_authority_page),
    authority_link_flag = !is.na(MDD_authority_link),
    authority_page_link_flag = !is.na(MDD_authority_page_link),
    type_coordinates_flag = !is.na(MDD_type_latitude),
    type_specimen_flag = !is.na(MDD_type_kind),
    type_specimen_link_flag = !is.na(MDD_type_specimen_link)
  )

# Apply the logical flags to the summary
val_nom_summary_df <- val_nom_summary_df %>%
  rowwise() %>%
  mutate(
    authority_author_count = sum(synonyms_df$authority_author_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    authority_year_count = sum(synonyms_df$authority_year_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    year_since_2000_count = sum(synonyms_df$MDD_year >= 2000 & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status)), na.rm = TRUE),
    original_combination_total = sum(synonyms_df$original_combination_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    original_rank_count = sum(synonyms_df$original_rank_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    authority_citation_count = sum(synonyms_df$authority_citation_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    verified_authority_citation_count = sum(synonyms_df$verified_authority_citation_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    authority_page_count = sum(synonyms_df$authority_page_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    authority_link_count = sum(synonyms_df$authority_link_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    authority_page_link_count = sum(synonyms_df$authority_page_link_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    type_locality_count = sum(synonyms_df$type_locality_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    original_type_locality_count = sum(synonyms_df$MDD_validity == status & !is.na(synonyms_df$MDD_original_type_locality) | grepl(status, synonyms_df$MDD_nomenclature_status) & !is.na(synonyms_df$MDD_original_type_locality)),
    type_coordinates_count = sum(synonyms_df$type_coordinates_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    type_specimen_count = sum(synonyms_df$type_specimen_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status))),
    type_specimen_link_count = sum(synonyms_df$type_specimen_link_flag & (synonyms_df$MDD_validity == status | grepl(status, synonyms_df$MDD_nomenclature_status)))
  ) %>%
  ungroup()

# Calculate totals directly from the synonyms_df
total_synonyms <- flagged_synonyms_df %>%
  summarise(
    status_type = "Total",
    status = "total_synonyms",
    total_names = n(),
    authority_author_count = sum(authority_author_flag),
    authority_year_count = sum(authority_year_flag),
    year_since_2000_count = sum(MDD_year >= 2000, na.rm = TRUE),
    original_combination_total = sum(original_combination_flag),
    original_rank_count = sum(original_rank_flag),
    authority_citation_count = sum(authority_citation_flag),
    verified_authority_citation_count = sum(verified_authority_citation_flag),
    authority_page_count = sum(authority_page_flag),
    authority_link_count = sum(authority_link_flag),
    authority_page_link_count = sum(authority_page_link_flag),
    type_locality_count = sum(type_locality_flag),
    original_type_locality_count = sum(!is.na(MDD_original_type_locality)),
    type_coordinates_count = sum(type_coordinates_flag),
    type_specimen_count = sum(type_specimen_flag),
    type_specimen_link_count = sum(type_specimen_link_flag)
  )

# Combine the summaries and total_synonyms
nomenclature_summary_df <- bind_rows(val_nom_summary_df, total_synonyms)

# Arrange columns in the desired order
nomenclature_summary_df <- nomenclature_summary_df %>%
  select(status_type, status, everything())

# View final nomenclature summary dataframe
print(nomenclature_summary_df)

# Saving the nomenclature summary as a CSV file
write.csv(nomenclature_summary_df, "supplementary_files\\nomenclature_data_summary.csv", row.names = FALSE)

######Table 1 MDD Comparison ######

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

# Define a refined function for comparing versions 1.0 to 1.1 to get diff values
compare_sciname_versions_1_0_to_1_1 <- function(version_1_0, version_1_1) {
  # Extract ID, sciname, and authority year for both versions, and 'if new' category for v1.1
  version_1_0 <- version_1_0 %>%
    select(ID_number, SciName, Authority_year)
  
  version_1_1 <- version_1_1 %>%
    select(id, SciName, Authority_sp_year, IfNew_category)
  
  # Identify names in 1.1 that are not in 1.0 (potential splits or de novo)
  new_scinames <- setdiff(version_1_1$SciName, version_1_0$SciName)
  
  # For each new sciname, check if it has a matching ID in 1.0
  split_count <- 0
  de_novo_count <- 0
  for (name in new_scinames) {
    corresponding_id <- version_1_1 %>% filter(SciName == name) %>% pull(id)
    # Ensure corresponding_id is not empty
    if (length(corresponding_id) > 0) {
      # Check if this ID exists in version 1.0
      if (!(corresponding_id %in% version_1_0$ID_number)) {
        # If not, determine if it's a split or de novo based on the authority year
        year <- version_1_1 %>% filter(SciName == name) %>% pull(Authority_sp_year)
        # Ensure year is not NA before making comparisons
        if (!is.na(year)) {
          if (year < 2004) {
            # Added check: Ensure IfNew_category is 'splitFromExisting'
            category <- version_1_1 %>% filter(SciName == name) %>% pull(IfNew_category)
            if (!is.na(category) && category == "splitFromExisting") {
              split_count <- split_count + 1
            }
          } else {
            de_novo_count <- de_novo_count + 1
          }
        }
      }
    }
  }
  # Identify names in 1.0 that are not in 1.1 (potential lumps)
  removed_scinames <- setdiff(version_1_0$SciName, version_1_1$SciName)
  lump_count <- 0
  for (name in removed_scinames) {
    corresponding_id <- version_1_0 %>% filter(SciName == name) %>% pull(ID_number)
    # Ensure corresponding_id is not empty
    if (length(corresponding_id) > 0) {
      # Check if this ID exists in version 1.1
      if (!(corresponding_id %in% version_1_1$id)) {
        lump_count <- lump_count + 1
      }
    }
  }
  return(c(de_novo_count, split_count, lump_count))
}

# Initialize an empty list for diff counts
calculated_diff_counts <- list()

# Calculate the diff counts for version 1.1
calculated_diff_counts[["MDD1.1"]] <- compare_sciname_versions_1_0_to_1_1(
  version_1_0 = mdd_versions[["MDD1.0"]],
  version_1_1 = mdd_versions[["MDD1.1"]]
)

# Manually fill in the counts for version 1.2 based on counts Connor did personally
calculated_diff_counts[["MDD1.2"]] <- c(de_novo = 97, split = 157, lump = 295)
calculated_diff_counts[["MDD1.3.1"]] <- c(de_novo = 0, split = 0, lump = 0)

# Add these counts to the version comparison dataframe
version_comparison_df[["MDD1.1"]][5:7] <- calculated_diff_counts[["MDD1.1"]]

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


######Table 2 Compendia Comparisons######

# Making a table to summarize the difference between major mammal compendia and the MDD
# This section builds from the Table 1 section

#STILL NEED TO ADD
#IUCN 2024
#Maybe Fischer?
#Maybe Troussart?
#Maybe Walkers Mammals of the World?

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
  MSW1_1982 = c(
    4170, NA, NA, NA, 1033, 135, 20  # MSW1 values
  ),
  MSW2_1993 = c(
    4631, NA, NA, NA, 1135, 132, 26  # MSW2 values
  ),
  MSW3_2005 = c(
    5416, 5338, 75, 3, 1230, 153, 29  # MSW3 values
  ),
  MDD1_0 = c(
    6495, 6382, 96, 17, 1316, 166, 27  # MDD1.0 values taken from the first table
  ),
  CMW_2020 = c(
    6554, 6451, 103, 20, 1343, 167, 27  # CMW 2020 values
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
    MSW1_1982 = md("**MSW1**<br>1982"),
    MSW2_1993 = md("**MSW2**<br>1993"),
    MSW3_2005 = md("**MSW3**<br>2005"),
    MDD1_0 = md("**MDD1.0**<br>2018"),
    CMW_2020 = md("**CMW**<br>2020"),
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
  ) %>%
  # Add a footnote for the species total in MSW2
  tab_footnote(
    footnote = "Corrected total per Solari and Baker (2007)",
    locations = cells_body(
      columns = MSW2_1993,
      rows = Taxa == "Species"
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

# Load the synonym data from excel files
#synonyms_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\Species_Syn_v2.0.csv")

# Filter the dataframe for relevant nomenclature statuses and years after 1757
synonym_since_1758_data <- synonyms_df %>%
  filter(MDD_year > 1757, 
         MDD_nomenclature_status %in% c("available", "as_emended", "preoccupied", "nomen_novum", "fully_suppressed", "partially_suppressed"))

# Create a subset of the filtered data where validity is exactly "species"
species_year_data <- synonym_since_1758_data %>%
  filter(MDD_validity == "species")

# Create a subset for 'Lumps': originally species but now synonyms
lumps_data <- synonym_since_1758_data %>%
  filter(MDD_original_rank == "species", MDD_validity == "synonym")

# Create a subset for 'Splits': originally synonym/subspecies/form/variety but now species
splits_data <- synonym_since_1758_data %>%
  filter(MDD_original_rank %in% c("synonym", "subspecies", "form", "variety"), MDD_validity == "species")

# Calculate total names and species names per year
names_per_year <- synonym_since_1758_data %>%
  group_by(MDD_year) %>%
  summarise(total_names = n())

species_per_year <- species_year_data %>%
  group_by(MDD_year) %>%
  summarise(species_count = n())

# Calculate counts of lumps and splits per year
lumps_per_year <- lumps_data %>%
  group_by(MDD_year) %>%
  summarise(lumps_count = n())

splits_per_year <- splits_data %>%
  group_by(MDD_year) %>%
  summarise(splits_count = n())

# Calculate the percentage of species, lumps, and splits per year and join with names_per_year
percent_species_per_year <- names_per_year %>%
  left_join(species_per_year, by = "MDD_year") %>%
  left_join(lumps_per_year, by = "MDD_year") %>%
  left_join(splits_per_year, by = "MDD_year") %>%
  mutate(
    species_count = coalesce(species_count, 0),  # Fill missing species counts with 0
    lumps_count = coalesce(lumps_count, 0),      # Fill missing lumps counts with 0
    splits_count = coalesce(splits_count, 0),    # Fill missing splits counts with 0
    percent_species = (species_count / total_names) * 100,
    percent_lumps = (lumps_count / total_names) * 100,
    percent_splits = (splits_count / total_names) * 100
  )

# Smooth the percent_species, percent_lumps, and percent_splits using a 10-year rolling mean
percent_species_per_year <- percent_species_per_year %>%
  arrange(MDD_year) %>%
  mutate(
    smoothed_percent_species = rollmean(percent_species, 10, fill = NA, align = "right"),
    smoothed_percent_lumps = rollmean(percent_lumps, 10, fill = NA, align = "right"),
    smoothed_percent_splits = rollmean(percent_splits, 10, fill = NA, align = "right")  # Fix here
  )

# Create the histogram with outlines and overlay the smoothed line graph with combined legends
names_over_time_plot <- ggplot() +
  geom_histogram(data = synonym_since_1758_data, aes(x = MDD_year, fill = "Available Names"), color = "black", binwidth = 1, alpha = 0.6) +
  geom_histogram(data = species_year_data, aes(x = MDD_year, fill = "Currently Valid Species"), color = "black", binwidth = 1, alpha = 0.6) +
  geom_line(data = percent_species_per_year, aes(x = MDD_year, y = smoothed_percent_species * max(names_per_year$total_names) / 100, color = "Described as Species"), size = 1) +
  geom_line(data = percent_species_per_year, aes(x = MDD_year, y = smoothed_percent_lumps * max(names_per_year$total_names) / 100, color = "Species Lumped Since Description"), size = 1) +
  geom_line(data = percent_species_per_year, aes(x = MDD_year, y = smoothed_percent_splits * max(names_per_year$total_names) / 100, color = "Species Split Since Description"), size = 1) +
  scale_y_continuous(
    name = "Total Names Described",
    breaks = seq(0, max(names_per_year$total_names), by = 100),  # Set breaks at every 100
    sec.axis = sec_axis(~ . / max(names_per_year$total_names) * 100, name = "Percent of Names")
  ) +
  labs(x = "Year") +
  scale_fill_manual(values = c("Available Names" = "gray70", "Currently Valid Species" = "gray30")) +
  scale_color_manual(values = c("Described as Species" = "blue3",
                                "Species Lumped Since Description" = "orange2",
                                "Species Split Since Description" = "red3")) + 
  theme(
    panel.background = element_rect(fill = "white", color = "gray90"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray90"),
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top"),
    legend.box = "horizontal",
    legend.spacing.x = unit(1, "cm"),
    legend.box.just = "left",
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_blank(),  # Remove legend titles
    legend.background = element_rect(fill = alpha("white", 0.3), color = NA), 
    legend.box.background = element_rect(fill = alpha("white", 0.3), color = NA)
  ) +
  
  # Custom guides for legends
  guides(
    fill = guide_legend(direction = "vertical", label.position = "right", keywidth = unit(1, "cm")),
    color = guide_legend(direction = "vertical", label.position = "right", keywidth = unit(1, "cm"))
  )

# Print Names over Time Graph
names_over_time_plot

# Save the Names over Time Graph as a JPEG file
ggsave("graphs\\names_over_time_graph.jpeg", 
       plot = names_over_time_plot,
       device = "jpeg", 
       width = 14, 
       height = 7, 
       units = "in", 
       dpi = 900)

######Species by Major Region Graph######

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
max_species_count <- max(biogeographic_realm_summary$species_count)

# Adjust the scaling so that species density aligns with species count, and ensure the desired axis limits (0-150)
scaling_factor <- max_species_count / 150

# Plot for Species Count by Biogeographic Realm
species_biorealm_plot <- ggplot(biogeographic_realm_summary, aes(x = reorder(region_name, -species_count))) +
  geom_bar(aes(y = species_count, fill = "Total Species"), stat = "identity", color = "black", width = 0.7, position = "stack") +
  geom_bar(aes(y = split_species + new_descriptions, fill = "Split Species"), stat = "identity", color = "black", width = 0.7, position = "stack") +
  geom_bar(aes(y = new_descriptions, fill = "New Species Descriptions"), stat = "identity", color = "black", width = 0.7, position = "stack") +
  geom_text(aes(y = species_count, label = species_count), vjust = -0.5, size = 3.5) +
  geom_point(aes(y = species_density * scaling_factor), color = "red", size = 3) +
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
  guides(fill = guide_legend(direction = "vertical")) +  # Align legend vertically
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "lines"))

# Compute max species_density and species_count values for scaling
max_species_count_continent <- max(continent_summary$species_count)

# Adjust the scaling so that species density aligns with species count, and ensure the desired axis limits (0-150)
scaling_factor_continent <- max_species_count_continent / 150

# Plot for Species Count by Continent with Fixed Secondary Axis
species_continent_plot <- ggplot(continent_summary, aes(x = reorder(region_name, -species_count))) +
  geom_bar(aes(y = species_count, fill = "Total Species"), stat = "identity", color = "black", width = 0.7) +
  geom_bar(aes(y = split_species + new_descriptions, fill = "Split Species"), stat = "identity", color = "black", width = 0.7) +
  geom_bar(aes(y = new_descriptions, fill = "New Species Descriptions"), stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(y = species_count, label = species_count), vjust = -0.5, size = 3.5) +
  geom_point(aes(y = species_density * scaling_factor_continent), color = "red", size = 3) +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_blank(),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, "lines"))

# Plot for Available Names by Biogeographic Realm
name_biorealm_plot <- ggplot(biogeographic_realm_summary, aes(x = reorder(region_name, -available_names), y = available_names)) +
  geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.7) +  
  geom_text(aes(label = available_names), vjust = -0.5, size = 3.5) +  
  labs(x = "Terrestrial Biogeographic Realm", y = "Available Names") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_blank())

# Plot for Available Names by Continent
name_continent_plot <- ggplot(continent_summary, aes(x = reorder(region_name, -available_names), y = available_names)) +
  geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.7) +  
  geom_text(aes(label = available_names), vjust = -0.5, size = 3.5) +  
  labs(x = "Continent", y = "Available Names") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_blank())

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
       height = 7, 
       units = "in", 
       dpi = 900)

ggsave("graphs/species_continent_plot.jpeg", 
       plot = species_continent_plot,
       device = "jpeg", 
       width = 14, 
       height = 7, 
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

# Creating a table to summarize the data presented in the above graphs

library(dplyr)
library(gt)

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
create_map <- function(data, fill_var, fill_label, low_color, high_color, show_top_10_list = TRUE) {
  top_10_countries <- data %>%
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
    just = "left", gp = gpar(fontsize = 8, fontface = "bold", col = "black")
  )
  
  map_plot <- ggplot(data) +
    geom_sf(aes_string(fill = fill_var), color = "black") +  # General map borders remain
    scale_fill_gradient(low = low_color, high = high_color, na.value = "gray90") +
    coord_sf(crs = "+proj=robin", expand = FALSE) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.key.width = unit(15, "cm"),
      legend.key.height = unit(0.5, "cm"),
      plot.margin = margin(10, 10, 10, 10),
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
        barwidth = unit(15, "cm"),
        barheight = unit(0.5, "cm")
      )
    )
  
  if (show_top_10_list) {
    map_plot <- map_plot + annotation_custom(label_grob)
  }
  
  return(map_plot)
}

# Create and display each map
map_total_species <- create_map(world_with_data, "species_count", "Total Species", "beige", "dodgerblue3", show_top_10_list = TRUE)
map_new_species <- create_map(world_with_data, "new_since_MSW3", "Newly Recognized Species Since MSW3", "beige", "dodgerblue3", show_top_10_list = TRUE)
map_threatened_species <- create_map(world_with_data, "threatened_percent", "Proportion of Threatened Species (NT, VU, EN, CR, EW)", "beige", "dodgerblue3", show_top_10_list = FALSE)
map_data_deficient <- create_map(world_with_data, "data_deficient_percent", "Proportion of Understudied Species (DD, NE)", "beige", "dodgerblue3", show_top_10_list = FALSE)
map_endemic_species <- create_map(world_with_data, "endemic_percent", "Proportion of Endemic Species", "beige", "dodgerblue3", show_top_10_list = FALSE)
map_available_names <- create_map(world_with_data, "available_names", "Available Name Type Localities", "beige", "dodgerblue3", show_top_10_list = TRUE)

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
new_species_since_2000 <- synonyms_df %>%
  filter(MDD_validity == "species" & MDD_year >= 2000)

# Filter for synonyms or subspecies described after 2000
non_species_since_2000 <- synonyms_df %>%
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
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.width = unit(15, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.margin = margin(10, 10, 10, 10),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      title = "New Species Described Since 2000",
      title.position = "top",
      title.hjust = 0,
      barwidth = unit(15, "cm"),
      barheight = unit(0.5, "cm")
    )
  )

# Create the point legend separately using grobs
point_legend_grob <- grobTree(
  textGrob("Type Localities", x = unit(0.1, "npc"), y = unit(0.3, "npc"), hjust = 0, gp = gpar(fontsize = 10, fontface = "bold")),
  pointsGrob(x = unit(0.1, "npc"), y = unit(0.2, "npc"), pch = 19, gp = gpar(col = "red", fontsize = 10)),
  textGrob("Valid Species", x = unit(0.2, "npc"), y = unit(0.2, "npc"), hjust = 0, gp = gpar(fontsize = 9)),
  pointsGrob(x = unit(0.1, "npc"), y = unit(0.1, "npc"), pch = 18, gp = gpar(col = "blue", fontsize = 10)),
  textGrob("Synonyms/Subspecies", x = unit(0.2, "npc"), y = unit(0.1, "npc"), hjust = 0, gp = gpar(fontsize = 9))
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
    legend.title = element_text(size = 12, face = "bold", hjust = 0),
    legend.text = element_text(size = 10),
    legend.key.width = unit(15, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.margin = margin(10, 10, 20, 10),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0,
      barwidth = unit(15, "cm"),
      barheight = unit(0.5, "cm")
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

######NOT FOR THIS PAPER type_specimen_df Supplement Table######

# Summarizing the data associated with type specimens on the MDD for a supplemental table

# Load packages
#library(readxl)
#library(readr)
#library(dplyr)
#library(tidyr)
#library(stringr)
#library(purrr)

# Load the synonym and museum metadata sheets from excel files
#synonyms_df <- read.csv("base_files\\MDD_v2.0\\MDD_v2.0\\Species_Syn_v2.0.csv")
#museums_metadata <- read_excel("base_files\\museums_metadata.xlsx")

# Remove nomen_novum to avoid repeated types for multiple names
filtered_synonyms <- synonyms_df %>%
  filter(MDD_nomenclature_status != "nomen_novum")

# Extract known institution labels from the collection dataset
known_institution_labels <- unique(museums_metadata$label)

# Define a function to extract museum labels correctly and generalize "Name collection" cases
extract_institution_codes <- function(holotype) {
  # Check if the holotype is missing or NA
  if (is.na(holotype) || holotype == "") {
    return(NA_character_)  # If the type is NA or empty, return NA
  }
  
  # Split the string by commas first to handle multiple specimens in one field
  parts <- str_split(holotype, ",\\s*")[[1]]
  
  # Extract the museum label from each part
  codes <- lapply(parts, function(part) {
    # Extract up to the first separator (space, colon, period, hyphen, slash)
    label <- str_extract(part, "^[^\\s:.,/-]+")
    
    # Generalize to handle cases with text string "Name collection" by checking if it's a single word not in known institution labels
    if (!label %in% known_institution_labels && !str_detect(label, "\\d") && nchar(label) > 1) {
      return(paste(label, "collection"))
    } else {
      return(label)
    }
  })
  
  # Return unique and non-empty codes
  unique_codes <- unique(unlist(codes[!is.na(codes) & codes != ""]))
  
  # Return NA if no valid codes were found
  if (length(unique_codes) == 0) {
    return(NA_character_)
  }
  
  return(unique_codes)
}

# Apply the function to extract institution codes
filtered_synonyms <- filtered_synonyms %>%
  rowwise() %>%
  mutate(institution_codes = list(extract_institution_codes(MDD_holotype))) %>%
  ungroup()

# Flatten the list of institution codes into separate rows
synonyms_expanded <- filtered_synonyms %>%
  unnest(institution_codes) %>%
  filter(!is.na(institution_codes) & institution_codes != "") %>%
  distinct()

# Summarize the data by institution, counting every appearance
summary_df <- synonyms_expanded %>%
  group_by(institution_codes) %>%
  summarise(
    total_type_specimens = n(),
    holotype_count = sum(MDD_type_kind == "holotype", na.rm = TRUE),
    syntype_count = sum(MDD_type_kind == "syntypes", na.rm = TRUE),
    lectotype_count = sum(MDD_type_kind == "lectotype", na.rm = TRUE),
    neotype_count = sum(MDD_type_kind == "neotype", na.rm = TRUE),
    nonexistent_count = sum(MDD_type_kind == "nonexistent", na.rm = TRUE)
  )

# Merge with the museums metadata to include lat/lon
type_specimen_df <- museums_metadata %>%
  select(label, name, country, state, city, museum_city_latitude, museum_city_longitude) %>%
  left_join(summary_df, by = c("label" = "institution_codes")) %>%
  rename(
    museum_label = label,
    museum_name = name,
    museum_country = country,
    museum_subregion = state,
    museum_city = city
  )

# Identify and summarize unmatched labels (used to check for issues)
unmatched_labels <- synonyms_expanded %>%
  filter(!institution_codes %in% museums_metadata$label)

unmatched_count <- unmatched_labels %>%
  summarise(total_type_specimens = n())

# Calculate Total Types (excluding NAs and empty values)
total_types <- type_specimen_df %>%
  filter(!is.na(museum_label) & museum_label != "") %>%
  summarise(
    museum_name = "Total Types",
    total_type_specimens = sum(total_type_specimens, na.rm = TRUE),
    holotype_count = sum(holotype_count, na.rm = TRUE),
    syntype_count = sum(syntype_count, na.rm = TRUE),
    lectotype_count = sum(lectotype_count, na.rm = TRUE),
    neotype_count = sum(neotype_count, na.rm = TRUE),
    nonexistent_count = sum(nonexistent_count, na.rm = TRUE)
  )

# Combine the final summary with Total Unmatched and Total Types rows
type_specimen_df <- bind_rows(
  type_specimen_df,
  data.frame(museum_name = "Total Unmatched", 
             total_type_specimens = unmatched_count$total_type_specimens,
             holotype_count = NA, syntype_count = NA,
             lectotype_count = NA, neotype_count = NA,
             nonexistent_count = NA),
  total_types
)

# Remove museums with no mammal type localities in them
type_specimen_df <- type_specimen_df %>%
  filter(!is.na(total_type_specimens))

# View the unmatched labels summary (was used to check for issues)
print(unmatched_labels)

# View the final type specimen summary
print(type_specimen_df)

# Saving the nomenclature summary as a CSV file
write.csv(type_specimen_df, "supplementary_files\\type_specimen_data_summary.csv", row.names = FALSE)


######NOT FOR THIS PAPER Type Specimen Museum Map######

# Creating a map of the where type specimens are primarily housed, 
# including a gradient map and point data for all museums with 10 or more type specimens 

# Load packages
#library(rnaturalearth)
#library(sf)
#library(dplyr)
#library(ggplot2)
#library(scales)

# Summarize the number of type specimens per country from the type_specimen_df
type_specimen_summary <- type_specimen_df %>%
  group_by(museum_country) %>%
  summarise(total_type_specimens = sum(total_type_specimens, na.rm = TRUE)) %>%
  rename(region_name = museum_country) %>%
  left_join(
    geographic_metadata %>%
      select(region_name = Region, iso3_code = `ISSO_3166-1_A-3_Code`) %>%
      mutate(iso3_upper = toupper(iso3_code)),
    by = "region_name"
  )

# Match the summarized data to the world map using ISO3 codes first
world_with_type_specimens <- world_map_updated %>%
  left_join(type_specimen_summary, by = c("iso_a3" = "iso3_upper"))

# For any unmatched countries, attempt to match by country name using name_long
unmatched_countries <- type_specimen_summary %>%
  filter(!iso3_upper %in% world_with_type_specimens$iso_a3)

# Second join based on country name
world_with_type_specimens <- world_with_type_specimens %>%
  left_join(
    unmatched_countries %>%
      select(region_name, total_type_specimens),
    by = c("name_long" = "region_name"),
    suffix = c("", ".name")
  ) %>%
  mutate(total_type_specimens = coalesce(total_type_specimens, total_type_specimens.name)) %>%
  select(-total_type_specimens.name)

# Extract latitude and longitude points for the top 50 museums based on total type specimens
top_50_museums <- type_specimen_df %>%
  arrange(desc(total_type_specimens)) %>%
  slice_head(n = 50) %>%
  mutate(
    museum_city_latitude = as.numeric(museum_city_latitude),
    museum_city_longitude = as.numeric(museum_city_longitude)
  ) %>%
  filter(!is.na(museum_city_latitude) & !is.na(museum_city_longitude)) %>%
  filter(between(museum_city_latitude, -90, 90) & between(museum_city_longitude, -180, 180)) %>%
  st_as_sf(coords = c("museum_city_longitude", "museum_city_latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(world_with_type_specimens))

# Filter out non-numeric latitude/longitude values before conversion
filtered_type_specimen_df <- type_specimen_df %>%
  filter(!is.na(museum_city_latitude) & !is.na(museum_city_longitude)) %>%
  filter(grepl("^-?\\d+(\\.\\d+)?$", museum_city_latitude) & grepl("^-?\\d+(\\.\\d+)?$", museum_city_longitude)) %>%
  mutate(
    museum_city_latitude = as.numeric(museum_city_latitude),
    museum_city_longitude = as.numeric(museum_city_longitude)
  )

# Create map for museum and country type specimen housing
map_type_specimens <- ggplot(world_with_type_specimens) +
  # Fill layer for all countries
  geom_sf(aes(fill = rescale(pmin(total_type_specimens, 1000), to = c(0, 1))), color = "black") +
  # Add points for all museums with 10 or more type specimens
  geom_sf(
    data = filtered_type_specimen_df %>%
      filter(total_type_specimens >= 10) %>%
      filter(!is.na(museum_city_latitude) & !is.na(museum_city_longitude)) %>%
      filter(between(museum_city_latitude, -90, 90) & between(museum_city_longitude, -180, 180)) %>%
      st_as_sf(coords = c("museum_city_longitude", "museum_city_latitude"), crs = 4326, remove = FALSE) %>%
      st_transform(crs = st_crs(world_with_type_specimens)),
    # Adjust point size based on the number of specimens
    aes(size = total_type_specimens),
    color = "red",
    alpha = 0.7,
    shape = 19
  ) +
  scale_fill_gradient(
    low = "beige", 
    high = "dodgerblue3", 
    na.value = "white",
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = c("0", "500", "1000+")
  ) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.width = unit(15, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.margin = margin(10, 10, 10, 10),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      title = "Type Specimens per Country",
      title.position = "top",
      title.hjust = 0,
      barwidth = unit(15, "cm"),
      barheight = unit(0.5, "cm")
    )
  )

# Create the point legend separately using grobs with gradation
point_legend_grob <- grobTree(
  textGrob("Museum Locations", x = unit(0.1, "npc"), y = unit(0.4, "npc"), hjust = 0, gp = gpar(fontsize = 10, fontface = "bold")),
  pointsGrob(x = unit(0.1, "npc"), y = unit(0.3, "npc"), pch = 19, size = unit(2, "mm"), gp = gpar(col = "red")),
  textGrob("10", x = unit(0.2, "npc"), y = unit(0.3, "npc"), hjust = 0, gp = gpar(fontsize = 9)),
  pointsGrob(x = unit(0.1, "npc"), y = unit(0.2, "npc"), pch = 19, size = unit(4, "mm"), gp = gpar(col = "red")),
  textGrob("100", x = unit(0.2, "npc"), y = unit(0.2, "npc"), hjust = 0, gp = gpar(fontsize = 9)),
  pointsGrob(x = unit(0.1, "npc"), y = unit(0.1, "npc"), pch = 19, size = unit(6, "mm"), gp = gpar(col = "red")),
  textGrob("500+", x = unit(0.2, "npc"), y = unit(0.1, "npc"), hjust = 0, gp = gpar(fontsize = 9))
)

# Combine the map and point legend
type_specimens_map <- map_type_specimens +
  annotation_custom(point_legend_grob, xmin = -14000000, xmax = -6000000, ymin = -1000000, ymax = -6000000)  # Use the provided coordinates for placement

# Print the final map
print(type_specimens_map)

# Saving the type locality map as a JPEG file
ggsave(
  filename = paste0("map_figures\\type_specimens_map.jpg"),
  plot = type_specimens_map,
  device = "jpeg",
  width = 15, height = 10, units = "in",
  dpi = 300
)


######NOT FOR THIS PAPER Country Taxonomic Disparity Map######

# Creating a map that scales the number of described species
# in a country by the number of species recognized in the country
# to quantify country-level taxonomic disparity

# Values closer to -1 have less available names described in the country compared to the total documented species
# Values closer to 0 have around equal available names described in the country to the total documented species
# Values closer to 1 have more available names described in the country compared to the total documented species

# Load packages
#library(rnaturalearth)
#library(sf)
#library(dplyr)
#library(ggplot2)

# Calculate the ratio difference between available names and species records per country
world_with_data <- world_with_data %>%
  mutate(
    scaled_ratio = ifelse(
      species_count + available_names > 0, 
      (available_names - species_count) / (available_names + species_count), 
      NA
    )
  )

# Create the ratio map using the -1 to 1 scale
map_taxonomic_disparity <- ggplot(world_with_data) +
  geom_sf(aes(fill = scaled_ratio), color = "black") +
  scale_fill_gradient2(
    low = "yellow", 
    mid = "grey", 
    high = "blue", 
    midpoint = 0,  # 0 represents equal numbers of species and available names
    na.value = "gray90",
    limits = c(-1, 1),  # Ensure the scale runs from -1 to 1
    breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = c("-1", "-0.5", "0", "0.5", "1")
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.width = unit(15, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.margin = margin(10, 10, 10, 10),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      title = "Taxonomic Disparity Ratio",
      title.position = "top",
      title.hjust = 0,
      barwidth = unit(15, "cm"),
      barheight = unit(0.5, "cm")
    )
  )

# Print the map
print(map_taxonomic_disparity)

# Saving the type locality map as a JPEG file
ggsave(
  filename = paste0("map_figures\\taxonomic_disparity_map.jpg"),
  plot = map_taxonomic_disparity,
  device = "jpeg",
  width = 15, height = 10, units = "in",
  dpi = 300
)



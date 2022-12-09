# subregion_script.R

# this script uses tidyverse to take the MDD as a csv and convert it into a csv showing alphabetically sorted USA level state lists (with known presence vs. vagrant/possible presence from MDD "?" designations). This could easily be repurposed for subregions of any/multiple countries listed in MDD, but does not currently have that functionality.

# load package -----------------------------------------------------------

library(tidyverse)

# country tidying -------------------------------------------------------

MDD_tbl <-
  
  # read file: change to path of your downloaded MDD csv file
  
  read_csv('C:/Users/madel/Documents/R_scripts_files/MDD/MDD_12_9_22.csv') %>% 
  # don't include domesticated species or recently extinct species
  # don't include the one species with no country distribution data
  
  filter(
    extinct == 0,
    domestic == 0,
    !is.na(countryDistribution)) %>%
  
  # select relevant columns only
  
  select(sciName,
         mainCommonName,
         subregionDistribution,
         countryDistribution)  %>% 
  
  # convert from camel case to all lowercase
  
  set_names(
    names(.) %>% 
      tolower()) %>% 
  
  # need to make countries into tidyish data
  # this makes lists of countries
  
  mutate(
    
    # split old column into vector on | delimiter
    
    countries = str_split(countrydistribution, pattern = "\\|"),
    
    # dropping original column
    
    .keep = "unused") 

# subregion tidying -------------------------------------------------------

# create long version of MDD, unnesting the list-column of countries and filter for only US (including AK and HI which are listed separately in MDD at country level)

long_US_only <-
  MDD_tbl %>% 
  rename(country = countries) %>% 
  unnest_longer(country) %>% 
  
  # filter to only United States then remove column
  
  filter(country %in% c('United States',
                        'United States?',
                        'Alaska',
                        'Alaska?',
                        "Hawai'i",
                        "Hawai'i?")) %>% 
  select(-country) %>% 
  
  # have to subset distinct rows or will get repeats for AK/HI
  
  distinct() %>% 
  
  # theoretically would have to do this again but luckily don't right now
  
  mutate(
    subregiondistribution = 
      str_remove(subregiondistribution, "USA\\(")) %>% 
  
  mutate(
    subregiondistribution = 
      str_remove(subregiondistribution, "\\)")) %>% 
  
  mutate(
    
    # split old column into vector on | delimiter
    
    state = str_split(subregiondistribution, pattern = ","),
    
    # dropping original column
    
    .keep = "unused") %>% 
  
  unnest_longer(state) %>% 
  filter(!state == '')

# make state lists --------------------------------------------------------

# take previous tibble and switch question marks into column denoting certainty and sorts into tibble for easy download and view

regions_with_certainty <-
  long_US_only %>% 
  
  # create certainty column with booleans
  
  mutate(certainty =
           str_detect(state, "\\?")) %>% 
  
  # remove question marks now that col is populated (and blank spaces)
  
  mutate(state =
           str_remove(state, '\\?')) %>%
  mutate(state =
           str_remove(state, ' ')) %>%
  
  # rename booleans for readability
  
  mutate(certainty =
           str_replace(certainty , 'FALSE', 'Known Presence')) %>% 
  mutate(certainty =
           str_replace(certainty , 'TRUE', 'Vagrant/Possible')) %>% 
  
  # sort
  
  arrange(state, certainty, sciname) %>% 
  
  select(state, sciname, maincommonname, certainty)

# final: write file; clean up ---------------------------------------------

# write file: insert file path/name

write_csv(regions_with_certainty, 
          'C:/Users/madel/Documents/R_scripts_files/MDD/state_list_MDD_12_9_22.csv')

# remove untidied and temporary MDD tibbles:

rm(MDD_tbl, long_US_only)  
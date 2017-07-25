## DATA FOR DURATION ####
# Read in data
data_duration = list.files("phonetic_analysis/data",
                       pattern = "*_duration.csv", full.names = T) %>%
  map(read_csv) %>%
  bind_rows()

# Clean data
data_duration_clean = data_duration


## READ IN DATA FOR FORMANTS ####
# Read in data
data_formants = list.files("phonetic_analysis/data",
                           pattern = "*_formants.csv", full.names = T) %>%
  map(read_csv) %>%
  bind_rows()

# Clean data
data_formants_clean = data_formants

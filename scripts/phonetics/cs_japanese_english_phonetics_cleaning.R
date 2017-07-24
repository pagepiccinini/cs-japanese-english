## DATA FOR DURATION ####
# Read in data
data_duration = list.files("data/phonetics/textfiles_phonetics",
                       pattern = "*_duration.csv", full.names = T) %>%
  map(read_csv) %>%
  bind_rows()

# Clean data
data_duration_clean = data_duration %>%
  separate(file_name, into = c("pair", "prompt", "speaker"), sep = "_")


## READ IN DATA FOR FORMANTS ####
# Read in data
data_formants = list.files("data/phonetics/textfiles_phonetics",
                           pattern = "*_formants.csv", full.names = T) %>%
  map(read_csv) %>%
  bind_rows()

# Clean data
data_formants_clean = data_formants %>%
  separate(file_name, into = c("pair", "prompt", "speaker"), sep = "_")

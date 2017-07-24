## DATA FOR DURATION ####
# Read in data
files_names = list.files("data/phonetics/textfiles_phonetics")

data_duration = list.files("data/phonetics/textfiles_phonetics",
                       full.names = T) %>%
  map(read.table, header=T, sep="\t") %>%
  map2(files_names, function(df, file.name) df %>%
              mutate(file = file.name)) %>%
  bind_rows() %>%
  mutate(file = sub(".txt", "", file))

# Clean data
data_duration_clean = data_phonetics_duration %>%
  separate(file, into = c("pair", "prompt", "speaker"), sep = "_") %>%
  separate(text, into = c("word", "phoneme", "lg_pre", "lg_post",
                          "type", "presence"), sep = "_") %>%
  mutate(presence = if_else(is.na(presence), 1, 0)) %>%
  mutate(duration = tmax - tmin)


## READ IN DATA FOR FORMANTS ####
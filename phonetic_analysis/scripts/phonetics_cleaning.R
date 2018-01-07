## READ IN WORD COUNT DATA ####
data_word_count = read.table("word_analysis/data/generated/wordcounts.txt",header=T) %>%
  # Add in demographic information
  inner_join(demo_simple)


## READ IN DATA FOR DURATION ####
# Read in data
data_duration = list.files("phonetic_analysis/data",
                       pattern = "*_duration.csv", full.names = T) %>%
  map(read_csv) %>%
  bind_rows() %>%
  # Add in demographic information
  inner_join(demo_simple)

# Clean data
data_duration_clean = data_duration %>%
  mutate(duration_ms = if_else(presence == 0, 0, duration_ms)) %>%
  left_join(data_word_count, by = c("prompt","pair","speaker", "acquisition_English_pct_exposed"))


## READ IN DATA FOR FORMANTS ####
# Read in data
data_formants = list.files("phonetic_analysis/data",
                           pattern = "*_formants.csv", full.names = T) %>%
  map(read_csv) %>%
  bind_rows() %>%
  # Add in demographic information
  inner_join(demo_simple)

# Clean data
data_formants_clean = data_formants %>%
  # Bark transform F1, F2, F3
  mutate(f1_bark = 26.81 / (1 + 1960 / f1) - 0.53) %>%
  mutate(f2_bark = 26.81 / (1 + 1960 / f2) - 0.53) %>%
  mutate(f3_bark = 26.81 / (1 + 1960 / f3) - 0.53) %>%
  # Normalize F1 and F2 based on F3
  mutate(f1_norm_bark = f3_bark - f1_bark) %>%
  mutate(f2_norm_bark = f3_bark - f2_bark) %>%
  left_join(data_word_count, by = c("prompt","pair","speaker", "acquisition_English_pct_exposed"))



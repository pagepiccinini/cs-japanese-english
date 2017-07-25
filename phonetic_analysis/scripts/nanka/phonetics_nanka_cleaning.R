## ORGANIZE DATA ####
# Separate out "nanka" data
data_duration_nanka = data_duration_clean %>%
  # Focus on the word "nanka"
  filter(word == "nanka")

# Get durations based on moraic partition
data_duration_nanka_moraic = data_duration_nanka %>%
  # Arrange to allow for correct spread
  arrange(pair, prompt, speaker, line) %>%
  # Add column for total duration of token
  mutate(duration_na = duration) %>%
  mutate(duration_n = lead(duration)) %>%
  mutate(duration_ka = lead(duration, 2)) %>%
  filter(sound == "na") %>%
  mutate(duration_sd = sd(c(duration_na, duration_n, duration_ka)))

# Get durations based on syllabic partition
data_duration_nanka_syllabic = data_duration_nanka %>%
  # Arrange to allow for correct spread
  arrange(pair, prompt, speaker, line) %>%
  # Add column for total duration of token
  mutate(duration_nan = duration + lead(duration)) %>%
  mutate(duration_ka = lead(duration, 2)) %>%
  filter(sound == "na") %>%
  mutate(duration_sd = sd(c(duration_nan, duration_ka)))



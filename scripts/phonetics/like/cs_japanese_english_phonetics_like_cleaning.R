## ORGANIZE DURATION DATA ####
# Separate out "like" data
data_duration_like = data_duration_clean %>%
  # Focus on the word "like"
  filter(word == "like")

# Separate out each phoneme
data_duration_like_lai = data_duration_like %>%
  # Focus on the phoneme /lai/
  filter(sound == "l" | sound == "i") %>%
  # Arrange to allow for correct spread
  arrange(pair, prompt, speaker, line) %>%
  # Add column for total duration of token
  mutate(duration_l = duration) %>%
  mutate(duration_ai = lead(duration)) %>%
  filter(sound == "l") %>%
  mutate(duration_lai = duration_l + duration_ai)

data_duration_like_kclosure = data_duration_like %>%
  # Focus on the phoneme /ai/
  filter(sound == "k-closure")

data_duration_like_kburst = data_duration_like %>%
  # Focus on the phoneme /ai/
  filter(sound == "k-burst")


## ORGANIZE FORMANT DATA ####
# Separate out "like" data
data_formants_like = data_formants_clean %>%
  # Focus on the word "like"
  filter(word == "like")

# Separate out each phoneme
data_formants_like_lai = data_formants_like %>%
  # Focus on the phoneme /lai/
  filter(sound == "l" | sound == "i")


  # Arrange to allow for correct spread
  arrange(pair, prompt, speaker, line) %>%
  # Add column for total duration of token
  mutate(duration_l = duration) %>%
  mutate(duration_ai = lead(duration)) %>%
  filter(phoneme == "l") %>%
  mutate(duration_lai = duration_l + duration_ai)


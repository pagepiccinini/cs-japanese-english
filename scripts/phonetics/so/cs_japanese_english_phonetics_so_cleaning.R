## ORGANIZE DURATION DATA ####
# Separate out "so" data
data_duration_so = data_duration_clean %>%
  # Focus on the word "so"
  filter(word == "so-eng" | word == "so-jap") %>%
  # Arrange to allow for correct spread
  arrange(pair, prompt, speaker, line) %>%
  # Add column for total duration of token
  mutate(duration_s = duration) %>%
  mutate(duration_o = lead(duration)) %>%
  filter(sound == "s") %>%
  mutate(duration_so = duration_s + duration_o)



## ORGANIZE FORMANT DATA ####
data_formants_so_o = data_formants_clean %>%
  # Focus on word "so"
  filter(word == "so-eng" | word == "so-jap") %>%
  # Focus on vowel
  filter(sound == "o")



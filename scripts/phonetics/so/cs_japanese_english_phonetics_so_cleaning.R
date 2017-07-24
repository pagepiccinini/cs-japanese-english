## ORGANIZE DATA ####
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





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
  filter(sound == "o") %>%
  # Add percentages
  group_by(pair, prompt, speaker, line) %>%
  mutate(percentage = round(time / max(time), 1)) %>%
  ungroup() %>%
  # Get mean of percentage
  group_by(pair, prompt, speaker, lang_pre, lang_post, line, percentage) %>%
  summarise(f1 = mean(f1, na.rm = T),
            f2 = mean(f2, na.rm = T),
            f3 = mean(f3, na.rm = T)) %>%
  ungroup()


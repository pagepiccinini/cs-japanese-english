## ORGANIZE DURATION DATA ####
# Separate out "so" data
data_duration_so = data_duration_clean %>%
  # Focus on the word "so"
  filter(word == "so-eng" | word == "so-jap") %>%
  # Arrange to allow for correct spread
  arrange(pair, prompt, speaker, line) %>%
  # Add column for total duration of token
  mutate(duration_s = duration_ms) %>%
  mutate(duration_o = lead(duration_ms)) %>%
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
  summarise(f1_norm_sum = mean(f1_norm_bark, na.rm = T),
            f2_norm_sum = mean(f2_norm_bark, na.rm = T)) %>%
  ungroup()

data_formants_so_o_global = data_formants_clean %>%
  # Focus on word "so"
  filter(word == "so-eng" | word == "so-jap") %>%
  # Focus on vowel
  filter(sound == "o") %>%
  # Add percentages
  group_by(pair, prompt, speaker, line) %>%
  mutate(percentage = round(time / max(time), 1)) %>%
  ungroup() %>%
  # Get mean of percentage
  group_by(pair, prompt, speaker, eng_percent, line, percentage) %>%
  summarise(f1_norm_sum = mean(f1_norm_bark, na.rm = T),
            f2_norm_sum = mean(f2_norm_bark, na.rm = T)) %>%
  ungroup()


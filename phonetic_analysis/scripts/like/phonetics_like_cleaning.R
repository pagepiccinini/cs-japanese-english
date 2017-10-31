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
  mutate(duration_l = duration_ms) %>%
  mutate(duration_ai = lead(duration_ms)) %>%
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
  filter(sound == "l" | sound == "i") %>%
  # Update line number and time
  mutate(line_lai = ifelse(sound == "l", line, line - 1)) %>%
  # Update time column
  mutate(time_real = tmin + time) %>%
  # Get percentages
  group_by(pair, prompt, speaker, line_lai) %>%
  mutate(percentage = round((time_real - min(time_real)) /
           (max(time_real) - min(time_real)), 1)) %>%
  # Get mean of percentage
  group_by(pair, prompt, speaker, lang_pre, lang_post, line_lai, percentage) %>%
  summarise(f1_norm_sum = mean(f1_norm_bark, na.rm = T),
            f2_norm_sum = mean(f2_norm_bark, na.rm = T)) %>%
  ungroup()

# Separate out each phoneme
data_formants_like_lai_global = data_formants_like %>%
  # Focus on the phoneme /lai/
  filter(sound == "l" | sound == "i") %>%
  # Update line number and time
  mutate(line_lai = ifelse(sound == "l", line, line - 1)) %>%
  # Update time column
  mutate(time_real = tmin + time) %>%
  # Get percentages
  group_by(pair, prompt, speaker, line_lai) %>%
  mutate(percentage = round((time_real - min(time_real)) /
                              (max(time_real) - min(time_real)), 1)) %>%
  # Get mean of percentage
  group_by(pair, prompt, speaker, eng_percent, line_lai, percentage) %>%
  summarise(f1_norm_sum = mean(f1_norm_bark, na.rm = T),
            f2_norm_sum = mean(f2_norm_bark, na.rm = T)) %>%
  ungroup()
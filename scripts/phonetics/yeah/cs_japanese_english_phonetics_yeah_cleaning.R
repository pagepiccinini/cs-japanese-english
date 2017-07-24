## ORGANIZE DATA FOR DURATION ####
# Separate out "yeah" data
data_duration_yeah = data_duration_clean %>%
  # Focus on the word "like"
  filter(word == "yeah") %>%
  # Arrange to allow for correct spread
  arrange(pair, prompt, speaker, line) %>%
  # Add column for total duration of token
  mutate(duration_y = duration) %>%
  mutate(duration_eah = lead(duration)) %>%
  filter(sound == "y") %>%
  mutate(duration_yeah = duration_y + duration_eah)


## ORGANIZE DATA FOR FORMANTS ####
# Separate out "yeah" data
data_formants_yeah_eah = data_formants_clean %>%
  # Focus on the word "like"
  filter(word == "yeah") %>%
  # Focus on phoneme
  filter(sound == "eah") %>%
  # Get mean formants
  group_by(line, pair, prompt, speaker, lang_pre, lang_post) %>%
  summarise(f1_mean = mean(f1, na.rm = T),
            f2_mean = mean(f2, na.rm = T)) %>%
  ungroup()

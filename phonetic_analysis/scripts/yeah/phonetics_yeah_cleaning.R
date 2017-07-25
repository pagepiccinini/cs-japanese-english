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
  # Add percentages
  group_by(pair, speaker, prompt, line) %>%
  mutate(midpoint = round(min(time) + ((max(time) - min(time)) / 2), 3)) %>%
  ungroup() %>%
  # Filter to midpoint
  filter(time == midpoint)

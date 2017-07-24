## ORGANIZE DATA ####
# Separate out "yeah" data
data_duration_yeah = data_duration_clean %>%
  # Focus on the word "like"
  filter(word == "yeah") %>%
  # Arrange to allow for correct spread
  arrange(pair, prompt, speaker, line) %>%
  # Add column for total duration of token
  mutate(duration_y = duration) %>%
  mutate(duration_eah = lead(duration)) %>%
  filter(phoneme == "y") %>%
  mutate(duration_yeah = duration_y + duration_eah)


## MAKE FIGURE OF DURATIONS ####
# 'yeah' duration
duration_yeah.plot = ggplot(data_duration_yeah, aes(x = lg_pre, y = duration_yeah,
                                                            fill = lg_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_yeah.plot

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
  filter(phoneme == "s") %>%
  mutate(duration_so = duration_s + duration_o)


## MAKE FIGURE OF DURATIONS ####
# 'so' duration
duration_so.plot = ggplot(data_duration_so, aes(x = lg_pre, y = duration_so,
                                                    fill = lg_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_so.plot


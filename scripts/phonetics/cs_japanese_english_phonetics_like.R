## ORGANIZE DATA ####
# Separate out "like" data
data_duration_like = data_duration_clean %>%
  # Focus on the word "like"
  filter(word == "like")

# Separate out each phoneme
data_duration_like_lai = data_duration_like %>%
  # Focus on the phoneme /lai/
  filter(phoneme == "l" | phoneme == "i") %>%
  # Arrange to allow for correct spread
  arrange(pair, prompt, speaker, line) %>%
  # Add column for total duration of token
  mutate(duration_l = duration) %>%
  mutate(duration_ai = lead(duration)) %>%
  filter(phoneme == "l") %>%
  mutate(duration_lai = duration_l + duration_ai)

data_duration_like_kclosure = data_duration_like %>%
  # Focus on the phoneme /ai/
  filter(phoneme == "k-closure")

data_duration_like_kburst = data_duration_like %>%
  # Focus on the phoneme /ai/
  filter(phoneme == "k-burst")


## MAKE FIGURE OF DURATIONS ####
# /lai/ duration
duration_like_lai.plot = ggplot(data_duration_like_lai, aes(x = lg_pre, y = duration_lai,
                                   fill = lg_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_like_lai.plot

# [k]-closure presence
ggplot(data_duration_like_kclosure, aes(x = lg_pre, y = presence,
                                       fill = lg_post)) +
  geom_point()

# [k]-burst presence
ggplot(data_duration_like_kburst, aes(x = lg_pre, y = presence,
                                        fill = lg_post)) +
  geom_point()

# [k]-burst duration
duration_like_kburst.plot = ggplot(data_duration_like_kburst, aes(x = lg_pre, y = duration,
                                                            fill = lg_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_like_kburst.plot

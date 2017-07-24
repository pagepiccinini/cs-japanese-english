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

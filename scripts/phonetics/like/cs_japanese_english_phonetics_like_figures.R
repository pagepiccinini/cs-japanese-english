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


## MAKE FIGURE OF FORMANTS ####
# /l/
like_l_formants.plot = ggplot(data_phonetics_like_l, aes(x = time, y = f1, col = lang_pre)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /l/ in 'like'") +
  xlab("Time into /l/") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

like_l_formants.plot
ggsave("figures/like_l_formants.pdf", like_l_formants.plot, width = 7, height = 7, unit = "in")

# /ai/
like_i_formants.plot = ggplot(data_phonetics_like_i, aes(x = time, y = f1, col = lang_pre)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /ai/ in 'like'") +
  xlab("Time into /ai/") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

like_i_formants.plot
ggsave("figures/like_i_formants.pdf", like_i_formants.plot, width = 7, height = 7, unit = "in")



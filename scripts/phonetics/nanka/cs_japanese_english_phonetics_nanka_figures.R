## ORGANIZE DATA ####
data_duration_nanka_moraic_figs = data_duration_nanka_moraic %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese")))

data_duration_nanka_syllabic_figs = data_duration_nanka_syllabic %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese")))


## MAKE FIGURE OF DURATIONS ####
# 'nanka' moraic partition
duration_nanka_moraic.plot = ggplot(data_duration_nanka_moraic_figs,
                                    aes(x = lang_pre, y = duration_sd,
                                        fill = lang_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_nanka_moraic.plot

# 'nanka' syllabic partition
duration_nanka_syllabic.plot = ggplot(data_duration_nanka_syllabic_figs,
                                      aes(x = lang_pre, y = duration_sd,
                                          fill = lang_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_nanka_syllabic.plot




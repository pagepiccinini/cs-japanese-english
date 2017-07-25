## ORGANIZE DATA ####
# Duration and presence
data_duration_like_lai_figs = data_duration_like_lai %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese")))

data_presence_like_kclosure_figs = data_duration_like_kburst %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  group_by(pair, speaker, lang_pre, lang_post) %>%
  summarise(mean_presence = mean(presence, na.rm = T)) %>%
  ungroup()

data_duration_like_kburst_figs = data_duration_like_kburst %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese")))

data_presence_like_kburst_figs = data_duration_like_kburst_figs %>%
  group_by(pair, speaker, lang_pre, lang_post) %>%
  summarise(mean_presence = mean(presence, na.rm = T)) %>%
  ungroup()

# Formants
data_formants_like_lai_figs = data_formants_like_lai %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese")))


## MAKE DURATION AND PRESENCE FIGURES ####
# /lai/ duration
duration_like_lai.plot = ggplot(data_duration_like_lai_figs,
                                aes(x = lang_pre, y = duration_lai,
                                    fill = lang_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_like_lai.plot

# [k]-closure presence
presence_like_kclos.plot = ggplot(data_presence_like_kclosure_figs,
       aes(x = lang_pre, y = mean_presence,
           fill = lang_post)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Percentage of time\n[k]-closure present",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top") 

presence_like_kclos.plot

# [k]-burst presence
presence_like_kburst.plot = ggplot(data_presence_like_kburst_figs,
                                  aes(x = lang_pre, y = mean_presence,
                                      fill = lang_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Language pre-switch",
       y = "Percentage of time\n[k]-burst present",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top") 

presence_like_kburst.plot

# [k]-burst duration
duration_like_kburst.plot = ggplot(data_duration_like_kburst_figs,
                                   aes(x = lang_pre, y = duration,
                                       fill = lang_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_like_kburst.plot


## MAKE FIGURE OF FORMANTS ####
# /lai/ over time
formants_like_lai.plot = ggplot(data_formants_like_lai_figs,
                              aes(x = percentage, y = f1, col = lang_pre)) + 
  geom_point() +
  geom_smooth() +
  geom_point(aes(y = f2)) +
  geom_smooth(aes(y = f2)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Time into /lai/",
       y = "Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

formants_like_lai.plot

# /lai/ F2 x F1
formants_like_lai_vs.plot = ggplot(data_formants_like_lai_figs,
                                aes(x = f2, y = f1,
                                    col = percentage)) + 
  geom_point() +
  geom_smooth() +
  scale_x_reverse() +
  scale_y_reverse() +
  #scale_color_brewer(palette = "Dark2") +
  labs(x = "F2 (Hz)",
       y = "F1 (Hz)") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

formants_like_lai_vs.plot


## ORGANIZE DATA ####
# Duration
data_duration_so_figs = data_duration_so %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese")))

# Formants
data_formants_so_o_figs = data_formants_so_o %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese")))


## MAKE FIGURE OF DURATIONS ####
# 'so' duration
duration_so.plot = ggplot(data_duration_so_figs,
                          aes(x = lang_pre, y = duration_so,
                              fill = lang_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_so.plot


## MAKE FIGURE OF FORMANTS ####
# /o/ formants
ggplot(data_formants_so_o_figs,
       aes(x = time, y = f1,
           color = lang_post, shape = lang_pre)) +
  geom_point() +
  geom_point(aes(y = f2)) +
  labs(x = "Time (s)",
       y = "Formants (Hz)",
       shape = "Language pre-switch",
       color = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

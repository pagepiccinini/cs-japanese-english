## ORGANIZE DATA ####
# Duration
data_duration_yeah_figs = data_duration_yeah %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese")))

# Formants
data_formants_yeah_eah_figs = data_formants_yeah_eah %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese")))
  

## MAKE FIGURE OF DURATIONS ####
# 'yeah' duration
duration_yeah.plot = ggplot(data_duration_yeah_figs,
                            aes(x = lang_pre, y = duration_yeah,
                               fill = lang_post)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Language pre-switch",
       y = "Duration (s)",
       fill = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_yeah.plot


## MAKE FIGURE OF FORMANTS ####
# /ae/
formants_yeah_eah.plot = ggplot(data_formants_yeah_eah_figs,
                         aes(x = f2, y = f1,
                             col = lang_post, shape = lang_pre)) + 
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(x = "F2",
       y = "F1",
       color = "Language post-switch") +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.position = "top")

formants_yeah_eah.plot

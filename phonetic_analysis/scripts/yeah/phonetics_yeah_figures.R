## ORGANIZE DATA ####
# Duration
data_duration_yeah_figs = data_duration_yeah %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(context = paste(lang_pre, lang_post))

# Formants
data_formants_yeah_eah_figs = data_formants_yeah_eah %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  mutate(context = paste(lang_pre, lang_post))
  

## MAKE FIGURE OF DURATIONS ####
# 'yeah' duration
duration_yeah.plot = ggplot(data_duration_yeah_figs,
                            aes(x = context, y = duration_yeah,
                               fill = context)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Language context",
       y = "Duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_yeah.plot
ggsave("phonetic_analysis/figures/yeah/duration_yeah.pdf", duration_yeah.plot,
       width = 7, height = 7, units = "in")

## MAKE FIGURE OF FORMANTS ####
# /ae/
formants_yeah_eah.plot = ggplot(data_formants_yeah_eah_figs,
                                   aes(x = context, y = f1_norm_bark,
                                       fill = context)) + 
  geom_boxplot() +
  geom_boxplot(aes(y = f2_norm_bark)) +
  scale_y_reverse() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Language context",
       y = "Frequency (Bark normalized)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.position = "top")

formants_yeah_eah.plot
ggsave("phonetic_analysis/figures/yeah/formants_yeah_eah.pdf", formants_yeah_eah.plot,
       width = 7, height = 7, units = "in")

# /ae/ F2 x F1
formants_yeah_eah_vs.plot = ggplot(data_formants_yeah_eah_figs,
                         aes(x = f2_norm_bark, y = f1_norm_bark,
                             col = context)) + 
  geom_point() +
  scale_color_brewer(palette = "PRGn") +
  labs(x = "F2 (Bark normalized)",
       y = "F1 (Bark normalized)",
       color = "") +
  theme_classic() +
  theme(text = element_text(size = 16),
        legend.position = "top")

formants_yeah_eah_vs.plot
ggsave("phonetic_analysis/figures/yeah/formants_yeah_eah_vs.pdf", formants_yeah_eah_vs.plot,
       width = 7, height = 7, units = "in")

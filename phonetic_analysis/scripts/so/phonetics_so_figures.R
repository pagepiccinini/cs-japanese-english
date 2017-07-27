## ORGANIZE DATA ####
# Duration
data_duration_so_figs = data_duration_so %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  mutate(context = paste(lang_pre, lang_post))

# Formants
data_formants_so_o_figs = data_formants_so_o %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  mutate(context = paste(lang_pre, lang_post)) 


## MAKE FIGURE OF DURATIONS ####
# 'so' duration
duration_so.plot = ggplot(data_duration_so_figs,
                          aes(x = context, y = duration_so,
                              fill = context)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Language context",
       y = "Duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_so.plot
ggsave("phonetic_analysis/figures/so/duration_so.pdf", duration_so.plot,
       width = 7, height = 7, units = "in")


## MAKE FIGURE OF FORMANTS ####
# /o/
formants_so_o.plot = ggplot(data_formants_so_o_figs,
                            aes(x = percentage, y = f1_norm_sum,
                                color = context)) +
  geom_point() +
  geom_smooth() +
  geom_point(aes(y = f2_norm_sum)) +
  geom_smooth(aes(y = f2_norm_sum)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_reverse() +
  scale_color_brewer(palette = "PRGn") +
  labs(x = "Percentage into /oU/",
       y = "Formants (Bark normalized)",
       color = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

formants_so_o.plot
ggsave("phonetic_analysis/figures/so/formants_so_o.pdf", formants_so_o.plot,
       width = 7, height = 7, units = "in")

# /o/ F2 x F1
formants_so_o_vs.plot = ggplot(data_formants_so_o_figs,
                               aes(x = f2_norm_sum, y = f1_norm_sum,
                                   color = context)) +
  geom_point() +
  geom_smooth() +
  scale_color_brewer(palette = "PRGn") +
  labs(x = "F2 (Bark normalized)",
       y = "F1 (Bark normalizd)",
       color = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

formants_so_o_vs.plot
ggsave("phonetic_analysis/figures/so/formants_so_o_vs.pdf", formants_so_o_vs.plot,
       width = 7, height = 7, units = "in")

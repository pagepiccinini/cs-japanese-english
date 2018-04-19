## ORGANIZE DATA ####
# Duration
data_duration_so_figs = data_duration_so %>%
  # Filter to only English-English and Japanese-Japanese tokens
  filter((lang_pre == "eng" & lang_post == "eng") | (lang_pre == "jap" & lang_post == "jap")) %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5) %>%
  # Contrast code context
  mutate(context_contrast = if_else(lang_pre == "eng", -0.5, 0.5))


# Formants
data_formants_so_o_figs = data_formants_so_o %>%
  # Filter to only English-English and Japanese-Japanese tokens
  filter((lang_pre == "eng" & lang_post == "eng") | (lang_pre == "jap" & lang_post == "jap")) %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5) %>%
  # Contrast code context
  mutate(context_contrast = if_else(lang_pre == "eng", -0.5, 0.5))%>%
  # Bin percent exposure for figures
  mutate(eng_perc_cat =  "<50") %>%
  mutate(eng_perc_cat = replace(eng_perc_cat , eng_percent > 0.5, ">50"))



## MAKE FIGURE OF DURATIONS ####
# 'so' duration
duration_so.plot = ggplot(data_duration_so_figs,
                          aes(x = eng_percent_centered, y = duration_so,
                              color = factor(context_contrast))) +
  geom_point() +
  geom_smooth()+
  scale_fill_brewer(palette = "Accent") +
  labs(x = "% English in conversation",
       y = "[soU] duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top")

duration_so.plot
ggsave("phonetic_analysis/figures/so/duration_so_exposure.pdf", duration_so.plot,
       width = 7, height = 7, units = "in")

## MAKE FIGURE OF FORMANTS ####
# /o/
formants_so_o_engpercent.plot = ggplot(data_formants_so_o_figs,
                            aes(x = percentage, y = f1_norm_sum,
                                color = factor(eng_perc_cat))) +
  geom_point() +
  geom_smooth() +
  geom_point(aes(y = f2_norm_sum)) +
  geom_smooth(aes(y = f2_norm_sum)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_reverse() +
  scale_color_brewer(palette = "Accent") +
  labs(x = "Percentage into /oU/",
       y = "Formants (Bark normalized)",
       color = "") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top")

formants_so_o_engpercent.plot
ggsave("phonetic_analysis/figures/so/formants_so_o_engpercent.pdf", formants_so_o_engpercent.plot,
       width = 7, height = 7, units = "in")

formants_so_o_context.plot = ggplot(data_formants_so_o_figs,
                                       aes(x = percentage, y = f1_norm_sum,
                                           color = factor(context_contrast))) +
  geom_point() +
  geom_smooth() +
  geom_point(aes(y = f2_norm_sum)) +
  geom_smooth(aes(y = f2_norm_sum)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_reverse() +
  scale_color_brewer(palette = "Accent") +
  labs(x = "Percentage into /oU/",
       y = "Formants (Bark normalized)",
       color = "") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top")

formants_so_o_context.plot
ggsave("phonetic_analysis/figures/so/formants_so_o_context.pdf", formants_so_o_context.plot,
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


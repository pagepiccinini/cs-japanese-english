## ORGANIZE DATA ####
# Duration
data_duration_yeah_figs = data_duration_yeah %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)

# Formants
data_formants_yeah_eah_figs = data_formants_yeah_eah %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)
  

## MAKE FIGURE OF DURATIONS BY LOCAL LANGUAGE CONTEXT####
# 'yeah' duration
duration_yeah.plot = ggplot(data_duration_yeah_figs,
                            aes(x = eng_percent_centered, y = duration_yeah)) +
  geom_point() +
  geom_smooth()+
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "% English in conversation (centered)",
       y = "Duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top")

duration_yeah.plot
ggsave("phonetic_analysis/figures/yeah/duration_yeah_exposure.pdf", duration_yeah.plot,
       width = 7, height = 7, units = "in")

## MAKE FIGURE OF FORMANTS####
# /ae/
formants_yeah_eah.plot = ggplot(data_formants_yeah_eah_figs,
                                   aes(x = eng_percent_centered, y = f1_norm_bark)) + 
  geom_point() +
  geom_smooth() + 
  geom_point(aes(y = f2_norm_bark)) +
  geom_smooth(aes(y = f2_norm_bark)) +
  scale_y_reverse() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "% English in conversation (centered)",
       y = "Frequency (Bark normalized)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 20),
        legend.position = "top")

formants_yeah_eah.plot
ggsave("phonetic_analysis/figures/yeah/formants_yeah_eah_exposure.pdf", formants_yeah_eah.plot,
       width = 7, height = 7, units = "in")

# /ae/ F2 x F1
formants_yeah_eah_vs.plot = ggplot(data_formants_yeah_eah_figs,
                         aes(x = f2_norm_bark, y = f1_norm_bark,
                             col = context)) + 
  geom_point(size = 3) +
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


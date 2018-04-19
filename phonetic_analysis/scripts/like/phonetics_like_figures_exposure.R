## ORGANIZE DATA ####
# Duration and presence
# Duration
data_duration_like_lai_figs = data_duration_like_lai %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)

data_presence_like_kclosure_figs = data_duration_like_kclosure %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5) %>%
  group_by(pair, speaker, eng_percent_centered) %>%
  summarise(mean_presence = mean(presence, na.rm = T)) %>%
  ungroup() 


data_duration_like_kburst_figs = data_duration_like_kburst %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5) 

data_presence_like_kburst_figs = data_duration_like_kburst_figs %>%
  group_by(pair, speaker, eng_percent_centered) %>%
  summarise(mean_presence = mean(presence, na.rm = T)) %>%
  ungroup() 


# Formants
data_formants_like_lai_figs = data_formants_like %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Focus on the phoneme /lai/
  filter(sound == "l" | sound == "i") %>%
  # Update line number and time
  mutate(line_lai = ifelse(sound == "l", line, line - 1)) %>%
  # Update time column
  mutate(time_real = tmin + time) %>%
  # Get percentages
  group_by(pair, prompt, speaker, line_lai) %>%
  mutate(percentage = round((time_real - min(time_real)) /
                              (max(time_real) - min(time_real)), 1)) %>%
  # Bin percentages for figure
  mutate(percentage_bin = ntile(percentage, 4)*25)%>%
  # Get mean of percentage
  group_by(pair, prompt, speaker, acquisition_English_pct_exposed, eng_percent, line_lai, percentage, percentage_bin) %>%
  summarise(f1_norm_sum = mean(f1_norm_bark, na.rm = T),
            f2_norm_sum = mean(f2_norm_bark, na.rm = T)) %>%
  ungroup() %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5) %>%
  # Bin percent exposure for figures
  mutate(eng_perc_cat = ntile(eng_percent, 4)*25)
  

## MAKE PLOT ON PERCENTAGE ENGLISH

perc_english.plot = ggplot(data_word_count,
                           aes(x = reorder(paste(pair,prompt,speaker),eng_percent), y = eng_percent, fill=pair)) +
  geom_bar(stat= "identity") +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Conversation",
       y = "Percent English",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top",axis.text.x = element_text(angle = 60, hjust = 1))
perc_english.plot
ggsave("phonetic_analysis/figures/dperc_english.pdf", perc_english.plot,
       height = 7, width = 7, units = "in")


## MAKE DURATION AND PRESENCE FIGURES ####
# /lai/ duration
duration_like_lai.plot = ggplot(data_duration_like_lai_figs,
                                aes(x = eng_percent_centered, y = duration_lai)) +
  geom_point() +
  geom_smooth() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "% English in conversation (centered)",
       y = "[lai] duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top")

duration_like_lai.plot
ggsave("phonetic_analysis/figures/like/duration_like_lai_exposure.pdf", duration_like_lai.plot,
       height = 7, width = 7, units = "in")

# [k]-closure presence
presence_like_kclos.plot = ggplot(data_presence_like_kclosure_figs,
       aes(x = eng_percent_centered, y = mean_presence)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "% English in conversation (centered)",
       y = "Percentage of time\n[k]-closure present",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top") 

presence_like_kclos.plot
ggsave("phonetic_analysis/figures/like/presence_like_kclos_exposure.pdf", presence_like_kclos.plot,
       height = 7, width = 7, units = "in")

# [k]-burst presence
presence_like_kburst.plot = ggplot(data_presence_like_kburst_figs,
                                  aes(x = eng_percent_centered, y = mean_presence)) +
  geom_point() +
  geom_smooth() +
  scale_fill_brewer(palette = "PRGn") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "% English in conversation (centered)",
       y = "Percentage of time\n[k]-burst present",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top") 

presence_like_kburst.plot
ggsave("phonetic_analysis/figures/like/presence_like_kburst_exposure.pdf", presence_like_kburst.plot,
       height = 7, width = 7, units = "in")

# [k]-burst duration
duration_like_kburst.plot = ggplot(data_duration_like_kburst_figs,
                                   aes(x = eng_percent_centered, y = duration_ms)) +
  geom_point() +
  geom_smooth() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "% English in conversation (centered)",
       y = "[k]-burst duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_like_kburst.plot
ggsave("phonetic_analysis/figures/like/duration_like_kburst_exposure.pdf", duration_like_kburst.plot,
       height = 7, width = 7, units = "in")


## MAKE FIGURE OF FORMANTS ####
# /lai/ over time by english exposure
formants_like_lai_byperc.plot = ggplot(data_formants_like_lai_figs,
                              aes(x = percentage, y = f1_norm_sum,
                                  col = factor(eng_perc_cat))) + 
  geom_point() +
  geom_smooth() +
  geom_point(aes(y = f2_norm_sum)) +
  geom_smooth(aes(y = f2_norm_sum)) +
  scale_color_brewer(palette = "PRGn") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_reverse() +
  labs(x = "Percentage into /lai/",
       y = "Frequency (Bark normalized)",
       color = "") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top")

formants_like_lai_byperc.plot
ggsave("phonetic_analysis/figures/like/formants_like_lai_bypercent_exposure.pdf", formants_like_lai_byperc.plot,
       width = 7, height = 7, units = "in")

# /lai/ by english exposure
formants_like_lai.plot = ggplot(data_formants_like_lai_figs,
                                aes(x = eng_percent_centered, y = f1_norm_sum
) )+ 
  geom_point() +
  geom_smooth() +
  geom_point(aes(y = f2_norm_sum)) +
  geom_smooth(aes(y = f2_norm_sum)) +
  scale_color_brewer(palette = "PRGn") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_reverse() +
  labs(x = "% English in conversation (centered)",
       y = "Frequency (Bark normalized)",
       color = "") +
  theme_classic() +
  theme(text = element_text(size = 20), legend.position = "top")

formants_like_lai.plot
ggsave("phonetic_analysis/figures/like/formants_like_lai_exposure.pdf", formants_like_lai.plot,
       width = 7, height = 7, units = "in")

# /lai/ F2 x F1
formants_like_lai_vs.plot = ggplot(data_formants_like_lai_figs,
                                aes(x = f2_norm_sum, y = f1_norm_sum,
                                    col = factor(eng_perc_cat))) + 
  #geom_point() +
  geom_smooth() +
  scale_color_brewer(palette = "PRGn") +
  labs(x = "F2 (Bark normalized)",
       y = "F1 (Bark normalized)",
       color = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

formants_like_lai_vs.plot
ggsave("phonetic_analysis/figures/like/formants_like_lai.pdf", formants_like_lai.plot,
       width = 7, height = 7, units = "in")



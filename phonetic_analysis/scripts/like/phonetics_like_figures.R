## ORGANIZE DATA ####
# Duration and presence
data_duration_like_lai_figs = data_duration_like_lai %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  mutate(context = paste(lang_pre, lang_post))

data_presence_like_kclosure_figs = data_duration_like_kclosure %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  group_by(pair, speaker, lang_pre, lang_post) %>%
  summarise(mean_presence = mean(presence, na.rm = T)) %>%
  ungroup() %>%
  mutate(context = paste(lang_pre, lang_post))

data_presence_like_kclosure_global_figs = data_duration_like_kclosure %>%
  group_by(pair, speaker, eng_percent) %>%
  summarise(mean_presence = mean(presence, na.rm = T)) %>%
  ungroup() 

data_duration_like_kburst_figs = data_duration_like_kburst %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  mutate(context = paste(lang_pre, lang_post))

data_presence_like_kburst_figs = data_duration_like_kburst_figs %>%
  group_by(pair, speaker, lang_pre, lang_post) %>%
  summarise(mean_presence = mean(presence, na.rm = T)) %>%
  ungroup() %>%
  mutate(context = paste(lang_pre, lang_post))

data_presence_like_kburst_global_figs = data_duration_like_kburst_figs %>%
  group_by(pair, speaker, eng_percent) %>%
  summarise(mean_presence = mean(presence, na.rm = T)) %>%
  ungroup() 

# Formants
data_formants_like_lai_figs = data_formants_like_lai %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  mutate(context = paste(lang_pre, lang_post))

data_formants_like_lai_figs_global = data_formants_like_lai_global %>%
  mutate(eng_perc_cat = eng_percent) %>%
  mutate(eng_perc_cat = replace(eng_perc_cat , eng_percent <=0.25, 25)) %>%
  mutate(eng_perc_cat = replace(eng_perc_cat , eng_percent > 0.25 & eng_percent <= 0.5, 50)) %>% 
  mutate(eng_perc_cat = replace(eng_perc_cat , eng_percent > 0.5 & eng_percent <= 0.75, 75)) %>%
  mutate(eng_perc_cat = replace(eng_perc_cat , eng_percent > 0.75, 100))
 


## MAKE DURATION AND PRESENCE FIGURES BY LOCAL LANGUAGE CONTEXT####
# /lai/ duration
duration_like_lai.plot = ggplot(data_duration_like_lai_figs,
                                aes(x = context, y = duration_lai,
                                    fill = context)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Language context",
       y = "[lai] duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_like_lai.plot
ggsave("phonetic_analysis/figures/like/duration_like_lai.pdf", duration_like_lai.plot,
       height = 7, width = 7, units = "in")

# [k]-closure presence
presence_like_kclos.plot = ggplot(data_presence_like_kclosure_figs,
       aes(x = context, y = mean_presence,
           fill = context)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Language context",
       y = "Percentage of time\n[k]-closure present",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top") 

presence_like_kclos.plot
ggsave("phonetic_analysis/figures/like/presence_like_kclos.pdf", presence_like_kclos.plot,
       height = 7, width = 7, units = "in")

# [k]-burst presence
presence_like_kburst.plot = ggplot(data_presence_like_kburst_figs,
                                  aes(x = context, y = mean_presence,
                                      fill = context)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PRGn") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Language context",
       y = "Percentage of time\n[k]-burst present",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top") 

presence_like_kburst.plot
ggsave("phonetic_analysis/figures/like/presence_like_kburst.pdf", presence_like_kburst.plot,
       height = 7, width = 7, units = "in")

# [k]-burst duration
duration_like_kburst.plot = ggplot(data_duration_like_kburst_figs,
                                   aes(x = context, y = duration_ms,
                                       fill = context)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Language context",
       y = "[k]-burst duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_like_kburst.plot
ggsave("phonetic_analysis/figures/like/duration_like_kburst.pdf", duration_like_kburst.plot,
       height = 7, width = 7, units = "in")



## MAKE PLOT ON PERCENTAGE ENGLISH

perc_english.plot = ggplot(data_word_count,
                                aes(x = reorder(paste(pair,prompt,speaker),eng_percent), y = eng_percent, fill=pair)) +
  geom_bar(stat= "identity") +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Conversation",
       y = "Percentage English",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")
ggsave("phonetic_analysis/figures/dperc_english.pdf", perc_english.plot,
       height = 7, width = 7, units = "in")



## MAKE DURATION AND PRESENCE FIGURES BY GLOBAL LANGUAGE CONTEXT####
# /lai/ duration
duration_like_lai_global.plot = ggplot(data_duration_like_lai_figs,
                                aes(x = eng_percent, y = duration_lai)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Percentage English",
       y = "[lai] duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_like_lai_global.plot
ggsave("phonetic_analysis/figures/like/duration_like_lai_global.pdf", duration_like_lai_global.plot,
       height = 7, width = 7, units = "in")

# [k]-closure presence
presence_like_kclos_global.plot = ggplot(data_presence_like_kclosure_global_figs,
                                  aes(x = eng_percent, y = mean_presence)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Percentage English",
       y = "Percentage of time\n[k]-closure present",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top") 

presence_like_kclos_global.plot
ggsave("phonetic_analysis/figures/like/presence_like_kclos_global.pdf", presence_like_kclos_global.plot,
       height = 7, width = 7, units = "in")

# [k]-burst presence
presence_like_kburst_global.plot = ggplot(data_presence_like_kburst_global_figs,
                                   aes(x = eng_percent, y = mean_presence)) +
  geom_point() +
  geom_smooth() +
  scale_fill_brewer(palette = "PRGn") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Percentage English",
       y = "Percentage of time\n[k]-burst present",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top") 

presence_like_kburst.plot
ggsave("phonetic_analysis/figures/like/presence_like_kburst_global.pdf", presence_like_kburst_global.plot,
       height = 7, width = 7, units = "in")

# [k]-burst duration
duration_like_kburst_global.plot = ggplot(data_duration_like_kburst_figs,
                                   aes(x = eng_percent, y = duration_ms)) +
  geom_point() +
  geom_smooth() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Percentage English",
       y = "[k]-burst duration (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_like_kburst_global.plot
ggsave("phonetic_analysis/figures/like/duration_like_kburst_global.pdf", duration_like_kburst_global.plot,
       height = 7, width = 7, units = "in")

## MAKE FIGURE OF FORMANTS BY LOCAL LANGUAGE CONTEXT####
# /lai/ over time
formants_like_lai.plot = ggplot(data_formants_like_lai_figs,
                              aes(x = percentage, y = f1_norm_sum,
                                  col = context)) + 
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
  theme(text = element_text(size = 16), legend.position = "top")

formants_like_lai.plot
ggsave("phonetic_analysis/figures/like/formants_like_lai.pdf", formants_like_lai.plot,
       width = 7, height = 7, units = "in")

# /lai/ F2 x F1
formants_like_lai_vs.plot = ggplot(data_formants_like_lai_figs,
                                aes(x = f2_norm_sum, y = f1_norm_sum,
                                    col = context)) + 
  geom_point() +
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




## MAKE FIGURE OF FORMANTS BY GLOBAL LANGUAGE CONTEXT####
# /lai/ over time
formants_like_lai_global.plot = ggplot(data_formants_like_lai_figs_global,
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
  theme(text = element_text(size = 16), legend.position = "top")

formants_like_lai_global.plot
ggsave("phonetic_analysis/figures/like/formants_like_lai_global.pdf", formants_like_lai_global.plot,
       width = 7, height = 7, units = "in")

# /lai/ F2 x F1
formants_like_lai_vs_global.plot = ggplot(data_formants_like_lai_figs_global,
                                   aes(x = f2_norm_sum, y = f1_norm_sum,
                                       col = factor(eng_perc_cat))) + 
  geom_point() +
  geom_smooth() +
  scale_color_brewer(palette = "PRGn") +
  labs(x = "F2 (Bark normalized)",
       y = "F1 (Bark normalized)",
       color = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

formants_like_lai_vs_global.plot
ggsave("phonetic_analysis/figures/like/formants_like_lai_global.pdf", formants_like_lai_vs_global.plot,
       width = 7, height = 7, units = "in")



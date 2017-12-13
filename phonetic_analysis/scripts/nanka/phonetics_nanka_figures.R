## ORGANIZE DATA ####
data_duration_nanka_moraic_figs = data_duration_nanka_moraic %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  mutate(context = paste(lang_pre, lang_post))

data_duration_nanka_syllabic_figs = data_duration_nanka_syllabic %>%
  mutate(lang_pre = factor(lang_pre,
                           levels = c("eng", "jap"),
                           labels = c("English", "Japanese"))) %>%
  mutate(lang_post = factor(lang_post,
                            levels = c("eng", "jap"),
                            labels = c("English", "Japanese"))) %>%
  mutate(context = paste(lang_pre, lang_post))


## MAKE FIGURE OF DURATIONS  BY LOCAL LANGUAGE CONTEXT####
# 'nanka' moraic partition
duration_nanka_moraic.plot = ggplot(data_duration_nanka_moraic_figs,
                                    aes(x = context, y = duration_sd,
                                        fill = context)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Language context",
       y = "Moraic duration standard deviation (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_nanka_moraic.plot
ggsave("phonetic_analysis/figures/nanka/duration_nanka_moraic.pdf", duration_nanka_moraic.plot,
       width = 7, height = 7, units = "in")

# 'nanka' syllabic partition
duration_nanka_syllabic.plot = ggplot(data_duration_nanka_syllabic_figs,
                                      aes(x = context, y = duration_sd,
                                          fill = context)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PRGn") +
  labs(x = "Language context",
       y = "Syllabic duration standard deviation (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_nanka_syllabic.plot
ggsave("phonetic_analysis/figures/nanka/duration_nanka_syllabic.pdf", duration_nanka_syllabic.plot,
       width = 7, height = 7, units = "in")


## MAKE FIGURE OF DURATIONS  BY GLOBAL LANGUAGE CONTEXT####
# 'nanka' moraic partition
duration_nanka_moraic_global.plot = ggplot(data_duration_nanka_moraic_figs,
                                    aes(x = eng_percent, y = duration_sd,
                                        fill = eng_percent)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Percentage English",
       y = "Moraic duration standard deviation (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_nanka_moraic_global.plot
ggsave("phonetic_analysis/figures/nanka/duration_nanka_moraic.pdf", duration_nanka_moraic_global.plot,
       width = 7, height = 7, units = "in")

# 'nanka' syllabic partition
duration_nanka_syllabic_global.plot = ggplot(data_duration_nanka_syllabic_figs,
                                      aes(x = eng_percent, y = duration_sd,
                                          fill = eng_percent)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Percentage English",
       y = "Syllabic duration standard deviation (ms)",
       fill = "") +
  theme_classic() +
  theme(text = element_text(size = 16), legend.position = "top")

duration_nanka_syllabic_global.plot
ggsave("phonetic_analysis/figures/nanka/duration_nanka_syllabic_global.pdf", duration_nanka_syllabic.plot,
       width = 7, height = 7, units = "in")



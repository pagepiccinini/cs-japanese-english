## ORGANIZE DATA ####
# Duration
data_duration_like_lai_ana = data_duration_like_lai %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)

# Burst
data_presence_like_kclosure_ana = data_duration_like_kclosure %>%
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)

data_presence_like_kburst_ana = data_duration_like_kburst_figs


# Formants
data_formants_like_lai_ana = data_formants_like %>%
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
  # Get mean of percentage
  group_by(pair, prompt, speaker, acquisition_English_pct_exposed, eng_percent, line_lai, percentage) %>%
  summarise(f1_norm_sum = mean(f1_norm_bark, na.rm = T),
            f2_norm_sum = mean(f2_norm_bark, na.rm = T)) %>%
  ungroup() %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)


## RUN MODELS ON DURATION ####
# Full model
duration_like_lai_exp.full.lme = lmer(duration_lai ~  eng_percent_centered +
                                      (1|pair/speaker),
                                      REML = FALSE,
                                      data = data_duration_like_lai_ana)

summary(duration_like_lai_exp.full.lme)

# Removing percent exposure questions - trending
duration_like_lai_exp.nope.lme = update(duration_like_lai_exp.full.lme, . ~ .
                                              - eng_percent_centered)

anova(duration_like_lai_exp.full.lme, duration_like_lai_exp.nope.lme)

#RUN MODELS ON CLOSURE AND BURST

k_closure_presence_like.full.lme = glmer(presence ~ eng_percent_centered +
                                           (1|pair/speaker), data_presence_like_kclosure_ana, family = binomial)
summary(k_closure_presence_like.full.lme)

# Removing percent exposure questions - n.s
k_closure_presence_like.nope.lme = update(k_closure_presence_like.full.lme, . ~ .
                                       - eng_percent_centered)

anova(k_closure_presence_like.full.lme, k_closure_presence_like.nope.lme)


## RUN MODELS ON FORMANTS - F1 ####
# Full model
formants_like_f1_exp.full.lme = lmer(f1_norm_sum ~ eng_percent_centered + percentage +
                                     (1+percentage|pair/speaker),
                                     REML = FALSE,
                                     data = data_formants_like_lai_ana)

summary(formants_like_f1_exp.full.lme)

# Removing percent exposure questions - n.s
formants_like_f1_exp.nope.lme = update(formants_like_f1_exp.full.lme, . ~ .
                                             - eng_percent_centered)

anova(formants_like_f1_exp.full.lme, formants_like_f1_exp.nope.lme)

# Removing percentage - SIGNIFICANT
formants_like_f1_exp.nopct.lme = update(formants_like_f1_exp.full.lme, . ~ .
                                              - percentage)

anova(formants_like_f1_exp.full.lme, formants_like_f1_exp.nopct.lme)


## RUN MODELS ON FORMANTS - F2 ####
# Full model
formants_like_f2_exp.full.lme = lmer(f2_norm_sum ~ eng_percent_centered + percentage +
                                     (1+percentage|pair/speaker),
                                     REML = FALSE,
                                     data = data_formants_like_lai_ana)

summary(formants_like_f2_exp.full.lme)

# Removing percent exposure questions - SIGNIFICANT
formants_like_f2_exp.nope.lme = update(formants_like_f2_exp.full.lme, . ~ .
                                       - eng_percent_centered)

anova(formants_like_f2_exp.full.lme, formants_like_f2_exp.nope.lme)

# Removing percentage - SIGNIFICANT
formants_like_f2_exp.nopct.lme = update(formants_like_f2_exp.full.lme, . ~ .
                                        - percentage)

anova(formants_like_f2_exp.full.lme, formants_like_f2_exp.nopct.lme)

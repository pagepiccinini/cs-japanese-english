## ORGANIZE DATA ####
# Duration
data_duration_like_lai_ana = data_duration_like_lai %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Make LEAP-Q question a decimal
  mutate(eng_leapq_exposure = acquisition_English_pct_exposed / 100) %>%
  # Bin exposure and percent
  mutate(eng_leapq_binned = if_else(eng_leapq_exposure <= 0.50, "0_50", "50_100")) %>%
  mutate(eng_percent_binned = if_else(eng_percent <= 0.50, "0_50", "50_100")) %>%
  # Contrast code binned groups
  mutate(eng_leapq_binned_contrast = if_else(eng_leapq_binned == "0_50", -0.5, 0.5)) %>%
  mutate(eng_percent_binned_contrast = if_else(eng_percent_binned == "0_50", -0.5, 0.5))

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
  # Make LEAP-Q question a decimal
  mutate(eng_leapq_exposure = acquisition_English_pct_exposed / 100) %>%
  # Bin exposure and percent
  mutate(eng_leapq_binned = if_else(eng_leapq_exposure <= 0.50, "0_50", "50_100")) %>%
  mutate(eng_percent_binned = if_else(eng_percent <= 0.50, "0_50", "50_100")) %>%
  # Contrast code binned groups
  mutate(eng_leapq_binned_contrast = if_else(eng_leapq_binned == "0_50", -0.5, 0.5)) %>%
  mutate(eng_percent_binned_contrast = if_else(eng_percent_binned == "0_50", -0.5, 0.5))


## RUN MODELS ON DURATION ####
# Full model
duration_like_lai_exp_leapq.full.lme = lmer(duration_lai ~  eng_percent_binned_contrast * eng_leapq_binned_contrast +
                                            (1|pair/speaker),
                                            REML = FALSE,
                                            data = data_duration_like_lai_ana)

summary(duration_like_lai_exp_leapq.full.lme)

# Removing percent exposure - SIGNIFICANT
duration_like_lai_exp_leapq.nope.lme = update(duration_like_lai_exp_leapq.full.lme, . ~ .
                                        - eng_percent_binned_contrast)

anova(duration_like_lai_exp_leapq.full.lme, duration_like_lai_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
duration_like_lai_exp_leapq.nolq.lme = update(duration_like_lai_exp_leapq.full.lme, . ~ .
                                              - eng_leapq_binned_contrast)

anova(duration_like_lai_exp_leapq.full.lme, duration_like_lai_exp_leapq.nolq.lme)

# Remove interaction - n.s.
duration_like_lai_exp_leapq.noint.lme = update(duration_like_lai_exp_leapq.full.lme, . ~ .
                                        - eng_leapq_binned_contrast:eng_percent_binned_contrast)


anova(duration_like_lai_exp_leapq.full.lme, duration_like_lai_exp_leapq.noint.lme)


## RUN MODELS ON FORMANTS - F1 ####
# Full model
formants_like_f1_exp_leapq.full.lme = lmer(f1_norm_sum ~  eng_percent_binned_contrast * eng_leapq_binned_contrast + percentage +
                                           (1+percentage|pair/speaker),
                                           REML = FALSE,
                                           data = data_formants_like_lai_ana)

summary(formants_like_f1_exp_leapq.full.lme)

# Removing percent exposure - n.s
formants_like_f1_exp_leapq.nope.lme = update(formants_like_f1_exp_leapq.full.lme, . ~ .
                                              - eng_percent_binned_contrast)

anova(formants_like_f1_exp_leapq.full.lme, formants_like_f1_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
formants_like_f1_exp_leapq.nolq.lme = update(formants_like_f1_exp_leapq.full.lme, . ~ .
                                              - eng_leapq_binned_contrast)

anova(formants_like_f1_exp_leapq.full.lme, formants_like_f1_exp_leapq.nolq.lme)

# Removing percentage - SIGNIFICANT
formants_like_f1_exp_leapq.nopct.lme = update(formants_like_f1_exp_leapq.full.lme, . ~ .
                                             - percentage)

anova(formants_like_f1_exp_leapq.full.lme, formants_like_f1_exp_leapq.nopct.lme)

# Remove interaction - n.s.
formants_like_f1_exp_leapq.noint.lme = update(formants_like_f1_exp_leapq.full.lme, . ~ .
                                               - eng_leapq_binned_contrast:eng_percent_binned_contrast)


anova(formants_like_f1_exp_leapq.full.lme, formants_like_f1_exp_leapq.noint.lme)


## RUN MODELS ON FORMANTS - F2 ####
# Full model
formants_like_f2_exp_leapq.full.lme = lmer(f2_norm_sum ~  eng_percent_binned_contrast * eng_leapq_binned_contrast + percentage +
                                           (1+percentage|pair/speaker),
                                           REML = FALSE,
                                           data = data_formants_like_lai_ana)

summary(formants_like_f2_exp_leapq.full.lme)

# Removing percent exposure - n.s
formants_like_f2_exp_leapq.nope.lme = update(formants_like_f2_exp_leapq.full.lme, . ~ .
                                             - eng_percent_binned_contrast)

anova(formants_like_f2_exp_leapq.full.lme, formants_like_f2_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
formants_like_f2_exp_leapq.nolq.lme = update(formants_like_f2_exp_leapq.full.lme, . ~ .
                                             - eng_leapq_binned_contrast)

anova(formants_like_f2_exp_leapq.full.lme, formants_like_f2_exp_leapq.nolq.lme)

# Removing percentage - SIGNIFICANT
formants_like_f2_exp_leapq.nopct.lme = update(formants_like_f2_exp_leapq.full.lme, . ~ .
                                              - percentage)

anova(formants_like_f2_exp_leapq.full.lme, formants_like_f2_exp_leapq.nopct.lme)

# Remove interaction - n.s.
formants_like_f2_exp_leapq.noint.lme = update(formants_like_f2_exp_leapq.full.lme, . ~ .
                                              - eng_leapq_binned_contrast:eng_percent_binned_contrast)

anova(formants_like_f2_exp_leapq.full.lme, formants_like_f2_exp_leapq.noint.lme)

## ORGANIZE DATA ####
# Duration
data_duration_yeah_ana = data_duration_yeah %>%
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
data_formants_yeah_eah_ana = data_formants_yeah_eah %>%
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


## RUN MODELS ON DURATION ####
# Full model
duration_yeah_exp_leapq.full.lme = lmer(duration_yeah ~ eng_percent_binned_contrast * eng_leapq_binned_contrast +
                                        (1 | pair/speaker),
                                        data_duration_yeah_ana,
                                        REML = F)

summary(duration_yeah_exp_leapq.full.lme)

# Removing percent exposure - n.s.
duration_yeah_exp_leapq.nope.lme = update(duration_yeah_exp_leapq.full.lme, . ~ .
                                              - eng_percent_binned_contrast)

anova(duration_yeah_exp_leapq.full.lme, duration_yeah_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
duration_yeah_exp_leapq.nolq.lme = update(duration_yeah_exp_leapq.full.lme, . ~ .
                                          - eng_leapq_binned_contrast)

anova(duration_yeah_exp_leapq.full.lme, duration_yeah_exp_leapq.nolq.lme)

# Removing interaction - n.s.
duration_yeah_exp_leapq.noint.lme = update(duration_yeah_exp_leapq.full.lme, . ~ .
                                          - eng_percent_binned_contrast:eng_leapq_binned_contrast)

anova(duration_yeah_exp_leapq.full.lme, duration_yeah_exp_leapq.noint.lme)


## RUN MODELS ON FORMANTS - F1 ####
# Full model
formants_yeah_f1_exp_leapq.full.lme = lmer(f1_norm_bark ~ eng_percent_binned_contrast * eng_leapq_binned_contrast +  
                                     (1 | pair/speaker),
                                     data_formants_yeah_eah_ana,
                                     REML = F)

summary(formants_yeah_f1_exp_leapq.full.lme)

# Removing percent exposure - SIGNIFICANT
formants_yeah_f1_exp_leapq.nope.lme = update(formants_yeah_f1_exp_leapq.full.lme, . ~ .
                                          - eng_percent_binned_contrast)

anova(formants_yeah_f1_exp_leapq.full.lme, formants_yeah_f1_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
formants_yeah_f1_exp_leapq.nolq.lme = update(formants_yeah_f1_exp_leapq.full.lme, . ~ .
                                          - eng_leapq_binned_contrast)

anova(formants_yeah_f1_exp_leapq.full.lme, formants_yeah_f1_exp_leapq.nolq.lme)

# Removing interaction - SIGNIFICANT
formants_yeah_f1_exp_leapq.noint.lme = update(formants_yeah_f1_exp_leapq.full.lme, . ~ .
                                           - eng_percent_binned_contrast:eng_leapq_binned_contrast)

anova(formants_yeah_f1_exp_leapq.full.lme, formants_yeah_f1_exp_leapq.noint.lme)


## RUN MODELS ON FORMANTS - F2 ####
# Full model
formants_yeah_f2_exp_leapq.full.lme = lmer(f2_norm_bark ~ eng_percent_binned_contrast * eng_leapq_binned_contrast +  
                                           (1 | pair/speaker),
                                           data_formants_yeah_eah_ana,
                                           REML = F)

summary(formants_yeah_f2_exp_leapq.full.lme)

# Removing percent exposure - n.s.
formants_yeah_f2_exp_leapq.nope.lme = update(formants_yeah_f2_exp_leapq.full.lme, . ~ .
                                             - eng_percent_binned_contrast)

anova(formants_yeah_f2_exp_leapq.full.lme, formants_yeah_f2_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
formants_yeah_f2_exp_leapq.nolq.lme = update(formants_yeah_f2_exp_leapq.full.lme, . ~ .
                                             - eng_leapq_binned_contrast)

anova(formants_yeah_f2_exp_leapq.full.lme, formants_yeah_f2_exp_leapq.nolq.lme)

# Removing interaction - n.s.
formants_yeah_f2_exp_leapq.noint.lme = update(formants_yeah_f2_exp_leapq.full.lme, . ~ .
                                              - eng_percent_binned_contrast:eng_leapq_binned_contrast)

anova(formants_yeah_f2_exp_leapq.full.lme, formants_yeah_f2_exp_leapq.noint.lme)

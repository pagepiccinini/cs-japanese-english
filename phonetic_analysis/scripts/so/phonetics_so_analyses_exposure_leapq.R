## ORGANIZE DATA ####
# Duration
data_duration_so_ana = data_duration_so %>%
  # Filter to only English-English and Japanese-Japanese tokens
  filter((lang_pre == "eng" & lang_post == "eng") | (lang_pre == "jap" & lang_post == "jap")) %>%
  # Make LEAP-Q question a decimal
  mutate(eng_leapq_exposure = acquisition_English_pct_exposed / 100) %>%
  # Bin exposure and percent
  mutate(eng_leapq_binned = if_else(eng_leapq_exposure <= 0.50, "0_50", "50_100")) %>%
  mutate(eng_percent_binned = if_else(eng_percent <= 0.50, "0_50", "50_100")) %>%
  # Contrast code binned groups
  mutate(eng_leapq_binned_contrast = if_else(eng_leapq_binned == "0_50", -0.5, 0.5)) %>%
  mutate(eng_percent_binned_contrast = if_else(eng_percent_binned == "0_50", -0.5, 0.5)) %>%
  mutate(context_contrast = if_else(lang_pre == "eng", -0.5, 0.5))

# Formants
data_formants_so_o_ana = data_formants_so_o %>%
  # Filter to only English-English and Japanese-Japanese tokens
  filter((lang_pre == "eng" & lang_post == "eng") | (lang_pre == "jap" & lang_post == "jap")) %>%
  # Make LEAP-Q question a decimal
  mutate(eng_leapq_exposure = acquisition_English_pct_exposed / 100) %>%
  # Bin exposure and percent
  mutate(eng_leapq_binned = if_else(eng_leapq_exposure <= 0.50, "0_50", "50_100")) %>%
  mutate(eng_percent_binned = if_else(eng_percent <= 0.50, "0_50", "50_100")) %>%
  # Contrast code binned groups
  mutate(eng_leapq_binned_contrast = if_else(eng_leapq_binned == "0_50", -0.5, 0.5)) %>%
  mutate(eng_percent_binned_contrast = if_else(eng_percent_binned == "0_50", -0.5, 0.5)) %>%
  mutate(context_contrast = if_else(lang_pre == "eng", -0.5, 0.5))


## RUN MODELS ON DURATION ####
# Full model
duration_so_exp_leapq.full.lme = lmer(duration_so ~ eng_percent_binned_contrast * eng_leapq_binned_contrast * context_contrast +
                                      (1 + context_contrast| pair/speaker),
                                      data_duration_so_ana,
                                      REML = F)

summary(duration_so_exp_leapq.full.lme)

# Removing percent exposure - n.s.
duration_so_exp_leapq.nope.lme = update(duration_so_exp_leapq.full.lme, . ~ .
                                        - eng_percent_binned_contrast)

anova(duration_so_exp_leapq.full.lme, duration_so_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
duration_so_exp_leapq.nolq.lme = update(duration_so_exp_leapq.full.lme, . ~ .
                                        - eng_leapq_binned_contrast)

anova(duration_so_exp_leapq.full.lme, duration_so_exp_leapq.nolq.lme)

# Removing context - n.s.
duration_so_exp_leapq.nocx.lme = update(duration_so_exp_leapq.full.lme, . ~ .
                                        - context_contrast)

anova(duration_so_exp_leapq.full.lme, duration_so_exp_leapq.nocx.lme)

# Removing percent exposure x LEAP-Q question interaction - MODEL FAIL
duration_so_exp_leapq.nopexlq.lme = update(duration_so_exp_leapq.full.lme, . ~ .
                                        - eng_percent_binned_contrast:eng_leapq_binned_contrast)

anova(duration_so_exp_leapq.full.lme, duration_so_exp_leapq.nopexlq.lme)

# Removing percent exposure x context interaction - n.s.
duration_so_exp_leapq.nopexcx.lme = update(duration_so_exp_leapq.full.lme, . ~ .
                                           - eng_percent_binned_contrast:context_contrast)

anova(duration_so_exp_leapq.full.lme, duration_so_exp_leapq.nopexcx.lme)

# Removing LEAP-Q question x context interaction - MODEL FAIL
duration_so_exp_leapq.lqxcx.lme = update(duration_so_exp_leapq.full.lme, . ~ .
                                           - eng_leapq_binned_contrast:context_contrast)

anova(duration_so_exp_leapq.full.lme, duration_so_exp_leapq.lqxcx.lme)

# Removing percent exposure x LEAP-Q x context question interaction - n.s.
duration_so_exp_leapq.nopexlqxcx.lme = update(duration_so_exp_leapq.full.lme, . ~ .
                                           - eng_percent_binned_contrast:eng_leapq_binned_contrast:context_contrast)

anova(duration_so_exp_leapq.full.lme, duration_so_exp_leapq.nopexlqxcx.lme)


## RUN MODELS ON FORMANTS - F1 ####
# Full model (model failed with both percentage and context as random slopes)
formants_so_f1_exp_leapq.full.lme = lmer(f1_norm_sum ~ eng_percent_binned_contrast * eng_leapq_binned_contrast * context_contrast + percentage +
                                         (1 + percentage | pair/speaker),
                                         data_formants_so_o_ana,
                                         REML = F)

summary(formants_so_f1_exp_leapq.full.lme)

# Removing percent exposure - n.s.
formants_so_f1_exp_leapq.nope.lme = update(formants_so_f1_exp_leapq.full.lme, . ~ .
                                        - eng_percent_binned_contrast)

anova(formants_so_f1_exp_leapq.full.lme, formants_so_f1_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
formants_so_f1_exp_leapq.nolq.lme = update(formants_so_f1_exp_leapq.full.lme, . ~ .
                                           - eng_leapq_binned_contrast)

anova(formants_so_f1_exp_leapq.full.lme, formants_so_f1_exp_leapq.nolq.lme)

# Removing context - n.s.
formants_so_f1_exp_leapq.nocx.lme = update(formants_so_f1_exp_leapq.full.lme, . ~ .
                                           - context_contrast)

anova(formants_so_f1_exp_leapq.full.lme, formants_so_f1_exp_leapq.nocx.lme)

# Removing percentage - n.s.
formants_so_f1_exp_leapq.nopct.lme = update(formants_so_f1_exp_leapq.full.lme, . ~ .
                                           - percentage)

anova(formants_so_f1_exp_leapq.full.lme, formants_so_f1_exp_leapq.nopct.lme)

# Removing percent exposure x LEAP-Q question interaction - n.s.
formants_so_f1_exp_leapq.nopexlq.lme = update(formants_so_f1_exp_leapq.full.lme, . ~ .
                                           - eng_percent_binned_contrast:eng_leapq_binned_contrast)

anova(formants_so_f1_exp_leapq.full.lme, formants_so_f1_exp_leapq.nopexlq.lme)

# Removing percent exposure x context interaction - SIGNIFICANT
formants_so_f1_exp_leapq.nopexcx.lme = update(formants_so_f1_exp_leapq.full.lme, . ~ .
                                              - eng_percent_binned_contrast:context_contrast)

anova(formants_so_f1_exp_leapq.full.lme, formants_so_f1_exp_leapq.nopexcx.lme)

# Removing LEAP-Q question x context interaction - n.s.
formants_so_f1_exp_leapq.nolqxcx.lme = update(formants_so_f1_exp_leapq.full.lme, . ~ .
                                              - eng_leapq_binned_contrast:context_contrast)

anova(formants_so_f1_exp_leapq.full.lme, formants_so_f1_exp_leapq.nolqxcx.lme)

# Removing percent exposure x LEAP-Q question x context interaction - SIGNIFICANT
formants_so_f1_exp_leapq.nopexlqxcx.lme = update(formants_so_f1_exp_leapq.full.lme, . ~ .
                                              - eng_percent_binned_contrast:eng_leapq_binned_contrast:context_contrast)

anova(formants_so_f1_exp_leapq.full.lme, formants_so_f1_exp_leapq.nopexlqxcx.lme)


## RUN MODELS ON FORMANTS - F2 ####
# Full model (model failed with both percentage and context as random slopes)
formants_so_f2_exp_leapq.full.lme = lmer(f2_norm_sum ~ eng_percent_binned_contrast * eng_leapq_binned_contrast * context_contrast + percentage +
                                         (1 + percentage | pair/speaker),
                                         data_formants_so_o_ana,
                                         REML = F)

summary(formants_so_f2_exp_leapq.full.lme)

# Removing percent exposure - n.s.
formants_so_f2_exp_leapq.nope.lme = update(formants_so_f2_exp_leapq.full.lme, . ~ .
                                           - eng_percent_binned_contrast)

anova(formants_so_f2_exp_leapq.full.lme, formants_so_f2_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
formants_so_f2_exp_leapq.nolq.lme = update(formants_so_f2_exp_leapq.full.lme, . ~ .
                                           - eng_leapq_binned_contrast)

anova(formants_so_f2_exp_leapq.full.lme, formants_so_f2_exp_leapq.nolq.lme)

# Removing context - n.s.
formants_so_f2_exp_leapq.nocx.lme = update(formants_so_f2_exp_leapq.full.lme, . ~ .
                                           - context_contrast)

anova(formants_so_f2_exp_leapq.full.lme, formants_so_f2_exp_leapq.nocx.lme)

# Removing percentage - n.s.
formants_so_f2_exp_leapq.nopct.lme = update(formants_so_f2_exp_leapq.full.lme, . ~ .
                                            - percentage)

anova(formants_so_f2_exp_leapq.full.lme, formants_so_f2_exp_leapq.nopct.lme)

# Removing percent exposure x LEAP-Q question interaction - n.s.
formants_so_f2_exp_leapq.nopexlq.lme = update(formants_so_f2_exp_leapq.full.lme, . ~ .
                                              - eng_percent_binned_contrast:eng_leapq_binned_contrast)

anova(formants_so_f2_exp_leapq.full.lme, formants_so_f2_exp_leapq.nopexlq.lme)

# Removing percent exposure x context interaction - n.s.
formants_so_f2_exp_leapq.nopexcx.lme = update(formants_so_f2_exp_leapq.full.lme, . ~ .
                                              - eng_percent_binned_contrast:context_contrast)

anova(formants_so_f2_exp_leapq.full.lme, formants_so_f2_exp_leapq.nopexcx.lme)

# Removing LEAP-Q question x context interaction - n.s.
formants_so_f2_exp_leapq.nolqxcx.lme = update(formants_so_f2_exp_leapq.full.lme, . ~ .
                                              - eng_leapq_binned_contrast:context_contrast)

anova(formants_so_f2_exp_leapq.full.lme, formants_so_f2_exp_leapq.nolqxcx.lme)

# Removing percent exposure x LEAP-Q question x context interaction - SIGNIFICANT
formants_so_f2_exp_leapq.nopexlqxcx.lme = update(formants_so_f2_exp_leapq.full.lme, . ~ .
                                                 - eng_percent_binned_contrast:eng_leapq_binned_contrast:context_contrast)

anova(formants_so_f2_exp_leapq.full.lme, formants_so_f2_exp_leapq.nopexlqxcx.lme)

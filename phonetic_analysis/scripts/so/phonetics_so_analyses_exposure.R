## ORGANIZE DATA ####
# Duration
data_duration_so_ana = data_duration_so %>%
  # Filter to only English-English and Japanese-Japanese tokens
  filter((lang_pre == "eng" & lang_post == "eng") | (lang_pre == "jap" & lang_post == "jap")) %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5) %>%
  # Contrast code context
  mutate(context_contrast = if_else(lang_pre == "eng", -0.5, 0.5))

# Formants
data_formants_so_o_ana = data_formants_so_o %>%
  # Filter to only English-English and Japanese-Japanese tokens
  filter((lang_pre == "eng" & lang_post == "eng") | (lang_pre == "jap" & lang_post == "jap")) %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5) %>%
  # Contrast code context
  mutate(context_contrast = if_else(lang_pre == "eng", -0.5, 0.5))


## RUN MODELS ON DURATION ####
# Full model
duration_so_exp.full.lme = lmer(duration_so ~ eng_percent_centered * context_contrast +
                                (1 + context_contrast| pair/speaker),
                                data_duration_so_ana,
                                REML = F)

summary(duration_so_exp.full.lme)

# Removing percent exposure - n.s.
duration_so_exp.nope.lme = update(duration_so_exp.full.lme, . ~ .
                                        - eng_percent_centered)

anova(duration_so_exp.full.lme, duration_so_exp.nope.lme)

# Removing context - n.s.
duration_so_exp.nocx.lme = update(duration_so_exp.full.lme, . ~ .
                                  - context_contrast)

anova(duration_so_exp.full.lme, duration_so_exp.nocx.lme)

# Removing interaction - SIGNIFICANT
duration_so_exp.noint.lme = update(duration_so_exp.full.lme, . ~ .
                                  - eng_percent_centered:context_contrast)

anova(duration_so_exp.full.lme, duration_so_exp.noint.lme)


## RUN MODELS ON FORMANTS - F1 ####
# Full model
formants_so_f1_exp.full.lme = lmer(f1_norm_sum ~ eng_percent_centered * context_contrast + percentage +
                                   (1 + context_contrast + percentage | pair/speaker),
                                   data_formants_so_o_ana,
                                   REML = F)

summary(formants_so_f1_exp.full.lme)

# Removing percent exposure - n.s.
formants_so_f1_exp.nope.lme = update(formants_so_f1_exp.full.lme, . ~ .
                                           - eng_percent_centered)

anova(formants_so_f1_exp.full.lme, formants_so_f1_exp.nope.lme)

# Removing context - n.s.
formants_so_f1_exp.nocx.lme = update(formants_so_f1_exp.full.lme, . ~ .
                                     - context_contrast)

anova(formants_so_f1_exp.full.lme, formants_so_f1_exp.nocx.lme)

# Removing percentage - n.s.
formants_so_f1_exp.nopct.lme = update(formants_so_f1_exp.full.lme, . ~ .
                                     - percentage)

anova(formants_so_f1_exp.full.lme, formants_so_f1_exp.nopct.lme)

# Removing interaction - SIGNIFICANT
formants_so_f1_exp.noint.lme = update(formants_so_f1_exp.full.lme, . ~ .
                                     - eng_percent_centered:context_contrast)

anova(formants_so_f1_exp.full.lme, formants_so_f1_exp.noint.lme)


## RUN MODELS ON FORMANTS - F2 ####
# Full model
formants_so_f2_exp.full.lme = lmer(f2_norm_sum ~ eng_percent_centered * context_contrast + percentage +
                                   (1 + context_contrast + percentage | pair/speaker),
                                   data_formants_so_o_ana,
                                   REML = F)

summary(formants_so_f2_exp.full.lme)

# Removing percent exposure - MODEL FAIL
formants_so_f2_exp.nope.lme = update(formants_so_f2_exp.full.lme, . ~ .
                                     - eng_percent_centered)

anova(formants_so_f2_exp.full.lme, formants_so_f2_exp.nope.lme)

# Removing context - MODEL FAIL
formants_so_f2_exp.nocx.lme = update(formants_so_f2_exp.full.lme, . ~ .
                                     - context_contrast)

anova(formants_so_f2_exp.full.lme, formants_so_f2_exp.nocx.lme)

# Removing percentage - MODEL FAIL
formants_so_f2_exp.nopct.lme = update(formants_so_f2_exp.full.lme, . ~ .
                                      - percentage)

anova(formants_so_f2_exp.full.lme, formants_so_f2_exp.nopct.lme)

# Removing interaction - n.s.
formants_so_f2_exp.noint.lme = update(formants_so_f2_exp.full.lme, . ~ .
                                      - eng_percent_centered:context_contrast)

anova(formants_so_f2_exp.full.lme, formants_so_f2_exp.noint.lme)

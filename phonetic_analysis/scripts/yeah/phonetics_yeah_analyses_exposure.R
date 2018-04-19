## ORGANIZE DATA ####
# Duration
data_duration_yeah_ana = data_duration_yeah %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)


# Formants
data_formants_yeah_eah_ana = data_formants_yeah_eah %>%
  # Filter to only English-English tokens
  filter(lang_pre == "eng" & lang_post == "eng") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)


## RUN MODELS ON DURATION ####
# Full model
duration_yeah_exp.full.lme = lmer(duration_yeah ~ eng_percent_centered +
                                  (1| pair/speaker),
                                  data_duration_yeah_ana,
                                  REML = F)

summary(duration_yeah_exp.full.lme)

# Removing percent exposure - n.s.
duration_yeah_exp.nope.lme = update(duration_yeah_exp.full.lme, . ~ .
                                          - eng_percent_centered)

anova(duration_yeah_exp.full.lme, duration_yeah_exp.nope.lme)


## RUN MODELS ON FORMANTS - F1 ####
# Full model
formants_yeah_f1_exp.full.lme = lmer(f1_norm_bark ~ eng_percent_centered +  
                                     (1 | pair/speaker),
                                     data_formants_yeah_eah_ana,
                                     REML = F)

summary(formants_yeah_f1_exp.full.lme)

# Removing percent exposure - n.s.
formants_yeah_f1_exp.nope.lme = update(formants_yeah_f1_exp.full.lme, . ~ .
                                             - eng_percent_centered)

anova(formants_yeah_f1_exp.full.lme, formants_yeah_f1_exp.nope.lme)


## RUN MODELS ON FORMANTS - F2 ####
# Full model
formants_yeah_f2_exp.full.lme = lmer(f2_norm_bark ~ eng_percent_centered +  
                                     (1 | pair/speaker),
                                     data_formants_yeah_eah_ana,
                                     REML = F)

summary(formants_yeah_f2_exp.full.lme)

# Removing percent exposure - SIGNIFICANT
formants_yeah_f2_exp.nope.lme = update(formants_yeah_f2_exp.full.lme, . ~ .
                                       - eng_percent_centered)

anova(formants_yeah_f2_exp.full.lme, formants_yeah_f2_exp.nope.lme)

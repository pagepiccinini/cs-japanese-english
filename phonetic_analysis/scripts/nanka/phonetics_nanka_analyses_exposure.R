## ORGANIZE DATA ####
# Moraic data
data_duration_nanka_moraic_ana = data_duration_nanka_moraic %>%
  # Filter to only Japanese-Japanese tokens
  filter(lang_pre == "jap" & lang_post == "jap") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)

# Syllabic data
data_duration_nanka_syllabic_ana = data_duration_nanka_syllabic %>%
  # Filter to only Japanese-Japanese tokens
  filter(lang_pre == "jap" & lang_post == "jap") %>%
  # Center percent exposure on 50%
  mutate(eng_percent_centered = eng_percent - 0.5)


## RUN MODELS ON MORAIC ANALYSIS ####
# Full model
moraic_nanka_exp.full.lme = lmer(duration_sd ~ eng_percent_centered +
                                 (1 | pair/speaker),
                                 data_duration_nanka_moraic_ana,
                                 REML = F)

summary(moraic_nanka_exp.full.lme)

# Removing percent exposure - n.s.
moraic_nanka_exp.nope.lme = update(moraic_nanka_exp.full.lme, . ~ .
                                         - eng_percent_centered)

anova(moraic_nanka_exp.full.lme, moraic_nanka_exp.nope.lme)


## RUN MODELS ON SYLLABIC ANALYSIS ####
# Full model
syllabic_nanka_exp.full.lme = lmer(duration_sd ~ eng_percent_centered +
                                   (1 | pair/speaker),
                                   data_duration_nanka_syllabic_ana,
                                   REML = F)

summary(syllabic_nanka_exp.full.lme)

# Removing percent exposure - n.s.
syllabic_nanka_exp.nope.lme = update(syllabic_nanka_exp.full.lme, . ~ .
                                   - eng_percent_centered)

anova(syllabic_nanka_exp.full.lme, syllabic_nanka_exp.nope.lme)

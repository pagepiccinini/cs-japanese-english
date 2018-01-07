## ORGANIZE DATA ####
# Moraic data
data_duration_nanka_moraic_ana = data_duration_nanka_moraic %>%
  # Filter to only Japanese-Japanese tokens
  filter(lang_pre == "jap" & lang_post == "jap") %>%
  # Make LEAP-Q question a decimal
  mutate(eng_leapq_exposure = acquisition_English_pct_exposed / 100) %>%
  # Bin exposure and percent
  mutate(eng_leapq_binned = if_else(eng_leapq_exposure <= 0.50, "0_50", "50_100")) %>%
  mutate(eng_percent_binned = if_else(eng_percent <= 0.50, "0_50", "50_100")) %>%
  # Contrast code binned groups
  mutate(eng_leapq_binned_contrast = if_else(eng_leapq_binned == "0_50", -0.5, 0.5)) %>%
  mutate(eng_percent_binned_contrast = if_else(eng_percent_binned == "0_50", -0.5, 0.5))

# Syllabic data
data_duration_nanka_syllabic_ana = data_duration_nanka_syllabic %>%
  # Filter to only Japanese-Japanese tokens
  filter(lang_pre == "jap" & lang_post == "jap") %>%
  # Make LEAP-Q question a decimal
  mutate(eng_leapq_exposure = acquisition_English_pct_exposed / 100) %>%
  # Bin exposure and percent
  mutate(eng_leapq_binned = if_else(eng_leapq_exposure <= 0.50, "0_50", "50_100")) %>%
  mutate(eng_percent_binned = if_else(eng_percent <= 0.50, "0_50", "50_100")) %>%
  # Contrast code binned groups
  mutate(eng_leapq_binned_contrast = if_else(eng_leapq_binned == "0_50", -0.5, 0.5)) %>%
  mutate(eng_percent_binned_contrast = if_else(eng_percent_binned == "0_50", -0.5, 0.5))


## RUN MODELS ON MORAIC ANALYSIS ####
# Full model
moraic_nanka_exp_leapq.full.lme = lmer(duration_sd ~ eng_percent_binned_contrast * eng_leapq_binned_contrast +
                                       (1 | pair/speaker),
                                       data_duration_nanka_moraic_ana,
                                       REML = F)

summary(moraic_nanka_exp_leapq.full.lme)

# Removing percent exposure - n.s.
moraic_nanka_exp_leapq.nope.lme = update(moraic_nanka_exp_leapq.full.lme, . ~ .
                                           - eng_percent_binned_contrast)

anova(moraic_nanka_exp_leapq.full.lme, moraic_nanka_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
moraic_nanka_exp_leapq.nolq.lme = update(moraic_nanka_exp_leapq.full.lme, . ~ .
                                         - eng_leapq_binned_contrast)

anova(moraic_nanka_exp_leapq.full.lme, moraic_nanka_exp_leapq.nolq.lme)

# Removing interaction - n.s.
moraic_nanka_exp_leapq.noint.lme = update(moraic_nanka_exp_leapq.full.lme, . ~ .
                                         - eng_percent_binned_contrast:eng_leapq_binned_contrast)

anova(moraic_nanka_exp_leapq.full.lme, moraic_nanka_exp_leapq.noint.lme)


## RUN MODELS ON SYLLABIC ANALYSIS ####
# Full  model
syllabic_nanka_exp_leapq.full.lme = lmer(duration_sd ~ eng_percent_binned_contrast * eng_leapq_binned_contrast +
                                         (1 | pair/speaker),
                                         data_duration_nanka_syllabic_ana,
                                         REML = F)

summary(syllabic_nanka_exp_leapq.full.lme)

# Removing percent exposure - n.s.
syllabic_nanka_exp_leapq.nope.lme = update(syllabic_nanka_exp_leapq.full.lme, . ~ .
                                         - eng_percent_binned_contrast)

anova(syllabic_nanka_exp_leapq.full.lme, syllabic_nanka_exp_leapq.nope.lme)

# Removing LEAP-Q question - n.s.
syllabic_nanka_exp_leapq.nolq.lme = update(syllabic_nanka_exp_leapq.full.lme, . ~ .
                                           - eng_leapq_binned_contrast)

anova(syllabic_nanka_exp_leapq.full.lme, syllabic_nanka_exp_leapq.nolq.lme)

# Removing interaction - n.s.
syllabic_nanka_exp_leapq.noint.lme = update(syllabic_nanka_exp_leapq.full.lme, . ~ .
                                           - eng_percent_binned_contrast:eng_leapq_binned_contrast)

anova(syllabic_nanka_exp_leapq.full.lme, syllabic_nanka_exp_leapq.noint.lme)

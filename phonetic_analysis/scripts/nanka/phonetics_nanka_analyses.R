## ORGANIZE DATA ####

data_duration_nanka_moraic_ana = data_duration_nanka_moraic_figs %>%
  filter(!is.na(lang_pre) & !is.na(lang_post))
contrasts(data_duration_nanka_moraic_ana$lang_pre) = c(-1,1) 
contrasts(data_duration_nanka_moraic_ana$lang_post) = c(-1,1) 

data_duration_nanka_syllabic_ana = data_duration_nanka_syllabic_figs %>%
  filter(!is.na(lang_pre) & !is.na(lang_post))
contrasts(data_duration_nanka_syllabic_ana$lang_pre) = c(-1,1) 
contrasts(data_duration_nanka_syllabic_ana$lang_post) = c(-1,1) 

##DESCRIPTIVES

#counts are similar for all measures analyzed
data_duration_nanka_moraic_ana %>%
  count(context) 

##ANALYZE DATA LIKE PREREGISTERED
#this analysis shows higher SD in E-E contexts for moraic partition, which is consistent with predictions
#However, same results with syllabic partition, suggesting this is a function of frequency distribution of contexts
#But effect is weaker for syllabic partition, so maybe we can say sth about this


#moraic partition SD
#The standard deviation should be smaller in Japanese contexts compared to English contexts
#moraic_nanka.full.lme = lmer(duration_sd ~ lang_pre * lang_post + (1 + lang_pre * lang_post | pair/speaker), data_duration_nanka_moraic_ana, REML = F)
#red model without IA does not converge
moraic_nanka.full.lme = lmer(duration_sd ~ lang_pre * lang_post + (1 | pair/speaker), data_duration_nanka_moraic_ana, REML = F)
summary(moraic_nanka.full.lme)
#reduce down
moraic_nanka.red1.lme = lmer(duration_sd ~ lang_pre + lang_post + (1  | pair/speaker), data_duration_nanka_moraic_ana, REML = F)
moraic_nanka.red2.lme = lmer(duration_sd ~ lang_pre + (1  | pair/speaker), data_duration_nanka_moraic_ana, REML = F)
moraic_nanka.red3.lme = lmer(duration_sd ~ 1 + (1  | pair/speaker), data_duration_nanka_moraic_ana, REML = F)
anova(moraic_nanka.full.lme,moraic_nanka.red1.lme, moraic_nanka.red2.lme, moraic_nanka.red3.lme)
#significant effect of all

#means of sd are consistent with prediction - however, might be a function of number of instances?
data_duration_nanka_moraic_ana %>%
  group_by(context) %>%
  summarize(meansd = mean(duration_sd, na.rm = T))

#syllabic partition SD
#The standard deviation should be smaller in English contexts compared to Japanese contexts
syllabic_nanka.full.lme = lmer(duration_sd ~ lang_pre * lang_post + (1 + lang_pre * lang_post | pair/speaker), data_duration_nanka_syllabic_ana, REML = F)
summary(syllabic_nanka.full.lme)
#take out IA
syllabic_nanka.red1.lme = lmer(duration_sd ~ lang_pre + lang_post + (1 + lang_pre * lang_post | pair/speaker), data_duration_nanka_syllabic_ana, REML = F)
syllabic_nanka.red2.lme = lmer(duration_sd ~ lang_pre + (1 + lang_pre * lang_post | pair/speaker), data_duration_nanka_syllabic_ana, REML = F)
syllabic_nanka.red3.lme = lmer(duration_sd ~ 1 + (1 + lang_pre * lang_post | pair/speaker), data_duration_nanka_syllabic_ana, REML = F)
anova(syllabic_nanka.full.lme,syllabic_nanka.red1.lme,syllabic_nanka.red2.lme,syllabic_nanka.red3.lme)
#marginally significant effect of IA, not after

#yeah so actually same pattern here suggesting this is more a function of sample size per cell.
#but at least stronger for Japanese!
data_duration_nanka_syllabic_ana %>%
  group_by(context) %>%
  summarize(meansd = mean(duration_sd, na.rm = T))


#ALTERNATIVE ANALYSIS DUE TO SMALL NUMBER OF CODE SWITCHING TOKENS
#use global percentage of time spoken one language as covariate
#here: percentage English by speaker/prompt/pair

#no significant effects

#moraic partition SD
#The standard deviation should be smaller in Japanese contexts compared to English contexts
#moraic_nanka_global.full.lme = lmer(duration_sd ~ eng_percent + (1 + eng_percent| pair/speaker), data_duration_nanka_moraic_ana, REML = F)
moraic_nanka_global.full.lme = lmer(duration_sd ~ eng_percent + (1 | pair/speaker), data_duration_nanka_moraic_ana, REML = F)
#red model without IA does not converge
summary(moraic_nanka_global.full.lme)
moraic_nanka_global.red1.lme = lmer(duration_sd ~ 1 + (1 | pair/speaker), data_duration_nanka_moraic_ana, REML = F)
anova(moraic_nanka_global.full.lme,moraic_nanka_global.red1.lme)
#ns

#syllabic partition SD
#The standard deviation should be smaller in English contexts compared to Japanese contexts
syllabic_nanka_global.full.lme = lmer(duration_sd ~ eng_percent + (1 + eng_percent | pair/speaker), data_duration_nanka_syllabic_ana, REML = F)
summary(syllabic_nanka_global.full.lme)
syllabic_nanka_global.red1.lme = lmer(duration_sd ~ 1 + (1 + eng_percent | pair/speaker), data_duration_nanka_syllabic_ana, REML = F)
anova(syllabic_nanka_global.full.lme,syllabic_nanka_global.red1.lme)
#ns
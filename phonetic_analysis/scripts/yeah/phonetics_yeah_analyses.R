## ORGANIZE DATA ####
# Duration
data_duration_yeah_ana = data_duration_yeah_figs
contrasts(data_duration_yeah_ana$lang_pre) = c(-1,1) 
contrasts(data_duration_yeah_ana$lang_post) = c(-1,1) 

data_duration_so_global_ana = data_duration_so_global_figs

#Formants
data_formants_yeah_eah_ana = data_formants_yeah_eah_figs
contrasts(data_formants_yeah_eah_ana$lang_pre) = c(-1,1) 
contrasts(data_formants_yeah_eah_ana$lang_post) = c(-1,1) 

data_formants_so_o_global_ana = data_formants_so_o_global_figs

##DESCRIPTIVES

#counts are similar for all measures analyzed
data_duration_yeah_ana %>%
  count(context) 

##ANALYZE DATA LIKE PREREGISTERED

#yeah duration
#There are no specific predictions for this analysis. The duration may be shorter in English if the final vowel takes on more of schwa quality.
#duration_yeah.full.lme = lmer(duration_yeah ~ lang_pre * lang_post + (1 +lang_pre * lang_post | pair/speaker), data_duration_yeah, REML = F )
#non-conv for reduced model
#duration_yeah.full.lme = lmer(duration_yeah ~ lang_pre * lang_post + (1 +lang_pre + lang_post | pair/speaker), data_duration_yeah, REML = F )
duration_yeah.full.lme = lmer(duration_yeah ~ lang_pre * lang_post + (1 | pair/speaker), data_duration_yeah, REML = F )
summary(duration_yeah.full.lme)
#take out IA
duration_yeah.red1.lme = lmer(duration_yeah ~ lang_pre + lang_post + (1 | pair/speaker), data_duration_yeah, REML = F )
summary(duration_yeah.red1.lme)
duration_yeah.red2.lme = lmer(duration_yeah ~  lang_post + (1 | pair/speaker), data_duration_yeah, REML = F )
duration_yeah.red3.lme = lmer(duration_yeah ~ 1 + (1 | pair/speaker), data_duration_yeah, REML = F )
anova(duration_yeah.full.lme,duration_yeah.red1.lme,duration_yeah.red2.lme,duration_yeah.red3.lme)
#no effect

#yeah formants
#We expect quality differences:  open central unrounded vowel in Japanese, near-open front unrounded vowel in English
formants_yeah_f1.full.lme = lmer(f1_norm_bark ~ lang_pre * lang_post  + (1 +lang_pre * lang_post | pair/speaker), data_formants_yeah_eah_ana, REML = F )
summary(formants_yeah_f1.full.lme)
formants_yeah_f1.red1.lme = lmer(f1_norm_bark ~ lang_pre + lang_post  + (1 +lang_pre * lang_post | pair/speaker), data_formants_yeah_eah_ana, REML = F )
anova(formants_yeah_f1.full.lme,formants_yeah_f1.red1.lme)
#ns (didn't reduce further down)

formants_yeah_f2.full.lme = lmer(f2_norm_bark ~ lang_pre * lang_post  + (1 +lang_pre * lang_post | pair/speaker), data_formants_yeah_eah_ana, REML = F )
summary(formants_yeah_f2.full.lme)
formants_yeah_f2.red1.lme = lmer(f2_norm_bark ~ lang_pre + lang_post  + (1 +lang_pre * lang_post | pair/speaker), data_formants_yeah_eah_ana, REML = F )
anova(formants_yeah_f2.full.lme,formants_yeah_f2.red1.lme)
#ns (didn't reduce further down)

#ALTERNATIVE ANALYSIS DUE TO SMALL NUMBER OF CODE SWITCHING TOKENS
#use global percentage of time spoken one language as covariate
#here: percentage English by speaker/prompt/pair

#yeah duration
#There are no specific predictions for this analysis. The duration may be shorter in English if the final vowel takes on more of schwa quality.
duration_yeah_global.full.lme = lmer(duration_yeah ~ eng_percent + (1 + eng_percent | pair/speaker), data_duration_yeah, REML = F )
summary(duration_yeah_global.full.lme)
duration_yeah_global.red1.lme = lmer(duration_yeah ~ 1 + (1 + eng_percent | pair/speaker), data_duration_yeah, REML = F )
summary(duration_yeah_global.full.lme)
anova(duration_yeah_global.full.lme,duration_yeah_global.red1.lme)
#ns


#yeah formants
#We expect quality differences:  open central unrounded vowel in Japanese, near-open front unrounded vowel in English
#formants_yeah_f1_global.full.lme = lmer(f1_norm_bark ~ eng_percent  + (1 + eng_percent | pair/speaker), data_formants_yeah_eah_ana, REML = F )
# red model 1 did not converge
formants_yeah_f1_global.full.lme = lmer(f1_norm_bark ~ eng_percent  + (1 | pair/speaker), data_formants_yeah_eah_ana, REML = F )
summary(formants_yeah_f1_global.full.lme)
formants_yeah_f1_global.red1.lme = lmer(f1_norm_bark ~ 1  + (1  | pair/speaker), data_formants_yeah_eah_ana, REML = F )
anova(formants_yeah_f1_global.full.lme,formants_yeah_f1_global.red1.lme)
#ns

#formants_yeah_f2_global.full.lme = lmer(f2_norm_bark ~ eng_percent  + (1 + eng_percent | pair/speaker), data_formants_yeah_eah_ana, REML = F )
formants_yeah_f2_global.full.lme = lmer(f2_norm_bark ~ eng_percent  + (1 | pair/speaker), data_formants_yeah_eah_ana, REML = F )
# red model did not converge
summary(formants_yeah_f2_global.full.lme)
formants_yeah_f2_global.red1.lme = lmer(f2_norm_bark ~ 1  + (1  | pair/speaker), data_formants_yeah_eah_ana, REML = F )
anova(formants_yeah_f2_global.full.lme,formants_yeah_f2_global.red1.lme)
#marginally significant: J-J context lower F2


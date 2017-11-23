## ORGANIZE DATA ####
# Duration
data_duration_so_ana = data_duration_so_figs
contrasts(data_duration_so_ana$lang_pre) = c(-1,1) 
contrasts(data_duration_so_ana$lang_post) = c(-1,1) 

#Formants
data_formants_so_o_ana = data_formants_so_o_figs
contrasts(data_formants_so_o_ana$lang_pre) = c(-1,1) 
contrasts(data_formants_so_o_ana$lang_post) = c(-1,1) 

##DESCRIPTIVES

#counts are similar for all measures analyzed
data_duration_so_ana %>%
  count(context) 

##ANALYZE DATA LIKE PREREGISTERED

#so duration
#The duration is expected to be longer in Japanese contexts than English contexts
duration_so.full.lme = lmer(duration_so ~ lang_pre * lang_post + (1 +lang_pre * lang_post | pair/speaker), data_duration_so, REML = F )
summary(duration_so.full.lme)
#take out IA
duration_so.red1.lme = lmer(duration_so ~ lang_pre + lang_post + (1 +lang_pre * lang_post | pair/speaker), data_duration_so, REML = F )
anova(duration_so.full.lme,duration_so.red1.lme)
#no effect

#so formants
#We expect a flat vowel sound in Japanese context, and a diphthongized vowel in English contexts
#first models did not converge
#formants_so_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post + (1 +lang_pre * lang_post | pair/speaker), data_formants_so_o_ana, REML = F )
#formants_so_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post + (1 +lang_pre + lang_post | pair/speaker), data_formants_so_o_ana, REML = F )
formants_so_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post + (1 | pair/speaker), data_formants_so_o_ana, REML = F )
summary(formants_so_f1.full.lme)
formants_so_f1.red1.lme = lmer(f1_norm_sum ~ lang_pre + lang_post + (1 | pair/speaker), data_formants_so_o_ana, REML = F )
anova(formants_so_f1.full.lme,formants_so_f1.red1.lme)

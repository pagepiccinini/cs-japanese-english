## ORGANIZE DATA ####
# Duration
data_duration_so_ana = data_duration_so_figs
contrasts(data_duration_so_ana$lang_pre) = c(-1,1) 
contrasts(data_duration_so_ana$lang_post) = c(-1,1) 

data_duration_so_global_ana = data_duration_so_global_figs

#Formants
data_formants_so_o_ana = data_formants_so_o_figs
contrasts(data_formants_so_o_ana$lang_pre) = c(-1,1) 
contrasts(data_formants_so_o_ana$lang_post) = c(-1,1) 

data_formants_so_o_global_ana = data_formants_so_o_global_figs

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
#formants_so_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post * percentage + (1 +lang_pre * lang_post * percentage | pair/speaker), data_formants_so_o_ana, REML = F )
#formants_so_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post * percentage + (1 +lang_pre + lang_post + percentage | pair/speaker), data_formants_so_o_ana, REML = F )
formants_so_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post * percentage + (1 | pair/speaker), data_formants_so_o_ana, REML = F )
summary(formants_so_f1.full.lme)
formants_so_f1.red1.lme = lmer(f1_norm_sum ~ lang_pre * lang_post + percentage + (1 | pair/speaker), data_formants_so_o_ana, REML = F )
formants_so_f1.red2.lme = lmer(f1_norm_sum ~ lang_pre + lang_post + percentage + (1 | pair/speaker), data_formants_so_o_ana, REML = F )
summary(formants_so_f1.red2.lme)
formants_so_f1.red3.lme = lmer(f1_norm_sum ~ lang_pre + lang_post  + (1 | pair/speaker), data_formants_so_o_ana, REML = F )
formants_so_f1.red4.lme = lmer(f1_norm_sum ~ lang_pre +  (1 | pair/speaker), data_formants_so_o_ana, REML = F )
formants_so_f1.red5.lme = lmer(f1_norm_sum ~ 1  + (1 | pair/speaker), data_formants_so_o_ana, REML = F )
anova(formants_so_f1.full.lme,formants_so_f1.red1.lme,formants_so_f1.red2.lme,formants_so_f1.red3.lme,formants_so_f1.red4.lme,formants_so_f1.red5.lme)
#sig IA with percentage
#sig IA of contexts
#sig main effect of percentage
#hmm significant but figures don't really show a clear pattern 

#formants_so_f2.full.lme = lmer(f2_norm_sum ~ lang_pre * lang_post * percentage + (1 +lang_pre * lang_post * percentage | pair/speaker), data_formants_so_o_ana, REML = F )
formants_so_f2.full.lme = lmer(f2_norm_sum ~ lang_pre * lang_post * percentage + (1 +lang_pre + lang_post + percentage | pair/speaker), data_formants_so_o_ana, REML = F )
summary(formants_so_f2.full.lme)
formants_so_f2.red1.lme = lmer(f2_norm_sum ~ lang_pre * lang_post + percentage + (1 +lang_pre + lang_post + percentage| pair/speaker), data_formants_so_o_ana, REML = F )
formants_so_f2.red2.lme = lmer(f2_norm_sum ~ lang_pre + lang_post + percentage + (1 +lang_pre + lang_post + percentage| pair/speaker), data_formants_so_o_ana, REML = F )
summary(formants_so_f2.red2.lme)
formants_so_f2.red3.lme = lmer(f2_norm_sum ~ lang_pre +  percentage + (1 +lang_pre + lang_post + percentage| pair/speaker), data_formants_so_o_ana, REML = F )
formants_so_f2.red4.lme = lmer(f2_norm_sum ~ lang_pre  + (1 +lang_pre + lang_post + percentage| pair/speaker), data_formants_so_o_ana, REML = F )
formants_so_f2.red5.lme = lmer(f2_norm_sum ~ 1 + (1 +lang_pre + lang_post + percentage| pair/speaker), data_formants_so_o_ana, REML = F )
anova(formants_so_f2.full.lme,formants_so_f2.red1.lme,formants_so_f2.red2.lme,formants_so_f2.red3.lme,formants_so_f2.red4.lme,formants_so_f2.red5.lme)
#sig IA with percentage
#sig IA between contexts
#again slightly hard to see patterns

#ALTERNATIVE ANALYSIS DUE TO SMALL NUMBER OF CODE SWITCHING TOKENS
#use global percentage of time spoken one language as covariate
#here: percentage English by speaker/prompt/pair

#so duration
#The duration is expected to be longer in Japanese contexts than English contexts
#duration_so_global.full.lme = lmer(duration_so ~ eng_percent + (1 +eng_percent | pair/speaker), data_duration_so_global_ana, REML = F )
duration_so_global.full.lme = lmer(duration_so ~ eng_percent + (1  | pair/speaker), data_duration_so_global_ana, REML = F )
summary(duration_so_global.full.lme)
duration_so_global.red1.lme = lmer(duration_so ~ 1+ (1  | pair/speaker), data_duration_so_global_ana, REML = F )
anova(duration_so_global.full.lme,duration_so_global.red1.lme)
#no effect

#so formants
#We expect a flat vowel sound in Japanese context, and a diphthongized vowel in English contexts
formants_so_f1_global.full.lme = lmer(f1_norm_sum ~ eng_percent * percentage + (1 +eng_percent * percentage | pair/speaker), data_formants_so_o_global_ana, REML = F )
summary(formants_so_f1_global.full.lme)
formants_so_f1_global.red1.lme = lmer(f1_norm_sum ~  eng_percent + percentage + (1 +eng_percent * percentage | pair/speaker), data_formants_so_o_global_ana, REML = F )
formants_so_f1_global.red2.lme = lmer(f1_norm_sum ~  percentage + (1 +eng_percent * percentage | pair/speaker), data_formants_so_o_global_ana, REML = F )
formants_so_f1_global.red3.lme = lmer(f1_norm_sum ~  1 + (1 +eng_percent * percentage | pair/speaker), data_formants_so_o_global_ana, REML = F )
anova(formants_so_f1_global.full.lme,formants_so_f1_global.red1.lme,formants_so_f1_global.red2.lme,formants_so_f1_global.red3.lme)
#ns although looks diff in fig

#formants_so_f2_global.full.lme = lmer(f2_norm_sum ~ eng_percent * percentage + (1 +eng_percent * percentage | pair/speaker), data_formants_so_o_global_ana, REML = F )
formants_so_f2_global.full.lme = lmer(f2_norm_sum ~ eng_percent * percentage + (1 +eng_percent + percentage | pair/speaker), data_formants_so_o_global_ana, REML = F )
summary(formants_so_f2_global.full.lme)
formants_so_f2_global.red1.lme = lmer(f2_norm_sum ~  eng_percent + percentage + (1 +eng_percent + percentage | pair/speaker), data_formants_so_o_global_ana, REML = F )
formants_so_f2_global.red2.lme = lmer(f2_norm_sum ~   percentage + (1 +eng_percent + percentage | pair/speaker), data_formants_so_o_global_ana, REML = F )
formants_so_f2_global.red3.lme = lmer(f2_norm_sum ~  1 + (1 +eng_percent + percentage | pair/speaker), data_formants_so_o_global_ana, REML = F )
anova(formants_so_f2_global.full.lme,formants_so_f2_global.red1.lme,formants_so_f2_global.red2.lme,formants_so_f2_global.red3.lme)
#ns although looks diff in fig

## ORGANIZE DATA

#Duration and presence
data_duration_like_lai_ana = data_duration_like_lai_figs 
contrasts(data_duration_like_lai_ana$lang_pre) = c(-1,1) 
contrasts(data_duration_like_lai_ana$lang_post) = c(-1,1) 

data_presence_like_kburst_ana = data_duration_like_kburst_figs 
contrasts(data_presence_like_kburst_ana$lang_pre) = c(-1,1) 
contrasts(data_presence_like_kburst_ana$lang_post) = c(-1,1) 

#Formants
data_formants_like_lai_ana = data_formants_like_lai_figs 
contrasts(data_formants_like_lai_ana$lang_pre) = c(-1,1)
contrasts(data_formants_like_lai_ana$lang_post) = c(-1,1)

##DESCRIPTIVES

#counts are similar for all measures analyzed
data_duration_like_lai_ana %>%
  count(context) 



##ANALYZE DATA

#[lai] duration
#this first model fails to converge
#duration_like_lai.full.lme = lmer(duration_lai ~ lang_pre * lang_post + (1 +lang_pre * lang_post | pair/speaker), data_duration_like_lai )
#I am here resorting to reduced models but we could also try other optimizers etc
#this second model works, but interestingly, it fails to converge for the reduced model without IAS
#duration_like_lai.full.lme = lmer(duration_lai ~ lang_pre * lang_post + (1 +lang_pre + lang_post | pair/speaker), data_duration_like_lai )
#resorting to very simple RE structure for now
duration_like_lai.full.lme = lmer(duration_lai ~ lang_pre * lang_post + (1 | pair/speaker), data_duration_like_lai )
summary(duration_like_lai.full.lme)
#taking away IA to check for significance
duration_like_lai.red1.lme = lmer(duration_lai ~ lang_pre + lang_post + (1  | pair/speaker), data_duration_like_lai )
anova(duration_like_lai.full.lme,duration_like_lai.red1.lme)

#[lai] formants
#f1
#this first model fails to converge
#formants_like_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre * lang_post * percentage | pair/speaker),data_formants_like_lai_ana)
formants_like_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana)
summary(formants_like_f1.full.lme)
#dropping percentage to check for significance
formants_like_f1.red1.lme = lmer(f1_norm_sum ~ lang_pre * lang_post +(1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana)
anova(formants_like_f1.full.lme,formants_like_f1.red1.lme)

#f2
#first two models fail to converge
#formants_like_f2.full.lme = lmer(f2_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre * lang_post * percentage | pair/speaker),data_formants_like_lai_ana)
#formants_like_f2.full.lme = lmer(f2_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana)
formants_like_f2.full.lme = lmer(f2_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre + lang_post | pair/speaker),data_formants_like_lai_ana)
summary(formants_like_f2.full.lme)
formants_like_f2.red1.lme = lmer(f2_norm_sum ~ lang_pre * lang_post  +(1 + lang_pre + lang_post | pair/speaker),data_formants_like_lai_ana)
anova(formants_like_f2.full.lme,formants_like_f2.red1.lme)


#[k-burst] presence

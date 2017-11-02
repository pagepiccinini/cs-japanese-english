## ORGANIZE DATA
#problem: with reduced RE structure more gets significant in general - how to compare??

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

data_formants_like_lai_ana_global = data_formants_like_lai_figs_global 


##DESCRIPTIVES

#counts are similar for all measures analyzed
data_duration_like_lai_ana %>%
  count(context) 

##ANALYZE DATA LIKE PREREGISTERED

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

#haven't done closure & burst presence anas
#[k-burst] presence

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


#ALTERNATIVE ANALYSIS DUE TO SMALL NUMBER OF CODE SWITCHING TOKENS
#use global percentage of time spoken one language as covariate
#here: percentage English by speaker/prompt/pair

#[lai] duration
duration_like_lai.full.lme = lmer(duration_lai ~ eng_percent + (1 +eng_percent | pair/speaker), data_duration_like_lai, REML=FALSE)
duration_like_lai.red1.lme = lmer(duration_lai ~ 1+ (1 +eng_percent | pair/speaker), data_duration_like_lai, REML=FALSE)
anova(duration_like_lai.full.lme,duration_like_lai.red1.lme)

#[lai] formants
#f1
#this first model fails to converge
#formants_like_f1.full.lme = lmer(f1_norm_sum ~ eng_percent * percentage +(1 + eng_percent * percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f1.full.lme = lmer(f1_norm_sum ~ eng_percent * percentage +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
summary(formants_like_f1.full.lme)
#dropping IA to check for significance
formants_like_f1.red1.lme = lmer(f1_norm_sum ~ eng_percent + percentage +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
anova(formants_like_f1.full.lme,formants_like_f1.red1.lme) #no difference
#dropping percentage into vowel next
formants_like_f1.red2.lme = lmer(f1_norm_sum ~ eng_percent +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
#red2 does not converge, so redo the whole bunch with reduced RE structure
formants_like_f1.full.lme = lmer(f1_norm_sum ~ eng_percent * percentage +(1 | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f1.red1.lme = lmer(f1_norm_sum ~ eng_percent + percentage +(1 | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f1.red2.lme = lmer(f1_norm_sum ~ eng_percent +(1 | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f1.red3.lme = lmer(f1_norm_sum ~ 1+(1 | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)

anova(formants_like_f1.full.lme,formants_like_f1.red1.lme,formants_like_f1.red2.lme,formants_like_f1.red3.lme) #marginally significant
#first two changes significant

#[lai] formants
#f2
#this first model fails to converge
#formants_like_f2.full.lme = lmer(f2_norm_sum ~ eng_percent * percentage +(1 + eng_percent * percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f2.full.lme = lmer(f2_norm_sum ~ eng_percent * percentage +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
summary(formants_like_f2.full.lme)
#dropping factors to check for significance
formants_like_f2.red1.lme = lmer(f2_norm_sum ~ eng_percent + percentage +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f2.red2.lme = lmer(f2_norm_sum ~ eng_percent +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f2.red3.lme = lmer(f2_norm_sum ~ 1 +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
anova(formants_like_f2.full.lme,formants_like_f2.red1.lme,formants_like_f2.red2.lme,formants_like_f2.red3.lme) #significant
#first tow changes significant


k]-closure presence–Measurement:Each token will be coded for whether the closure for the [k] is present or not.–Prediction:The closure is predicted to be more likely in Japanese contexts than English contexts,as final stops are often unreleased in English (Lisker 1999).–Statistical analysis:glmer(k_closure_presence ~ language_proceeding * language_following +(1 + langauge_proceeding * language_following | dyad/speaker), family = “binomial”)•[k]-burst presence–Measurement:Each token will be coded for whether the burst for the [k] is present or not.–Prediction:The burst is predicted to be more likely in Japanese contexts than English contexts,as final stops are often unreleased in English (Lisker 1999).–Statistical analysis:glmer(k_burst_presence ~ language_proceeding * language_following +(1 + langauge_proceeding * language_following | dyad/speaker), family = “binomial”)•[k]-burst duratio
## ORGANIZE DATA
#problem: with reduced RE structure more gets significant in general - how to compare??

#Duration and presence
data_duration_like_lai_ana = data_duration_like_lai_figs 
contrasts(data_duration_like_lai_ana$lang_pre) = c(-1,1) 
contrasts(data_duration_like_lai_ana$lang_post) = c(-1,1) 

data_presence_like_kclosure_ana = data_duration_like_kclosure
contrasts(data_presence_like_kburst_ana$lang_pre) = c(-1,1) 
contrasts(data_presence_like_kburst_ana$lang_post) = c(-1,1) 

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

##1. ANALYZE DATA LIKE PREREGISTERED
#not used in the end because of scarcity of switch words
#put in SI?

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
#The duration is expected to be longer in Japanese contexts than English contexts
duration_like_lai.full.lme = lmer(duration_lai ~ eng_percent + (1 +eng_percent | pair/speaker), data_duration_like_lai, REML=FALSE)
duration_like_lai.red1.lme = lmer(duration_lai ~ 1+ (1 +eng_percent | pair/speaker), data_duration_like_lai, REML=FALSE)
anova(duration_like_lai.full.lme,duration_like_lai.red1.lme) 
#no effect

#[lai] formants
#We predict quality differences early on (during the [l]) as a postalveolar flap will be used in Japanese contexts
#and an alveolar lateral approximant in English contexts, as Japanese lacks the lateral approximant [@Okada1991].
#There will also be differences later on in the production, as English uses the lower and more central [\textipa{I}] vowel
#while Japanese will use the higher and more fronted [i] vowel.

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

anova(formants_like_f1.full.lme,formants_like_f1.red1.lme,formants_like_f1.red2.lme,formants_like_f1.red3.lme) 
#first two changes significant; i.e IA language%/%into vowel & main effect %into vowel
#figure suggests rather inconsistent differences (i.e., low and high english percentage kind of overlap)
#follow-up:analyse high & low % into vowel
formants_like_f1.02.full.lme = lmer(f1_norm_sum ~ eng_percent+(1 | pair/speaker),data_formants_like_lai_ana_global[data_formants_like_lai_ana_global$percentage < 0.3,], REML=FALSE)
summary(formants_like_f1.02.full.lme)
formants_like_f1.08.full.lme = lmer(f1_norm_sum ~ eng_percent+(1 | pair/speaker),data_formants_like_lai_ana_global[data_formants_like_lai_ana_global$percentage > 0.7,], REML=FALSE)
summary(formants_like_f1.08.full.lme)
#follow-up does not really show anything
#(this is one of the cases where model with fuller RE structure did not lead to diff between full & red.1, but lesser RE structure did)

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
#first two changes significant; i.e IA language%/%into vowel & main effect %into vowel
#figure again shows overlap between high and low english percentage

#[k]-closure presence
#The closure is predicted to be more likely in Japanese contexts than English contexts
k_closure_presence_like.full.lme = glmer(presence ~ eng_percent +(1 + eng_percent | pair/speaker), data_presence_like_kclosure_ana, family = binomial)
summary(k_closure_presence_like.full.lme)
k_closure_presence_like.red1.lme = glmer(presence ~ 1 +(1 + eng_percent | pair/speaker), data_presence_like_kclosure_ana, family = binomial)
anova(k_closure_presence_like.full.lme,k_closure_presence_like.red1.lme)
#no effect

#[k]-burst presence
#The burst is predicted to be more likely in Japanese contexts than English contexts
#nc k_burst_like.full.lme = glmer(presence ~ eng_percent +(1 + eng_percent | pair/speaker), data_presence_like_kburst_ana, family = binomial)
k_burst_presence_like.full.lme = glmer(presence ~ eng_percent +(1 | pair/speaker), data_presence_like_kburst_ana, family = binomial)
summary(k_burst_presence_like.full.lme)
k_burst_presence_like.red1.lme = glmer(presence ~ 1 +(1 | pair/speaker), data_presence_like_kburst_ana, family = binomial)
anova(k_burst_presence_like.full.lme,k_burst_presence_like.red1.lme)
#no effect

#[k]-burst duration
#Japanese tokens are predicted to have a shorter duration
k_burst_duration_like.full.lme = lmer(duration ~ eng_percent +( 1+ eng_percent| pair/speaker), data_presence_like_kburst_ana, REML=FALSE)
summary(k_burst_duration_like.full.lme)
k_burst_duration_like.red1.lme = lmer(duration ~ 1 +( 1+ eng_percent| pair/speaker), data_presence_like_kburst_ana, REML=FALSE)
anova(k_burst_duration_like.full.lme,k_burst_duration_like.red1.lme)
#marginally significant

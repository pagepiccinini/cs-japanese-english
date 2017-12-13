## ORGANIZE DATA
#problem: with reduced RE structure more gets significant in general - how to compare??





##DESCRIPTIVES

#very unequal distribution of counts
data_duration_like_lai_ana %>%
  count(context) 

##1. ANALYZE DATA LIKE PREREGISTERED
#not used in the end because of scarcity of switch words
#put in SI?
#Predictions are confirmed for [lai] duration. 
#We do find differences in F2 based on post_lang (not sure whether the direction makes sense).
#No other predictions (related to burst/closure) were confirmed.

#[lai] duration
#The duration is expected to be longer in Japanese contexts than English contexts
#this first model fails to converge
#duration_like_lai.full.lme = lmer(duration_lai ~ lang_pre * lang_post + (1 +lang_pre * lang_post | pair/speaker), data_duration_like_lai_ana, REML=F  )
#second model does not converge for red model
#duration_like_lai.full.lme = lmer(duration_lai ~ lang_pre * lang_post + (1 +lang_pre + lang_post | pair/speaker), data_duration_like_lai_ana, REML=F  )
duration_like_lai.full.lme = lmer(duration_lai ~ lang_pre * lang_post + (1 | pair/speaker), data_duration_like_lai_ana, REML=F )
summary(duration_like_lai.full.lme)
#taking away IA to check for significance
duration_like_lai.red1.lme = lmer(duration_lai ~ lang_pre + lang_post + (1  | pair/speaker), data_duration_like_lai_ana, REML=F  )
anova(duration_like_lai.full.lme,duration_like_lai.red1.lme)
#significant effect: Japanese-Japanese context most long (like predicted)

data_duration_like_lai_ana  %>%
  group_by(context) %>%
  summarize(mean = mean(duration_lai, na.rm = T))

#[lai] formants
#We predict quality differences early on and later
#f1
#this first model fails to converge
#formants_like_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre * lang_post * percentage | pair/speaker),data_formants_like_lai_ana, REML = F)
formants_like_f1.full.lme = lmer(f1_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana, REML = F)
summary(formants_like_f1.full.lme)
formants_like_f1.red1.lme = lmer(f1_norm_sum ~ lang_pre * lang_post + percentage + (1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana, REML = F)
formants_like_f1.red2.lme = lmer(f1_norm_sum ~ lang_pre + lang_post + percentage + (1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana, REML = F)
summary(formants_like_f1.red2.lme)
formants_like_f1.red3.lme = lmer(f1_norm_sum ~ lang_pre + lang_post + (1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana, REML = F)
summary(formants_like_f1.red3.lme)
formants_like_f1.red4.lme = lmer(f1_norm_sum ~ lang_post + (1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana, REML = F)
formants_like_f1.red5.lme = lmer(f1_norm_sum ~ 1 + (1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana, REML = F)
anova(formants_like_f1.full.lme,formants_like_f1.red1.lme,formants_like_f1.red2.lme,formants_like_f1.red3.lme,formants_like_f1.red4.lme,formants_like_f1.red5.lme)
#significant main effect of percentage into vowel sound (decreasing F1)


#f2
#first two models fail to converge
#formants_like_f2.full.lme = lmer(f2_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre * lang_post * percentage | pair/speaker),data_formants_like_lai_ana, , REML = F)
#formants_like_f2.full.lme = lmer(f2_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre + lang_post + percentage | pair/speaker),data_formants_like_lai_ana, , REML = F)
formants_like_f2.full.lme = lmer(f2_norm_sum ~ lang_pre * lang_post * percentage +(1 + lang_pre + lang_post | pair/speaker),data_formants_like_lai_ana, REML = F)
summary(formants_like_f2.full.lme)
formants_like_f2.red1.lme = lmer(f2_norm_sum ~ lang_pre * lang_post + percentage  +(1 + lang_pre + lang_post | pair/speaker),data_formants_like_lai_ana, REML = F)
formants_like_f2.red2.lme = lmer(f2_norm_sum ~ lang_pre + lang_post + percentage  +(1 + lang_pre + lang_post | pair/speaker),data_formants_like_lai_ana, REML = F)
formants_like_f2.red3.lme = lmer(f2_norm_sum ~ lang_pre + lang_post   +(1 + lang_pre + lang_post | pair/speaker),data_formants_like_lai_ana, REML = F)
summary(formants_like_f2.red3.lme)
formants_like_f2.red4.lme = lmer(f2_norm_sum ~ lang_pre    +(1 + lang_pre + lang_post | pair/speaker),data_formants_like_lai_ana, REML = F)
formants_like_f2.red5.lme = lmer(f2_norm_sum ~ 1  +(1 + lang_pre + lang_post | pair/speaker),data_formants_like_lai_ana, REML = F)
anova(formants_like_f2.full.lme,formants_like_f2.red1.lme,formants_like_f2.red2.lme,formants_like_f2.red3.lme,formants_like_f2.red4.lme,formants_like_f2.red5.lme)
#significant main effect of percentage into vowel sound (increasing F2)
#sign main effect of lang_post: higher F2 in English contexts (does that make sense?)
data_formants_like_lai_ana  %>%
  group_by(lang_post) %>%
  summarize(mean = mean(f2_norm_sum, na.rm = T))


#[k]-closure presence
#The closure is predicted to be more likely in Japanese contexts than English contexts
#k_closure_presence_like.full.lme = glmer(presence ~ lang_pre * lang_post +(1 + lang_pre * lang_post | pair/speaker), data_presence_like_kclosure_ana, family = binomial)
#k_closure_presence_like.full.lme = glmer(presence ~ lang_pre * lang_post +(1 + lang_pre + lang_post | pair/speaker), data_presence_like_kclosure_ana, family = binomial)
k_closure_presence_like.full.lme = glmer(presence ~ lang_pre * lang_post +(1  | pair/speaker), data_presence_like_kclosure_ana, family = binomial)
summary(k_closure_presence_like.full.lme)
k_closure_presence_like.red1.lme = glmer(presence ~ lang_pre + lang_post +(1  | pair/speaker), data_presence_like_kclosure_ana, family = binomial)
k_closure_presence_like.red2.lme = glmer(presence ~ 1 +(1  | pair/speaker), data_presence_like_kclosure_ana, family = binomial)
anova(k_closure_presence_like.full.lme,k_closure_presence_like.red1.lme,k_closure_presence_like.red2.lme)
#no effect

#[k]-burst presence
#The burst is predicted to be more likely in Japanese contexts than English contexts
#k_burst_presence_like.full.lme = glmer(presence ~ lang_pre * lang_post +(lang_pre * lang_post | pair/speaker), data_presence_like_kburst_ana, family = binomial)
#k_burst_presence_like.full.lme = glmer(presence ~ lang_pre * lang_post +(lang_pre + lang_post | pair/speaker), data_presence_like_kburst_ana, family = binomial)
#k_burst_presence_like.full.lme = glmer(presence ~ lang_pre * lang_post +(1 | pair/speaker), data_presence_like_kburst_ana, family = binomial)
k_burst_presence_like.full.lme = glmer(presence ~ lang_pre * lang_post +(1 | pair), data_presence_like_kburst_ana, family = binomial)
summary(k_burst_presence_like.full.lme)
k_burst_presence_like.red1.lme = glmer(presence ~ lang_pre + lang_post +(1 | pair), data_presence_like_kburst_ana, family = binomial)
k_burst_presence_like.red2.lme = glmer(presence ~ 1 +(1 | pair), data_presence_like_kburst_ana, family = binomial)
anova(k_burst_presence_like.full.lme,k_burst_presence_like.red1.lme,k_burst_presence_like.red2.lme)
#no effect

#[k]-burst duration
#Japanese tokens are predicted to have a shorter duration
k_burst_duration_like.full.lme = lmer(duration ~ lang_pre * lang_post +( 1+ lang_pre * lang_post| pair/speaker), data_presence_like_kburst_ana, REML=FALSE)
summary(k_burst_duration_like.full.lme)
k_burst_duration_like.red1.lme = lmer(duration ~ lang_pre + lang_post +( 1+ lang_pre * lang_post| pair/speaker), data_presence_like_kburst_ana, REML=FALSE)
k_burst_duration_like.red2.lme = lmer(duration ~ 1 +( 1+ lang_pre * lang_post| pair/speaker), data_presence_like_kburst_ana, REML=FALSE)
anova(k_burst_duration_like.full.lme,k_burst_duration_like.red1.lme,k_burst_duration_like.red2.lme)
#no effect


#ALTERNATIVE ANALYSIS DUE TO SMALL NUMBER OF CODE SWITCHING TOKENS
#use global percentage of time spoken one language as covariate
#here: percentage English by speaker/prompt/pair
#These analyses show some significant effects for formants; however, they do not seem very consistent wrt the speech context.

#[lai] duration
#The duration is expected to be longer in Japanese contexts than English contexts
duration_like_lai_global.full.lme = lmer(duration_lai ~ eng_percent + (1 +eng_percent | pair/speaker), data_duration_like_lai, REML=FALSE)
duration_like_lai_global.red1.lme = lmer(duration_lai ~ 1+ (1 +eng_percent | pair/speaker), data_duration_like_lai, REML=FALSE)
anova(duration_like_lai_global.full.lme,duration_like_lai_global.red1.lme) 
#no effect

#[lai] formants
#We predict quality differences early on (during the [l]) as a postalveolar flap will be used in Japanese contexts
#and an alveolar lateral approximant in English contexts, as Japanese lacks the lateral approximant [@Okada1991].
#There will also be differences later on in the production, as English uses the lower and more central [\textipa{I}] vowel
#while Japanese will use the higher and more fronted [i] vowel.
#f1
#formants_like_f1.full.lme = lmer(f1_norm_sum ~ eng_percent * percentage +(1 + eng_percent * percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
#fails to converge for red 2 model
#formants_like_f1_global.full.lme = lmer(f1_norm_sum ~ eng_percent * percentage +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
#formants_like_f1_global.full.lme = lmer(f1_norm_sum ~ eng_percent * percentage +(1 + eng_percent  | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f1_global.full.lme = lmer(f1_norm_sum ~ eng_percent * percentage +(1 | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
summary(formants_like_f1_global.full.lme)
formants_like_f1_global.red1.lme = lmer(f1_norm_sum ~ eng_percent + percentage +(1  | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
summary(formants_like_f1_global.red1.lme)
formants_like_f1_global.red2.lme = lmer(f1_norm_sum ~ eng_percent +(1 | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f1_global.red3.lme = lmer(f1_norm_sum ~ 1 +(1 | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
anova(formants_like_f1_global.full.lme,formants_like_f1_global.red1.lme,formants_like_f1_global.red2.lme,formants_like_f1_global.red3.lme) #no difference
#significant IA effect
#significant main effect of percentage into vowel
#figure suggests rather inconsistent differences (i.e., the lowes and highest english percentages kind of overlap)

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
formants_like_f2_global.full.lme = lmer(f2_norm_sum ~ eng_percent * percentage +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
summary(formants_like_f2.full.lme)
#dropping factors to check for significance
formants_like_f2_global.red1.lme = lmer(f2_norm_sum ~ eng_percent + percentage +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
summary(formants_like_f2_global.red1.lme)
formants_like_f2_global.red2.lme = lmer(f2_norm_sum ~ eng_percent +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
formants_like_f2_global.red3.lme = lmer(f2_norm_sum ~ 1 +(1 + eng_percent + percentage | pair/speaker),data_formants_like_lai_ana_global, REML=FALSE)
anova(formants_like_f2_global.full.lme,formants_like_f2_global.red1.lme,formants_like_f2_global.red2.lme,formants_like_f2_global.red3.lme) 
#significant IA effect
#significant main effect of percentage into vowel
#figure again shows overlap between high and low english percentage

#[k]-closure presence
#The closure is predicted to be more likely in Japanese contexts than English contexts
k_closure_presence_like_global.full.lme = glmer(presence ~ eng_percent +(1 + eng_percent | pair/speaker), data_presence_like_kclosure_ana, family = binomial)
summary(k_closure_presence_like.full.lme)
k_closure_presence_like_global.red1.lme = glmer(presence ~ 1 +(1 + eng_percent | pair/speaker), data_presence_like_kclosure_ana, family = binomial)
anova(k_closure_presence_like_global.full.lme,k_closure_presence_like_global.red1.lme)
#no effect

#[k]-burst presence
#The burst is predicted to be more likely in Japanese contexts than English contexts
#nc k_burst_like.full.lme = glmer(presence ~ eng_percent +(1 + eng_percent | pair/speaker), data_presence_like_kburst_ana, family = binomial)
k_burst_presence_like_global.full.lme = glmer(presence ~ eng_percent +(1 | pair/speaker), data_presence_like_kburst_ana, family = binomial)
summary(k_burst_presence_like_global.full.lme)
k_burst_presence_like_global.red1.lme = glmer(presence ~ 1 +(1 | pair/speaker), data_presence_like_kburst_ana, family = binomial)
anova(k_burst_presence_like_global.full.lme,k_burst_presence_like_global.red1.lme)
#no effect

#[k]-burst duration
#Japanese tokens are predicted to have a shorter duration
k_burst_duration_like_global.full.lme = lmer(duration ~ eng_percent +( 1+ eng_percent| pair/speaker), data_presence_like_kburst_ana, REML=FALSE)
summary(k_burst_duration_like_global.full.lme)
k_burst_duration_like_global.red1.lme = lmer(duration ~ 1 +( 1+ eng_percent| pair/speaker), data_presence_like_kburst_ana, REML=FALSE)
anova(k_burst_duration_like_global.full.lme,k_burst_duration_like_global.red1.lme)
#marginally significant; actually slightly longer duration for Japanese
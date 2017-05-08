## LOAD PACKAGES ####
source("scripts/cs_japanese_english_packages.R")


## LOAD FUNCTIONS ####
source("scripts/cs_japanese_english_functions.R")


## EXTRACT TEXTGRIDS TO TEXTFILES ####
source("scripts/cs_japanese_english_extraction.R")


## MAIN ANALYSIS SCRIPTS ####
source("scripts/cs_japanese_english_cleaning.R")
source("scripts/cs_japanese_english_figures.R")


## RUN EXTRA ANALYSIS SCRIPTS ####
source("scripts/analyses/cs_japanese_english_analysis_discmark.R")


## GET DATA FOR SPECIFIC SPEAKER ####
# Set file parameters
file = "e06_s1_seg"
#speaker = paste(group, "01", sep="_")
#speaker_gender = "male"
#min_hz = if_else(speaker_gender == "female", 100, 50)
#max_hz = if_else(speaker_gender == "female", 400, 350)


## PULL OUT PHONETIC DATA SCRIPTS ####
source("scripts/analyses/cs_japanese_english_analysis_phonetics.R")

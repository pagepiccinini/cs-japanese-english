## LOAD PACKAGES ####
source("scripts/cs_japanese_english_packages.R")


## LOAD FUNCTIONS ####
source("scripts/cs_japanese_english_functions.R")


## GET AND ANALYZE WORD TOKEN DATA ####
# Extract TextGrids to textfile
source("scripts/words/cs_japanese_english_words_extraction.R")

# Clean textfiles of words
source("scripts/words/cs_japanese_english_words_cleaning.R")

# Make figures of word token data
source("scripts/words/cs_japanese_english_words_figures.R")

# Run analysis on counts for specific disourse markers
source("scripts/words/cs_japanese_english_words_discmark.R")


## GET DATA FOR SPECIFIC SPEAKER ####
# Set file parameters
file = "e06_s1_seg"
#speaker = paste(group, "01", sep="_")
#speaker_gender = "male"
#min_hz = if_else(speaker_gender == "female", 100, 50)
#max_hz = if_else(speaker_gender == "female", 400, 350)


## PULL OUT PHONETIC DATA SCRIPTS ####
source("scripts/analyses/cs_japanese_english_analysis_phonetics.R")

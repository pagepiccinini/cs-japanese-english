## LOAD PACKAGES ####
source("phonetic_analysis/scripts/phonetics_packages.R")


## LOAD FUNCTIONS ####
source("phonetic_analysis/scripts/phonetics_functions.R")


## READ IN PHONETIC DATA ####
source("phonetic_analysis/scripts/phonetics_cleaning.R")

##READ IN WORD PERCENTAGE COUNTS
data_word_count = read.table("word_analysis/data/generated/wordcounts.txt",header = T)

## RUN WORD SPECIFIC ANALYSES ####
# Run analysis "like"
source("phonetic_analysis/scripts/like/phonetics_like_cleaning.R")
source("phonetic_analysis/scripts/like/phonetics_like_figures.R")

# Run analysis for "yeah"
source("phonetic_analysis/scripts/yeah/phonetics_yeah_cleaning.R")
source("phonetic_analysis/scripts/yeah/phonetics_yeah_figures.R")

# Run analysis for "so"
source("phonetic_analysis/scripts/so/phonetics_so_cleaning.R")
source("phonetic_analysis/scripts/so/phonetics_so_figures.R")

# Run analysis for nanka
source("phonetic_analysis/scripts/nanka/phonetics_nanka_cleaning.R")
source("phonetic_analysis/scripts/nanka/phonetics_nanka_figures.R")


## SAVE IMAGE OF DATA ####
save.image("phonetic_analysis/environment.RData")

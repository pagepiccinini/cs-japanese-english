## LOAD PACKAGES ####
source("word_analysis/scripts/words_packages.R")


## LOAD FUNCTIONS ####
source("word_analysis/scripts/words_functions.R")


## EXTRACT AND CLEAN WORD TOKEN DATA ####
# Extract TextGrids to textfile
source("word_analysis/scripts/words_extraction.R")

# Clean textfiles of words
source("word_analysis/scripts/words_cleaning.R")


## MAKE FIGURES AND ANALYZE DATA ####
# Make figures of word token data
source("word_analysis/scripts/words_figures.R")

# Run analysis on counts for specific disourse markers
source("word_analysis/scripts/words_discmark.R")



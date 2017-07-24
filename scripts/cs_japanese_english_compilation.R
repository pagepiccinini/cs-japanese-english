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


## EXTRACT PHONETIC DATA ####
# Set parameters for a specific speaker
pair = "e06"
prompt = "a00"
speaker = "s1"
speaker_gender = "female"

# Set the parameters for the formant analysis
max_formant = if_else(speaker_gender == "female", 5500, 4000)

formant_arguments = list(0.001,        # Time step (s)
                         5,            # Max. number of formants
                         max_formant,  # Maximum formant (Hz) <- change to be based on gender
                         0.025,        # Window length (s)
                         50)           # Pre-emphasis from (Hz)


# Pull out phonetic data
source("scripts/phonetics/cs_japanese_english_phonetics_extraction.R")


## READ IN AND ANALYZE PHONETIC DATA ####
# Read in all phonetic data
source("scripts/phonetics/cs_japanese_english_phonetics_cleaning.R")

# Run analysis "like"
source("scripts/phonetics/like/cs_japanese_english_phonetics_like_cleaning.R")
source("scripts/phonetics/like/cs_japanese_english_phonetics_like_figures.R")

# Run analysis for "yeah"
source("scripts/phonetics/yeah/cs_japanese_english_phonetics_yeah_cleaning.R")
source("scripts/phonetics/yeah/cs_japanese_english_phonetics_yeah_figures.R")

# Run analysis for "so"
source("scripts/phonetics/cs_japanese_english_phonetics_so.R")

# Run analysis for nanka
source("scripts/phonetics/cs_japanese_english_phonetics_nanka.R")


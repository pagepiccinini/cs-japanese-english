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


## GET AND ANALYZE PHONETIC DATA ####
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

# Read in all phonetic data
source("scripts/phonetics/cs_japanese_english_phonetics_cleaning.R")

# Run analysis for specific words
source("scripts/phonetics/cs_japanese_english_phonetics_like.R")
source("scripts/phonetics/cs_japanese_english_phonetics_yeah.R")
source("scripts/phonetics/cs_japanese_english_phonetics_so.R")
source("scripts/phonetics/cs_japanese_english_phonetics_nanka.R")


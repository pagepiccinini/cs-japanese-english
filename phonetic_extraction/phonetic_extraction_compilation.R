## LOAD PACKAGES ####
source("phonetic_extraction/scripts/phonetic_extraction_packages.R")


## LOAD FUNCTIONS ####
source("phonetic_extraction/scripts/phonetic_extraction_functions.R")


## SET PARAMETERS ####
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
                         0.001,        # Window length (s)
                         50)           # Pre-emphasis from (Hz)


## EXTRACT AND SAVE PHONETIC DATA ####
source("phonetic_extraction/scripts/phonetic_extraction_extraction.R")


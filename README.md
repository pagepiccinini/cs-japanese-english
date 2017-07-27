# Introduction

This repository includes three main directories:

* Word extraction and analysis (word_analysis)
* Phonetic extraction (phonetic_extraction)
* Phonetic analysis (phonetic_analysis)

Each contains a compilation script for running the accompanying set of scripts. Details about what each directory does is included below. There is also a fourth `write_up` directory for any pre-registrations, reports, or final papers.

# Word extraction and analysis

This directory includes scripts for extracting word and phoneme counts for TextGrids of annotated conversations. To run the scripts be sure to have all of your TextGrids in the `word_analysis/data/textgrids` folder. A given TextGrid should have 4 tiers, two for each speaker one for each language.

Run the code in the `word_analysis/words_compilations.R` script. This will output a series of text files to the `word_analysis/data/textfiles` folder with a script of the conversation. It will also create a series of figures in the `word_analysis/figures` folder.

# Phonetic extraction

This directory is used to extract the phonetics (duration and formants) of all segmented sounds. To run the scripts have all of your recordings, one for each speaker as a mono recording, in the `phonetic_extraction/data/recordings` folder. You should also have one TextGrid per speaker in the `phonetic_extraction/data/textgrids` folder; it will have a single tier of segmentations.

Open the `phonetic_extraction/phonetic_extraction_compilation.R` script. You will have to run it once per speaker and conversation. First, set the speaker specific parameters, `pair`, `prompt`, `speaker`, and `speaker_gender`. Then run the rest of the lines in the script. Repeat this for each speaker / conversation. The data will be saved to a folder in the phonetic analysis part of the project.

# Phonetic analysis

This directory does the phonetic analysis of the data. All data is already saved. All you need to do is open the `phonetic_analysis/phonetics_compilation.R` script and run the code. This will result in a series of figures in the `phonetic_analysis/figures` folder. There are several subfolders, one for each of the four analyses being conducted.
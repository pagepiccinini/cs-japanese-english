# Make table from TextGrid for phonetics
table_to_textgrid_phonetics = function(inputfile, outputfile) {
  praat( "Down to Table...",
         arguments = list( TRUE, # Include line number
                           6,    # Time decimals
                           TRUE, # Include tier names
                           FALSE  # Include empty intervals
         ), # End list()
         input = inputfile,
         output = paste(getwd(), "/phonetic_analysis/data/", outputfile, ".txt", sep = ""),
         filetype = "tab-separated"
  )
}

# Get formant information from a sound file
formants_extracter = function(filename, wavfile, durationfile, line, formant_arguments) {
  
  # Get variables ready
  curr_file <<- filename
  curr_start <<- durationfile$tmin[line]
  curr_phoneme <<- durationfile$sound[line]
  curr_time <<- sub(Sys.time(), pattern = " ", replacement = "_")
  
  temp = readWave(wavfile,
                  from = durationfile$tmin[line], to = durationfile$tmax[line],
                  units = "seconds")
  
  savewav(temp, file = paste("phonetic_extraction/data/temp/", curr_file, "_", curr_time, ".wav", sep = ""))
  
  # Set location paths for wav files and formant files, Note: spaces not allowed in path
  wav_loc = paste(getwd(), "/", list.files(path = "phonetic_extraction/data/temp", full.names = TRUE), sep = "")
  formant_loc = sub(wav_loc, pattern = ".wav", replacement = ".Formant")
  table_loc = sub(wav_loc, pattern = ".wav", replacement = ".txt")
  
  # Set arguments for tables
  tab_args = list( TRUE, # Include frame number
                   TRUE, # Include time
                   3,    # Time decimals
                   TRUE, # Include intensity
                   3,    # Intensity decimals
                   TRUE, # Include number of formants
                   3,    # Frequency decimals
                   TRUE )# Include bandwidths
  
  # Perform Praat call to get formants based on input wav
  praat("To Formant (burg)...",
        arguments = formant_arguments,
        input = wav_loc,
        output = formant_loc)
  
  # Perform Praat call to save formant information to a table
  praat("Down to Table...",
        arguments = tab_args,
        input = formant_loc,
        output = table_loc,
        filetype = "tab-separated")
  
  # Read in table and clean up data
  read.table(file = table_loc, header = TRUE, sep = "\t", na.strings="--undefined--" ) %>%
    # Only keep time, F1, F2, and F3
    select(time.s., F1.Hz., F2.Hz., F3.Hz.) %>%
    # Rename time, f1, and f2 columns
    rename(time = time.s.) %>%
    rename(f1 = F1.Hz.) %>%
    rename(f2 = F2.Hz.) %>%
    rename(f3 = F3.Hz.) %>%
    # Save gender information for intial settings
    mutate(gender = speaker_gender) %>%
    mutate(file_name = curr_file) %>%
    mutate(tmin = curr_start) %>%
    mutate(sound = curr_phoneme)
  
}


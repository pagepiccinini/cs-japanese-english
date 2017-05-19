## READ IN WAV FILE AND TEXTGRID ####
# Set name of file
file = paste0(pair, "_", prompt, "_", speaker)

# Read in wav file
data_wav = paste(getwd(), "/data/recordings/",
                 file, ".wav", sep="")

# Set location of TextGrid
textgrid_loc = paste(getwd(), "/data/phonetics/textgrids_phonetics/",
                     file, ".TextGrid", sep = "")


## TURN TEXTGIRD INTO TABLE AND SAVE ####
# Save duration data
table_to_textgrid_phonetics(textgrid_loc, file)

# Read in duration data
data_phonetics = read.table(paste("data/phonetics/textfiles_phonetics/", file, ".txt", sep = ""),
                  header = T, sep = "\t") %>%
  # Make columns with detailed information about each token
  separate(text,
           into = c("word", "sound", "lang_pre", "lang_post", "type", "presence"),
           sep = "_",
           remove = FALSE) %>%
  # Update presence column for sounds that are present
  mutate(presence = if_else(is.na(presence), 1, 0)) %>%
  # Compute duration of segments
  mutate(duration = tmax - tmin) %>%
  # Add column with file name
  mutate(file_name = file)

# Save formant data
for (i in 1:dim(data_phonetics)[1]) {
  # Save pitch information
  formants_temp = formants_extracter(file, data_wav, data_phonetics, i, formant_arguments)
  
  # Save data and remove temp files
  if(!exists("data_formants") == TRUE){
    data_formants = formants_temp
    unlink(paste("data/temp/", file, "_", curr_time, ".wav", sep = ""))
    unlink(paste("data/temp/", file, "_", curr_time, ".Formant", sep = ""))
    unlink(paste("data/temp/", file, "_", curr_time, ".txt", sep = ""))
  } else {
    data_formants = bind_rows(data_formants, formants_temp)
    unlink(paste("data/temp/", file, "_", curr_time, ".wav", sep = ""))
    unlink(paste("data/temp/", file, "_", curr_time, ".Formant", sep = ""))
    unlink(paste("data/temp/", file, "_", curr_time, ".txt", sep = ""))
  }
  rm(formants_temp)
}

# Combine duration and formant data
data_phonetics_formants = inner_join(data_phonetics, data_formants)


## READ IN WAV FILE AND TEXTGRID ####
# Set name of file
file = paste0(pair, "_", prompt, "_", speaker)

# Read in wav file
data_wav = paste(getwd(), "/phonetic_extraction/data/recordings/",
                 file, ".wav", sep="")

# Set location of TextGrid
textgrid_loc = paste(getwd(), "/phonetic_extraction/data/textgrids/",
                     file, ".TextGrid", sep = "")


## TURN TEXTGIRD INTO TABLE AND SAVE ####
# Save duration data
table_to_textgrid_phonetics(textgrid_loc, file)

# Read in duration data
data_duration = read.table(paste("phonetic_analysis/data/", file, ".txt", sep = ""),
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
  mutate(duration_ms = duration * 1000) %>%
  # Add column with file name
  mutate(file_name = file) %>%
  # Make columns with desired information about file
  separate(file_name,
           into = c("pair", "prompt", "speaking"),
           sep = "_",
           remove = FALSE)

# Save formant data
for (i in 1:dim(data_duration)[1]) {
  # Save pitch information
  formants_temp = formants_extracter(file, data_wav, data_duration, i, formant_arguments)
  
  # Save data and remove temp files
  if(!exists("data_formants") == TRUE){
    data_formants = formants_temp
    unlink(paste("phonetic_extraction/data/temp/", file, "_", curr_time, ".wav", sep = ""))
    unlink(paste("phonetic_extraction/data/temp/", file, "_", curr_time, ".Formant", sep = ""))
    unlink(paste("phonetic_extraction/data/temp/", file, "_", curr_time, ".txt", sep = ""))
  } else {
    data_formants = bind_rows(data_formants, formants_temp)
    unlink(paste("phonetic_extraction/data/temp/", file, "_", curr_time, ".wav", sep = ""))
    unlink(paste("phonetic_extraction/data/temp/", file, "_", curr_time, ".Formant", sep = ""))
    unlink(paste("phonetic_extraction/data/temp/", file, "_", curr_time, ".txt", sep = ""))
  }
  rm(formants_temp)
}

# Combine duration and formant data
data_duration_formants = inner_join(data_duration, data_formants)


## SAVE DATA TO TEXT FILES ####
# Durations
write_csv(data_duration,
            paste0("phonetic_analysis/data/", file, "_duration.csv"))

# Formants
write_csv(data_duration_formants,
          paste0("phonetic_analysis/data/", file, "_formants.csv"))


## GET RID OF TEMP DATA ####
rm(data_duration, data_formants, data_duration_formants)


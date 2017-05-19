## READ IN WAV FILE AND TEXTGRID ####
# Read in wav file
data_wav =
  paste("data/recordings/", group, "/", speaker, ".wav", sep="")

# Set location of TextGrid
textgrid_loc = paste(getwd(), "/data/textgrids/phonetics/",
                     file, ".TextGrid", sep = "")


## TURN TEXTGIRD INTO TABLE AND SAVE ####
# Save data
table_to_textgrid_phonetics(textgrid_loc, paste(file))

# Read in data
data_phonetics = read.table(paste("data/textfiles/phonetics/", file, ".txt", sep = ""),
                  header = T, sep = "\t") %>%
  # Make columns with detailed information about each token
  separate(text,
           into = c("word", "sound", "lang_pre", "lang_post", "type", "presence"),
           sep = "_",
           remove = FALSE) %>%
  # Update presence column for sounds that are present
  mutate(presence = if_else(is.na(presence), 1, 0)) %>%
  # Compute duration of segments
  mutate(duration = tmax - tmin)
  


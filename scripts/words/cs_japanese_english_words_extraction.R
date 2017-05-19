## GET LOCATION AND NAME INFORMATION OF TEXTGRIDS ####
# Get file locations
files = paste(getwd(), "/", list.files("data/words/textgrids_words", full.names = T),
              sep = "")

# Get file names
names = sub(".TextGrid", "", list.files("data/words/textgrids_words"))


## TRANSFORM DATA FROM TEXTGRID TO TEXTFILE ####
# Save files
for (i in 1:length(files)) {
  table_to_textgrid(files[i], names[i])
}

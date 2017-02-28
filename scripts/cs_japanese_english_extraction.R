## TRANSFORM DATA FROM TEXTGRID TO TEXTFILE
# Get file locations
files = paste(getwd(), "/", list.files("data/textgrids", full.names = T),
              sep = "")

# Get file names
names = sub(".TextGrid", "", list.files("data/textgrids"))

# Save files
for (i in 1:length(files)) {
  table_to_textgrid(files[i], names[i])
}

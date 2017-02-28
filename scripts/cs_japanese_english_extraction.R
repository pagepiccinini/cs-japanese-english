## TRANSFORM DATA FROM TEXTGRID TO TEXTFILE
# Get file locations
files = paste(getwd(), "/", list.files("data/textgrids", full.names = T),
              sep = "")

# Get file names
names = sub(".TextGrid", "", list.files("data/textgrids"))

# Save files
for (i in 1:length(files_full)) {
  table_to_textgrid(files_full[i], names[i])
}

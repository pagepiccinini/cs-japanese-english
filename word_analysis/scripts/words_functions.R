# Make table from TextGrid for words
table_to_textgrid = function(inputfile, outputfile) {
  praat( "Down to Table...",
         arguments = list( TRUE, # Include line number
                           6,    # Time decimals
                           TRUE, # Include tier names
                           FALSE  # Include empty intervals
                          ), # End list()
         input = inputfile,
         output = paste(getwd(), "/word_analysis/data/textfiles/", outputfile, ".txt", sep = ""),
         filetype = "tab-separated"
         )
}
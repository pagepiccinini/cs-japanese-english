# Make table from TextGrid for words
table_to_textgrid = function(inputfile, outputfile) {
  praat( "Down to Table...",
         arguments = list( TRUE, # Include line number
                           6,    # Time decimals
                           TRUE, # Include tier names
                           FALSE  # Include empty intervals
                          ), # End list()
         input = inputfile,
         output = paste(getwd(), "/data/words/textfiles_words/", outputfile, ".txt", sep = ""),
         filetype = "tab-separated"
         )
}

# Make table from TextGrid for phonetics
table_to_textgrid_phonetics = function(inputfile, outputfile) {
  praat( "Down to Table...",
         arguments = list( TRUE, # Include line number
                           6,    # Time decimals
                           TRUE, # Include tier names
                           FALSE  # Include empty intervals
         ), # End list()
         input = inputfile,
         output = paste(getwd(), "/data/textfiles/phonetics/", outputfile, ".txt", sep = ""),
         filetype = "tab-separated"
  )
}


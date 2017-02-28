## READ IN DATA ####
# Get file names
names = sub(".TextGrid", "", list.files("data/textgrids"))

# Read in data and add names  
data = list.files("data/textfiles", full.names = T) %>%
  # Read in data to table
  map(read.table, header = T, sep = "\t", quote = "", fileEncoding = "utf-16be") %>%
  # Add column for file names
  map2(names, function(df, names) df %>%
         mutate(file = names)) %>%
  # Combine into one data frame
  bind_rows()


## CLEAN DATA ####
data_clean = data %>%
  # Make separte columns for pair, speaker, and language information
  separate(tier, into = c("pair", "speaker", "language")) %>%
  # Add a column for prompt
  mutate(prompt = substr(file, 4, 6))


## GET WORD COUNTS FOR EACH LANGUAGE ####
# English
data_eng_clean = data_clean %>%
  # Focus only on English tokens
  filter(language == "eng") %>%
  # Drop non-word vocalizations
  mutate(text_clean = gsub("<.*?>", "", text)) %>%
  # Expand to one row per word
  unnest_tokens(word, text_clean)

# Japanese
data_jap_clean = data_clean %>%
  # Focus only on Japanese tokens
  filter(language == "jap") %>%
  # Drop non-word vocalizations
  mutate(text_clean = gsub("<.*?>", "", text)) %>%
  # Expand to one row per word
  unnest_tokens(word, text_clean, token = stringr::str_split, pattern = "　")


## GET SUMMARY INFORMATION OF WORDS ####
# English
data_eng_sum = data_eng_clean %>%
  count(word, sort = T)

# Japanese
data_jap_sum = data_jap_clean %>%
  count(word, sort = T)

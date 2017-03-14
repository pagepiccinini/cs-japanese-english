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

# Read in Clearpond English database
clearpond_english = read.table("data/clearpond_english.txt", header = T, sep = "\t") %>%
  # Make all words lowercase
  mutate(word = tolower(word))


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
  unnest_tokens(word, text_clean) %>%
  # Add word number within utterance
  group_by(prompt, pair, speaker, tmin, text) %>%
  mutate(word_number = row_number()) %>%
  mutate(number_words_utt = n()) %>%
  ungroup() %>%
  # Note if first, last, or medial
  mutate(word_position = if_else(word_number == 1, "first",
                      if_else(word_number == number_words_utt, "last", "medial")))

# Japanese
data_jap_clean = data_clean %>%
  # Focus only on Japanese tokens
  filter(language == "jap") %>%
  # Drop non-word vocalizations
  mutate(text_clean = gsub("<.*?>", "", text)) %>%
  # Expand to one row per word
  unnest_tokens(word, text_clean, token = stringr::str_split, pattern = "　") %>%
  # Add word number within utterance
  group_by(prompt, pair, speaker, tmin, text) %>%
  mutate(word_number = row_number()) %>%
  mutate(number_words_utt = n()) %>%
  ungroup() %>%
  # Note if first, last, or medial
  mutate(word_position = if_else(word_number == 1, "first",
                      if_else(word_number == number_words_utt, "last", "medial")))


## GET SUMMARY INFORMATION OF WORDS ####
# English
data_eng_sum = data_eng_clean %>%
  count(word, sort = T) %>%
  # Add in phonetic and frequency information
  left_join(clearpond_english) %>%
  # Make column for first phoneme
  mutate(first_phoneme = gsub("\\..*$", "", phonemes))

data_eng_firstphone = data_eng_sum %>%
  # Get type and token counts for initial phonemes
  group_by(first_phoneme) %>%
  summarise(types = n(),
            tokens = sum(n, na.rm = T)) %>%
  ungroup() %>%
  # Sort by number of tokens
  arrange(desc(tokens))

# Japanese
data_jap_sum = data_jap_clean %>%
  count(word, sort = T)

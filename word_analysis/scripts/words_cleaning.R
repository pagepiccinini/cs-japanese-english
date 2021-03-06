## READ IN DATA ####
# Get file names
names = sub(".txt", "", list.files("word_analysis/data/textfiles"))

# Read in data and add names  
data = list.files("word_analysis/data/textfiles", full.names = T) %>%
  # Read in data to table
  map(read.table, header = T, sep = "\t", quote = "", fileEncoding = "utf-16be") %>%
  # Add column for file names
  map2(names, function(df, names) df %>%
         mutate(file = names)) %>%
  # Combine into one data frame
  bind_rows() %>%
  # Convert to tibble so easier to look at
  as_tibble()

# Read in Clearpond English database
clearpond_english = read_table2("word_analysis/data/dictionaries/clearpond_english.txt") %>%
  # Make all words lowercase
  mutate(word = tolower(word))

# Read in Kana to alphabet
kana_alphabet = read_csv("word_analysis/data/dictionaries/kana_alphabet.csv")


## CLEAN DATA ####
data_clean = data %>%
  # Make separte columns for pair, speaker, and language information
  separate(tier, into = c("pair", "speaker", "language")) %>%
  # Add a column for prompt
  mutate(prompt = substr(file, 4, 6)) %>%
  # Get distance from previous / following utterance by for a given speaker
  group_by(prompt, pair, speaker) %>%
  arrange(tmin) %>%
  mutate(prev_utt_end = lag(tmax)) %>%
  mutate(follow_utt_begin = lead(tmin)) %>%
  # Code if utterance is pre-, post-, or dual-switch, or monolingual
  mutate(cs_pre = ifelse(is.na(follow_utt_begin), "no",
                  ifelse(abs(follow_utt_begin - tmax) < 0.3 & language != lead(language), "yes", "no"))) %>%
  mutate(cs_post = ifelse(is.na(prev_utt_end), "no",
                   ifelse(abs(prev_utt_end - tmin) < 0.3 & language != lag(language), "yes", "no"))) %>%
  mutate(cs_dual = ifelse(cs_pre == "yes" & cs_post == "yes", "yes", "no")) %>%
  mutate(utt_type = ifelse(cs_pre == "yes" | cs_post == "yes", "cs", "ml")) %>%
  mutate(cs_type = ifelse(utt_type == "cs" & cs_dual == "yes", "dual",
                   ifelse(utt_type == "cs" & cs_pre == "yes", "pre",
                   ifelse(utt_type == "cs" & cs_post == "yes", "post", NA)))) %>%
  ungroup()


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
                      if_else(word_number == number_words_utt, "last", "medial"))) %>%
  # Add in phonetic and frequency information
  left_join(clearpond_english) %>%
  # Make column for first phoneme
  mutate(first_phoneme = gsub("\\..*$", "", phonemes))

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
                      if_else(word_number == number_words_utt, "last", "medial"))) %>%
  # Make column for first phoneme
  mutate(first_phoneme = substr(word, 1, 1))


## GET SUMMARY INFORMATION OF WORDS ####
# English
data_eng_sum = data_eng_clean %>%
  count(word, sort = T)

data_eng_firstphone = data_eng_clean %>%
  # Drop NAs
  filter(!is.na(first_phoneme)) %>%
  # Get type and token counts for initial phonemes
  group_by(first_phoneme) %>%
  summarise(types = n_distinct(word),
            tokens = n()) %>%
  ungroup() %>%
  # Sort by number of tokens
  arrange(desc(tokens))

# Japanese
data_jap_sum = data_jap_clean %>%
  count(word, sort = T)

data_jap_firstphone = data_jap_clean %>%
  # Join alphabetic data
  left_join(kana_alphabet, by = c("first_phoneme" = "katakana")) %>%
  # Drop NAs
  filter(!is.na(first_phoneme)) %>%
  # Get type and token counts for initial phonemes
  group_by(first_phoneme, alphabet) %>%
  summarise(type = n_distinct(word),
            tokens = n()) %>%
  ungroup() %>%
  # Sory by number of tokens
  arrange(desc(tokens))

## GET WORD COUNTS BY LANGUAGE
#count words
data_eng_count = data_eng_clean %>%
  group_by(prompt, pair, speaker) %>%
  summarize(eng_words = n()) %>%
  ungroup()

data_jap_count = data_jap_clean %>%
  group_by(prompt, pair, speaker) %>%
  summarize(jap_words = n()) %>%
  ungroup()

#bind and calculate percentage
data_count = full_join(data_eng_count,data_jap_count, by = c("prompt","pair","speaker")) %>%
  mutate(eng_words = replace(eng_words, is.na(eng_words), 0))%>%
  mutate(jap_words = replace(jap_words, is.na(jap_words), 0))%>%
  mutate(eng_percent = eng_words/(jap_words+eng_words))

#save for use in phonetic analysis
write.table(data_count,"word_analysis/data/generated/wordcounts.txt",row.names=F)

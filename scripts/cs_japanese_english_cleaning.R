## READ IN DATA ####
# Get file names
names = sub(".TextGrid", "", list.files("data/textgrids"))

# Read in data and add names  
data = list.files("data/textfiles", full.names = T) %>%
  map(read.table, header = T, sep = "\t", quote = "", fileEncoding = "utf-16be") %>%
  map2(names, function(df, names) df %>%
         mutate(file = names)) %>%
  bind_rows()


## CLEAN DATA ####
data_clean = data %>%
  separate(tier, into = c("pair", "speaker", "language")) %>%
  mutate(conversation = substr(file, 4, 6))


## GET WORD COUNTS FOR EACH LANGUAGE ####
# English
data_eng_clean = data_clean %>%
  filter(language == "eng") %>%
  unnest_tokens(word, text)

# Japanese
data_jap_clean = data_clean %>%
  filter(language == "jap") %>%
  unnest_tokens(word, text, token = stringr::str_split, pattern = "　")


## GET SUMMARY INFORMATION OF WORDS ####
# English
data_eng_sum = data_eng_clean %>%
  count(word, sort = T)

# Japanese
data_jap_sum = data_jap_clean %>%
  count(word, sort = T)

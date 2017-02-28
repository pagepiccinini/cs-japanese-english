test = read.table("data/japanese/e07_b00_s1_jap.txt", header = T, sep = "\t")


## READ IN DATA ####
# Initial data
data_eng = list.files("data/english", full.names = T) %>%
  map(read.table, header = T, sep = "\t", quote = "") %>%
  bind_rows()

data_jap = list.files("data/japanese", full.names = T) %>%
  map(read.table, header = T, sep = "\t", quote = "") %>%
  bind_rows()

# Clean up data
data_eng_clean = data_eng %>%
  separate(tier, into = c("pair", "conversation", "speaker", "language")) %>%
  unnest_tokens(word, utterance)

data_jap_clean = data_jap %>%
  separate(tier, into = c("pair", "conversation", "speaker", "language")) %>%
  unnest_tokens(word, utterance, token = stringr::str_split, pattern = "　")


## GET SUMMARY INFORMATION OF WORDS ####
# English
data_eng_sum = data_eng_clean %>%
  count(word, sort = T)

# Japanese
data_jap_sum = data_jap_clean %>%
  count(word, sort = T)

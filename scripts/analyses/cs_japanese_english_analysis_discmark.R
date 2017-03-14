## ORGANIZE DATA ####
# English
data_eng_discmark = data_eng_clean %>%
  # Only look at tokens of the words "like", "yeah", and "so"
  filter(word == "like" | word == "yeah" | word == "so")

# Japanese
data_jap_discmark = data_jap_clean %>%
  # Only look at tokens of the words "ナンカ" (nanka) and "ソー" (saw)
  filter(word == "ナンカ" | word == "ソー")


## GET POSITION AND UTTERANCE TYPE COUNTS ####
# English
data_eng_discmark_sum = data_eng_discmark %>%
  count(word, utt_type, word_position)

# Japanese
data_jap_discmark_sum = data_jap_discmark %>%
  count(word, utt_type, word_position)




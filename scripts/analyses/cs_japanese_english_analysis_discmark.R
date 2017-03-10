## ORGANIZE DATA ####
# Focus on "like"s for English
data_eng_like = data_eng_clean %>%
  # Only look at tokens of the word like
  filter(word == "like")

# Focus on "nanka"s for Japanese
data_jap_nanka = data_jap_clean %>%
  # Only look at tokens of the word nanka
  filter(word == "ナンカ")

# Get position counts
data_eng_like_sum = data_eng_like %>%
  count(word_position)
  
data_jap_nanka_sum = data_jap_nanka %>%
  count(word_position)

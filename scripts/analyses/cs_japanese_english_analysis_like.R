## ORGANIZE DATA ####
# Focus on "like"s
data_eng_like = data_eng_clean %>%
  # Only look at tokens of the word like
  filter(word == "like")

# Get position counts
data_eng_like_sum = data_eng_like %>%
  count(word_position)
  
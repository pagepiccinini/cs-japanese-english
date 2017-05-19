## ORGANIZE DATA ####
# English
data_eng_figs = data_eng_sum %>%
  # Get ranking of words by frequency
  mutate(ranking = row_number())

data_eng_firstphone_figs = data_eng_firstphone %>%
  # Get ranking of phonemes by frequency
  mutate(ranking = row_number())

# Japanese
data_jap_figs = data_jap_sum %>%
  mutate(ranking = row_number())


## MAKE FIGURES OF WORD COUNTS ####
# English
english_top20words.plot = ggplot(filter(data_eng_figs, ranking <= 20), aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("English: Top 20 Words Used") +
  xlab("Word") +
  ylab("Count") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

english_top20words.plot
ggsave("figures/english_top20words.pdf", english_top20words.plot, width = 7, height = 7, unit = "in")

# Japanese
japanese_top20words.plot = ggplot(filter(data_jap_figs, ranking <= 20), aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Japanese: Top 20 Words Used") +
  xlab("Word") +
  ylab("Count") +
  theme_classic(base_family="HiraKakuProN-W3") +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

japanese_top20words.plot
ggsave("figures/japanese_top20words.pdf", japanese_top20words.plot, width = 7, height = 7, unit = "in",
       device = cairo_pdf)


## MAKE FIGURES OF FIRST PHONEME COUNTS ####
# English
english_top20phonemes.plot = ggplot(filter(data_eng_firstphone_figs, ranking <= 20),
                                   aes(x = reorder(first_phoneme, -tokens), y = tokens)) +
  geom_bar(stat = "identity") +
  ggtitle("English: Top 20 First Phonemes Used") +
  xlab("Phoneme") +
  ylab("Count") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

english_top20phonemes.plot
ggsave("figures/english_topphonemes20.pdf", english_top20phonemes.plot, width = 7, height = 7, unit = "in")






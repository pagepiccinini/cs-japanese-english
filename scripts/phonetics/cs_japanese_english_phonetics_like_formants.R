## ORGANIZE DATA ####
# Separate out "like" data
data_phonetics_duration_like = data_phonetics_duration_clean %>%
  # Focus on the word "like"
  filter(word == "like")

# Separate out each phoneme
data_phonetics_like_lai = data_phonetics_like %>%
  # Focus on the phoneme /l/
  filter(sound == "l")

data_phonetics_like_i = data_phonetics_like %>%
  # Focus on the phoneme /ai/
  filter(sound == "i")


## MAKE FIGURE OF FORMANTS ####
# /l/
like_l_formants.plot = ggplot(data_phonetics_like_l, aes(x = time, y = f1, col = lang_pre)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /l/ in 'like'") +
  xlab("Time into /l/") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

like_l_formants.plot
ggsave("figures/like_l_formants.pdf", like_l_formants.plot, width = 7, height = 7, unit = "in")

# /ai/
like_i_formants.plot = ggplot(data_phonetics_like_i, aes(x = time, y = f1, col = lang_pre)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /ai/ in 'like'") +
  xlab("Time into /ai/") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

like_i_formants.plot
ggsave("figures/like_i_formants.pdf", like_i_formants.plot, width = 7, height = 7, unit = "in")


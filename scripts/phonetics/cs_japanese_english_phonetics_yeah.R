## ORGANIZE DATA ####
# Separate out "yeah" data
data_phonetics_yeah = data_phonetics_formants %>%
  # Focus on the word "yeah"
  filter(word == "yeah")

# Separate out each phoneme
data_phonetics_yeah_y = data_phonetics_yeah %>%
  # Focus on the phoneme /j/
  filter(sound == "y")

data_phonetics_yeah_eah = data_phonetics_yeah %>%
  # Focus on the phoneme /ae/
  filter(sound == "eah")


## MAKE FIGURE OF FORMANTS ####
# /j/
yeah_y_formants.plot = ggplot(data_phonetics_yeah_y, aes(x = time, y = f1, col = lang_post)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /j/ in 'yeah'") +
  xlab("Time into vowel") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

yeah_y_formants.plot
ggsave("figures/yeah_y_formants.pdf", yeah_y_formants.plot, width = 7, height = 7, unit = "in")

# /ae/
yeah_eah_formants.plot = ggplot(data_phonetics_yeah_eah, aes(x = time, y = f1, col = lang_post)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /ae/ in 'yeah'") +
  xlab("Time into vowel") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

yeah_eah_formants.plot
ggsave("figures/yeah_eah_formants.pdf", yeah_eah_formants.plot, width = 7, height = 7, unit = "in")

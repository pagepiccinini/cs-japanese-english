## ORGANIZE DATA ####
# Separate out "so" data
data_phonetics_so = data_phonetics_formants %>%
  # Focus on the word "so"
  filter(word == "so-eng" | word == "so-jap")

# Separate out each phoneme
data_phonetics_so_s = data_phonetics_so %>%
  # Focus on the phoneme /s/
  filter(sound == "s")

data_phonetics_so_o = data_phonetics_so %>%
  # Focus on the phoneme /o/
  filter(sound == "o")
 
 
## MAKE FIGURE OF FORMANTS ####
so_formants.plot = ggplot(data_phonetics_so_o, aes(x = time, y = f1, col = word)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /o/ in 'so'") +
  xlab("Time into vowel") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

so_formants.plot
ggsave("figures/so_formants.pdf", so_formants.plot, width = 7, height = 7, unit = "in")

  
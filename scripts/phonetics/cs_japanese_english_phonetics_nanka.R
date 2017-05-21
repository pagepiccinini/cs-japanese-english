## ORGANIZE DATA ####
# Separate out "nanka" data
data_phonetics_nanka = data_phonetics_formants %>%
  # Focus on the word "nanka"
  filter(word == "nanka")

# Separate out each phoneme
data_phonetics_nanka_na = data_phonetics_nanka %>%
  # Focus on the phoneme /na/
  filter(sound == "na")

data_phonetics_nanka_n = data_phonetics_nanka %>%
  # Focus on the phoneme /n/
  filter(sound == "n")

data_phonetics_nanka_ka = data_phonetics_nanka %>%
  # Focus on the phoneme /ka/
  filter(sound == "ka")


## MAKE FIGURE OF FORMANTS ####
# /na/
nanka_na_formants.plot = ggplot(data_phonetics_nanka_na, aes(x = time, y = f1, col = lang_post)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /na/ in 'nanka'") +
  xlab("Time into vowel") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

nanka_na_formants.plot
ggsave("figures/nanka_na_formants.pdf", nanka_na_formants.plot, width = 7, height = 7, unit = "in")

# /n/
nanka_n_formants.plot = ggplot(data_phonetics_nanka_n, aes(x = time, y = f1, col = lang_post)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /n/ in 'nanka'") +
  xlab("Time into vowel") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

nanka_n_formants.plot
ggsave("figures/nanka_n_formants.pdf", nanka_n_formants.plot, width = 7, height = 7, unit = "in")

# /ka/
nanka_ka_formants.plot = ggplot(data_phonetics_nanka_ka, aes(x = time, y = f1, col = lang_post)) + 
  facet_wrap(~line) +
  geom_point() +
  geom_point(aes(y = f2)) +
  scale_color_manual(values = c(brewer.pal(5, "PRGn")[1], brewer.pal(5, "PRGn")[5])) +
  ggtitle("F1 and F2 for /ka/ in 'nanka'") +
  xlab("Time into vowel") +
  ylab("Frequency in Hz") +
  theme_classic() +
  theme(text = element_text(size = 18), axis.text.x = element_text(angle = 60, hjust = 1))

nanka_ka_formants.plot
ggsave("figures/nanka_ka_formants.pdf", nanka_ka_formants.plot, width = 7, height = 7, unit = "in")

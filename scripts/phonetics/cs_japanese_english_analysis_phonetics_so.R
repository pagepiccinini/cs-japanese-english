## ORGANIZE DATA ####
# Separate out "so" data
data_phonetics_so = data_phonetics %>%
  # Focus on the word "so"
  filter(word == "so-eng" | word == "so-jap")

# Separate out each phoneme
data_phonetics_so_s = data_phonetics_so %>%
  # Focus on the phoneme /s/
  filter(sound == "s")

data_phonetics_so_o = data_phonetics_so %>%
  # Focus on the phoneme /o/
  filter(sound == "o") %>%
  # Extract additional phonetic information (formants)
  
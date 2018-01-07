## READ IN DATA ####
demo = read_csv("data/demographics.csv")


## SEPARATE DATA INTO UNIQUE DATAFRAMES TO CLEAN ####
# General information
demo_general = demo %>%
  select(pair:acquisition_lg5, culture1:disability_explanation)

# Percent use languages
demo_pct = demo %>%
  select(pair, speaker, acquisition_lg1, acquisition_lg2, acquisition_lg1_pct_exposed:acquisition_lg5_pct_speak) %>%
  # Gather data so each question is now a variable instead of a column
  gather(acquisition_lg1_pct_exposed:acquisition_lg5_pct_speak, key = "question", value = "percent") %>%
  # Changes names of questions to match the order of acquisition for English and Japanese
  mutate(question = str_replace(question, "lg1", acquisition_lg1)) %>%
  mutate(question = str_replace(question, "lg2", acquisition_lg2)) %>%
  select(-c(acquisition_lg1, acquisition_lg2)) %>%
  filter(!is.na(question)) %>%
  # Make variables columns again
  spread(question, percent)

# English and Japanese specific information
demo_lg_specific = demo %>%
  select(pair, speaker, language1, language2, language1_order_learn:language2_accent_identified_nonnative) %>%
  # Gather data so each question is now a variable instead of a column
  gather(language1_order_learn:language2_accent_identified_nonnative, key = "question", value = "response") %>%
  # Changes names of questions to match the order of acquisition for English and Japanese
  mutate(question = str_replace(question, "language1", language1)) %>%
  mutate(question = str_replace(question, "language2", language2)) %>%
  select(-c(language1, language2)) %>%
  filter(!is.na(question)) %>%
  # Make variables columns again
  spread(question, response)


## COMBINE CLEANED DEMOGRAPHIC DATA INTO SINGLE DATAFRAME ####
demo_clean = demo_general %>%
  left_join(demo_pct) %>%
  left_join(demo_lg_specific)


## SIMPLIFY DATA ONLY TO COLUMN USED FOR ANALYSIS ####
demo_simple = demo_clean %>%
  select(pair, speaker, acquisition_English_pct_exposed)

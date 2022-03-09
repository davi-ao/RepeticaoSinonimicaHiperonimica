####################################
# CONCEPT RECALL PROBABILITY TABLE #
####################################

# Initial configuration --------------------------------------------------------
library(tidyverse)
library(udpipe)
library(wordnet)

m_eng = udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')

setDict('wn3.1.dict/dict/')

# Load lemma recall list -----------------------------------------------------
recall = read_file('NCI_recall.txt') %>%
  tibble(recall = str_split(., '\\n') %>% unlist()) %>%
  select(recall) %>%
  filter(recall != '') %>%
  mutate(participant = row_number())

recall_annotated = lapply(1:nrow(recall), function(p) {
  udpipe_annotate(m_eng, recall$recall[p]) %>%
    as_tibble() %>%
    mutate(participant = p)
}) %>%
  bind_rows() %>%
  select(participant, lemma, upos) %>%
  filter(!upos %in% c(
    'ADP', 'AUX', 'CCONJ', 'DET', 'PART', 'SCONJ', 'PUNCT', 'SYM', 'NUM')) %>%
  mutate(lemma = lemma %>% str_to_lower())

# Function get_words_synonyms() defined in NCI.Rmd
recall_synonyms = get_words_synonyms(recall_annotated$lemma, 
                                     recall_annotated$upos)

recall_final = recall_annotated %>% 
  left_join(recall_synonyms, by = 'lemma')

# Load text --------------------------------------------------------------------
text = read_file('NCI_text1.txt')

text_annotated = text %>% 
  udpipe_annotate(m_eng, .) %>%
  as_tibble() %>%
  select(sentence_id, lemma, upos) %>%
  filter(upos != 'PUNCT' & !str_detect(lemma, '\\W')) %>%
  mutate(lemma = lemma %>% str_to_lower()) %>%
  group_by(sentence_id) %>%
  distinct() %>%
  ungroup()

recall_participant = tibble(participant = rep(1:14, nrow(text_annotated)) %>% 
                              sort(),
                            lemma = rep(text_annotated$lemma, 14),
                            recall = NA)

# Check if word in text or synonym exist in recall -----------------------------
for(r in 1:nrow(recall_participant)) {
  recall_participant$recall[r] = recall_participant$lemma[r] %in% (
    recall_final %>% 
      filter(participant == recall_participant$participant[r]) %>% 
      .$lemma) ||
    recall_participant$lemma[r] %in% (
      recall_final %>% 
        filter(participant == recall_participant$participant[r]) %>%
        .$synonyms)
}

# Save in CSV format -----------------------------------------------------------
write_csv(recall_participant, 'recall_participants.csv')

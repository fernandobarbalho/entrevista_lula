library(readr)
library(tidytext)
library(forcats)

entrevista_lula <- read_delim("entrevista_lula.txt",
                              delim = ":", escape_double = FALSE, col_names = FALSE,
                              trim_ws = TRUE)

names(entrevista_lula)<- c("participante","fala")


#########Quantidade de intervenções

entrevista_lula %>%
  group_by(participante)%>%
  summarise(
    n=n()
  )%>%
  ungroup() %>%
  mutate(participante = reorder(participante,n)) %>%
  ggplot()+
  geom_col(aes(y=participante, x=n), fill= "white")+
  theme(
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(hjust = 0.7)
  ) +
  labs(
    x= "Número de intervenções"
  )



#######Quantidade de palavras
unnested_entrevista<-
entrevista_lula %>%
  tidytext::unnest_tokens(word,fala)



library(stopwords)

library(tibble)

stopword <- as_tibble(stopwords::stopwords("pt"))
stopword <- rename(stopword, word=value)
stopword <- bind_rows(stopword,tibble(word=c("é","porque","vai","então","ser","dizer","assim")))
unnested_entrevista <- anti_join(unnested_entrevista, stopword, by = 'word')


unnested_entrevista %>%
  filter(participante=="Luiz Inácio Lula da Silva") %>%
  group_by(participante, word) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup()%>%
  mutate(word= reorder(word,quantidade)) %>%
  filter(quantidade>=10) %>%
  ggplot() +
  geom_col(aes(x=quantidade, y=word), color="#505050",  fill="white") +
  facet_wrap(participante~.)+
  theme(
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(hjust = 0.7),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white")
  )




unnested_entrevista %>%
  filter(participante=="Renata Vasconcellos") %>%
  group_by(participante, word) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup()%>%
  mutate(word= reorder(word,quantidade)) %>%
  filter(quantidade>=4) %>%
  ggplot() +
  geom_col(aes(x=quantidade, y=word), color="#505050",  fill="white") +
  facet_wrap(participante~.)+
  theme(
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(hjust = 0.7),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white")
  )



unnested_entrevista %>%
  filter(participante=="William Bonner") %>%
  group_by(participante, word) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup()%>%
  mutate(word= reorder(word,quantidade)) %>%
  filter(quantidade>=4) %>%
  ggplot() +
  geom_col(aes(x=quantidade, y=word), color="#505050",  fill="white") +
  facet_wrap(participante~.)+
  theme(
    panel.background = element_rect(fill = "#15202B"),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(hjust = 0.7),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white")
  )




##Nuvem de palavras
library(wordcloud)
unnested_entrevista %>%
  filter(participante=="Luiz Inácio Lula da Silva") %>%
  count(word)%>%
  with(wordcloud(word,n,max.words = 150, colors=brewer.pal(6,"Dark2"),random.order=FALSE))


set.seed(1972)
unnested_entrevista %>%
  filter(participante=="Renata Vasconcellos") %>%
  count(word)%>%
  with(wordcloud(word,n,max.words = 150, colors=brewer.pal(6,"Dark2"),random.order=FALSE))

set.seed(1972)
unnested_entrevista %>%
  filter(participante=="William Bonner") %>%
  count(word)%>%
  with(wordcloud(word,n,max.words = 150, colors=brewer.pal(6,"Dark2"),random.order=FALSE))



#######Análise tf-idf

tabela <- unnested_entrevista %>%
  count(participante, word, sort = TRUE) %>%
  bind_tf_idf(word, participante, n) %>%
  mutate(word = fct_reorder(word, tf_idf))

graph<-
  tabela %>%
  group_by(participante) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~participante, ncol = 3, scales = "free") +
  coord_flip() +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        axis.title = element_blank())


#######Análises de bigrama

palavras_bigrams <-
  entrevista_lula %>%
  filter(participante=="Luiz Inácio Lula da Silva") %>%
  unnest_tokens(bigram, fala, token = "ngrams", n = 2)



palavras_bigrams_count <- palavras_bigrams %>%
  count(bigram, sort = TRUE)


library(tidyr)

# seperate words
bigrams_separated <- palavras_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filter stop words and NA
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopword$word) %>%
  filter(!word2 %in% stopword$word) %>%
  filter(!is.na(word1))


# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)


library(igraph)

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n >=2) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(13)


ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  theme_void()


entrevista_lula%>%
  filter(str_detect(fala,"gente"),
         participante == "Luiz Inácio Lula da Silva")%>%
  select(fala)


entrevista_lula %>%
  readr::write_delim(file="entrevista_lula.csv", delim = ":")

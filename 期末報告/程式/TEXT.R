library(qs)
library(tidytext)
library(tidyverse)
library(forcats)
library(ggplot2)
library(igraph)
library(ggraph)
library(stringr)


data = qread("D:/The_Most_Important_Thing/Book")

for (i in 1:21) {
  data[[i]] = data[[i]][-c(1:3)]
}

chap = paste0("Chap_",1:21)

x = cbind(Chapter=chap[1],text = data[[1]]) %>% as.data.frame()

for (i in 2:21) {
  c = cbind(Chapter=chap[i],text = data[[i]]) %>% as.data.frame()
  x  = rbind(x,c)
}


tidy_df <- x %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


tokens_count <- tidy_df %>% 
  group_by(word) %>% 
  summarise(sum = n()) %>% 
  filter(sum > 20) %>%
  arrange(desc(sum))

library(wordcloud2)
wordcloud2(tokens_count)




sentiment_count <- tidy_df %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  count(sentiment, Chapter) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

sentiment_count = sentiment_count %>%
  extract(Chapter, "Chap", "(\\d+)", convert = TRUE)

sentiment_count$superfluous=NULL

gather_sent = gather(sentiment_count,key = "sentiment",value = "n",-Chap)


gather_sent %>%
  ggplot(aes(Chap,n,color=sentiment)) +
  geom_point() +
  geom_line(size = 0.75) +  
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  scale_x_continuous(name = NULL,limits = c(1,21),n.breaks = 11)+
  geom_text(aes(label=n),
            position = position_dodge(width = 0.9),
            vjust= -0.6,
            color="black", size=4)






two_grams <- x %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)


two_grams_separated <- two_grams %>%
  separate(word, c("word1", "word2"), sep = " ")

two_grams_filtered <- two_grams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

two_grams_united <- two_grams_filtered %>%
  unite(word, word1, word2, sep = " ")


two_grams_tf_idf <- two_grams_united %>%
  count(Chapter, word) %>%
  bind_tf_idf(word, Chapter, n) %>%
  arrange(desc(tf_idf))


library(igraph)
two_grams_count <- two_grams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- two_grams_count %>%
  filter(n > 5) %>%
  graph_from_data_frame()


bigram_graph
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)







chapter_words = tidy_df %>%
  count(Chapter,word, sort = TRUE)


total_words <- chapter_words %>% 
  group_by(Chapter) %>% 
  summarize(total = sum(n))

book_words <- left_join(chapter_words, total_words)
book_words$n = as.numeric(book_words$n)
book_words$total = as.numeric(book_words$total)


book_tf_idf <- book_words %>%
  bind_tf_idf(word, Chapter, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))


Chapter_tokens_count <- tidy_df %>% 
  group_by(Chapter,word) %>% 
  summarise(sum = n()) %>% 
  arrange(desc(sum))

Chapter_tokens_count = Chapter_tokens_count %>%
  extract(Chapter, "Chap", "(\\d+)", convert = TRUE)

Chapter_tokens_count %>%
  filter(word %in% c("risk", "investors", "market", "investment", "returns", "price")) %>%
  ggplot(aes(Chap,sum,color=word)) +
  geom_point() +
  geom_line(size = 0.75) +  
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  scale_x_continuous(name = NULL,limits = c(1,21),n.breaks = 11)+
  geom_text(aes(label=sum),
            position = position_dodge(width = 0.9),
            vjust= -0.6,
            color="black", size=3.5)+
  facet_wrap(~ word, scales = "free_y")



chapters_dtm <- Chapter_tokens_count %>%
  cast_dtm(Chap, word, sum)

library(topicmodels)

chapter_lda  <- LDA(chapters_dtm, k = 4,method= "Gibbs",
                    control = list(seed = 5566))

chapter_topics <- tidy(chapter_lda, matrix = "beta")

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 8) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms


top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()





lda_gamma <- tidy(chapter_lda, matrix = "gamma")
lda_gamma

lda_gamma_filter = lda_gamma %>% filter(gamma>0.3)
# write_csv(lda_gamma_filter,"C:/Users/Lai/Desktop/gamma.csv")



ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 2) +
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Distribution of probability for each topic",
       y = "Number of Chapters", x = expression(gamma))


tidy_df %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free") +
  labs(x = "Frequency of this word in the recent financial articles", y = NULL)



book_tf_idf %>%
  filter(Chapter %in% chap[c(8,9,10,11,15,18)]) %>%
  group_by(Chapter) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Chapter)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  facet_wrap(~Chapter, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)

book_tf_idf %>%
  filter(Chapter %in% chap[c(1,2,3,4,12)]) %>%
  group_by(Chapter) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Chapter)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  facet_wrap(~Chapter, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)

book_tf_idf %>%
  filter(Chapter %in% chap[c(14,16,17,21)]) %>%
  group_by(Chapter) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Chapter)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  facet_wrap(~Chapter, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


book_tf_idf %>%
  filter(Chapter %in% chap[c(5,6,7,13,19,20)]) %>%
  group_by(Chapter) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Chapter)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  facet_wrap(~Chapter, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)


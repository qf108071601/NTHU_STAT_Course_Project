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

chap = paste0("Doc",1:21)

x = cbind(document=chap[1],text = data[[1]]) %>% as.data.frame()

for (i in 2:21) {
  c = cbind(document=chap[i],text = data[[i]]) %>% as.data.frame()
  x  = rbind(x,c)
}


tidy_df <- x %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


tokens_count <- tidy_df %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  filter(n > 20) %>%
  arrange(desc(n))

chapter_words = tidy_df %>%
  count(document,word, sort = TRUE)
chapter_words$book = "Value Investing"



data2 = qread("D:/The_Most_Important_Thing/Tech")

for (i in 1:17) {
  data2[[i]] = data2[[i]][-c(1)]
}

chap_2 = paste0("Doc",22:38)

x_2 = cbind(document=chap_2[1],text = data2[[1]]) %>% as.data.frame()

for (i in 2:17) {
  c = cbind(document=chap_2,text = data2[[i]]) %>% as.data.frame()
  x_2  = rbind(x_2,c)
}


tidy_df_2 <- x_2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


tokens_count_2 <- tidy_df_2 %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  filter(n > 20) %>%
  arrange(desc(n))

chapter_words_2 = tidy_df_2 %>%
  count(document,word, sort = TRUE)
chapter_words_2$book = "Technical Analysis"


# library(wordcloud2)
# wordcloud2(tokens_count_2)

two_grams <- x_2 %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)

two_grams_separated <- two_grams %>%
  separate(word, c("word1", "word2"), sep = " ")

two_grams_filtered <- two_grams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

two_grams_united <- two_grams_filtered %>%
  unite(word, word1, word2, sep = " ")


two_grams_tf_idf <- two_grams_united %>%
  count(document, word) %>%
  bind_tf_idf(word, document, n) %>%
  arrange(desc(tf_idf))


library(igraph)
two_grams_count <- two_grams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- two_grams_count %>%
  filter(n > 10) %>%
  graph_from_data_frame()


bigram_graph
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


All = rbind(chapter_words,chapter_words_2)
All$book = as.factor(All$book)


total_words <- All %>% 
  group_by(book) %>% 
  summarize(total = sum(n))


book_words <- left_join(All, total_words)


book_tf_idf <- book_words %>%
  bind_tf_idf(word, document, n)

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))


library(forcats)

b = book_tf_idf %>% 
  filter(!near(tf, 1)) %>%
  arrange(desc(tf_idf)) %>%
  group_by(book) %>%
  distinct(word, book, .keep_all = TRUE) %>% # 去重: distinct
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>% 
  ungroup() 

b %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(tf_idf, word, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)



book_dtm <- All %>%
  cast_dtm(document, word, n)



library(topicmodels)

chapter_lda  <- LDA(book_dtm, k = 2,method= "Gibbs",
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


ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 2) +
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Distribution of probability for each topic",
       y = "Number of Chapters", x = expression(gamma))


Class = lda_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  summarise(topic)



Class[which(Class$document %in% chap),"Book"]="Value Investing"
Class[which(Class$document %in% chap_2),"Book"]="Technical Analysis"

table(Class$topic,Class$Book)



# Train/Test 

chapter_lda  <- LDA(book_dtm, k = 10,method= "VEM",
                    control = list(seed = 5566))

chapter_topics <- tidy(chapter_lda, matrix = "beta")
lda_gamma <- tidy(chapter_lda, matrix = "gamma")


Class = lda_gamma
Class[which(Class$document %in% chap),"Book"]="Value Investing"
Class[which(Class$document %in% chap_2),"Book"]="Technical Analysis"

Class$Book = as.factor(Class$Book)
Class$document = as.factor(Class$document)
Class$topic = as.factor(Class$topic)

C = spread(Class,key = topic,value = gamma)

CC = C[,3:12] %>% as.matrix()

rownames(CC) = C$Book

library(pheatmap)
bk <- c(seq(-1,-0.0001,by=0.001),
        seq(0.00001,1,by=0.001))

pheatmap(CC,
         color = c(
           colorRampPalette(
             colors = c("navyblue","white"))((length(bk)/2)),
           colorRampPalette(
             colors = c("white","firebrick"))(length(bk)/2)),
         breaks = bk,
         cluster_row = FALSE,
         cluster_cols = FALSE,
         fontsize = 11,
         na_col="white",
         display_numbers = F)


set.seed(12345)
index = sample(1:38,26,replace = F)

train = C[index,]
test = C[-index,]


library(e1071)
model = svm(formula = Book ~ .,
            data = train[,-1])
summary(model)

test.pred = predict(model, test[,-1])

# 訓練資料的混淆矩陣
table(real=test$Book, predict=test.pred)



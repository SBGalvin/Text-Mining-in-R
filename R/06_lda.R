
# 00) Notes ---------------------------------------------------------------



# 01) Convert to DTM ------------------------------------------------------


rd_dtm <- rd_cm %>% 
          unnest_tokens(word, comment, token = "ngrams", n = 1) %>% # (i)
          mutate(word = str_extract(word, "[a-z']+")) %>% 
          anti_join(., stop_words, by = "word") %>% 
          group_by(doc, word) %>% 
          summarise(count = n()) %>% 
          ungroup() %>% 
          rename(document = doc) %>% 
          cast_dtm(document, word, count)


# 02) LDA -----------------------------------------------------------------
# per words
rd_lda <- LDA(rd_dtm, k = 2, control = list(seed = 42))


rd_topics <- tidy(rd_lda, matrix = "beta")


# Overall topics

n_10_terms <- rd_topics %>% 
              group_by(topic) %>% 
              slice_max(beta, n = 10) %>% 
              ungroup() %>% 
              arrange(topic, -beta)


Topics_10_terms <- n_10_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(y = term, x = beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic, scales = "free")+
  scale_y_reordered()+
  theme_minimal()

saveRDS(Topics_10_terms, 'output/reddit/Topics_10_terms.RDS')


beta_wide <- rd_topics %>%
             mutate(topic = paste0("topic", topic)) %>%
             pivot_wider(names_from = topic, values_from = beta) %>% 
             filter(topic1 > .001 | topic2 > .001) %>%
             mutate(log_ratio = log2(topic2 / topic1))

PHI_log_ratio <- beta_wide %>% 
                 mutate(GG = factor(ifelse(log_ratio >0, "A", "B"), ordered = TRUE)) %>% 
                 top_n(50, abs(log_ratio)) %>% 
                 ggplot(aes(y = fct_reorder(term, log_ratio),
                            x = log_ratio)) + 
                 geom_col(aes(fill = GG),
                          show.legend = FALSE) + 
                 labs(y = "",
                      x = "log ratio of phi between topic 2 and topic 1 (base 2)")+
                 theme_minimal()


saveRDS(PHI_log_ratio, 'output/reddit/PHI_log_ratio.RDS')

# 03) LDA per comment -----------------------------------------------------


# using the roughly speaking 4 topics we observed earlier

cm_lda <- LDA(cm_dtm, k = 4, control = list(seed = 42))


cm_topics <- tidy(rd_lda, matrix = "gamma")

Topic_Allocation <- cm_topics %>% 
  ggplot(aes(y = gamma, x = factor(topic) ))+
  geom_col(aes(fill = factor(topic)), show.legend = FALSE)+
  scale_y_continuous(breaks = c(0, .5, 1))+
  facet_wrap(~document, ncol = 3)+
  theme_minimal()

saveRDS(Topic_Allocation, 'output/reddit/Topic_Allocation.RDS')

assignments <- augment(rd_lda, data = rd_dtm)


# 04) Merging Topics to data ----------------------------------------------
# take the top topic per comment
# then do a sentiment analysis

cm_words_topic <- rd_cm %>% 
                  unnest_tokens(word, comment, token = "ngrams", n = 1) %>% # (i)
                  mutate(word = str_extract(word, "[a-z']+")) %>% 
                  anti_join(., stop_words, by = "word") %>% 
                  left_join(y = cm_topics %>%  
                                rename(comment_id = document),
                            by = "comment_id")



Assignment_sentiment <- assignments %>% 
  rename(word = term) %>% 
  left_join(y = afinn, by = "word") %>%
  filter(!is.na(value)) %>% 
  left_join(y = bing, by = "word") %>% 
  filter(!is.na(sentiment))





Sentiment_Plot <- Assignment_sentiment %>% 
  ggplot(aes(x = factor(.topic), y = value))+
  geom_jitter(aes(colour = sentiment, shape = document), width = .25)+
  xlab('Topic')+
  theme_minimal()+
  theme(legend.position = "bottom")


saveRDS(Sentiment_Plot, 'output/reddit/Avg_sentiment.RDS')

cm_words_topic %>% 
  select(-gamma) %>% 
  left_join(y = afinn, by = "word") %>%
  filter(!is.na(value)) %>% 
  group_by(topic) %>% 
  summarise(Avg = mean(value)) %>% 
              ungroup() %>% 
  
  ggplot(aes(x = topic, y = Avg))+
           geom_jitter(width = .25)


# 00) Notes ---------------------------------------------------------------
# This file is for descriptive statistics for words
# inclusive of counts of words and sentiment by group using nrc lexicon



# 01) Look at the most frequent words -------------------------------------
wrd_count <- uni_cm %>% 
              group_by(word) %>% 
              summarise(Count = n()) %>% 
              ungroup() %>% 
              arrange(desc(Count))




# convert to ordered factor
wrd_count$word <- factor(wrd_count$word, 
                         levels = wrd_count$word[order(wrd_count$Count, decreasing = F)],
                         ordered = TRUE)



Word_Count_Plot <- wrd_count %>% 
                    filter(Count >= 8) %>% 
                    ggplot(aes(x = Count, y = word))+
                    geom_col(colour = 'grey50')+
                    theme_minimal()+
                    theme(legend.position= "none")+
                    theme(axis.text.y = element_text(size = 6))

saveRDS(Word_Count_Plot, 'output/reddit/Word_Count_Plot.RDS')

# We already know that NA is missing, and there are quite a few

# 02) Word Count by sentiment Class ---------------------------------------

# using the nrc lexcion


# Positive and negative word counts ~~~~~~~~~~~~~~~~~~~~
pos_neg_counts <- uni_cm %>% 
                  inner_join(nrc, by = "word") %>%  
                  count(word, sentiment, sort = TRUE) %>% 
                  filter(sentiment %in% c("positive", "negative")) %>% 
                  group_by(sentiment) %>% 
                  summarise(Count = n()) %>% 
                  ungroup()

pos_neg_counts


# Plot of most frequent Positive and negative words ~~~~~~~~~~~~~~
Pos_Neg_word_plot <- uni_cm %>% 
                     inner_join(nrc, by = "word") %>%  
                     count(word, sentiment, sort = TRUE) %>% 
                     filter(sentiment %in% c("positive", "negative")) %>% 
                     group_by(sentiment) %>%
                     slice_max(n, n = 10) %>% 
                     ungroup() %>%
                     mutate(word =  reorder(word, n)) %>% 
                     ggplot(aes(n, word, fill = sentiment)) +
                     geom_col(show.legend = FALSE) +
                     facet_wrap(~sentiment, scales = "free_y") +
                     theme_minimal()

saveRDS(Pos_Neg_word_plot, 'output/reddit/Pos_Neg_word_plot.RDS')


# Looking a the sub-class-themes of words ~~~~~~~~~~~~~~~~~~~~~~~
word_class <- wrd_count %>% 
                inner_join(nrc, by = "word") %>% 
                group_by(sentiment) %>% 
                summarise(S_class = n()) %>% 
                ungroup() %>% 
                mutate(proportion = S_class / sum(S_class))

# convert to ordered factor
word_class$sentiment <- factor(word_class$sentiment, 
                               levels = word_class$sentiment[order(word_class$S_class, decreasing = F)],
                               ordered = TRUE)

Wrd_Class_plot <- word_class %>% 
                  ggplot(aes(x = sentiment, y = S_class))+
                  geom_col(aes(fill = sentiment),
                           colour = "grey50")+
                  geom_text(aes(y = S_class+5, label = round(proportion, 3)))+
                  theme_minimal()+
                  theme(legend.position = "none")

saveRDS(Wrd_Class_plot, 'output/reddit/Wrd_Class_plot.RDS')

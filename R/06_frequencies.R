# create a tidy format 
cm_td <- uni_cm %>% 
          select(doc, comment_id, word) %>% 
          group_by(doc, comment_id, word) %>%
          summarise(count = n()) %>% 
          mutate(total = sum(count)) %>% 
          mutate(`term frequency` = count/total) %>% 
          ungroup()
          
# Histogram of term frequency
TF_hist <- cm_td %>% 
  ggplot(aes(x = `term frequency`))+
  geom_histogram()+
  theme_minimal()

saveRDS(TF_hist, 'output/reddit/TF_hist.RDS')


# Visualising Zipfs law ---------------------------------------------------

#  the rank-frequency distribution is an inverse relation
# a larger corpus of comments or multiple documents may more closely 
# resemble the curve

total_comments <- rd_cm %>% 
                  unnest_tokens(word, comment, token = "ngrams", n = 1) %>% # (i)
                  mutate(word = str_extract(word, "[a-z']+")) %>%  
                  group_by(doc, word) %>% 
                  summarise(count = n()) %>% 
                  ungroup() %>% 
                  mutate(total = sum(count),
                         `term frequency` = count/total) %>% 
                  arrange(desc(count)) %>% 
                  mutate(rank = row_number())

# Get linear model line
tc_lm <- lm(log10(`term frequency`) ~ log10(rank), 
            data = total_comments[total_comments$rank > 10, ])

tc_lm$coefficients["(Intercept)"]
tc_lm$coefficients["log10(rank)"]


# plot
Zipf_plot <- total_comments %>% 
  ggplot(aes(x = rank, y = `term frequency`, group = doc))+
  geom_abline(intercept = tc_lm$coefficients["(Intercept)"], 
              slope     = tc_lm$coefficients["log10(rank)"], 
              color = "grey10", 
              linetype = 2) +
  geom_line(aes(colour = doc), show.legend = FALSE)+
  scale_x_log10() +
  scale_y_log10()+
  theme_minimal()
  


saveRDS(Zipf_plot, 'output/reddit/Zipf_plot.RDS')

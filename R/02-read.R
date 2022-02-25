# 00) Notes ---------------------------------------------------------------
# This script is for reading in data from the raw data folder only
# and then exploring the data (a little)



# 01) Read in Data --------------------------------------------------------

rd_op <- read_csv('data/reddit/thread.csv')    # original posts
rd_cm <- read_csv('data/reddit/comments.csv')  # corresponding comments



# 02) Explore -------------------------------------------------------------

str(rd_op)
str(rd_cm)
unique(rd_op$doc)
unique(rd_cm$doc)


# bar plot showing OP comments
OP_comments <- rd_cm %>%  
  select(doc, comment_id) %>% unique() %>% 
  group_by(doc) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = doc, y = Count))+
  geom_col(aes(fill = factor(doc)), show.legend = FALSE)+
  theme_minimal()

saveRDS(OP_comments, 'output/reddit/OP_comments.RDS')


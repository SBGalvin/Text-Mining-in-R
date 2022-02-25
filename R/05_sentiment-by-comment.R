
# 00) Notes ---------------------------------------------------------------
# IN this script we are going to take a look at sentiment by comment



# 01) Plot the sentiment by comment ---------------------------------------
Sent_by_comment <- uni_cm %>% 
                   left_join(y = afinn, by = "word") %>% 
                   filter(!is.na(value)) %>% 
                   group_by(doc, comment_id) %>% 
                   mutate(Avg = mean(value)) %>% 
                   ungroup() %>% 
                   
                   mutate(Col = ifelse(Avg <= 0, "Negative", "Positive")) %>% 
                   ggplot(aes(x = comment_id, y = Avg))+
                   geom_col(aes(fill = Col))+
                   facet_wrap(~doc, ncol = 1)+
                   theme_minimal()+
                   theme(axis.text.x = element_blank())+
                   theme(legend.position = "bottom",
                         legend.title = element_blank())


saveRDS(Sent_by_comment, 'output/reddit/Sent_by_comment.RDS')

# Question is it this a mostly negative reaction to the post or something else?
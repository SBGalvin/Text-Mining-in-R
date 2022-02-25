
# 00) Notes ---------------------------------------------------------------

# this script is for alterations that will make analysis a little easier

# 01) Text to lower case --------------------------------------------------
# convert all column names to lower case
colnames(rd_cm) <- tolower(colnames(rd_cm))

# convert column to lowr case
rd_cm           <- rd_cm %>% mutate(comment = tolower(comment))



# 02) Remove new line special character -----------------------------------
# \n is a new line separator, we wil have to remove those

rd_cm <- rd_cm %>% mutate(comment = str_replace_all(comment, pattern = "\\n", replacement = " "))



# 03) Remove numbers from string ------------------------------------------

# numbers are generally not useful for the kind of analysis we want so we will have to remove those
# note for later if we look at comment id 1_2 ther is a +1, we will remove that at another point

rd_cm[rd_cm$comment_id == "1_2", ]$comment



# 04) Word/unigram --------------------------------------------------------

# Now we are going to 
#     (i)   break down the comments into unigrams
#     (ii)  remove numbers
#     (iii) remove stop words

uni_cm <- rd_cm %>% 
           unnest_tokens(word, comment, token = "ngrams", n = 1) %>% # (i)
           mutate(word = str_extract(word, "[a-z']+")) %>%           # (ii)
           anti_join(., stop_words, by = "word")                     # (iiI)

# from here we will do the rest of our analysis


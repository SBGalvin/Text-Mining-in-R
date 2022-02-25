
# 00) Notes ---------------------------------------------------------------
# this script is for setting options and loading R extension packages

# 01) Options -------------------------------------------------------------

set.seed(42)
options(scipen = 999, stringsAsFactors = FALSE)



# 02) Packages ------------------------------------------------------------

install.packages("tidytext")
install.packages("tidyverse")
install.packages("textdata")
install.packages("topicmodels")


library(tidyverse)
library(tidytext)
library(stringr)
library(topicmodels)
library(tidyr)


# 03) Additional data -----------------------------------------------------
# diictionaries may need additional downloads

data(stop_words)                 # stop word data
nrc   <- get_sentiments("nrc")   # sentiment lexicon: multicategory
bing  <- get_sentiments("bing")  # sentiment lexicon: binary
afinn <- get_sentiments("afinn") # sentiment lexicon:  -5 : 5

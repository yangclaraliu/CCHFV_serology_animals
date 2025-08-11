require(readxl)
require(tidyverse)
require(tidytext)
require(magrittr)

df <- read_xlsx("data/CCHF data_2.xlsx")

# categorise studies based on the species they look at
top_hitter_1 <- tibble(text = df$`Target species`) %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = T) %>% 
  dplyr::filter(n >= 5)

# we are including wild boars using bigrams, the deer here are also separate 
# species so that wouldn't count here either.
top_hitter_1 <- top_hitter_1[1:6,]

top_hitter_2 <- tibble(text = df$`Target species`) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = T) %>% 
  dplyr::filter(!is.na(bigram),
                n >= 5)

top_hitter <- c(top_hitter_1$word, top_hitter_2$bigram)

for(i in 1:length(top_hitter)){
  df[[top_hitter[i]]] <- grepl(top_hitter[i], 
                               df$`Target species`, 
                               ignore.case = T) %>% 
    as.numeric
}

df %<>% 
  mutate(total_animals = rowSums(across(all_of(top_hitter)))) %>% 
  rename(year_pub = `Year of publication`,
         year_study = `Study period`) 


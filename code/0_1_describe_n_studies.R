library(tidytext)

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

count_studies_by_species <- df[,c("Title", "First author", "Target species")]

for(i in 1:length(top_hitter)){
  count_studies_by_species[[top_hitter[i]]] <- grepl(top_hitter[i], 
                                                     count_studies_by_species$`Target species`, 
                                                     ignore.case = T) %>% 
    as.numeric
}

count_studies <- data.frame(top_hitter = top_hitter) %>% 
  mutate(n_studies = 0)

for(i in 1:length(top_hitter)){
  count_studies$n_studies[i] <- count_studies_by_species %>% 
    dplyr::filter(.data[[top_hitter[i]]] == 1) %>% 
    dplyr::select(Title, `First author`, top_hitter[i]) %>% 
    distinct() %>% 
    nrow()
}

count_studies_by_species %>% 
  mutate(total_animals = rowSums(across(all_of(top_hitter)))) %>% 
  dplyr::filter(total_animals == 0)%>% 
  dplyr::select(Title, `First author`, total_animals) %>% 
  distinct() %>% 
  nrow() -> n_studies_others

count_studies %<>% 
  add_row(top_hitter = "others",
          n_studies = n_studies_others)

count_studies %>% 
  arrange(desc(n_studies)) %>% 
  ggplot(., aes(x = reorder(top_hitter, -n_studies), y = n_studies)) +
  geom_bar(stat = "identity", alpha = 0.2, color = "black") +
  scale_x_discrete(labels = tools::toTitleCase) +
  theme_bw() +
  labs(x = "Species",
       y = "Number of studies found") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)) -> p

ggsave("figures/n_studies.png",
       p,
       width = 10, height = 10)

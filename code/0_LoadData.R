if(!require(pacman)) install.packages("pacman")
p_load(readxl, tidyverse, tidytext, magrittr, countrycode, meta)

df <- read_xlsx("data/CCHF data_2 (1).xlsx", sheet = "Data")

df %>% summary

study_by_year <- read_rds("data/study_by_year.rds")
study_by_loc <- read_rds("data/df_geocoded.rds")

# categorise studies based on the species they look at
top_hitter_1 <- tibble(text = df$`Host species`) %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = T) %>% 
  dplyr::filter(n >= 5)

# we are including wild boars using bigrams, the deer here are also separate 
# species so that wouldn't count here either.
top_hitter_1 <- top_hitter_1[1:6,]

top_hitter_2 <- tibble(text = df$`Host species`) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = T) %>% 
  dplyr::filter(!is.na(bigram),
                n >= 5)

top_hitter <- c(top_hitter_1$word, top_hitter_2$bigram)

for(i in 1:length(top_hitter)){
  df[[top_hitter[i]]] <- grepl(top_hitter[i], 
                               df$`Host species`, 
                               ignore.case = T) %>% 
    as.numeric
}

df[,"iso3"] <- countrycode(df$`Study (country)`, "country.name", "iso3c")
df[,"continent"] <- countrycode(df$`Study (country)`, "country.name", "continent")

data.frame(species = top_hitter) %>% 
  head(4) %>% 
  add_row(species = "others") %>% 
  mutate(labels = str_to_title(species)) -> tags_species

df %<>%
  mutate(prop = Numerator / Denominator) %>%
  rowwise() %>%
  mutate(ci = list(binom::binom.wilson(Numerator, Denominator, conf.level = 0.95)),
         continent = case_when(`Study (country)` == "Turkiye" ~ "Europe",
                               `Study (country)` == "Kosovo" ~ "Europe",
                               TRUE ~ continent)) %>% 
  unnest_wider(ci) %>% 
  left_join(study_by_year, by = "Title") %>% 
  left_join(  study_by_loc %>%
                distinct() %>% 
                dplyr::select(Title, lat, long, `Study (country)`, `Study (region)`),
            by = c("Title", "Study (country)","Study (region)")) %>% 
  mutate(bias = case_when(`Risk of bias` == "Low" ~ 2,
                          `Risk of bias` == "Moderate" ~ 1,
                          `Risk of bias` == "High" ~ 0),
         bias = factor(bias, levels = c(0, 1, 2)))

if(!require(pacman)) install.packages("pacman")
p_load(readxl, tidyverse, tidytext, magrittr, countrycode, meta)

df <- read_xlsx("data/CCHF data_2.xlsx")
study_by_year <- read_rds("data/study_by_year.rds")
study_by_year <- read_rds("data/df_geocoded.rds")

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

df %>% 
  mutate(total_animals = rowSums(across(all_of(top_hitter)))) %>% 
  rename(year_pub = `Year of publication`,
         year_study = `Study period`) 

df[,"iso3"] <- countrycode(df$`Study (country)`, "country.name", "iso3c")
df[,"continent"] <- countrycode(df$`Study (country)`, "country.name", "continent")

df %<>%
  mutate(prop = Numerator / Denominator) %>%
  rowwise() %>%
  mutate(ci = list(binom::binom.wilson(Numerator, Denominator, conf.level = 0.95))) %>%
  unnest_wider(ci)

# df %>% 
#   
# 
df %>%
  dplyr::filter(cattle == 1) %>%
  rownames_to_column(var = "id") %>%
  ggplot(., (aes(x = id, y = mean, color = `Risk of bias`))) +
  geom_point() +
  geom_segment(aes(y = lower, yend = upper, x = id, xend = id)) +
  facet_wrap(~`Risk of bias`)



require(meta)

m1 <- metaprop(
  event = Numerator,
  n = Denominator,
  studlab = `First author`,
  data = df %>% dplyr::filter(cattle == 1, !is.na(continent)),
  sm = "PLOGIT",     # logit transformed proportions
  method.ci = "WS", # CI method for individual studies
  comb.fixed = FALSE,   # random effects model
  comb.random = TRUE,
  method.tau = "ML",  # DerSimonian-Laird tau^2 estimator
  subgroup = `continent`, 
  hakn = TRUE           # Hartung-Knapp adjustment
)

forest(m1)
summary(m1)

m1_reg <- metareg(m1, ~ year)
summary(m1_reg)
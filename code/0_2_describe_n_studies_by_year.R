df %>% 
  dplyr::select(Title, `First author`, year_pub) %>% 
  distinct() %>% 
  ggplot(., aes(x = year_pub)) +
  geom_bar(stat = "count", alpha = 0.2, color = "black") +
  theme_bw() +
  labs(x = "Year of Publication",
       y = "Number of studies found") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)) -> p

ggsave("figures/n_studies_by_year_pub.png",
       p,
       width = 10, height = 10)

df %>% 
  separate_rows(year_study, sep = "\\s*;\\s*") %>% 
  dplyr::select(Title, `First author`, year_pub, year_study, `Target species`) %>% 
  dplyr::group_by(Title, `First author`, year_pub, year_study, `Target species`) %>% 
  distinct() %>% 
  separate(year_study, into = c("year_study_start",
                                "year_study_end"),
           sep = "-") %>%
  mutate(year_study_start_new = case_when(nchar(year_study_start) == 4 ~ ymd(paste0(year_study_start, "-01-01")),
                                          nchar(year_study_start) == 7 ~ my(year_study_start)),
         year_study_end = if_else(is.na(year_study_end), year_study_start, year_study_end),
         year_study_end_new = case_when(nchar(year_study_end) == 4 ~ ymd(paste0(year_study_end, "-12-31")),
                                        nchar(year_study_end) == 7 ~ my(year_study_end))) %>% 
  ungroup %>% 
  dplyr::select(Title, year_study_start_new, year_study_end_new) %>% 
  distinct() -> tab_year_study

tab_year_study %>% 
  dplyr::group_by(Title) %>% 
  summarise(year_study_start = min(year_study_start_new, na.rm = T),
            year_study_end = max(year_study_end_new, na.rm = T)) %>% 
  mutate(year_study = year(year_study_end)) -> tab_year_study_to_merge

write_rds(tab_year_study_to_merge, "study_by_year.rds")

df %>% 
  dplyr::filter(Title == "A seroepidemiological survey of Crimean Congo hemorrhagic fever among cattle in North Kordufan State, Sudan") %>% View()

tab_year_study %>% 
  .[complete.cases(.),] %>% 
  rowwise() %>% 
  mutate(date = list(seq(year_study_start_new,
                         year_study_end_new,
                         by = "day"))) %>% 
  unnest(date) -> tab_year_study_expanded

tab_year_study_expanded %>% 
  count(date, name = "n_studies") %>% 
  ggplot(., aes(x = date, y = n_studies)) +
  geom_col(fill = "#00464e")  +
  theme_bw() +
  labs(x = "Study period covered",
       y = "Number of studies found") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)) -> p

ggsave("figures/n_studies_by_year_study.png",
       p,
       width = 15, height = 10)

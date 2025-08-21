# mean sero prevalence by species and by continent

df %>%
  dplyr::filter(cattle == 1 | sheep == 1 | goat == 1 |  camel == 1) %>%
  dplyr::select(mean, lower, upper, year_study_start, `Study (country)`, continent, cattle, sheep, goat, camel) %>% 
  pivot_longer(cols = c("cattle", "sheep", "goat", "camel"),
               names_to = "species",
               values_to = "marker") %>% 
  mutate(species = factor(species, levels = c("cattle", "sheep", "goat", "camel"), labels = c("Cattle", "Sheep", "Goat", "Camel"))) %>% 
  dplyr::filter(marker == 1) %>% 
  ggplot(., aes(fill = species, x = species, y = mean)) +
  geom_boxplot() +
  facet_wrap(~continent) +
  ggsci::scale_fill_futurama() +
  theme_bw() +
  theme(strip.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none") +
  labs(y = "Mean seroprevalence", x = "Species")

ggsave("figures/mean_seroprevalence_by_species.png", width = 15, height = 8)


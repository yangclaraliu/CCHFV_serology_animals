require(tidygeocoder)
library(rnaturalearth)
library(sf)

geo_tags <- c("Title", "First author", "Study (country)", "Study (region)", "Latitude (N/S)","Longitude (E/W)")
sf_use_s2(FALSE) 

world <- ne_countries(scale = "medium", returnclass = "sf")
world_minus <- world %>%
 st_crop(xmin = -30, xmax = 140, ymin = -60, ymax = 90)

df %>% 
  dplyr::select(Title, long, lat, cattle, camel, sheep, goat, `Study (country)`) %>% 
  .[complete.cases(.),] %>% 
  mutate(others = if_else(cattle == 0 & sheep == 0 & goat == 0 & camel == 0, 1, 0)) %>% 
  pivot_longer(cols = tags_species$species, names_to = "species") %>% 
  dplyr::filter(value == 1) %>% 
  dplyr::select(-value) %>% 
  mutate(species = factor(species, levels = tags_species$species, labels = tags_species$labels)) -> tab

geocoded_studies <- st_as_sf(tab, coords = c("long", "lat"), crs = st_crs(world))

ggplot(world_minus) +
  geom_sf() +
  theme_void() +
  geom_sf(data = geocoded_studies, #%>%  dplyr::filter(`Study (country)` == "Poland"), 
          aes(color = species, shape = species),
          size = 3) +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  ggsci::scale_color_cosmic() +
  labs(color = "Species", shape = "Species") +  
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)),
         shape = guide_legend(override.aes = list(alpha = 1, size = 3))
  ) -> p

ggsave("figures/map_studies.png", p, height = 10, width = 10)

p +
  facet_wrap(~species, ncol = 2) +
  theme(# strip.text = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.spacing   = unit(2, "mm"),        # gap between panels (both axes)
        # or separately:
        # panel.spacing.x = unit(2, "mm"),
        # panel.spacing.y = unit(1, "mm"),
        strip.text      = element_text(margin = margin(2, 2, 2, 2)),  # thinner strip padding
        plot.margin     = margin(5, 5, 5, 5)) -> p_by_species

ggsave("figures/map_studies_by_species.png", p_by_species, height = 20, width = 15)

# we need to check if the geolocating algorithm is actually correct
joined <- st_join(geocoded_studies, world["name"])
joined %>% dplyr::filter(`Study (country)` != name) %>% 
  dplyr::select(name, `Study (country)`) %>% 
  distinct()

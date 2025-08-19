colnames(df)

geo_tags <- c("Title", "First author", "Study (country)", "Study (region)", "Latitude (N/S) (36ºN-38º60'N)","Longitude (E/W) (1º75'W-7º25'W)" )

df[,geo_tags] %>% 
  mutate(address = paste( `Study (region)`, ", ",`Study (country)`)) %>% 
  geocode(address, method = "arcgis") -> df_geocoded

tmp <- geocode(data.frame(address = "India"), address, method = "arcgis")


ç[(is.na(df_geocoded$long) & df_geocoded$`Study (country)` == "India"), c("lat", "long")] <- tmp[,c("lat", "long")]
  

write_rds(df_geocoded, "data/df_geocoded.rds")

df_geocoded %>% 
  dplyr::select(Title, `First author`, "lat", "long") %>% 
  distinct()

library(rnaturalearth)
library(sf)

sf_use_s2(FALSE) 

world <- ne_countries(scale = "medium", returnclass = "sf")
world_minus <- world %>%
  st_crop(xmin = -30, xmax = 140, ymin = -60, ymax = 90)

geocoded_studies <- st_as_sf(df_geocoded %>% .[complete.cases(.),], coords = c("long", "lat"), crs = st_crs(world))

ggplot(world_minus) +
  geom_sf() +
  theme_void() +
  geom_sf(data = geocoded_studies, alpha = 0.5, shape = 21) -> p

ggsave("figures/map_studies.png", p, height = 10, width = 10)

# only re-run this segment of codes if you would like to download the coordinates 
# from arcgis again.

df[,geo_tags] %>% 
  mutate(address = paste( `Study (region)`, ", ",`Study (country)`)) %>% 
  distinct() %>% 
  geocode(address, method = "arcgis") -> df_geocoded

tmp <- geocode(data.frame(address = "India"), address, method = "arcgis")
df_geocoded[(is.na(df_geocoded$long) & df_geocoded$`Study (country)` == "India"), c("lat", "long")] <- tmp[,c("lat", "long")]

tmp <- geocode(data.frame(address = "Poland"), address, method = "arcgis")
df_geocoded[df_geocoded$`Study (country)` == "Poland", c("lat", "long")] <- tmp[,c("lat", "long")]

tmp <- geocode(data.frame(address = "Bosnia and Herzegovina"), address, method = "arcgis")
df_geocoded[df_geocoded$`Study (country)` == "Bosnia and Herzegovina" & df_geocoded$lat < 0, c("lat", "long")] <- tmp[,c("lat", "long")]

tmp <- geocode(data.frame(address = "Benin"), address, method = "arcgis")
df_geocoded[df_geocoded$`Study (country)` == "Benin",  c("lat", "long")] <- tmp[,c("lat", "long")]

write_rds(df_geocoded, "data/df_geocoded.rds")

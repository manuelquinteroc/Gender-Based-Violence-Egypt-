# Figure S6: Survey responses by Egyptian Governatore
sf::sf_use_s2(FALSE)
latlong <- read.xlsx("Datasets/Map/latlong_base_endrespondents.xlsx", "Sheet1")

# latlong <- filter(latlong, responded == 1)

latlong <- filter(latlong, should_exclude == 0)

boundaries <- st_read("Datasets/Map/egy_admbnda_adm1_capmas_20170421/egy_admbnda_adm1_capmas_20170421.shp", quiet = TRUE)

lat_long_groups <- latlong %>% group_by(LocationLatitude, LocationLongitude) %>% 
  count()%>%
  arrange(desc(n))

# Append governatores names ----------------------------------------------------
st_coord <- st_as_sf(latlong, coords = c("LocationLongitude", "LocationLatitude"), crs = 4326)

# Check which respones belong to one governatore
st_coord$governatore <- 0 
# Obtain indexes where st_within find a governatore for a response and match to the boundaries' name
st_coord$governatore[st_within(st_coord, boundaries) %>% lengths > 0] <- boundaries[unlist(st_within(st_coord, boundaries, drop=FALSE)),]$ADM1_EN

# Write dataset with municipalities names
write.csv(st_coord, 'Datasets/Map/latlong_base_endrespondents_names.csv')

# ------------------------------------------------------------------------------

lat_long_sf <- latlong %>% dplyr::select(LocationLatitude, LocationLongitude) %>% drop_na() %>%
  st_as_sf(coords = c("LocationLongitude", "LocationLatitude"), crs = 4326)

boundaries$Responses <- boundaries %>% aggregate(mutate(lat_long_sf, counter = 1),., sum) %>%
  pull(counter)

provinces <- dplyr::select(st_drop_geometry(boundaries), ADM1_REF, ADM1_PCODE, Responses) %>% 
  arrange(desc(Responses))

resp_plot <- ggplot(data = boundaries) + geom_sf(aes(fill = Responses)) +  theme_map() + 
  scale_fill_viridis(trans = "log", 
                     labels = round(quantile(boundaries$Responses, probs = c(0.01, 0.33, 0.67, 1), na.rm = TRUE), digits = 0),
                     breaks = quantile(boundaries$Responses, probs = c(0.01, 0.33, 0.67, 1), na.rm = TRUE), 
                     discrete = FALSE) +
  geom_sf_text(aes(label = ADM1_EN), size=2, color = "black", seed = 10) +
  theme(legend.position = c(0.25, -0.15), legend.direction = "horizontal", legend.box.spacing = unit(0,"cm"), 
        legend.key.width = unit(1, "cm"),
        plot.margin=unit(c(1,0.75,1.55,1.2),"cm")) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, title.vjust = 0.5)) 

ggsave(resp_plot, filename = "Figures/S6.pdf", device = cairo_pdf, 
       width = 7, height = 5, units = "in", dpi = 300)


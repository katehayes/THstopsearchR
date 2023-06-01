# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #making map of the LSOAs and wards# # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

th_map_lsoa <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, lsoa_shape) %>% 
  mutate(count = 0) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_lsoa_shape)

  
th_map_ward <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(ward) %>% 
  mutate(count = 0) %>% 
  summarise(count = sum(count)) %>% 
  left_join(w_shape %>% 
              filter(LAD22NM == "Tower Hamlets") %>% 
              select(WD22NM, geometry) %>% 
              rename(ward = WD22NM)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)
  

th_map_ward <- ss_london %>% 
  st_drop_geometry() %>% 
  filter(LA %in% c("Tower Hamlets", "Hackney", "City of London"),
         !(LA == "Hackney" & !(ward %in% c('Hoxton East & Shoreditch',
                                           'Haggerston',
                                           'London Fields',
                                           'Victoria',
                                           'Hackney Wick')))) %>% 
  group_by(ward) %>% 
  mutate(count = 0) %>% 
  summarise(count = sum(count)) %>% 
  left_join(w_shape %>% 
              filter(LAD22NM %in% c("Tower Hamlets", "Hackney", "City of London")) %>% 
              select(WD22NM, geometry) %>% 
              rename(ward = WD22NM)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)


th_map <- ggplot() + 
  geom_sf(data = th_map_ward, fill = "grey", color = "grey") +
  geom_sf(data = th_map_lsoa, fill = NA, colour="white", linewidth = 0.2) +
  geom_sf(data = th_map_ward, fill = NA, color = "black", linewidth = 0.5) +
  # geom_text(data =ward_coords, aes(x = X, y = Y, label = ward)) +
  geom_sf_text(data = th_map_ward, aes(label = str_wrap(ward, 2)), colour = "black", size = 2.5) +
  theme_bw()

th_map 


check <- ss_london %>% 
  filter(is.na(ward))

check <- lsoa2LA %>% 
  filter(WD21NM == "Aldgate")

# ward_centroids <- st_centroid(th_map_ward)
# ward_coords <- as.data.frame(st_coordinates(ward_centroids))
# ward_coords$ward <- ward_centroids$ward

  
london_map <- road_lines %>% 
  ggplot() + 
  geom_sf()

london_map

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #Looking at relative frequency of stop and search # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


ss_map <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, ward, lsoa_shape) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
    geom_sf(colour="white") +
    scale_fill_viridis(option = "magma")

ss_map

# number of days in last year (excl november) that there was a s and s
ss_day_map <- ss_th %>% 
  st_drop_geometry() %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_day_map


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #Looking at use of stop and search powers # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

ss_sec60day_map <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Criminal Justice and Public Order Act 1994 (section 60)") %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_sec60_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_sec60day_map

ss_PACEday_map <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Police and Criminal Evidence Act 1984 (section 1)") %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_PACE_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_PACEday_map

ss_sec23day_map <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Misuse of Drugs Act 1971 (section 23)") %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_sec23_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_sec23day_map

ss_sec47day_map <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Firearms Act 1968 (section 47)") %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_sec47_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_sec47day_map







# day hour month? to find any patterns/temporal hotspots
ss_dhm_map <- ss_th %>% 
  st_drop_geometry() %>% 
  mutate(count = 1) %>% 
  group_by(time, day, month, lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  filter(lsoa == "Tower Hamlets 021C") %>% 
  ggplot() +
  geom_bar(aes(x = time, y = count), stat = "identity", position = "dodge2") +
  facet_wrap(~interaction(day, month))
ss_dhm_map





ss_map_month <- ss_th  %>% 
  group_by(lsoa, month) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  facet_wrap(~month) +
  scale_fill_viridis(option = "plasma")

ss_map_month



ss_powerspc <- ss_th %>% 
  group_by(lsoa, powers) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "powers",
              values_from = "count",
              values_fill = 0) %>% 
  mutate(tot = `Police and Criminal Evidence Act 1984 (section 1)` + `Misuse of Drugs Act 1971 (section 23)` +
           `Criminal Justice and Public Order Act 1994 (section 60)` + `Firearms Act 1968 (section 47)`,
         `Police and Criminal Evidence Act 1984 (section 1)` = `Police and Criminal Evidence Act 1984 (section 1)`/tot,
         `Misuse of Drugs Act 1971 (section 23)` = `Misuse of Drugs Act 1971 (section 23)`/tot,
         `Criminal Justice and Public Order Act 1994 (section 60)` = `Criminal Justice and Public Order Act 1994 (section 60)`/tot,
         `Firearms Act 1968 (section 47)` = `Firearms Act 1968 (section 47)`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(-c(lsoa, lsoa_shape),
               names_to = "powers",
               values_to = "pc") 


ss_power1 <- ss_powerspc %>% 
  filter(powers == "Police and Criminal Evidence Act 1984 (section 1)") %>% 
  ggplot(aes(fill=pc)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "plasma")

ss_power1


ss_power2 <- ss_powerspc %>% 
  filter(powers == "Misuse of Drugs Act 1971 (section 23)") %>% 
  ggplot(aes(fill=pc)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "plasma")

ss_power2


ss_power3 <- ss_powerspc %>% 
  filter(powers == "Criminal Justice and Public Order Act 1994 (section 60)") %>% 
  ggplot(aes(fill=pc)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "plasma")

ss_power3

ss_power4 <- ss_powerspc %>% 
  filter(powers == "Firearms Act 1968 (section 47)") %>% 
  ggplot(aes(fill=pc)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "plasma")

ss_power4

# Police and Criminal Evidence Act 1984 (section 1)
# Misuse of Drugs Act 1971 (section 23)
# Criminal Justice and Public Order Act 1994 (section 60)
# Firearms Act 1968 (section 47)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #Looking at use type of searches # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


ss_th_car <- ss_th %>% 
  filter(search_type %in% c("Person and Vehicle search", "Vehicle search")) %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, ward, lsoa_shape) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_lsoa_shape) %>% 
  bind_rows(add_missing_lsoa(ss_data = ss_th %>% 
                               filter(search_type %in% c("Person and Vehicle search", "Vehicle search")),
                             full_lsoa_list = th_lsoa_list, full_lsoa_shapes = lsoa_shape)) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")



ss_th_car

ss_th_person <- ss_th %>% 
  filter(search_type == "Person search")

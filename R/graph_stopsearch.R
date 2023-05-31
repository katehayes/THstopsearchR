

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
    scale_fill_viridis(option = "plasma")

ss_map


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








test <- get_googlemap(center = c(lon=-0.205, lat=51.516),
                      zoom = 16, maptype = "satellite", color="bw")


ggplot(data=subset(map.data, !is.na(HLE)), aes(fill=HLE, geometry=geometry))+
  geom_sf(colour=NA)
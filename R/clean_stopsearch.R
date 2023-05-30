w_shape <- st_read("/Users/katehayes/CLmodelR/temp_data/Wards_December_2022_Boundaries_UK_BFC_-3416072881830331872/WD_DEC_2022_UK_BFC.shp")
# going to use ward boundaries from the same place as below, the place the police linked, and hope it works
# actually maybe I've changed back
# w_shape <- st_read("/Users/katehayes/THdata/Wards_(E+W)_2011_Boundaries_(Full_Extent)/WD_DEC_2011_EW_BFE.shp")

# lsoa_shape <- st_read("/Users/katehayes/CLmodelR/temp_data/LSOA_Dec_2021_Boundaries_Full_Clipped_EW_BFC_2022_4005706377815092351/LSOA_2021_EW_BFC_V7.shp")
# lsoa_shape <- st_read("/Users/katehayes/THdata/LSOA_2011_EW_BFC_shp/LSOA_2011_EW_BFC.shp")
# god, police are using 2011 LSOAs for the data release..
# https://webarchive.nationalarchives.gov.uk/ukgwa/20160110200248/http://www.ons.gov.uk/ons/guide-method/geography/products/census/spatial/2011/index.html
# this is the link from the police data website
lsoa_shape <- st_read("/Users/katehayes/THdata/Lower_layer_super_output_areas_(E+W)_2011_Boundaries_(Full_Extent)_V2/LSOA_2011_EW_BFE_V2.shp")




# lsoa2LA <- read.csv("/Users/katehayes/THdata/OAs_to_LSOAs_to_MSOAs_to_LEP_to_LAD_(December_2022)_Lookup_in_England_(V2).csv")
lsoa2LA <- read_xlsx("/Users/katehayes/THdata/LSOA11_WD21_LAD21_EW_LU_V2.xlsx")
lsoa2ward <- read.csv("/Users/katehayes/THdata/LSOA_(2021)_to_Ward_to_Lower_Tier_Local_Authority_(May_2022)_Lookup_for_England_and_Wales.csv")
lsoa2lsoa <- read.csv("/Users/katehayes/THdata/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2).csv")


# this downloaded set is mising november...
common_path <- "/Users/katehayes/THdata/cb8d2cbd6199eb138766f1121208a4ba11593341"

files_to_read <- list.files(
  path = common_path,        # directory to search within
  pattern = "-metropolitan-stop-and-search.csv$",
  recursive = TRUE,          # search subdirectories
  full.names = TRUE          # return the full path
)

ss_geom <- lapply(files_to_read, read.csv) %>% 
  bind_rows() %>% 
  filter(!is.na(Latitude), 
         !is.na(Longitude)) %>% 
  distinct(Latitude, Longitude) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


# ss_geom$ward <- lonlat_to_ward(ss_sf = ss_geom)
# ss_geom$ward_shape <- lonlat_to_wshape(ss_sf = ss_geom)
ss_geom$lsoa <- lonlat_to_lsoa(ss_sf = ss_geom)
ss_geom$lsoa_shape <- lonlat_to_lsoashape(ss_sf = ss_geom)

ss_geom <- ss_geom %>% 
  left_join(lsoa2LA %>% 
              select(LSOA11NM, LAD21NM, WD21NM) %>% 
              rename(lsoa = LSOA11NM,
                     LA = LAD21NM,
                     ward = WD21NM))


ss_raw_03_22_03_23 <- lapply(files_to_read, read.csv) %>% 
  bind_rows() %>% 
  filter(!is.na(Latitude), 
         !is.na(Longitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_join(ss_geom)
 # have not yet managed to add ward shape - but thats fine


ss_th <- ss_raw_03_22_03_23 %>% 
  filter(LA == "Tower Hamlets") %>% 
  mutate(long_date = ymd_hms(Date),
         short_date = as.Date(Date)) %>% 
  mutate(time = hour(long_date),
         day = wday(long_date, label = TRUE),
         month = month(long_date, label = TRUE))

ss_th %>% 
  st_drop_geometry() %>% 
  filter(ward == "Mile End") %>% 
  mutate(count = 1) %>% 
  group_by(month, day) %>% 
  summarise(count = sum(count)) %>% 
  ggplot() +
  geom_bar(aes(x = day, y = count), 
           stat = "identity", position = "dodge2") +
  facet_wrap(~month)


  # check <- w_shape %>% 
#   select(WD22NM, geometry) %>% 
#   rename(ward = WD22NM,
#          ward_shape = geometry)

# check <- w_shape %>% 
#   filter(LAD22NM == "Tower Hamlets") %>% 
#   select(WD22NM)
# 
# check2 <- lsoa2LA %>% 
#   filter(LAD21NM == "Tower Hamlets") %>% 
#   select(WD21NM) %>% 
#   distinct(WD21NM)

# 


# OK we notice an inconsistency - the ward shape and LSOA shape seem to conflict sometimes
# the ward says whitechapel and the lsoa says hackney or city of london
 ss_th <- ss_raw_03_22_03_23 %>% 
  filter(LA == "Tower Hamlets")

 ss_th2 <- ss_raw_03_22_03_23 %>% 
   filter(grepl("Tower Hamlets", lsoa))
 
 # check <- ss_raw_03_22_03_23 %>% 
 #   st_drop_geometry() %>% 
 #   filter(LA == "Tower Hamlets") %>% 
 #   distinct(ward, ward2)
 
 check <- ss_raw_03_22_03_23 %>%
   st_drop_geometry() %>%
   filter(LA == "Tower Hamlets") %>%
   distinct(lsoa)
 


w_shape <- st_read("/Users/katehayes/CLmodelR/temp_data/Wards_December_2022_Boundaries_UK_BFC_-3416072881830331872/WD_DEC_2022_UK_BFC.shp")
lsoa_shape <- st_read("/Users/katehayes/CLmodelR/temp_data/LSOA_Dec_2021_Boundaries_Full_Clipped_EW_BFC_2022_4005706377815092351/LSOA_2021_EW_BFC_V7.shp")


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


ss_geom$LA <- lonlat_to_LA(ss_sf = ss_geom) 
ss_geom$ward <- lonlat_to_ward(ss_sf = ss_geom)
ss_geom$lsoa <- lonlat_to_lsoa(ss_sf = ss_geom)
ss_geom$ward_shape <- lonlat_to_wshape(ss_sf = ss_geom)
ss_geom3$lsoa_shape <- lonlat_to_lsoashape(ss_sf = ss_geom)

ss_raw_03_22_03_23 <- lapply(files_to_read, read.csv) %>% 
  bind_rows() %>% 
  filter(!is.na(Latitude), 
         !is.na(Longitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_join(ss_geom)



ss_check <- ss %>% 
  distinct(Latitude, Longitude) %>% 
  filter(!is.na(Latitude), 
         !is.na(Longitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ss_check$LA <- lonlat_to_LA(ss_sf = ss_check) 
ss_check$ward <- lonlat_to_ward(ss_sf = ss_check)
ss_check$lsoa <- lonlat_to_lsoa(ss_sf = ss_check)
ss_check$ward_shape <- lonlat_to_wshape(ss_sf = ss_check)
ss_check$lsoa_shape <- lonlat_to_lsoashape(ss_sf = ss_check)



ss_check2 <- ss_check %>% 
  filter(LA == "Tower Hamlets")


ss_check3 <-  ss %>% 
  filter(!is.na(Latitude), 
         !is.na(Longitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_join(ss_check)

# 
# 
# w_shape <- st_read("/Users/katehayes/temp_data/Wards_December_2022_Boundaries_UK_BFC_-3416072881830331872/WD_DEC_2022_UK_BFC.shp")
# lsoa_shape <- st_read("/Users/katehayes/temp_data/LSOA_Dec_2021_Boundaries_Full_Clipped_EW_BFC_2022_4005706377815092351/LSOA_2021_EW_BFC_V7.shp")
# 
# 
# common_path <- "/Users/katehayes/temp_data/bc285009488bfa668a43b86ba318bc626135d92d"
# 
# files_to_read <- list.files(
#   path = common_path,        # directory to search within
#   pattern = "-west-midlands-stop-and-search.csv$",
#   recursive = TRUE,          # search subdirectories
#   full.names = TRUE          # return the full path
# )
# 
# ss_raw_04_20_03_23 <- lapply(files_to_read, read.csv)
# 
# ss_04_20_03_23 <- ss_clean(raw_list = ss_raw_04_20_03_23)
# 
# ss <- bind_rows(ss_04_20_03_23)
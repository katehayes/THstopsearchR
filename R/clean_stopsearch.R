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
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) # 4326


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
         month = month(long_date, label = TRUE)) %>% 
  select(-c(Part.of.a.policing.operation, Policing.operation, 
            Outcome.linked.to.object.of.search, Removal.of.more.than.just.outer.clothing)) %>%  # these are all blank/non-informative
  rename(outcome = Outcome,
         powers = Legislation,
         search_type = Type,
         date = Date,
         gender = Gender,
         age = Age.range,
         ethnicity_self = Self.defined.ethnicity,
         ethnicity_officer = Officer.defined.ethnicity,
         reason = Object.of.search)

check <- ss_th %>% 
  st_transform(crs = 3857)


st_geometry(ss_th) <- "lsoa_shape"
  
  
th_lsoa_list <- lsoa2LA %>% 
  filter(LAD21NM == "Tower Hamlets") %>% 
  select(LSOA11NM, WD21NM) %>% 
  rename(lsoa = LSOA11NM,
         ward = WD21NM)

lsoa_list <- ss_th %>% 
  st_drop_geometry() %>% 
  distinct(lsoa)

missing_lsoa <- th_lsoa_list %>% 
  filter(!(lsoa %in% lsoa_list$lsoa)) %>% 
  select(lsoa)

missing_lsoa_shape <- th_lsoa_list %>% 
  filter(!(lsoa %in% lsoa_list$lsoa)) %>% 
  left_join(lsoa_shape %>% 
              select(LSOA11NM) %>% 
              rename(lsoa = LSOA11NM)) %>% 
  rename(lsoa_shape = geometry) %>% 
  mutate(count = 0) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)
  


 


library(tidyverse)
library(readxl)
library(parzer)
library(sf)
library(leaflet)
library(htmlwidgets)
library(rgdal)
library(zip)
library(aws.s3)

sw <- read_excel(file.choose())

#names(sw)
#sw %>% select(Name,LATMIN, LATMAX, LONMIN, LONMAX) %>% View()
sw2 <- sw %>%
  filter(Confidential != 'Y') %>% 
  mutate(LONMAX = str_replace(LONMAX, 'º', '°'),
         LONMIN = str_replace(LONMIN, 'º', '°')) %>% 
  mutate(LATMAX_dec = -abs(parse_lat(LATMAX)),
         LATMIN_dec = -abs(parse_lat(LATMIN)),
         LONMAX_dec = parse_lon(LONMAX),
         LONMIN_dec = parse_lon(LONMIN),
         lon = case_when(
           !is.na(LONMIN_dec) & !is.na(LONMAX_dec) ~ (LONMIN_dec+LONMAX_dec)/2,
           is.na(LONMIN_dec) & !is.na(LONMAX_dec) ~ LONMAX_dec,
           !is.na(LONMIN_dec) & is.na(LONMAX_dec) ~ LONMIN_dec),
          lat = case_when(
             !is.na(LATMIN_dec) & !is.na(LATMAX_dec) ~ (LATMIN_dec+LATMAX_dec)/2,
             is.na(LATMIN_dec) & !is.na(LATMAX_dec) ~ LATMAX_dec,
             !is.na(LATMIN_dec) & is.na(LATMAX_dec) ~ LATMIN_dec),
         ) %>% filter(!is.na(lon) & !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4283) %>% 
  select(
    "name" = Name,
    "type_of_si" = Type_of_site,
    "rig" = Rig,
    "when_lost" = When_Lost,
    "protected" = Protected,
    "date_inspe" = Date_Inspected,
    "where_lost" = Where_Lost,
    "region" = Region,
    LATMAX,LATMIN, LONMAX,LONMIN,
    LATMAX_dec, LATMIN_dec, LONMAX_dec, LONMIN_dec,
    "position" = Position_info,
    "constucti" = Construction,
    "engine" = Engine,
    'tona' = TONA,
    'tonb' = TONB,
    'country_bu' = Country_Built,
    'port_built' = Port_Built,
    'when_built' = When_Built,
    'port_regis' = Port_Registered,
    'offical_n' = Official_Number,
    'registrati' = Registration_Number,
    'length' = Length,
    'beam' = Beam,
    'draft' = Draft,
    'passengers' = Passengers, 
    'deaths' = Deaths,
    'crew' = Crew,
    'sinking' = Sinking,
    'sunk_code' = Sunk_Code,
    'port_from' = Port_from,
    'port_to' = Port_to,
    'master' = Master,
    'owner' = Owner,
    'file_numb' = File_Number,
    'found_rewa' = `When Found Reward`,
    'sources' = Sources,
    'industry1' = Industry_1,
    'industry2' = Industry_2,
    'url' = URL,
    'len_site' = Length_of_site,
    "bearin_bo" = Bearing_to_Bow,
    'min_depth' = `Min Depth of site`,
    'date_depth' = `Date depth taken`,
    'time_depth' = `Time Depth taken`,
    'max_depth' = `Max depth site`,
    'unique_n' = `Unique number`,
  ) %>% filter(abs(LATMAX_dec-LATMIN_dec) < 2 & abs(LONMAX_dec-LONMIN_dec) < 2)

#what about cargo, chartno, comments, found, date inspected

sw2  %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(popup = ~name, clusterOptions = markerClusterOptions())

#Export as a shapefile and zip it
dir.create(paste0(tempdir(),"/shipwrecks"))
st_write(sw2, paste0(tempdir(),"/shipwrecks/ShipwrecksWAM.shp"), delete_layer = TRUE, driver = "ESRI Shapefile")
zipr(zipfile = paste0(tempdir(),'/shipwrecks.zip'), files = paste0(tempdir(),"/shipwrecks"))

#Upload to cloud
put_object(file = paste0(tempdir(),'/shipwrecks.zip'),
           object = "shipwrecks.zip",
           region = "",
           bucket = "",
           key = "",
           secret = ""
           )


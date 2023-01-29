#API key: AIzaSyBUvQmqxo0FlDZPZv7YFgiSE9KVMbotMBo


#options(timeout = 1000)
#remotes::install_github("wfmackey/absmapsdata")

theme_set(theme_minimal())
options("scipen"=100, "digits"=2)

library(tidyverse)
library(readxl)
library(janitor)
library(reactable)

library(sf)
library(absmapsdata)

#----------

#map
map_sa2 <- sa22021
map_sa1 <- sa12021

map_sa2_2016<-sa22016

map_sa3 <- sa32021

map_sa4 <- sa42021


#import 
data_location<-"/Users/e5028514/Desktop/childcare/data/Linked Dataset_27_JAN_mp.xlsx"
#tab_names<-excel_sheets(path = data_location)

#sarah's data
#need to install cellranger package to use large cell block
#install.packages("cellranger")

linked_data<-read_excel(data_location, sheet=1, range="A5:AO2478")

nqs_data<-read_excel(data_location, sheet="NQS July 2022 data", range="A10:AA11640")

gis_data<-read_excel(data_location, sheet="QGIS Output", range="B15:H11644", col_names=FALSE)
colnames(gis_data)<-c("service_approval_number", "name", "post_code", "state", "full_name", "Lat", "Long")
#data_var<-read_excel(data_location, sheet=1, range="A4:AM4", col_names=FALSE)                        

linked_data<-linked_data%>%clean_names()

nqs_data<-nqs_data%>%clean_names()

gis_data<-gis_data%>%clean_names()

#test

nqs_data%>%filter(!service_approval_number %in%gis_data$service_approval_number )

gis_data%>%filter(is.na(Lat)|is.na(Long) |Lat==0 |Long==0)


#join center data (nqs) with gis_data to get Lat and Long

center_data<-left_join(nqs_data, gis_data)

#remove seifa - incorrect coding
center_data<-center_data%>%select(-c(
  seifa,
  sa2_code,
  sa2_name,
  post_code,
  state,
  full_name,
  latitude,
  longitude)
)

#center_data%>%write_csv("data/center_data.csv")

#center_data<-read_csv("data/center_data.csv")

#convert to sf

#ABS maps. 2021
map_sa2 <- sa22021

map_sa3 <- sa32021

map_sa4 <- sa42021

#ABS maps 2016
map_sa2_2016 <- sa22016

center_data = st_as_sf(center_data, coords = c("long", "lat"), crs = 4326)

class(center_data)

center_data_map_2021 <- st_join(map_sa2, center_data , 
                                join = st_contains)

center_data%>%filter(is.na(service_approval_number))

center_data_map_2021<-center_data_map_2021%>%filter(!is.na(service_approval_number))

#center_data_map_2021%>%st_write("data/center_data_map_2021.geojson")

#center_data_map_2021<-st_read("data/center_data_map_2021.geojson", quiet=TRUE)

center_data_map_2016 <- st_join(map_sa2_2016, center_data, 
                                join = st_contains)

center_data_map_2016<-center_data_map_2016%>%filter(!is.na(service_approval_number))

#center_data_map_2016%>%st_write("data/center_data_map_2016.geojson")

#center_data_map_2016<-st_read("data/center_data_map_2016.geojson", quiet=TRUE)



#select only nqs and sa2_code_2016, sa2_5dig_2016, sa2_name_2016, sa3_code_2016, sa3_name_2016, sa4_code_2016, sa4_name_2016

center_data_map_2016<-center_data_map_2016%>%select(
  c(service_approval_number,
    sa2_code_2016, sa2_5dig_2016, sa2_name_2016, 
    sa3_code_2016, sa3_name_2016, 
    sa4_code_2016, sa4_name_2016
)
)

center_data_map_2016<-st_drop_geometry(center_data_map_2016)

#center_data_map_2016%>%write_csv("data/center_data_map_2016_flat.csv")

#merge to 2021

center_data_map_2021<-left_join(center_data_map_2021, center_data_map_2016)

#center_data_map_2021%>%st_write("data/center_data_map_2021.geojson", append=TRUE)

#center_data_map_2021%>%st_write("data/center_data_map_2021.shp", append=TRUE)

#center_data_map_2021<-st_read("data/center_data_map_2021.geojson", quiet=TRUE)

#fee's data - based on sa3
fee_data<-read_excel("/Users/e5028514/Desktop/childcare/data/CBDC_fees-mp.xlsx", 
                     sheet="CBDC Fees", 
                     range="A3:J336", na=c("", "."))

fee_data<-fee_data%>%
  clean_names()%>%
  select(-c
         (sa4_name, state, sa4_code, sa3_name)
         )

fee_data<-fee_data%>%
  mutate(
    sa3_code=as.character(sa3_code)
  )

#merge with center data center_data_map_2021

center_data_map_2021<-left_join(center_data_map_2021, fee_data, by=c("sa3_code_2021"="sa3_code"))

#center_data_map_2021%>%st_write("data/center_data_map_2021.geojson", append=TRUE)
#center_data_map_2021%>%st_write("data/center_data_map_2021.shp", append=TRUE) #give error




#center_data_map_2021<-st_read("data/center_data_map_2021.geojson", quiet=TRUE)


#----------------------------
#---adding sfia data - based on 2016
# SEIFA data
# SEIFA for SA2 areas only available for 2016 SA areas but main linked data set uses 2021 SA. Is it worth transposing to 2021 or is there another better way of measuring this? 


seifa_data<-read_excel(data_location, sheet="SEIFA data", range="A10:G2194")
seifa_data<-seifa_data%>%select(
  -c(Code, Name)
)

colnames(seifa_data)<-c("sa2_code_2016", "sa2_name_2016","seifa_rank", "seifa_decile", "seifa_percentile")

seifa_data<-seifa_data%>%
  mutate(
    sa2_code_2016=as.character(sa2_code_2016)
  )

center_data_map_2021<-left_join(center_data_map_2021, seifa_data)


#!!!!delete existing geojson before recording
#center_data_map_2021%>%st_write("data/center_data_map_2021.geojson", append=TRUE)

#center_data_map_2021<-st_read("data/center_data_map_2021.geojson", quiet=TRUE)

#_______________

#--------
#connect maternal data
#Perinatal Health Data
#It contains six fairly straightforward variables that measure either things going well maternal and child health (attending 5 or more antenatal visits, attending an antenatal visit in the first trimester), or measure disadvantage/systems not working well (smoking during pregnancy, teenage mothers, low birth weight, small for gestational age).
#These variables are only available in SA3s



prenatal_data<-read_excel("/Users/e5028514/Desktop/childcare/data/SA3 Perinatal Health Data.xlsx", sheet=1, range="A8:R321", col_names=FALSE)

colnames(prenatal_data)<-c(
  "state", "sa3_code_2021", "sa3_name_2021", 
  "prenatal_5visits_n", "prenatal_5visits_perc",
  "prenatal_1tri_n", "prenatal_1tri_perc",
  "prenatal_smoking_n", "prenatal_smoking_perc",
  "prenatal_teen_mother_n", "prenatal_teen_n", "prenatal_teen_mother_perc",
  "prenatal_live_births_n", "prenatal_low_birthweight_n", "prenatal_low_birthweight_perc",
  "prenatal_singletons_n", "prenatal_small_n", "prenatal_small_perc"
)

prenatal_data<-prenatal_data%>%
  select(
    -c(
      state,
      sa3_name_2021
    )
  )

prenatal_data<-prenatal_data%>%
  mutate(
    sa3_code_2021=as.character(sa3_code_2021)
  )

center_data_map_2021<-left_join(center_data_map_2021, prenatal_data)

#--------
#connect enrolment data
# no change since updated file is used
#Enrolment Data
#I’ve attached an updated version of the main Linked Dataset document that contains two new columns on the main LINKED DATA sheet (columns X and Y) that capture 4 and 5 year olds who are not enrolled in primary school or preschool. The % rate in Column Y is a measure of the rate of 4 and 5 year olds who are aren’t old enough for primary school, but also aren’t attending preschool. It’s a proxy measure rather than a direct measure – additional info on this including source info is in the ‘Enrolment’ sheet within the document.


#-------FINAL SET

center_data_map_2021%>%st_write("data/center_data_map_2021.geojson", append=TRUE)

#center_data_map_2021<-st_read("data/center_data_map_2021.geojson", quiet=TRUE)

center_data_map_2021_flat<-st_drop_geometry(center_data_map_2021)

center_data_map_2021_flat%>%write_csv("data/center_data_map_2021_flat.csv")


#--------------------
#STOPPED HERE
#EXPLORATION

full_data_flat<-full_data_flat%>%
  mutate(
    sep_2021_mean_fee_per_hour=parse_number(sep_2021_mean_fee_per_hour)
    )

full_data_flat%>%
group_by(sa4_code)%>%
  summarise(
    max_meanFee=max(sep_2021_mean_fee_per_hour, na.rm=TRUE),
    min_meanFee=min(sep_2021_mean_fee_per_hour, na.rm=TRUE),
    mean_meanFee=mean(sep_2021_mean_fee_per_hour, na.rm=TRUE)
  )%>%ungroup()
  








#---------
# get addresses 

library(OpenStreetMap)
library(osmdata)
aus_bb <- getbb("Melbourne")
public_trasport<-aus_bb%>%opq()%>%
  add_osm_feature(key="public_transport", value="station")%>%
  osmdata_sf()


public_trasport$osm_points
#available features

library(ggmap)
aus_map <- get_map(aus_bb, maptype = "roadmap")

available_features()
#note: wholesale, cuisine, wholesale

available_tags("public_transport")
#note: childcare, kindergarten
#food_court, fast_food, biergarten, drinking_water, kitchen(?), marketplace (?), pub, water_point

####### user openstreetmap via tidygeocoder

library(tidygeocoder)

#read addresses



data_address<-read_excel(data_location, sheet="QGIS Output", range="C15:H11644", col_names=FALSE) 
colnames(data_address)<-c("name", "post_code", "state", "full_name", "Lat", "Long")

address_missing <- data_address %>%filter(Lat==0)
  geocode(address = full_name, method = "osm", verbose = TRUE)


#google maps
library(googleway)

key <- ""
set_key(key = key)
google_keys()

test<-address%>%head(5)


for (i in test$full_name){

df <- google_geocode(address =i,
                     key = key,
                     simplify = TRUE)
i
geocode_coordinates(df)
}

ggmap::register_google(key = "AIzaSyBUvQmqxo0FlDZPZv7YFgiSE9KVMbotMBo")

GeoCoded <- purrr::map_df(.x = address$full_name, .f = ggmap::geocode)

GeoCoded%>%write_csv("GeoCoded.csv")

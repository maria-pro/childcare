#API key: AIzaSyBUvQmqxo0FlDZPZv7YFgiSE9KVMbotMBo


#options(timeout = 1000)
#remotes::install_github("wfmackey/absmapsdata")

theme_set(theme_minimal())
options("scipen"=100, "digits"=2)

library(tidyverse)
library(readxl)
library(janitor)

library(sf)
library(absmapsdata)


#import 
data_location<-"/Users/e5028514/Desktop/childcare/data/SA2_ECEC_Linked_Dataset.xlsx"
tab_names<-excel_sheets(path = data_location)

#sarah's data
#need to install cellranger package to use large cell block
#install.packages("cellranger")

linked_data<-read_excel(data_location, sheet=1, range="A5:AM2478")

data_var<-read_excel(data_location, sheet=1, range="A4:AM4", col_names=FALSE)                        

linked_data<-linked_data%>%clean_names()

linked_data<-linked_data%>%
  mutate(
    sa2_code=as.character(sa2_code)
  )

#fee's data
fee_data<-read_excel("/Users/e5028514/Desktop/childcare/data/CBDC_fees.xlsx", sheet="CBDC Fees", range="A3:J336", na=c("", "."))

fee_data<-fee_data%>%clean_names()

fee_data<-fee_data%>%
  mutate(
    sa3_code=as.character(sa3_code),
    
  )%>%
  select(-starts_with("sa4"), 
         -sa3_name,
         -state)

#----------

#map
map_sa2 <- sa22021

map_sa3 <- sa32021

map_sa4 <- sa42021

#mapping
#map <-
#  map_sa3 %>%
#  filter(gcc_name_2021 == "Greater Melbourne") %>%   # let's just look Melbourne
#  ggplot() +
#  geom_sf(aes(geometry = geometry)) 


linked_data<-left_join(linked_data, map_sa2, by=c("sa2_code"="sa2_code_2021"))


st_geometry(linked_data)<-linked_data$geometry

#st_geometry(linked_data)

#st_write(linked_data, "test.geojson")


#linked_data%>%
#  filter(state_name_2021=="Victoria")%>%
#  ggplot(aes(geometry=geometry))+
#  geom_sf()
    
#full_data<- left_join(linked_data, fee_data, by=c("sa3_code_2021"= "sa3_code"))

#merge from sf

full_data<- merge(linked_data, fee_data, by.x="sa3_code_2021", by.y= "sa3_code")

#saving full data with fees to geojson - use this for map
full_data%>%st_write("data/full_data.geojson")

#use csv for wrangling 
#full_data_flat<-full_data%>%select(-geometry)

full_data_flat<-st_drop_geometry(full_data)
full_data_flat%>%write_csv("data/full_data_flat.csv")

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
  
  


%>%write_csv("data/full_data.csv")

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

key <- "AIzaSyBUvQmqxo0FlDZPZv7YFgiSE9KVMbotMBo"
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

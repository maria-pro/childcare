#options(timeout = 1000)
#remotes::install_github("wfmackey/absmapsdata")



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
fee_data<-read_excel("/Users/e5028514/Desktop/childcare/data/CBDC_fees.xlsx", sheet="CBDC Fees", range="A3:J336")

fee_data<-fee_data%>%clean_names()

fee_data<-fee_data%>%
  mutate(
    sa3_code=as.character(sa3_code)
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

#linked_data%>%
#  filter(state_name_2021=="Victoria")%>%
#  ggplot(aes(geometry=geometry))+
#  geom_sf()
    
full_data<- left_join(linked_data, fee_data, by=c("sa3_code_2021"= "sa3_code"))

full_data%>%write_csv("data/full_data.csv")

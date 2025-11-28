#Breanna Blackwood
#Created November 13, 2025
#Purpose: Read in and clean .dta files for analysis.

#Install haven package
#install.packages("haven")
library(haven)

#Read in the census 2010 file. 
#You can download the data from here: 

data_2010 <- read_dta("C:/Users/brebl/Downloads/nanda_grocery_Tract10_1990-2021_01P.dta")
head(data_2010)

#Because data from the food insecurity data was taken from years 2010-2019,
#I will filter out the data that includes supermarkets from these years.

tracts10data_2010_19 <- subset(data_2010, year %in% c(2010:2019))
head(tracts10data_2010_19)
tracts10data_2010_19$tract_fips10 <- as.numeric(tracts10data_2010_19$tract_fips10)
colnames(tracts10data_2010_19)[1] <- 'GEOID'

#This new datasets comprises all the census tracts in the country. 
#Since we only want census tracts in the three counties we are looking at
#(Miami-Dade, Broward, Palm Beach)

#I am using the code to get the census tracts from FoodInsecurity.Rmd, 
#but because these census tracts are from 2010 I'm going to use the year 2010 instead.
#Not sure how different the census tracts are from 2010 to 2019 for this area, 
#but I know the census tracts can change year to year so it may be a tad different. 
library(tidycensus)
library(tigris)
library(leaflet)
library(dplyr)


#Download SF census data
year = 2010
census_api_key("9562a8eb6a87b46dade54077ac971d32a9cf8131", install = TRUE, overwrite = TRUE)

options(tigris_use_cache = TRUE)
options(tigris_use_v7 = FALSE)

# MEDIAN HOME VALUE with Geometry
soflgeom <- get_acs(geography = "tract", year=year,
                   state = "FL", 
                   county = c(099, 086, 011),
                   variables = "B25077_001E",#honestly I don't care about the home value, I just want the geometry so ignore this.
                   geometry = TRUE)%>%
  mutate(GEOID=as.numeric(GEOID))
#This will give us the tracts for the counties we are focusing on. 

# Join Spatial with DF
spatial_census10<-geo_join(spatial_data=soflgeom , data_frame=tracts10data_2010_19, 
                   by_sp='GEOID', by_df='GEOID')
pal <- colorNumeric(
  palette = "viridis",
  domain = spatial_census10$count_totalfoodstores,
  na.color = "lightgrey"
)

#Leaflet plot
leaflet(spatial_census10) |>
  addTiles() |>
  addPolygons(fillColor = ~pal(count_totalfoodstores),
              fillOpacity = 0.7,
              color = "white",
              weight = 0.3)   |> addLegend(pal = pal,
               values = ~count_totalfoodstores, 
               title = "Legend",
               opacity = 1)

#Now the same for the 2020 data. 
#Read in the census 2010 file. 
#You can download the data from here: 

data_2020 <- read_dta("C:/Users/brebl/Downloads/nanda_grocery_Tract20_1990-2021_01P.dta")
head(data_2020)

#Because data from the food insecurity data was taken from years 2010-2019,
#I will filter out the data that includes supermarkets from these years.

tracts20data_2010_19 <- subset(data_2020, year %in% c(2010:2019))
head(tracts20data_2010_19)
tracts20data_2010_19$tract_fips20 <- as.numeric(tracts20data_2010_19$tract_fips20)
colnames(tracts20data_2010_19)[1] <- 'GEOID'

# Join Spatial with DF
spatial_census20<-geo_join(spatial_data=soflgeom , data_frame=tracts20data_2010_19, 
                           by_sp='GEOID', by_df='GEOID')


#Read in the SNAP food stores. 11/28/2025
#Since the data uses 2010 census, i think I will stick with that one, but maybe in the future we can look at the 2020 too? 

#This data goes by county. 
#downloaded from the link: 
snap_retailer <- read.csv("C:/Users/brebl/Downloads/snap_retailer_location_data.csv")

#Now that it is downloaded, 

###### MAPPING RATIO OF FOOD STORES



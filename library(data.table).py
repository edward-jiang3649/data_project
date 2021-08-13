

# Which city has the most traffic? Which city has the least?
# What month is the most busiest in the year?
# Which airport route has the busiest one?

library(data.table)
library(tidyverse)
library(stringr)
library(plotly)
library(readr)
library(xml2)

# ----------------------------------------------------------------------------------------

airport < - read_csv('../input/au-dom-traffic/audomcitypairs-20180406.csv')
airport$City1 < - airport$City1 % > % str_to_lower()
airport$City1 < - airport$City1 % > % str_to_title()
airport$City2 < - airport$City2 % > % str_to_lower()
airport$City2 < - airport$City2 % > % str_to_title()
airport < - airport % > % filter(Year < 2018)
airport < - airport % > % filter(Year >= 2000)


# ----------------------------------------------------------------------------------------

city < - fread("../input/world-cities-database/worldcitiespop.csv", data.table=FALSE)
##
Read 15.1 % of 3173958 rows
Read 24.6 % of 3173958 rows
Read 34.0 % of 3173958 rows
Read 37.2 % of 3173958 rows
Read 47.3 % of 3173958 rows
Read 61.4 % of 3173958 rows
Read 79.4 % of 3173958 rows
Read 88.2 % of 3173958 rows
Read 3173958 rows and 7 (of 7) columns from 0.153 GB file in 00: 00: 13
city.australia < - city % > % filter(Country == "au")
city.australia < - city.australia % > % select(-Country, -Population, -Region, -City)
names(city.australia)[1] < - "City"


# 5 Data Component

airport % > % str()
# Classes 'tbl_df', 'tbl' and 'data.frame':    13169 obs. of  12 variables:
# $ City1                : chr  "Albury" "Albury" "Albury" "Albury" ...
# $ City2                : chr  "Sydney" "Sydney" "Sydney" "Sydney" ...
# $ Month                : num  36526 36557 36586 36617 36647 ...
# $ Passenger_Trips      : num  8708 8785 10390 9693 9831 ...
# $ Aircraft_Trips       : num  401 398 423 394 418 403 458 589 566 580 ...
# $ Passenger_Load_Factor: num  62.5 63.6 70.4 70.7 67.3 67 63.9 59.5 64.2 53.1 ...
# $ Distance_GC_(km)     : num  452 452 452 452 452 452 452 452 452 452 ...
# $ RPKs                 : num  3936016 3970820 4696280 4381236 4443612 ...
# $ ASKs                 : num  6297264 6243024 6667904 6196016 6601912 ...
# $ Seats                : num  13932 13812 14752 13708 14606 ...
# $ Year                 : num  2000 2000 2000 2000 2000 2000 2000 2000 2000 2000 ...
# $ Month_num            : num  1 2 3 4 5 6 7 8 9 10 ...
# - attr(*, "spec")=
# .. cols(
# ..   City1 = col_character(),
# ..   City2 = col_character(),
# ..   Month = col_double(),
# ..   Passenger_Trips = col_double(),
# ..   Aircraft_Trips = col_double(),
# ..   Passenger_Load_Factor = col_double(),
# ..   `Distance_GC_(km)` = col_double(),
# ..   RPKs = col_double(),
# ..   ASKs = col_double(),
# ..   Seats = col_double(),
# ..   Year = col_double(),
# ..   Month_num = col_double()
# .. )

port.city < - c("Adelaide", "Albury", "Alice Springs", "Armidale", "Ayers Rock",
                "Ballina", "Brisbane", "Broome", "Bundaberg", "Burnie", "Cairns", "Canberra",
                "Coffs Harbour", "Darwin", "Devonport", "Dubbo", "Emerald", "Geraldton",
                "Gladstone", "Gold Coast", "Hamilton Island", "Hervey Bay", "Hobart",
                "Kalgoorlie", "Karratha", "Launceston", "Mackay", "Melbourne", "Mildura",
                "Moranbah", "Mount Isa", "Newcastle", "Newman", "Perth", "Port Hedland",
                "Port Lincoln", "Port Macquarie", "Proserpine", "Rockhampton", "Sunshine Coast",
                "Sydney", "Tamworth", "Townsville", "Wagga Wagga")

city.australia < - city.australia % > % filter(City % in % port.city)

airport < - merge(airport, city.australia, by.x="City1", by.y="City")
names(airport)[13] < - "City1.Latitude"
names(airport)[14] < - "City1.Longitude"

airport < - merge(airport, city.australia, by.x="City2", by.y="City")
names(airport)[15] < - "City2.Latitude"
names(airport)[16] < - "City2.Longitude"


# 7.1 Map Visualization of all routes
airport < - airport % > % mutate(id=rownames(airport))
airport.1 < - airport % >%
select(-contains("Latitude"), -contains("Longitude"))
airport.1 < - airport.1 % >%
gather('City1', 'City2', key="Airport.type", value="City")

airport.1$Airport.type < - airport.1$Airport.type % > % str_replace(pattern="City1", replacement="Departure")
airport.1$Airport.type < - airport.1$Airport.type % > % str_replace(pattern="City2", replacement="Arrive")
airport.1 < - merge(airport.1, city.australia, by.x="City", by.y="City")
world.map < - map_data("world")
au.map < - world.map % > % filter(region == "Australia")
au.map < - fortify(au.map)

ggplot() +
geom_map(data=au.map, map=au.map,
         aes(x=long, y=lat, group=group, map_id=region),
         fill="white", colour="black") +
ylim(-43, -10) +
xlim(110, 155) +
geom_point(data=airport.1, aes(x=Longitude, y=Latitude)) +
geom_line(data=airport.1, aes(x=Longitude, y=Latitude, group=id), colour="red", alpha=.1) +
labs(title="Australian Domestic Aircraft Routes")

# SICSS-Rutgers-2021 Final Project
# Group "The Social Butterflies"


## Packages needed
library(tidyverse)
library(igraph)
library(sf)

## Loading data and creating network
data <- read.table(file = 'Data/sci_country_country.tsv', sep = '\t', header = TRUE)
data <- data[!data$user_loc == data$fr_loc, ]
data <- na.omit(data)

summary(data$scaled_sci)
boxplot(data$scaled_sci)

small_sci <- data %>%
  filter(between(scaled_sci, quantile(scaled_sci, 0.3), quantile(scaled_sci, 0.9)))
summary(small_sci$scaled_sci)
boxplot(small_sci$scaled_sci)
small_sci <- rename(small_sci, "weight" = "scaled_sci")

sci_net <- graph_from_data_frame(d = small_sci, directed = T)
sci_net <- simplify(sci_net, remove.multiple = F, remove.loops = T) 
write.graph(sci_net, "Data/SCI Network_small.graphml", format = "graphml")

## Ploting only the US outbound flow

US_data  <- data[data$user_loc == "US", ]

countrycode <- read.csv("Data/countrycode.csv")
countrycode <- subset(countrycode, select = c("Alpha.2.code", "Country", "Latitude", "Longitude"))

US_data <- merge(US_data, countrycode, by.x = "fr_loc", by.y = "Alpha.2.code", all.x = T)
US_data <- na.omit(US_data)
US_data_sf <- US_data %>% st_as_sf(coords = c("Longitude", "Latitude"))


US_centroid <- read.table(file = 'Data/sci_country_country.tsv', sep = '\t', header = TRUE)
US_centroid <- na.omit(US_centroid)
US_centroid <- US_centroid[US_centroid$user_loc == US_centroid$fr_loc, ]
US_centroid <- US_centroid[US_centroid$user_loc == "US", ]
US_centroid <- merge(US_centroid, countrycode, by.x = "fr_loc", by.y = "Alpha.2.code", all.x = T)
US_centroid_sf <-US_centroid %>% st_as_sf(coords = c("Longitude", "Latitude"))

US_data_sf$US_centroid <- US_centroid_sf$geometry
summary(US_data) 

library(mapdeck)
key = "pk.eyJ1Ijoicnlud2FuZyIsImEiOiJja2tjNGo0cWwwNXhjMndwaWJmczd2eXV1In0.ebr5Xvzo1bxRZqXAXXAQ5Q"

str(US_data_sf)
US_data_sf%>% 
  mapdeck(token = key, style = mapdeck_style("dark"), pitch = 45) %>% 
  add_arc(
    origin = "US_centroid",
    destination = "geometry",
    stroke_width = "scaled_width",
    auto_highlight = TRUE,
    highlight_colour = "#8c43facc",
    tooltip = "tooltip"
  )


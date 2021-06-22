# SICSS-Rutgers-2021 Final Project
# Group 5

## Packages needed
library(tidyverse)
library(igraph)

## Loading data and creating network
data <- read.table(file = 'sci_country_country.tsv', sep = '\t', header = TRUE)

sci_net <- graph_from_data_frame(d = data, directed = T)
write.graph(sci_net, "Data/SCI Network.graphml", format = "graphml")




# SICSS-Rutgers-2021 Final Project
# Group: The Social Butterflies

# Packages needed
library(tidyverse)
library(igraph)
library(ggraph)
library(graphlayouts)
library(sf)
library(mapdeck)
library(countrycode)
library(WDI)
library(jsonlite)
library(stringr)
library(RColorBrewer)
library(sna)
library(intergraph)


# Loading data
data <- read.table(file = 'Data/sci_country_country.tsv', sep = '\t', header = TRUE)
data <- data[!data$user_loc == data$fr_loc, ]
data <- na.omit(data)

summary(data$scaled_sci)
boxplot(data$scaled_sci)

# Plotting only the US outbound flow
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
US_data_sf$scaled_sci_log <- US_data_sf$scaled_sci
US_data_sf$scaled_width <- US_data_sf$scaled_sci_log*0.0001


key = "YOUR MAPBOX API TOKEN!" ### You can access the API token via mapbox.com 

US_out_flow <- US_data_sf%>% 
  mapdeck(token = key, style = mapdeck_style("light"), pitch = 45) %>% 
  add_arc(
    origin = "US_centroid",
    destination = "geometry",
    palette = "viridis",
    stroke_width = "scaled_width",
    auto_highlight = TRUE,
    highlight_colour = "#8c43facc",
    tooltip = "tooltip"
  )

htmlwidgets::saveWidget(US_out_flow, file="Image/US_out_flow.html")
browseURL("Image/US_out_flow.html")


# Ploting only the US inbound flow
US_data <- data[data$fr_loc == "US", ]

countrycode <- read.csv("Data/countrycode.csv")
countrycode <- subset(countrycode, select = c("Alpha.2.code", "Country", "Latitude", "Longitude"))

US_data <- merge(US_data, countrycode, by.x = "user_loc", by.y = "Alpha.2.code", all.x = T)
US_data <- na.omit(US_data)
US_data_sf <- US_data %>% st_as_sf(coords = c("Longitude", "Latitude"))

US_centroid <- read.table(file = 'Data/sci_country_country.tsv', sep = '\t', header = TRUE)
US_centroid <- na.omit(US_centroid)
US_centroid <- US_centroid[US_centroid$user_loc == US_centroid$fr_loc, ]
US_centroid <- US_centroid[US_centroid$user_loc == "US", ]
US_centroid <- merge(US_centroid, countrycode, by.x = "fr_loc", by.y = "Alpha.2.code", all.x = T)
US_centroid_sf <-US_centroid %>% st_as_sf(coords = c("Longitude", "Latitude"))

US_data_sf$US_centroid <- US_centroid_sf$geometry
US_data_sf$scaled_sci_log <- US_data_sf$scaled_sci
US_data_sf$scaled_width <- US_data_sf$scaled_sci_log*0.0001


key = "YOUR MAPBOX API TOKEN!" ### You can access the API token via mapbox.com 

US_in_flow <- US_data_sf%>% 
  mapdeck(token = key, style = mapdeck_style("light"), pitch = 45) %>% 
  add_arc(
    origin = "geometry",
    destination = "US_centroid",
    palette = "spectral",
    stroke_width = "scaled_width",
    auto_highlight = TRUE,
    highlight_colour = "#FF0000D1",
    tooltip = "tooltip"
  )

htmlwidgets::saveWidget(US_in_flow, file="Image/US_in_flow.html")
browseURL("Image/US_in_flow.html")


# Merging the data
## loading in data
hdi <- read.csv("Data/HDI.csv")
polity <- read.csv("Data/p5v2018.csv")
nrow(hdi)
nrow(polity)

## prepping HDI
### selecting the cols for country, 2018, and 2019 (most recent year)
### renaming for easier reference
hdi <- hdi %>%
  select(Country, X2018, X2019) %>%
  rename(hdi_2018 = X2018, hdi_2019 = X2019)

### rows 190 down are regional summaries, not needed
hdi <- hdi[1:189, ]

## prepping polity
### filtering to the most recent year available (2018) and selecting needed columns
polity <- polity %>%
  filter(year==2018) %>%
  mutate(polity_score = coalesce(polity2, polity)) %>%
  select(scode, country, polity_score)

### adding the iso2 country code to each dataset
hdi$iso2 <- countrycode(hdi$Country, origin = "country.name", destination = "iso2c")
polity$iso2 <- countrycode(polity$country, origin = "country.name", destination = "iso2c")
### need to manually add temporary code for Kosovo
polity$iso2 <- with(polity, ifelse(is.na(iso2), "XK", iso2))

### joining the two:
df <- polity %>%
  full_join(hdi, by="iso2") %>%
  mutate(country = coalesce(country, Country)) %>%
  select(country, iso2, polity_score, hdi_2018, hdi_2019)

df$hdi_2019 <- ifelse(is.na(df$hdi_2019), df$hdi_2018, df$hdi_2019)
df$hdi_2018 <- NULL

### adding region
df$region <- countrycode(df$iso2, origin = "iso2c", destination = "region")

### get GDP per capita
gdp = WDI(indicator='NY.GDP.PCAP.CD',start=2019, end=2019)
gdp$year <- NULL
gdp$country <- NULL
gdp <- rename(gdp, "GDP" = "NY.GDP.PCAP.CD")

df <- merge(df, gdp, by.x = "iso2", by.y = "iso2c", all.x = T)

### getting GINI index (World Bank estimate)
gini=WDI(indicator='SI.POV.GINI',start=2014, end=2019) 
gini_wide <- gini %>%
  spread(year, SI.POV.GINI)
gini_final <- subset(gini_wide, select = c("iso2c", "country", "2019"))
gini_final <- rename(gini_final, "GINI" = "2019")
gini_final$GINI <-ifelse(is.na(gini_final$GINI), gini_wide$'2018', gini_final$GINI)
gini_final$GINI <-ifelse(is.na(gini_final$GINI), gini_wide$'2017', gini_final$GINI)
gini_final$GINI <-ifelse(is.na(gini_final$GINI), gini_wide$'2016', gini_final$GINI)
gini_final$GINI <-ifelse(is.na(gini_final$GINI), gini_wide$'2015', gini_final$GINI)
gini_final$GINI <-ifelse(is.na(gini_final$GINI), gini_wide$'2014', gini_final$GINI)
gini_final$country <- NULL

df <- merge(df, gini_final, by.x = "iso2", by.y = "iso2c", all.x = T)


### getting Individuals using the Internet (% of population)"   
internet = WDI(indicator='IT.NET.USER.ZS',start=2014, end=2019)
internet_wide <- internet %>%
  spread(year, IT.NET.USER.ZS)
internet_final <- subset(internet_wide, select = c("iso2c", "country", "2019"))
internet_final <- rename(internet_final, "Internet" = "2019")
internet_final$internet <-ifelse(is.na(internet_final$Internet), internet_wide$'2018', internet_final$Internet)
internet_final$internet <-ifelse(is.na(internet_final$Internet), internet_wide$'2017', internet_final$Internet)
internet_final$internet <-ifelse(is.na(internet_final$Internet), internet_wide$'2016', internet_final$Internet)
internet_final$internet <-ifelse(is.na(internet_final$Internet), internet_wide$'2015', internet_final$Internet)
internet_final$internet <-ifelse(is.na(internet_final$Internet), internet_wide$'2014', internet_final$Internet)

df <- merge(df, internet_final, by.x = "iso2", by.y = "iso2c", all.x = T)

### getting population census
pop = WDI(indicator = "SP.POP.TOTL", start = 2014, end = 2019)
pop_wide <- pop %>%
  spread(year, SP.POP.TOTL)
pop_final <- subset(pop_wide, select = c("iso2c", "country", "2019"))
pop_final <- rename(pop_final, "population" = "2019")
pop_final$population <-ifelse(is.na(pop_final$population), pop_wide$'2018', pop_final$population)
pop_final$population <-ifelse(is.na(pop_final$population), pop_wide$'2017', pop_final$population)
pop_final$population <-ifelse(is.na(pop_final$population), pop_wide$'2016', pop_final$population)
pop_final$population <-ifelse(is.na(pop_final$population), pop_wide$'2015', pop_final$population)
pop_final$population <-ifelse(is.na(pop_final$population), pop_wide$'2014', pop_final$population)
pop_final$country <- NULL
pop_final$year <- NULL

df <- merge(df, pop_final, by.x = "iso2", by.y = "iso2c", all.x = T)


### getting Political Stability and Absence of Violence/Terrorism: Estimate"   
polistable = WDI(indicator='PV.EST',start=2014, end=2019)
polistable_wide <- polistable %>%
  spread(year, PV.EST)
polistable_final <- subset(polistable_wide, select = c("iso2c", "country", "2019"))
polistable_final <- rename(polistable_final, "polistable" = "2019")
polistable_final$polistable<-ifelse(is.na(polistable_final$polistable), polistable_wide$'2018', polistable_final$polistable)
polistable_final$polistable<-ifelse(is.na(polistable_final$polistable), polistable_wide$'2017', polistable_final$polistable)
polistable_final$polistable<-ifelse(is.na(polistable_final$polistable), polistable_wide$'2016', polistable_final$polistable)
polistable_final$polistable<-ifelse(is.na(polistable_final$polistable), polistable_wide$'2015', polistable_final$polistable)
polistable_final$polistable<-ifelse(is.na(polistable_final$polistable), polistable_wide$'2014', polistable_final$polistable)
polistable_final$country <- NULL
polistable_final$year <- NULL

df <- merge(df, polistable_final, by.x = "iso2", by.y = "iso2c", all.x = T)

nodelist<-merge(nodelist, polistable_final, by.x = "ID", by.y = "iso2c", all.x = T)

### getting outbound mobility ratio, Number of students from a given country studying abroad as a percentage of the total tertiary enrolment in that country. 
outboundmob= WDI(indicator='UIS.OMR.56',start=2015, end=2020)
outboundmob_wide <- outboundmob %>%
  spread(year, UIS.OMR.56)
outbound_final <- subset(outboundmob_wide, select = c("iso2c", "country", "2020"))
outbound_final <- rename(outbound_final, "Mobility" = "2020")
outbound_final$Mobility <-ifelse(is.na(outbound_final$Mobility), outboundmob_wide$'2019', outbound_final$Mobility)
outbound_final$Mobility <-ifelse(is.na(outbound_final$Mobility), outboundmob_wide$'2018', outbound_final$Mobility)
outbound_final$Mobility <-ifelse(is.na(outbound_final$Mobility), outboundmob_wide$'2017', outbound_final$Mobility)
outbound_final$Mobility <-ifelse(is.na(outbound_final$Mobility), outboundmob_wide$'2016', outbound_final$Mobility)
outbound_final$Mobility <-ifelse(is.na(outbound_final$Mobility), outboundmob_wide$'2015', outbound_final$Mobility)
outbound_final$country <- NULL

df <- merge(df, outbound_final, by.x = "iso2", by.y = "iso2c", all.x = T)

### getting press freedom index
fsi <- fromJSON("https://gist.githubusercontent.com/planemad/76c9a87a4e2c785fb60081841c8c5d32/raw/d9e9d20c53f2dec82249b1f728c817ea39124a7d/world-press-freedom-index-2020.json",flatten=TRUE)
colnames(fsi)

pressfree <-fsi[c('ISO2','EN_country','Score 2020')]
pressfree$`Score 2020`<-gsub(",", ".", pressfree$`Score 2020`)
pressfree$`Score 2020`<-as.numeric(pressfree$`Score 2020`)
pressfree <- rename(pressfree, "press" = "Score 2020", "iso2" = "ISO2")
pressfree$EN_country<-NULL

df <- merge(df, pressfree, by = "iso2", all.x = T)


# Constructing nodelist and edgelist
## Node attributes
nodelist <- distinct(data, user_loc, .keep_all = T)
nodelist$fr_loc <- NULL
nodelist$scaled_sci <- NULL
nodelist <- rename(nodelist, "ID" = "user_loc")
nodelist <- merge(nodelist, df, by.x = "ID", by.y = "iso2", all.x = T)
nodelist$internet <- nodelist$internet/100

outdegree <- edgelist %>%
  group_by(user_loc) %>%
  summarize(sci = mean(scaled_sci))

nodelist <- merge(nodelist, outdegree, by.x = "ID", by.y = "user_loc", all.x = T)

write.csv(nodelist, "Data/Node attribute_full.csv", row.names = F)

nodelist_short <- nodelist[!is.na(nodelist$region),]
str(nodelist)
write.csv(nodelist_short, "Data/Node attribute_short.csv", row.names = F)


## Edgelist
countrylist <- nodelist$ID
edgelist <- data
write.csv(edgelist, "Data/Edgelist_full.csv", row.names = F)

edgelist_short <- edgelist[edgelist$user_loc %in% countrylist, ]
edgelist_short <- edgelist_short[edgelist_short$fr_loc %in% countrylist, ]
edgelist_short <- rename(edgelist_short, "weight" = "scaled_sci")

large_edgelist <- edgelist_short %>%
  filter(between(weight, quantile(weight, 0.3), quantile(weight, 0.9)))

write.csv(smaller_edgelist, "Data/Edgelist_short.csv", row.names = F)



## Network descriptions
large_sci_net <- graph_from_data_frame(d = large_edgelist, directed = T)
large_sci_net <- simplify(large_sci_net, remove.multiple = F, remove.loops = T) 
large_sci_net

clp <- cluster_label_prop(as.undirected(large_sci_net), weights=E(large_sci_net)$score)
cfg <- cluster_fast_greedy(as.undirected(large_sci_net))
cle <- cluster_leading_eigen(as.undirected(large_sci_net))
ci <- cluster_infomap(as.undirected(large_sci_net))
cwt <- cluster_walktrap(as.undirected(large_sci_net))

### Comparing the results of community detection is by modularity scores:
mod <- sapply(list(clp=clp, cfg=cfg, cle=cle, ci=ci, cwt=cwt), modularity)
sort(round(mod, 2))

### Assigning membership
V(large_sci_net)$community <- cfg$membership
community <- V(large_sci_net)$community

community_df <- as.data.frame(cbind(countrylist, community))
region_df <- subset(nodelist, select = c("ID", "region", "country"))
community_df <- merge(community_df, region_df, by.x = "countrylist", by.y = "ID")
community_df <- na.omit(community_df)
sci_df <- subset(nodelist, select = c("ID", "sci"))
community_df <- merge(community_df, sci_df, by.x = "countrylist", by.y = "ID", all.x = T)

write.csv(community_df, "Data/Community.csv", row.names = F)

### Visualizing
community_df %>%
  ggplot(aes(x=community, fill=region)) + 
  geom_bar() +
  scale_fill_brewer(palette = "Pastel1") +
  theme_bw() +
  labs(x = "Community", y = "Count", fill = "Region") +
  theme(text=element_text(family="Arial", face="bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))
ggsave("Image/Community", device = "png", dpi = 600)

sic_rank <- community_df %>%
  arrange(desc(sci)) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(country, sci), y = sci, fill = region)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Countries", y = "SCI", fill = "Regions") +
  theme_bw() +
  theme(text=element_text(family="Arial", face="bold"),
       axis.text = element_text(size = 12),
       axis.title = element_text(size = 15),
       legend.text = element_text(size = 12),
       legend.position = "bottom") +
  coord_flip()

ggsave("Image/SCI global rank top 20", device = "png", dpi = 600, width = 10)

global_rank <- community_df %>%
  ggplot(aes(x = reorder(country, sci), y = sci, fill = region)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Countries", y = "SCI", fill = "Regions") +
  theme_bw() +
  theme(text=element_text(family="Arial", face="bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.position = c(0.25, 0.7),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = "black"))

ggsave("Image/Gloal rank", device = "png", dpi = 600)


community_df_sub <- community_df%>%
  group_by(community) %>%
  slice_max(order_by = sci, n =20)

sic_rank_region <- community_df_sub %>%
  ggplot(aes(y = reorder(country,sci), x = sci, fill = region)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "SCI", y = "Countries", fill = "Regions") +
  theme_bw() +
  theme(text=element_text(family="Arial", face="bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15), 
        legend.text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~community, scales = "free_y",
             nrow = 2)

ggsave("Image/SCI global rank by communities", device = "png", dpi = 400, width = 15, height = 10)


## Linear network autocorrelation model

## Constructing network without NAs
countrylist_na <- nodelist_na$ID
edgelist_na <- edgelist[edgelist$user_loc %in% countrylist_na, ]
edgelist_na <- edgelist_na [edgelist_na $fr_loc %in% countrylist_na, ]
edgelist_na <- rename(edgelist_na, "source" = "user_loc", "target" = "fr_loc", "weight" = "scaled_sci")
nodelist_na <- na.omit(nodelist)
write.csv(nodelist_na, "Data/Node attribute_nona.csv", row.names = F)
smaller_sci_net <- graph_from_data_frame(d = edgelist_na, vertices = nodelist_na, directed = T)
smaller_sci_net <- simplify(smaller_sci_net, remove.multiple = F, remove.loops = T) 

sci_network <- asNetwork(smaller_sci_net)

sci <- log(sci_network %v% "sci")
GDP <- sci_network %v% "GDP"
GINI <- sci_network %v% "GINI"
hdi <- sci_network %v% "hdi_2019"
hdi <- as.numeric(hdi)
internet <- sci_network %v% "internet"
mobility <- sci_network %v% "Mobility"
ps <- sci_network %v% "polistable"
polity <- sci_network %v% "polity_score"
pop_log <- log(sci_network %v% "population")
region <- sci_network %v% "region"
press <- sci_network %v% "press"


lnam.model <- lnam(y = sci, x = cbind(hdi, GINI, internet, mobility, ps, polity, press), W1=sci_network)
summary(lnam.model)

lm.model <- lm(sci~hdi+GINI+internet+mobility+ps+polity+press, data=nodelist_na)
summary(lm.model)
AIC(lm.model)


### Visualizing
result <- read.csv("Data/Result.csv")
result$Variables <- factor(result$Variables, levels = c("Rho1.1", "Press control", "Polity score", "Political stability", "Outbound mobility", "Internet", "GINI", "HDI"))
result$P.value <- factor(ifelse(result$P.value <= 0.05, "Significant", "Not significant"))

result_plot <- result%>%
  ggplot(aes(x = Variables, y = Estimate, color = P.value)) +
  geom_line() +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin=Estimate-SE, ymax=Estimate+SE), width=.2,
                position=position_dodge(0.05)) +
  labs(color = "Significant level") +
  theme_bw() +
  theme(axis.title = element_text(size = 13),
        axis.text.x = element_text(size =12),
        axis.text.y = element_text(size =12),
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "transparent", color = NA),
        rect = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(colour = "transparent", fill = "white")) +
  labs(shape = "Significant level") +
  coord_flip()
result_plot
ggsave("Image/LNAM result", device = "png", dpi = 600, bg = "transparent")

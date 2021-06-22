library(tidyverse)
library(countrycode)
# setting working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# loading in data
hdi <- read.csv("HDI.csv")
polity <- read.csv("p5v2018.csv")
nrow(hdi)
nrow(polity)

# prepping hdi
# selecting the cols for country, 2018, and 2019 (most recent year)
# renaming for easier reference
hdi <- hdi %>%
  select(Country, X2018, X2019) %>%
  rename(hdi_2018 = X2018, hdi_2019 = X2019)

# rows 190 down are regional summaries, not needed
hdi <- hdi[1:189, ]

# prepping polity
# filtering to the most recent year available (2018) and selecting needed columns
polity <- polity %>%
  filter(year==2018) %>%
  mutate(polity_score = coalesce(polity2, polity)) %>%
  select(scode, country, polity_score)

# adding the iso2 country code to each dataset
hdi$iso2 <- countrycode(hdi$Country, origin = "country.name", destination = "iso2c")
polity$iso2 <- countrycode(polity$country, origin = "country.name", destination = "iso2c")
# need to manually add temporary code for Kosovo
polity$iso2 <- with(polity, ifelse(is.na(iso2), "XK", iso2))

# joining the two:
df <- polity %>%
  full_join(hdi, by="iso2") %>%
  mutate(country = coalesce(country, Country)) %>%
  select(country, iso2, polity_score, hdi_2018, hdi_2019)

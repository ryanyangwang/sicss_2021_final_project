###test###

df<-read.table("https://s3.us-east-1.amazonaws.com/hdx-production-filestore/resources/7c8f6c93-6272-4d39-9e5d-99cdc0053dfc/2020-12-16_country_country.tsv", head=T)




library("igraph") 
library("statnet") 
library("tidyverse")
library("WDI")

links<-df

#get GDP per capita
gdp = WDI(indicator='NY.GDP.PCAP.CD',start=2019, end=2019)

#get internet per thousand
internet = WDI(indicator='IT.NET.USER.P3',start=2019, end=2019)




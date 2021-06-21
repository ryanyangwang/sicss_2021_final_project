###test###

df<-read.table("https://s3.us-east-1.amazonaws.com/hdx-production-filestore/resources/7c8f6c93-6272-4d39-9e5d-99cdc0053dfc/2020-12-16_country_country.tsv?AWSAccessKeyId=AKIAXYC32WNARK756OUG&Expires=1624310148&Signature=CPDU7rBkHafhJE3aGwigId%2FmC4g%3D", head=T)




library("igraph") 
library("statnet") 
library("tidyverse")
library("WDI")

links<-df

#get GDP 
gdp = WDI(indicator='NY.GDP.PCAP.CD',start=2019, end=2019)
head(gdp)

###nodes and edgelist seem inconsistent 
#not run nodes<-subset(gdp, iso2c %in% links$user_loc)



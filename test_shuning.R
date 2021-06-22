###        The file contains codes getting country-level features            ###

#The dataframe includes iso2c ID for countries, which could be linked to FB SCI#
#There are a lot of missing data for GINI, HDI, outbound mobility ratio so that more years should be included.#


library("WDI")

#get GDP per capita
gdp = WDI(indicator='NY.GDP.PCAP.CD',start=2019, end=2019)

#get gini  "GINI index (World Bank estimate)"
gini=WDI(indicator='SI.POV.GINI',start=2017, end=2019) 

#get "Human Development Index " 2019, 2020 not available 
HDI = WDI(indicator='IDX.HDI.REV',start=2018, end=2018) 

#get "Individuals using the Internet (% of population)"   
internet = WDI(indicator='IT.NET.USER.ZS',start=2019, end=2019)

#get Political Stability and Absence of Violence/Terrorism: Estimate"   
polistable= WDI(indicator='PV.EST',start=2019, end=2019) 

#get outbound mobility ratio, Number of students from a given country studying abroad as a percentage of the total tertiary enrolment in that country. 
outboundmob= WDI(indicator='UIS.OMR.56',start=2017, end=2020) 

library("jsonlite")
fsi <- fromJSON("https://gist.githubusercontent.com/planemad/76c9a87a4e2c785fb60081841c8c5d32/raw/d9e9d20c53f2dec82249b1f728c817ea39124a7d/world-press-freedom-index-2020.json",flatten=TRUE)
colnames(fsi)

#get freedom express 
pressfree <-fsi[c('ISO2','EN_country','Score 2020')]
pressfree$`Score 2020`<-gsub(",", ".", pressfree$`Score 2020`)
pressfree$`Score 2020`<-as.numeric(pressfree$`Score 2020`)
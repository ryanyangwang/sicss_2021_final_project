###The file contains codes getting country-level features###

library("WDI")

#get GDP per capita
gdp = WDI(indicator='NY.GDP.PCAP.CD',start=2019, end=2019)

#get gini  "GINI index (World Bank estimate)"     
gini=WDI(indicator='SI.POV.GINI',start=2019, end=2019) 

#get "Human Development Index " 2019, 2020 not available 
HDI = WDI(indicator='IDX.HDI.REV',start=2018, end=2018) 

#get "Individuals using the Internet (% of population)"   
internet = WDI(indicator='IT.NET.USER.ZS',start=2019, end=2019)

#get Political Stability and Absence of Violence/Terrorism: Estimate"   
polistable= WDI(indicator='PV.EST',start=2019, end=2019) 

#get outbound mobility ratio, Number of students from a given country studying abroad as a percentage of the total tertiary enrolment in that country. 
outboundmob= WDI(indicator='UIS.OMR.56',start=2019, end=2019) 
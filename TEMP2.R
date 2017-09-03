library(devtools)
install_github('ramnathv/rCharts@dev')
install_github('ramnathv/rMaps')

library(rMaps)
library(rCharts)

library(googlesheets)
library(dplyr)
library(reshape2)
library(countrycode)

#Grabbing the data from Gapminder
gs_key("1B89xX-0BdNR-qjM7_s5tZi2WCCmobejXd4RaMFKxAIw", 
       verbose=FALSE, lookup = FALSE)

life_exp <- gs_key("1B89xX-0BdNR-qjM7_s5tZi2WCCmobejXd4RaMFKxAIw",
                         lookup = FALSE,  verbose=FALSE) %>% 
  gs_read(ws = "Data", check.names=FALSE)

#Subsetting the data to start from 1850
life_exp <-life_exp[,c(1,87:244)]
colnames(life_exp)[1] <-"Country"

#Putting the data into a long format
life_exp_long <- melt(life_exp, id.var = "Country")

#Creating the map
options(rcharts.cdn = TRUE)
map <- Datamaps$new()

map$set(dom = 'chart_1',
  scope = 'world',fills = fills,
  data = dat2[[1]], legend = TRUE,
  labels = TRUE)

map


ichoropleth(value ~ Country, 
            data = na.omit(life_exp_long),
            scope = 'world',
            pal = 'PuRd',
            ncuts = 5,
            animate = "variable")

ichoropleth(Crime ~ Code,
            data = datm2[,1:3],
            pal = 'PuRd',
            ncuts = 5,
            animate = 'Year')

life_exp_long$Color[is.na(life_exp_long$value)] <- NA

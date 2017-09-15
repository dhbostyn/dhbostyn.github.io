library(devtools)
#install_github('ramnathv/rCharts@dev')
#install_github('ramnathv/rMaps')

library(rMaps)
library(rCharts)

library(googlesheets)
library(plyr)
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
life_exp <-life_exp[,c(1,87:252)]
life_exp <- na.omit(life_exp)
colnames(life_exp)[1] <-"Country"

#Putting the data into a long format
life_exp_long <- melt(life_exp, id.var = "Country")
life_exp_long$Code <- NA
life_exp_long$Code <- countrycode(life_exp_long$Country, 'country.name', 'iso3c')
life_exp_long$Code[life_exp_long$Country == "Central African Rep."] <-  "CAF"

#life_exp_long$Code[life_exp_long$Country == "Kosovo"] <- "UNK" 
#life_exp_long$Code[life_exp_long$Country == "Saint Martin"] <- "MAF" 


#Fill
life_exp_long$fillKey <- NA
life_exp_long$fillKey[life_exp_long$value<80] <- ">70"
life_exp_long$fillKey[life_exp_long$value<70] <- "60 to 70"
life_exp_long$fillKey[life_exp_long$value<60] <- "50 to 60"
life_exp_long$fillKey[life_exp_long$value<50] <- "40 to 50"
life_exp_long$fillKey[life_exp_long$value<40] <- "30 to 40"
life_exp_long$fillKey[life_exp_long$value<30] <- "20 to 30"
life_exp_long$fillKey[life_exp_long$value<20] <- "<30"

fills = setNames(
  c(RColorBrewer::brewer.pal(7, 'RdYlGn'), 'white'),
  c(c("<30","20 to 30","30 to 40","40 to 50","50 to 60","60 to 70",">70"), 'defaultFill')
)

#Turning the data into a list
dat <- dlply(life_exp_long, "variable", function(x){
  y = toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'Code')
  return(y)
})

#Creating First Choropleth
options(rcharts.cdn = TRUE)
map <- Datamaps$new()
map$set(
  dom = 'chart_1',
  scope = 'world',
  fills = fills,
  data = dat[[1]],
  legend = TRUE,
  labels = FALSE
)

#Animated chloropleth
map2 = map$copy()
map2$set(
  bodyattrs = "ng-app ng-controller='rChartsCtrl'"
)
map2$addAssets(
  jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
)

map2$setTemplate(chartDiv = "
  <div id = 'chart_1' class = 'rChart datamaps'>
  <input id='slider' type='range' min=1850 max=2015 ng-model='year' width=200>
  <span ng-bind='year'></span>
    
  <script>
    function rChartsCtrl($scope){
      $scope.year = '1850';
      $scope.$watch('year', function(newYear){
        mapchart_1.updateChoropleth(chartParams.newData[newYear]);
      })
    }
  </script>
  </div>   "
)
map2$set(newData = dat)

#Add the play button
map3 = map2$copy()
map3$setTemplate(chartDiv = "
                 <div class='container'>
                 <button ng-click='animateMap()'>Play</button>
                 <div id='chart_1' class='rChart datamaps'></div>  
                 </div>
                 <script>
                 function rChartsCtrl($scope, $timeout){
                 $scope.year = 1850;
                 $scope.animateMap = function(){
                 if ($scope.year > 2014){
                 return;
                 }
                 mapchart_1.updateChoropleth(chartParams.newData[$scope.year]);
                 $scope.year += 1
                 $timeout($scope.animateMap, 100)
                 }
                 }
                 </script>"
)
map3



d1 <- ichoropleth(value ~ Code, data = subset(life_exp_long, variable == "2000"), pal = "RdYlGn", ncuts= 8, map="world", labels=FALSE)
d1
#Creating a map for a single year
d1$save("test.html", cdn = TRUE)



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

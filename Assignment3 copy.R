#Reilly McBride

#You will visually compare the spatial distribution of the two variables you selected (i.e., maps).

#-use at least three types of classifications to visualize the spatial distribution 
#and compare the impressions you get from them both between the classifications 
#and between the variables

library(tidyverse)
library(sf)
library(tmap)
library(RColorBrewer)

nyced <- st_read("NYC_Tract_ACS2008_12.shp")
summary(nyced)

subnyced <- nyced %>% select(cartodb_id, popunemplo, popover18, onlylessth) %>% 
  rename(censustract = cartodb_id, totalunemployed = popunemplo, 
         totalover18 = popover18, totallessthanhs = onlylessth)
subnyced <- filter(subnyced, totalunemployed != 0) #if it's 0 for one cat it was 0 for all
subnyced <- subnyced %>% mutate(percentlesshs = (totallessthanhs/totalover18)*100,
                                percentunemployed = (totalunemployed/totalover18)*100)
averages <- summarize(subnyced, avghs = mean(percentlesshs))
#subnyced <- mutate(subnyced, doingworse = percentlesshs > averages$avghs) (old, just labeled as TRUE or FALSE)
subnyced <- mutate(subnyced, doingworse = if_else((percentlesshs > averages$avghs),"Less Educated","More Educated"))


graphuni <- function(variable, legendTitle, classif, graphTitle, mainTitle) {
  tm_shape(subnyced, unit="mi") +
    tm_fill(variable, palette = "Greens", title=legendTitle, style=classif) +
    tm_borders(lwd = 0.2) +
    tm_scale_bar(width=0.20) +
    tm_layout(title = graphTitle, title.size = 0.9, title.position = c("center","bottom"), main.title=mainTitle, main.title.position="center")
}

#equal number of observations per break 

graphuni("percentlesshs", "% Pop. < HS Education", "quantile", "Quantile Map", "Education Levels in NYC")
graphuni("percentunemployed", "Percent Unemployed", "quantile", "Quantile Map", "Unemployment Rates in NYC")

#grouped based on high similarity within groups, high dissimilarity between groups

graphuni("percentlesshs", "% Pop. < HS Education", "jenks", "Natural Breaks", "Education Levels in NYC")
graphuni("percentunemployed", "Percent Unemployed", "jenks", "Natural Breaks", "Unemployment Rates in NYC")

#equal breaks, different number of observations per break 

graphuni("percentlesshs", "% Pop. < HS Education", "equal", "Equal Breaks", "Education Levels in NYC")
graphuni("percentunemployed", "Percent Unemployed", "equal", "Equal Breaks", "Unemployment Rates in NYC")

#-compare the spatial distributions for one variable between the subgroups you created

subnyced$dw <- as.factor(subnyced$doingworse)

graphfacets <- function(classif, mainTitle) {
  tm_shape(subnyced, unit="mi") +
    tm_fill("percentunemployed", title = "Unemployment Rate", palette="Greens", style=classif) +
    tm_borders(lwd = 0.1) +
    tm_facets(by = "dw",free.coords = FALSE,drop.units=FALSE) +
    tm_layout(main.title = mainTitle)
}
  
graphfacets("quantile", "Unemployment in NYC by Education Level (Quantile)")
graphfacets("jenks", "Unemployment in NYC by Education Level (Natural Breaks)")
graphfacets("equal", "Unemployment in NYC by Education Level (Equal Breaks)")

#-Make sure you have legends (placed appropriately) and a title. 
#Optionally, you can spiff it up with a compass rose or distance bar 
#(check out the ggplot documentation).

#Write a short description and interpretation of the results. Include all maps and code.





library(shiny)
library(leaflet)
library(htmltools)
library(dplyr)

dc_schools<-read.csv("https://raw.githubusercontent.com/bettyak/Developing_Data_Products/main/DC_Schools.csv", header =TRUE)
dc_schools=rename(dc_schools, school=ï..school)
dc_schools[is.na(dc_schools)]=0
dc_schools$totalCases= rowSums(dc_schools[,c(5:22)])

# first cut the continuous variable into bins
# these bins are now factors
dc_schools$totalCasesLvl <- cut(dc_schools$totalCases, 
                                c(1,3,7,17), include.lowest = T,
                                labels = c('<3', '3-7', '7+'))

# then assign a palette to this using colorFactor
# in this case it goes from red for the smaller values to yellow and green
# standard stoplight for bad, good, and best
caseCol <- colorFactor(palette = 'Dark2', dc_schools$totalCasesLvl)


server<-function(input, output) {
    
    
    output$mymap <- renderLeaflet({ 
        
        my_map<-leaflet(data=dc_schools) %>% 
        addTiles() %>% 
        addCircleMarkers(~Long,~Lat, 
                         radius = ~sqrt(totalCases*30), 
                         label = ~htmlEscape(school), 
                         popup = ~htmlEscape(totalCases), 
                         color=~caseCol(totalCasesLvl))%>%
        addLegend('bottomright', pal = caseCol, values =dc_schools$totalCasesLvl,
                  title = 'Douglas County Schools Covid Cases',
                  opacity = 1)
        return(my_map)
})
}

shinyApp(ui = ui, server=server)

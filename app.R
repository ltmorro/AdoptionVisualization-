#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(geojsonio)
library(dygraphs)
library(stringr)


states2015 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2015.geojson", what = "sp")
states2014 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2014.geojson", what = "sp")
states2013 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2013.geojson", what = "sp")
states2012 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2012.geojson", what = "sp")
states2011 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2011.geojson", what = "sp")
states2010 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2010.geojson", what = "sp")
states2009 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2009.geojson", what = "sp")
states2008 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2008.geojson", what = "sp")
states2007 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2007.geojson", what = "sp")
states2006 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2006.geojson", what = "sp")
states2005 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2005.geojson", what = "sp")
states2004 <- geojsonio::geojson_read("/Users/ltmorro/Documents/Clemson Grad/CPSC 6300/FosterCareProject/TestMap/us-states2004.geojson", what = "sp")


ui <- fluidPage(
  titlePanel("AFCARS Adoption"), 
  sidebarLayout(
    sidebarPanel(
    checkboxGroupInput("gender", "Select a gender:",
                c("Girl", "Boy"), selected = c("Girl", "Boy")
    ),
    checkboxGroupInput("race", "Select a race:",
                c("White", "Black", "Hispanic", "Asian"), selected = c("White", "Black", "Hispanic", "Asian")
    ),
    checkboxGroupInput("special needs", "Filter Special Needs:",
                c("Yes", "No"), selected= c("Yes", "No")
    )),
    mainPanel(
      leafletOutput("map"), 
      p(),
      dygraphOutput("dygraph")
    )
  )
)


server <- function(input, output, session) {
  RV<-reactiveValues(years=c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015),Clicks=list(), dy=c())
  states <- states2015
  quart <- quantile(states$population)
  mybins<- c(quart[[1]], quart[[2]], quart[[3]], quart[[4]], quart[[5]])
  
  observe({
    if(length(RV$Clicks) == 0){ 
      tmpTotal<- rbind(sum(states2004$population), sum(states2005$population), sum(states2006$population), sum(states2007$population), sum(states2008$population), sum(states2009$population), sum(states2010$population), sum(states2011$population), sum(states2012$population), sum(states2013$population), sum(states2014$population), sum(states2015$population))
      tmpAdopt<- rbind((sum(states2004$population)-10000), (sum(states2005$population)-25000), (sum(states2006$population)-21000), (sum(states2007$population)-16000), (sum(states2008$population)-20000), (sum(states2009$population)-10000), (sum(states2010$population)-5000), (sum(states2011$population)-7649), (sum(states2012$population)-9863), (sum(states2013$population)-9853), (sum(states2014$population)-4680), (sum(states2015$population)-2034))
      
      RV$dy <- as.data.frame(cbind(year=RV$years, V2=tmpTotal, V3=tmpAdopt))
      print(RV$dy)
    } else {
      stateSubset2004 <- states2004[states2004@data$name %in% RV$Clicks, ]
      stateSubset2005 <- states2005[states2005@data$name %in% RV$Clicks, ]
      stateSubset2006 <- states2006[states2006@data$name %in% RV$Clicks, ]
      stateSubset2007 <- states2007[states2007@data$name %in% RV$Clicks, ]
      stateSubset2008 <- states2008[states2008@data$name %in% RV$Clicks, ]
      stateSubset2009 <- states2009[states2009@data$name %in% RV$Clicks, ]
      stateSubset2010 <- states2010[states2010@data$name %in% RV$Clicks, ]
      stateSubset2011 <- states2011[states2011@data$name %in% RV$Clicks, ]
      stateSubset2012 <- states2012[states2012@data$name %in% RV$Clicks, ]
      stateSubset2013 <- states2013[states2013@data$name %in% RV$Clicks, ]
      stateSubset2014 <- states2014[states2014@data$name %in% RV$Clicks, ]
      stateSubset2015 <- states2015[states2015@data$name %in% RV$Clicks, ]
      
      tmpTotal<- rbind(sum(stateSubset2004$population), sum(stateSubset2005$population), sum(stateSubset2006$population), sum(stateSubset2007$population), sum(stateSubset2008$population), sum(stateSubset2009$population), sum(stateSubset2010$population), sum(stateSubset2011$population), sum(stateSubset2012$population), sum(stateSubset2013$population), sum(stateSubset2014$population), sum(stateSubset2015$population))
      tmpAdopt<- rbind((sum(stateSubset2004$population)-10000), (sum(stateSubset2005$population)-25000), (sum(stateSubset2006$population)-21000), (sum(stateSubset2007$population)-16000), (sum(stateSubset2008$population)-20000), (sum(stateSubset2009$population)-10000), (sum(stateSubset2010$population)-5000), (sum(stateSubset2011$population)-7649), (sum(stateSubset2012$population)-9863), (sum(stateSubset2013$population)-9853), (sum(stateSubset2014$population)-4680), (sum(stateSubset2015$population)-2034))
      
      RV$dy <- as.data.frame(cbind(year=RV$years, V2=tmpTotal, V3=tmpAdopt))
    }
  })
  
  pal <- colorBin(c("#ece7f2", "#a6bddb","#2b8cbe"), domain = states$population, bins = mybins)
  proxy <- leafletProxy("map")
  
  output$map <- renderLeaflet({
    leaflet(states) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addPolygons(layerId=states$name,
      fillColor = ~pal(population),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = sprintf(
        "<strong>%s</strong><br/>%g children",
        states$name, states$population
      )%>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(pal = pal, values = ~population, opacity = 0.7, title = NULL,
              position = "bottomright")
    })
  
    observeEvent(input$map_shape_click, {
      #create object for clicked polygon
      click <- input$map_shape_click
      
      RV$Clicks<-c(RV$Clicks, click$id)
      
      clickedPolys <- states[states@data$name %in% RV$Clicks, ]
      #if the shape has already been selected, remove it
      if(grepl("clicked",click$id)){
        #print("double click")
        RV$Clicks <- RV$Clicks[!RV$Clicks %in% click$id]
        if(str_count(click$id, "\\S+") > 2){
          RV$Clicks <- RV$Clicks[!RV$Clicks %in% word(click$id, 1, 2)]
        } else {
          RV$Clicks <- RV$Clicks[!RV$Clicks %in% word(click$id, 1)]
        }
        proxy %>% removeShape(layerId = click$id)
      #add the shape to the proxy map
      } else {
        proxy %>% addPolygons(data=clickedPolys, 
                              layerId=paste(clickedPolys$name, "clicked"), 
                              fillColor = ~pal(clickedPolys$population), 
                              fillOpacity = 1, 
                              weight=3, opacity=1, color="black", dashArray="",
                              highlight = highlightOptions(
                                weight = 5,
                                color = "#666",
                                dashArray = "",
                                fillOpacity = 0.7,
                                bringToFront = TRUE),
                              label = sprintf(
                                "<strong>%s</strong><br/>%g children",
                                clickedPolys$name, clickedPolys$population
                              ) %>% lapply(htmltools::HTML),
                              labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"))
      }
      #print(paste("Clicked object:",click$id))
      #print(paste("Clicks:",RV$Clicks))
      #print(clickedPolys@data$name)
      
    })
  
    #totalPop <- c(100000, 123000, 115000, 121000, 95000, 90000, 87000, 85000, 75000, 73000, 70000, 65000)
    #adoptedPop <- c(10000, 11000, 13000, 9000, 33500, 22000, 17000, 18000, 15000, 12000, 10000, 14000)
    output$dygraph <- renderDygraph(dygraph(RV$dy, main="Adoptions") %>%
                                      dySeries("V2", label="Adoptable Population", color="#2b8cbe", strokeWidth=2) %>% 
                                      dySeries("V3", label="Adopted Population", color="#ef8a62", strokeWidth=2) %>%
                                      dyOptions(maxNumberWidth=20) %>% 
                                      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                      dyCrosshair(direction="vertical") %>%
                                      dyAxis("x", label = "Year", rangePad = 20, valueRange=c(2004, 2015)) %>%
                                      dyAxis("y", label = "Population") %>%
                                      dyEvent("2008", "Adoption Incentives Program", labelLoc = "bottom")
    )
    
    observeEvent(input$dygraph_click, {
      activeYear <- input$dygraph_click$x
      states<-switch(activeYear-2003, states2004, states2005, states2006, states2007,states2008, states2009, states2010, states2011, states2012, states2013, states2014, states2015)
      print(summary(states))
      quart <- quantile(states$population)
      mybins<- c(quart[[1]], quart[[2]], quart[[3]], quart[[4]], quart[[5]])
      pal <- colorBin(c("#ece7f2", "#a6bddb","#2b8cbe"), domain = states$population, bins = mybins)
      
      proxy %>% clearShapes() %>% clearControls() %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
        addPolygons(data=states, layerId=states$name,
                    fillColor = ~pal(states$population),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = sprintf(
                      "<strong>%s</strong><br/>%g children",
                      states$name, states$population
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
      addLegend(pal = pal, values = states$population, opacity = 0.7, title = NULL,
                position = "bottomright")
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)

#library(rsconnect)
#rsconnect::deployApp("C:/Users/Luke Morrow/Desktop/Clemson/cpsc6300/TestMap")

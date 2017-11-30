#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(jsonlite)
library(plyr)
library(reshape2)
library(geojson)
library(leaflet)
library(dygraphs)

data <- read.csv("Complete_FosterCare_selections.csv")
states <- geojsonio::geojson_read("us-states2015.geojson", what = "sp")

ui <- fluidPage(title="Adoption Incentive Program of 2008", theme="bootstrap.css",
  #top title header
  fluidRow(
    column(12,
      h1(strong(HTML("<center>Adoption Incentive Program of 2008</center>")))
    )
  ),
  #subtitle header
  fluidRow(
    column(12,
           h3(em(HTML("<center>a visual exploration of its effectiveness</center>")))
    )
  ),
  #first row contains the checkboxes and the map
  fluidRow(
    column(2, 
           checkboxGroupInput("gender", "Select a gender:",
                              c("Girl", "Boy"), selected = c("Girl", "Boy")
           ),
           checkboxGroupInput("race", "Select a race:",
                              c("White", "Black", "Hispanic", "Other"), selected = c("White", "Black", "Hispanic", "Other")
           ),
           checkboxGroupInput("age", "Select an age group:",
                              choiceNames=c("Older than 9", "Younger than 9"), selected= c("over9", "under9"), choiceValues = list("over9", "under9")
           ),
           checkboxGroupInput("specNd", "Select Special Needs:",
                              choiceNames=c("Yes", "No"), selected= c("specYes", "specNo"), choiceValues = list("specYes", "specNo")
           )
    ),
    column(10, 
           leafletOutput("map")
    )
  ),
  fluidRow(
    column(12, 
           p()
    )
  ),
  #second row contains the instructions and the dygraph
  fluidRow(
    column(2, 
           p("There are several ways included in this visualization to explore the data."), br(), p("-By selecting features above, the map and graph will update to reflect the current subset."),
           br(), p("-By selecting states on the map, the graph will update to reflect the current subset."), br(), p("-By selecting a year on the graph, the map will update to reflect the current subset.")
    ),
    column(10, 
           dygraphOutput("dygraph")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  RV<-reactiveValues(Clicks=list())
  
  #the selection is reactive to which checkboxes are selected
  selection <- reactive({
    DF <- data    
    #we use grepl to return boolean TRUE or FALSE to subset our data
    subset(DF, grepl(tolower(paste(input$gender,collapse='|')), Sex) & grepl(tolower(paste(input$race,collapse='|')), Race) 
          & grepl(tolower(paste(input$age,collapse='|')), Age_Bin) & grepl(paste(input$specNd,collapse='|'), Spec_Nd))
    })
  
  #this is the data for the dygraph
  years <- reactive({
    years <- data.frame(matrix(ncol = 3, nrow = 12))
    names <- c("year", "TotalAdopted", "TotalPop")
    colnames(years) <- names
    years$year <- c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
    
    #subset the data by which states are clicked on the map
    dySelection <- subset(selection(), grepl(paste(RV$clickedPolys$name, collapse='|'), State))

    adopted <- subset(dySelection, grepl("adopted", Adopted))
    notAdopted <- subset(dySelection, grepl("notAdopted", Adopted))
    #we subset the adopted and not adopted data for every year, there probably is a better way to do this
    y2004 <- subset(adopted, grepl(2004, Year))
    y2005 <- subset(adopted, grepl(2005, Year))
    y2006 <- subset(adopted, grepl(2006, Year))
    y2007 <- subset(adopted, grepl(2007, Year))
    y2008 <- subset(adopted, grepl(2008, Year))
    y2009 <- subset(adopted, grepl(2009, Year))
    y2010 <- subset(adopted, grepl(2010, Year))
    y2011 <- subset(adopted, grepl(2011, Year))
    y2012 <- subset(adopted, grepl(2012, Year))
    y2013 <- subset(adopted, grepl(2013, Year))
    y2014 <- subset(adopted, grepl(2014, Year))
    y2015 <- subset(adopted, grepl(2015, Year))
    years$TotalAdopted <- c(sum(y2004$Numbers),
                            sum(y2005$Numbers),
                            sum(y2006$Numbers),
                            sum(y2007$Numbers),
                            sum(y2008$Numbers),
                            sum(y2009$Numbers),
                            sum(y2010$Numbers),
                            sum(y2011$Numbers),
                            sum(y2012$Numbers),
                            sum(y2013$Numbers),
                            sum(y2014$Numbers),
                            sum(y2015$Numbers))
    
    y2004 <- subset(notAdopted, grepl(2004, Year))
    y2005 <- subset(notAdopted, grepl(2005, Year))
    y2006 <- subset(notAdopted, grepl(2006, Year))
    y2007 <- subset(notAdopted, grepl(2007, Year))
    y2008 <- subset(notAdopted, grepl(2008, Year))
    y2009 <- subset(notAdopted, grepl(2009, Year))
    y2010 <- subset(notAdopted, grepl(2010, Year))
    y2011 <- subset(notAdopted, grepl(2011, Year))
    y2012 <- subset(notAdopted, grepl(2012, Year))
    y2013 <- subset(notAdopted, grepl(2013, Year))
    y2014 <- subset(notAdopted, grepl(2014, Year))
    y2015 <- subset(notAdopted, grepl(2015, Year))
    years$TotalPop <- c(sum(y2004$Numbers),
                        sum(y2005$Numbers),
                        sum(y2006$Numbers),
                        sum(y2007$Numbers),
                        sum(y2008$Numbers),
                        sum(y2009$Numbers),
                        sum(y2010$Numbers),
                        sum(y2011$Numbers),
                        sum(y2012$Numbers),
                        sum(y2013$Numbers),
                        sum(y2014$Numbers),
                        sum(y2015$Numbers))
    years$TotalPop <- as.numeric(years$TotalPop + years$TotalAdopted)
    totalAdopted <- sum(adopted$Numbers)
    totalNot <- sum(notAdopted$Numbers)
    
    #return the data for the dygraph
    return(years)
  })
  
  #this is the data for the map
  selectedYear <- reactive({
    #by default we will display 2015 data on the map
    if(is.null(input$dygraph_click)){
      clickedYear <- 2015
    } else {
      clickedYear <- as.numeric(input$dygraph_click$x)
    }
    #we subset the data based on which year has been clicked on the dygraph
    current <- subset(selection(), grepl(clickedYear, Year))

    #sum all the numbers to provide total population values to the states
    currentPop <- aggregate(current$Numbers, by=list(current$State), FUN=sum)
    states$population <- as.numeric(currentPop$x)
    #print(states$population)
    return(states)
    
  })
  
  pal <- colorBin(c("#ece7f2", "#a6bddb","#2b8cbe"), domain=c(0, 75, 500, 1500, 5000, 25000), bins=c(0, 75, 500, 1500, 5000, 25000), pretty=TRUE)
  
  #the output of the leaflet map
  output$map <- renderLeaflet({
    leaflet(states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
  })
  
  #handling the clicks on the map
  observeEvent(input$map_shape_click, {
    #create object for clicked polygon
    click <- input$map_shape_click
    
    RV$Clicks<-c(RV$Clicks, click$id)
    selected <- selectedYear()
    RV$clickedPolys <- selected[selected@data$name %in% RV$Clicks, ]
    #if the shape has already been selected, remove it
    if(grepl("clicked",click$id)){
      #print("double click")
      RV$Clicks <- RV$Clicks[!RV$Clicks %in% click$id]
      if(str_count(click$id, "\\S+") > 2){
        RV$Clicks <- RV$Clicks[!RV$Clicks %in% word(click$id, 1, 2)]
      } else {
        RV$Clicks <- RV$Clicks[!RV$Clicks %in% word(click$id, 1)]
      }
      RV$clickedPolys <- selected[selected@data$name %in% RV$Clicks, ]
      leafletProxy("map", data=states) %>% removeShape(layerId = click$id)
      #add the shape to the proxy map
    } else {
      leafletProxy("map", data=selectedYear()) %>% addPolygons(data=RV$clickedPolys, group="selected",
                            layerId=paste(RV$clickedPolys$name, "clicked"), 
                            fillColor = ~pal(RV$clickedPolys$population), 
                            fillOpacity = 1, 
                            weight=3, opacity=1, color="black", dashArray="",
                            highlight = highlightOptions(
                              weight = 5,
                              color = "#666",
                              dashArray = "",
                              fillOpacity = 0.7,
                              bringToFront = TRUE),
                            label = sprintf(
                              "<strong>%s</strong><br/>",
                              RV$clickedPolys$name
                            ) %>% lapply(htmltools::HTML),
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "15px",
                              direction = "auto"))
    }
    #print(paste("Clicked object:",click$id))
    #print(paste("Clicks:",RV$Clicks))
    #print(RV$clickedPolys@data$name)
    
  })
  
  #the output of the dygraph
  output$dygraph <- renderDygraph(dygraph(years(), main="Adoptions") %>%
                                    dySeries("TotalPop", label="Total Population", axis='y2', color="#2b8cbe", strokeWidth=2) %>% 
                                    dySeries("TotalAdopted", label="Adopted Population", color="#ef8a62", strokeWidth=2) %>%
                                    dyOptions(maxNumberWidth=20) %>% 
                                    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                    dyCrosshair(direction="vertical") %>%
                                    dyAxis("x", label = "Year", rangePad = 20, valueRange=c(2004, 2015)) %>%
                                    dyAxis("y", label = "Adopted Population") %>%
                                    dyAxis("y2", label = "Total Population", independentTicks = TRUE) %>%
                                    dyEvent("2008", "Adoption Incentives Program", labelLoc = "bottom")
  )
  
  #proxy for updating the leaflet if the selectedYear changes
  observe({
    proxy <- leafletProxy("map", data=selectedYear())
    if(is.null(input$dygraph_click)){
      clickedYear <- 2015
    } else {
      clickedYear <- as.numeric(input$dygraph_click$x)
    }
    currentTitle <- paste(clickedYear, " Population")
    selected <- selectedYear()
    proxy %>% clearControls() %>%
      clearGroup("original") %>% clearGroup("selected") %>% addPolygons(data=selectedYear(), layerId=selected$name, group="original",
                                             fillColor = ~pal(selected$population),
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
                                               selected$name, selected$population
                                             ) %>% lapply(htmltools::HTML),
                                             labelOptions = labelOptions(
                                               style = list("font-weight" = "normal", padding = "3px 8px"),
                                               textsize = "15px",
                                               direction = "auto")) %>%
      addLegend(pal = pal, values = selected$population, title= currentTitle, opacity = 0.7,
                position = "bottomright") 
    
    if(length(RV$Clicks) > 0){ 
      RV$clickedPolys <- selected[selected@data$name %in% RV$Clicks, ]
      
      proxy %>% clearGroup("selected") %>%
        addPolygons(data=RV$clickedPolys, group="selected",
                    layerId=paste(RV$clickedPolys$name, "clicked"), 
                    fillColor = ~pal(RV$clickedPolys$population), 
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
                      RV$clickedPolys$name, RV$clickedPolys$population
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

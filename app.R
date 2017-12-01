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
library(stringr)

data <- read.csv("Complete_FosterCare_selections.csv")
states <- geojsonio::geojson_read("us-states2015.geojson", what = "sp")

ui <- navbarPage(title="Adoption Incentive Program of 2008", theme="bootstrap.css",
  tabPanel("Explore", 
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
           dygraphOutput("dygraph", height="60%"), dygraphOutput("dygraph2", height="60%")
    )
  )
  ),
  tabPanel("About", 
           h1(strong(HTML("About our visualization"))),
           h3("What is the Incentive Program?"),
             p("► Started by Clinton administration in 1997 as Adoption and Safe Families act."),
             p("► Goal is to reduce the number of children lingering in foster care"),
             p("► 2008 Program sets new fiscal awards based on 2007 levels of adoptions "),
             p("► If a state adopts more children than their base, the value to the state is:"),
             p(HTML("&emsp;● $4000.00 per child")),
             p(HTML("&emsp;● $4000.00 more if the child is over 9 years old")),
             p(HTML("&emsp;● $4000.00 for “special needs” children under 9 years old")),
             p("► Since 2004, over $300,000,000.00 has been spent on incentives"),
           h3("Data:"),
            p("► Data for the Adoption and Foster Care Analysis and Reporting System (AFCARS) are given annually to the National Data Archive on Child Abuse and Neglect (NDACAN) for distribution to the research community by the Children’s Bureau."), 
            p("► Funding for the project is provided by the Children’s Bureau, Administration on Children, Youth and Families, Administration for Children and Families, U.S. Department of Health and Human Services."),
            p("► Observations range from 635,000 – 700,000 per year"),
            p("► Over 75 variables with case level information on each child"),
            p("► Visualization limited to the years 2004 - 2015"),
            p("► Approximately 6 million observations total")
  )
           
           
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  RV<-reactiveValues(Clicks=list(), total=TRUE)
  
  #the selection is reactive to which checkboxes are selected
  selection <- reactive({
    DF <- data    
    #we use grepl to return boolean TRUE or FALSE to subset our data
    subset(DF, grepl(tolower(paste(input$gender,collapse='|')), Sex) & grepl(tolower(paste(input$race,collapse='|')), Race) 
          & grepl(tolower(paste(input$age,collapse='|')), Age_Bin) & grepl(paste(input$specNd,collapse='|'), Spec_Nd))
    })
  
  #this is the data for the dygraph
  years <- reactive({
    years <- data.frame(matrix(ncol = 2, nrow = 12))
    names <- c("year", "TotalAdopted")
    colnames(years) <- names
    years$year <- c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
    
    #subset the data by which states are clicked on the map
    dySelection <- subset(selection(), grepl(paste(RV$clickedPolys$name, collapse='|'), State))

    adopted <- subset(dySelection, grepl("adopted", Adopted))
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
    
   
    totalAdopted <- sum(adopted$Numbers)
    #return the data for the dygraph
    return(years)
  })
  
  #this is the data for the dygraph 2
  yearsTotal <- reactive({
    years <- data.frame(matrix(ncol = 2, nrow = 12))
    names <- c("year", "TotalPop")
    colnames(years) <- names
    years$year <- c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
    
    #subset the data by which states are clicked on the map
    dySelection <- subset(selection(), grepl(paste(RV$clickedPolys$name, collapse='|'), State))
    
    notAdopted <- subset(dySelection, grepl("notAdopted", Adopted))
    #we subset the adopted and not adopted data for every year, there probably is a better way to do this
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
    totalNot <- sum(notAdopted$Numbers)
    #return the data for the dygraph
    return(years)
  })
  
  #this is the data for the map
  selectedYear <- reactive({
    RV$total = TRUE
    #by default we will display 2015 data on the map
    if(is.null(input$dygraph_click)){
      clickedYear <- 2015
    } else {
      clickedYear <- as.numeric(input$dygraph_click$x)
    } 
    
    #we subset the data based on which year has been clicked on the dygraph
    current <- subset(selection(), grepl(clickedYear, Year))
    current <- subset(current, grepl("notAdopted", Adopted))
    
    #sum all the numbers to provide total population values to the states
    currentPop <- aggregate(current$Numbers, by=list(current$State), FUN=sum)
    currentPop <- rbind(currentPop[-c(40), ], currentPop[c(40), ]) 
    
    states$population <- as.numeric(currentPop$x)
    #print(states$population)
    return(states)
    
  })
  
  #this is the adopted data for the map
  selectedYearAdopt <- reactive({
    RV$total = FALSE
    #by default we will display 2015 data on the map
    if(is.null(input$dygraph2_click)){
      clickedYear <- 2015
    } else {
      clickedYear <- as.numeric(input$dygraph2_click$x)
    } 
    
    #we subset the data based on which year has been clicked on the dygraph
    current <- subset(selection(), grepl(clickedYear, Year))
    current <- subset(current, grepl("adopted", Adopted))
    
    #sum all the numbers to provide total population values to the states
    currentPop <- aggregate(current$Numbers, by=list(current$State), FUN=sum)
    currentPop <- rbind(currentPop[-c(40), ], currentPop[c(40), ]) 
    
    states$population <- as.numeric(currentPop$x)
    print(c(states$name, states@data$population))
    #print(states$population)
    return(states)
    
  })
  
  
  pal <- colorBin(c("#ece7f2", "#a6bddb","#2b8cbe"), domain=c(0, 75, 500, 1500, 5000, 25000), bins=c(0, 75, 500, 1500, 5000, 25000), pretty=TRUE)
  palAdopt <- colorBin(c("#fee8c8", "#fdbb84","#e34a33"), domain=c(0, 75, 500, 1500, 5000, 25000), bins=c(0, 75, 500, 1500, 5000, 25000), pretty=TRUE)
  
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
    if(RV$total){
      selected <- selectedYear()
    } else {
      selected <- selectedYearAdopt()
    }
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
      if(RV$total){
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
                                                                   "<strong>%s</strong><br/>%g children",
                                                                   RV$clickedPolys$name, RV$clickedPolys$population
                                                                 ) %>% lapply(htmltools::HTML),
                                                                 labelOptions = labelOptions(
                                                                   style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                   textsize = "15px",
                                                                   direction = "auto"))
      } else {
        leafletProxy("map", data=selectedYearAdopt()) %>% addPolygons(data=RV$clickedPolys, group="selected",
                                                                 layerId=paste(RV$clickedPolys$name, "clicked"), 
                                                                 fillColor = ~palAdopt(RV$clickedPolys$population), 
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
      
    }
    #print(paste("Clicked object:",click$id))
    #print(paste("Clicks:",RV$Clicks))
    #print(RV$clickedPolys@data$name)
    
  })
  
  #the output of the dygraph
  output$dygraph <- renderDygraph(dygraph(yearsTotal(), main="Total Population", group="graphs") %>%
                                    dySeries("TotalPop", label="Total Population", color="#2b8cbe", strokeWidth=2) %>% 
                                    dyOptions(maxNumberWidth=20) %>% 
                                    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                    dyCrosshair(direction="vertical") %>%
                                    dyAxis("x", label = "Year", rangePad = 75, valueRange=c(2004, 2015), drawGrid=FALSE) %>%
                                    dyAxis("y", label = "Total Population", pixelsPerLabel="15", axisLabelWidth = 70) %>%
                                    dyEvent("2008", "Adoption Incentives Program", labelLoc = "bottom")
  )
  
  output$dygraph2 <- renderDygraph(dygraph(years(), main="Adopted Population", group="graphs") %>%
                                    dySeries("TotalAdopted", label="Adopted Population", color="#ef8a62", strokeWidth=2) %>%
                                    dyOptions( maxNumberWidth=20) %>% 
                                    dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                                    dyCrosshair(direction="vertical") %>%
                                    dyAxis("x", label = "Year", rangePad = 75, valueRange=c(2004, 2015), drawGrid=FALSE) %>%
                                    dyAxis("y", label = "Adopted Population", pixelsPerLabel="15", axisLabelWidth = 70) %>%
                                    dyEvent("2008", "Adoption Incentives Program", labelLoc = "bottom")
  )
  
  #proxy for updating the leaflet if the selectedYear changes
  observeEvent(input$dygraph_click, {
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
  
  #proxy for updating the adopted leaflet if the selectedYear changes
  observeEvent(input$dygraph2_click, {
    proxy <- leafletProxy("map", data=selectedYearAdopt())
    if(is.null(input$dygraph_click)){
      clickedYear <- 2015
    } else {
      clickedYear <- as.numeric(input$dygraph2_click$x)
    }
    currentTitle <- paste(clickedYear, " Adopted Population")
    selected <- selectedYearAdopt()
    proxy %>% clearControls() %>%
      clearGroup("original") %>% clearGroup("selected") %>% addPolygons(data=selectedYearAdopt(), layerId=selected$name, group="original",
                                                                        fillColor = ~palAdopt(selected$population),
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
      addLegend(pal = palAdopt, values = selected$population, title= currentTitle, opacity = 0.7,
                position = "bottomright") 
    
    if(length(RV$Clicks) > 0){ 
      RV$clickedPolys <- selected[selected@data$name %in% RV$Clicks, ]
      
      proxy %>% clearGroup("selected") %>%
        addPolygons(data=RV$clickedPolys, group="selected",
                    layerId=paste(RV$clickedPolys$name, "clicked"), 
                    fillColor = ~palAdopt(RV$clickedPolys$population), 
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

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

#clear memory
rm(list=ls(all=TRUE))

#load required packages
library(shiny)
library(rgdal)
library(leaflet)
library(rsconnect)
library(shinydashboard)
library(visNetwork)
library(dplyr)

#load data files
md.geocoded <- read.csv("rural_md_geocoded.csv", stringsAsFactors = F)
md.slave.data <- read.csv("enslaved_people_list.csv", stringsAsFactors = F)
clergy.net <- read.csv("priest_network.csv", header = T, as.is = T)

#CLERGY NETWORK SECTION
#generate html popup to use as node label for network visualization
clergy.net$popupw <- paste(sep = "", "<b>",clergy.net$ShinyName,"</b><br/>",
                           "Years: ", ifelse(is.na(clergy.net$ShinyDates), "Unknown", clergy.net$ShinyDates),"<br/>",
                           "Bible copies: ", ifelse(is.na(clergy.net$SubscriberNoCopies), "Unknown", clergy.net$SubscriberNoCopies),"<br/>",
                           "Enslaved People: ",ifelse(is.na(clergy.net$NumberSlaves),"Unknown",clergy.net$NumberSlaves), "<br/>",
                           "Notes: ",ifelse(is.na(clergy.net$NetworkNote),"N/A",clergy.net$NetworkNote),"<br/>"
) #end html popup


#MAPPING SECTION
#randomize points on the map
md.geocoded$latlong <- paste(md.geocoded$lat,md.geocoded$lon,sep="-")

md.geocoded$numSlaves <- as.integer(md.geocoded$NumberSlaves)


a<- data.frame(table(md.geocoded$latlong))
a$latlong <- as.character(a$Var1)
a$Var1 <- NULL
md.geocoded <- merge(md.geocoded,  a, by="latlong",all=T)
remove(a)
md.geocoded$lat_edit <-ifelse((md.geocoded$Freq > 1),  md.geocoded$lat - (runif(nrow(md.geocoded))-.5)/40,md.geocoded$lat)
md.geocoded$lon_edit <-ifelse((md.geocoded$Freq > 1), md.geocoded$lon - (runif(nrow(md.geocoded))-.5)/40,md.geocoded$lon)
md.geocoded$latlong<- NULL
md.geocoded$SlavOwnerText <- "Unknown"
md.geocoded[!is.na(md.geocoded$SlaveOwner),]$SlavOwnerText <- "Confirmed"

#map and color legend
md.spdf <-  SpatialPointsDataFrame(coords = md.geocoded[,c("lon_edit","lat_edit")], data = md.geocoded,
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
pal <- colorFactor(palette = 'Set1', domain =md.geocoded$LocationConfidenceLevel)
pal.slaves <- colorFactor(palette = 'Set1', domain =md.geocoded$SlavOwnerText)

#generate html popup
md.spdf$popupw <- paste(sep = "",  "<b>", md.spdf$ShinyName,"</b><br/>",
                        "Years: ", ifelse(is.na(md.spdf$ShinyDates), "Unknown", md.spdf$ShinyDates),"<br/>",
                        "Total Household Size: ",ifelse(is.na(md.spdf$CensusNumberOfHouseholdMembers),"Unknown",md.spdf$CensusNumberOfHouseholdMembers), "<br/>",
                        "Enslaved People: ",ifelse(is.na(md.spdf$NumberSlaves),"Unknown",md.spdf$NumberSlaves), "<br/>",
                        "Location: ",md.spdf$PlottedLocation, "<br/>", 
                        "Notes: ",ifelse(is.na(md.spdf$ShinyNote),"N/A",md.spdf$ShinyNote),"<br/>"
) #end html popup

#SHINY SECTION
#build Shiny interface
ui <- dashboardPage(
  dashboardHeader(title = "First U.S. Catholic Bible"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map",icon = icon("map-marker"), selected = TRUE, startExpanded = FALSE),
      checkboxInput("slaves", "Confirmed Slave Owners Only", value = FALSE, width = NULL),
      checkboxInput("priests", "Priests Only", value = FALSE, width = NULL),
      selectInput("toDisplay","Display",c("Slave Owner","LocationAccuracy"), selected = "Slave Owner"),
      menuItem("Network", tabName = "network", icon = icon("th")),
      menuItem("Raw Data", tabName = "rawdata",icon = icon("file")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )),
  dashboardBody(
    tabItems(
      tabItem("map",
              fluidRow(
                box(width = 9, status = "primary", solidHeader = TRUE,
                    title = "Subscribers in Rural Maryland",
                    leafletOutput("mymap", height = 500)),
                box(width = 3, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    title = "Household Size Example", 
                    plotOutput("chartTest", height = 200)),
                box(width = 3, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    title = "Another Output Example", 
                    plotOutput("anotherOutput", height = 200))
              )),
      tabItem("network",
              fluidRow(visNetworkOutput("network", height = 600)
              )),
      tabItem("rawdata")
    )
  ))

#define server logic
server <- function(input, output) {
  points <- eventReactive(c(input$slaves, input$priests), {
    working.spdf <- md.spdf
    if (input$slaves){
      working.spdf <- working.spdf[which(!is.na(working.spdf$SlaveOwner)),]
    }
    else
      if (input$priests){
        working.spdf <- working.spdf[which(!is.na(working.spdf$Priest)),]
      }
    return(working.spdf)
  }, ignoreNULL = FALSE)
  
  #reactive map output    
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldPhysical,
                       options = providerTileOptions(noWrap = TRUE),"Topographic")%>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE), group = "Modern") %>%
      addLayersControl(
        baseGroups = c("Topographic","Modern"),
        options = layersControlOptions(collapsed = TRUE)
      )
      
      # addCircleMarkers(data = points(),color = ~pal(LocationConfidenceLevel), popup = ~popupw) %>%
      # 
  })
  observe({
    if(input$toDisplay=="Slave Owner"){
      texty<-"SlavOwnerText"
      colorData <- pal.slaves(points()$SlavOwnerText)
      pal.name <- pal.slaves
      size <- points()$numSlaves/10
    }
    if (input$toDisplay=="LocationAccuracy"){
      texty<-"LocationConfidenceLevel"
      colorData <- pal(points()$LocationConfidenceLevel)
      pal.name <- pal
      size <-10
    }
  
  leafletProxy("mymap",data=points())%>%
    clearControls()%>%
    clearMarkers() %>%
    addCircleMarkers(data = points(),color = colorData, popup = ~popupw, radius = size) %>%
    addLegend("bottomleft",pal = pal.name,values=points()[[texty]], opacity = 1)%>%
    fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  
  })#End observe
  
  #define server logic required to draw a histogram
  output$chartTest<-renderPlot(
    hist(points()@data$CensusNumberOfHouseholdMembers)
  )
  
  #visNetwork output
  output$network <- renderVisNetwork({
    #create nodes
    nodes <- data.frame(id = 1:29,
                        label = paste(clergy.net$ShinyName), #node label
                        title = paste(clergy.net$popupw), #text on click
                        group = paste(clergy.net$Group) #group by status to assign different shapes to nodes
    )
    #create connections
    edges <- data.frame(from = c(1,1,1,1,1,1,1,1,1,1,
                                 1,1,1,1,1,1,1,1,1,1,
                                 1,1,1,1,1,1,1,1,
                                 10,11,28,16,7,7,8,
                                 12,13,13,27,27,
                                 19,19,26,26,26,26,26,
                                 20,20,20,20,21,21,
                                 29,29,18), 
                        to = c(2:29,
                               28,28,2,29,8,9,9,
                               2,19,20,18,2,
                               20,21,20,21,29,18,2,
                               21,29,18,2,29,2,
                               18,2,2))
    #create visNetwork
    visNetwork(nodes, edges) %>%
      # add drop-down option to select a particular person
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group") %>%
      #define shapes and colors for the clergy
      visGroups(groupname = "Clergy", color = list(background = "lightgray",border="darkgray"), shape = "box", borderWidth = 1) %>% 
      # define shapes and colors for the lay subscribers
      visGroups(groupname = "Lay", shape = "box")
  })
}

#run the application 
shinyApp(ui = ui, server = server)

#update the application in Shinyapps.io
# library(rsconnect)
# deployApp()

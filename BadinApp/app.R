# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/


rm(list=ls(all=TRUE))


#load required packages
library(shiny)
library(rgdal)
library(leaflet)
library(rsconnect)
library(shinydashboard)
library(visNetwork)
library(dplyr)
library(DT)
library(crosstalk)
library(datasets)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(V8)

#load data files
md.geocoded <- read.csv("rural_md_geocoded.csv")
md.slave.data <- read.csv("enslaved_people_list.csv", stringsAsFactors = F)
clergy.net <- read.csv("priest_network.csv", header = T, as.is = T)

nosidebar <- FALSE

#Colors and symbols
shapes = c(15, 18, 17, 16) # base R plotting symbols (http://www.statmethods.net/advgraphs/parameters.html)
#square = 15, diamond = 18, triangle = 17, circle = 16
loc.colors <- c('#666666', '#ffda09', '#ff6000', '#000066')
ens.colors <- c("#ff6000", "dark blue")
#processing
levels(md.geocoded$PlantationSize)[levels(md.geocoded$PlantationSize)==""] <- "Unknown"

#CLERGY NETWORK SECTION
#generate html popup to use as node label for network visualization
clergy.net$popupw <- paste(sep = "", "<b>",clergy.net$ShinyName,"</b><br/>",
                           "Years: ", ifelse(is.na(clergy.net$ShinyDates), "Unknown", clergy.net$ShinyDates),"<br/>",
                           "Bible copies: ", ifelse(is.na(clergy.net$SubscriberNoCopies), "Unknown", clergy.net$SubscriberNoCopies),"<br/>",
                           "Enslaved People: ",ifelse(is.na(clergy.net$NumberSlaves),"Unknown",clergy.net$NumberSlaves), "<br/>",
                           "Notes: ",ifelse(is.na(clergy.net$NetworkNote),"N/A",clergy.net$NetworkNote),"<br/>"
) #end html popup

#RAW DATA SECTION
#build the raw data tables
  #subscriber table
raw.data.tab <- read.csv("raw_data_tab.csv",stringsAsFactors = F)
rdt <- raw.data.tab %>%
  tibble::rownames_to_column()

  #enslaved table
enslaved.data.tab <- read.csv("enslaved_people_tab.csv",stringsAsFactors = F)
edt <- enslaved.data.tab %>%
  tibble::rownames_to_column()

#MAPPING SECTION
md.geocoded$LocationConfidenceLevel<- factor(md.geocoded$LocationConfidenceLevel, levels=c("State","County","City","Point"))
md.geocoded$latlong <- paste(md.geocoded$lat,md.geocoded$lon,sep="-")
md.geocoded$numSlaves <- as.integer(as.character(md.geocoded$NumberSlaves))

#randomize points on the map
a<- data.frame(table(md.geocoded$latlong))
a$latlong <- as.character(a$Var1)
a$Var1 <- NULL
md.geocoded <- merge(md.geocoded,  a, by="latlong",all=T)
remove(a)
md.geocoded$lat_edit <-ifelse((md.geocoded$Freq > 1),  md.geocoded$lat - (runif(nrow(md.geocoded))-.5)/40,md.geocoded$lat)
md.geocoded$lon_edit <-ifelse((md.geocoded$Freq > 1), md.geocoded$lon - (runif(nrow(md.geocoded))-.5)/40,md.geocoded$lon)
md.geocoded$latlong<- NULL
md.geocoded$SlavOwnerText <- "Unknown"
md.geocoded[!is.na(md.geocoded$SlaveOwner),]$SlavOwnerText <- "Confirmed Enslavers"

#map and color legend
md.spdf <-  SpatialPointsDataFrame(coords = md.geocoded[,c("lon_edit","lat_edit")], data = md.geocoded,
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))


pal <- colorFactor(palette =loc.colors, domain =md.geocoded$LocationConfidenceLevel)
pal.slaves <- colorFactor(palette = ens.colors, domain =md.geocoded$SlavOwnerText)

# shapes building: The legend is a major hack, but it may work
icon.size <- 30
pchIcons = function(pch = 1, names, width = icon.size, height = icon.size, bg = "transparent", col = "black", ...) {
  n = length(pch)
  files = character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f = paste("icons/",names[i], ".png", sep = "")
    png(f, width = width, height = height, bg = bg)
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
    dev.off()
    files[i] = f
  }
  files
}
iconFiles = pchIcons(shapes, col = loc.colors, lwd = 4, names = levels(md.geocoded$LocationConfidenceLevel))

html_legend <- paste("<img src='https://documents.library.nd.edu/documents/arch-lib/HUE-ND/Sisk_Sandbox/icons/State.png'>",levels(md.geocoded$LocationConfidenceLevel)[1],"<br/>",
                     "<img src='https://documents.library.nd.edu/documents/arch-lib/HUE-ND/Sisk_Sandbox/icons/County.png'>",levels(md.geocoded$LocationConfidenceLevel)[2],"<br/>",
                     "<img src='https://documents.library.nd.edu/documents/arch-lib/HUE-ND/Sisk_Sandbox/icons/City.png'>",levels(md.geocoded$LocationConfidenceLevel)[3],"<br/>",
                     "<img src='https://documents.library.nd.edu/documents/arch-lib/HUE-ND/Sisk_Sandbox/icons/Point.png'>",levels(md.geocoded$LocationConfidenceLevel)[4],"<br/>",sep="")

#generate html popup #
md.spdf$popupw <- paste(sep = "",  "<b>", md.spdf$ShinyName,"</b><br/>",
                        "Years: ", ifelse(is.na(md.spdf@data$ShinyDates), "Unknown", as.character(md.spdf@data$ShinyDates)),"<br/>",
                        "Total Household Size: ",ifelse(is.na(md.spdf$CensusNumberOfHouseholdMembers),"Unknown",as.character(md.spdf$CensusNumberOfHouseholdMembers)), "<br/>",
                        "Enslaved People: ",md.spdf$NumberSlaves, "<br/>",
                        "Location: ",md.spdf$PlottedLocation, "<br />"
                        # "Notes: ", md.spdf$Notes
) #end html popup


#SHINY SECTION
#build Shiny interface
ui <- dashboardPage(
  
  dashboardHeader(title = "First U.S. Catholic Bible"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map",icon = icon("map-marker"), selected = TRUE, startExpanded = FALSE),
      checkboxInput("slaves", "Confirmed Enslavers Only", value = FALSE, width = NULL),
      checkboxInput("priests", "Priests Only", value = FALSE, width = NULL),
      checkboxGroupInput("planSize", label = "Plantation Size", 
                         choices = levels(md.geocoded$PlantationSize),
                                          selected = levels(md.geocoded$PlantationSize)),
      selectInput("toDisplay","Display",c("Enslavers","LocationAccuracy"), selected = "Enslavers"),
      menuItem("Network", tabName = "network", icon = icon("th")),
      menuItem("Subscriber Data", tabName = "rawdata",icon = icon("file")),
      menuItem("Enslaved Data", tabName = "enslaveddata",icon = icon("file")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )),
  dashboardBody(useShinyjs(),
                extendShinyjs(text = "shinyjs.hidehead = function(parm){$('header').css('display', parm);}"),
    tags$head(tags$style(type = "text/css", "html, body {width:100%;height:100%}",
                         ".leaflet .legend i{
                         border-radius: 50%;
                         width: 10px;
                         height: 10px;
                         margin-top: 4px;
                         }
                         ")),
    tabItems(
      tabItem("map",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Subscribers in Rural Maryland",
                    leafletOutput("mymap", height = 500))
              )),
      tabItem("about",
              fluidRow(
                wellPanel(htmlOutput("about"))
              )
        
               ),
      tabItem("network",
              fluidRow(visNetworkOutput("network", height = 600)
              )),
      tabItem("rawdata",fluidRow(
        wellPanel(DT::dataTableOutput("rawoutput"),
                  fluidRow(p(class = 'text-center'))))
              ),
      tabItem("enslaveddata",fluidRow(
        wellPanel(DT::dataTableOutput("x2"),
                  fluidRow(p(class = 'text-center'))))
      )
    )
  ))

#define server logic
server <- function(input, output, session) {
  
  addLegendCustom <- function(second, map,colors, labels, sizes, opacity = 0.5){
    if (second){
      colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
      labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
      
      return(addLegend(map, "bottomleft", colors = colorAdditions, labels = labelAdditions, opacity = opacity)) 
      
    }else{
      return(clearTopoJSON(map))
    }
  }
  addLegendWithHTML <- function(locationAcc, map, paly = NA, valy = NA){
    if (locationAcc){
      return(addControl(map,html = html_legend, position = "bottomleft"))
    }else{
      return(addLegend(map,"bottomleft",pal = paly, values = valy, opacity = 1))
        
    }
  }
  addMarkersCustom <- function(locationAcc, map, colors = colorData, data, sizey = size){
    if (locationAcc){
      return(addMarkers(map,data = data, icon = icons(
        iconUrl = iconFiles[points()$LocationConfidenceLevel],
        popupAnchorX = 20, popupAnchorY = 0,iconAnchorX = icon.size/2, iconAnchorY= icon.size/2
      ), popup = ~popupw, options = markerOptions(opacity = .5)))
    }else{
      return(addCircleMarkers(map, data = data,color = colors,popup = ~popupw, radius = sizey))
    }
  }
  
  points <- eventReactive(c(input$slaves, input$priests, input$planSize), {
    working.spdf <- md.spdf[md.spdf$PlantationSize %in% input$planSize,]
    if (input$slaves){
      working.spdf <- working.spdf[which(!is.na(working.spdf$SlaveOwner)),]
    }
    if (input$priests){
        working.spdf <- working.spdf[which(!is.na(working.spdf$Priest)),]
    }
    return(working.spdf)
  }, ignoreNULL = FALSE)
  
  #reactive map output    
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TopOSMRelief,
                       options = providerTileOptions(noWrap = TRUE),"Topographic")%>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE), group = "Modern") %>%
      addLayersControl(
        baseGroups = c("Modern", "Topographic"),
        options = layersControlOptions(collapsed = TRUE)
      )
      
      # addCircleMarkers(data = points(),color = ~pal(LocationConfidenceLevel), popup = ~popupw) %>%
      # 
  })
  observe({
    if(input$toDisplay=="Enslavers"){
      texty<-"SlavOwnerText"
      colorData <- pal.slaves(points()$SlavOwnerText)
      pal.name <- pal.slaves
      size <- ifelse(is.na(md.spdf$numSlaves),5,as.numeric(md.spdf$numSlaves)/10)
      icons = FALSE
      second.legend <- TRUE
      locationAcc <- FALSE

    }
    if (input$toDisplay=="LocationAccuracy"){
      texty<-"LocationConfidenceLevel"
      colorData <- pal(points()$LocationConfidenceLevel)
      pal.name <- pal
      size <-10
      second.legend <- FALSE
      locationAcc <- TRUE
    }
  
  leafletProxy("mymap",data=points())%>%
    clearControls()%>%
    clearMarkers() %>%
    addMarkersCustom(data = points(), locationAcc = locationAcc, colors = colorData, sizey = size)%>%
    # addLegend("bottomleft",pal = pal.name,values=points()[[texty]], opacity = 1)%>%
    addLegendWithHTML(locationAcc = locationAcc, paly = pal.name, valy=points()[[texty]]) %>%
    addLegendCustom(second = second.legend, colors = pal.slaves("Confirmed Enslavers"), labels = c("Few People Enslaved", "", "Many People Enslaved"), sizes = c(5, 10, 20))%>%
    
    fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  
  
  
  
  
  #attempting to make it selectable via url
  query <- parseQueryString(session$clientData$url_search)
  print(query)
  if (!is.null(query[['display']])) {
    # updateSliderInput(session, "bins", value = query[['bins']])
    #      selectInput("toDisplay","Display",c("Enslavers","LocationAccuracy"), selected = "Enslavers"),
    if(query$display=="LocationAccuracy"){
      updateSelectInput(session, inputId = "toDisplay",selected = "LocationAccuracy")
    }
   
  }
  if(!is.null(query[['side']])){
    if(query$side == "no"){
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      js$hidehead('none') 
    }
  }
  
  
  #end selectable via url
  })#End observe
  
  #define server logic required to draw a histogram
  output$chartTest<-renderPlot(
    NULL
  )
  
  #about section
  output$about <- renderText ("This project was developed based on a gallery and online exhibition, <a href=https://collections.library.nd.edu/04f477d5b4/preserving-the-steadfastness-of-your-faith>“Preserving the Steadfastness of Your Faith’: Catholics in the Early American Republic,” </a> at Hesburgh Libraries, the University of Notre Dame. The site is supported in part by a Hesburgh Library Research Grant.<p></p> 
                             Rachel Bohlmann, PhD, American History Librarian at Hesburgh Libraries, started the project, and researched and interpreted the data.<p></p>
                             Suzanna Krivulskaya, PhD, Assistant Professor of History at California State University San Marcos, contributed to research, data analysis, and coding for the app.<p></p>
                             Matthew Sisk, PhD, Geographic Information Systems Librarian in the Narvari Family Center for Digital Scholarship at Hesburgh Libraries, created the project's original exhibition maps and geocoding, and is the project's GIS expert.
                             ")
  
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
  
  #output raw subscriber data table
  d <- SharedData$new(rdt, ~rowname)
  
  #highlight selected rows in the table
  output$rawoutput <- DT::renderDataTable(
    points()@data[,c("SubscriberLastName","SubscriberFirstName","SubscriberTitle","SubscriberLocation","PlottedLocation","NumberSlaves","ShinyNote")],
    options = list(scrollX = T)
    )#end renderDataTable
  
  #output raw esnslaved data table
  e <- SharedData$new(edt, ~rowname)
  
  #highlight selected rows in the table
  output$x2 <- DT::renderDataTable(
    edt[,2:17],
    options = list(scrollX = T)
    )# end renderDataTable
}

#run the application 
shinyApp(ui = ui, server = server)

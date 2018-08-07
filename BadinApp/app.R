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

#load data files
md.geocoded <- read.csv("rural_md_geocoded.csv", stringsAsFactors = F)
md.slave.data <- read.csv("enslaved_people_list.csv", stringsAsFactors = F)

#randomize points on the map
md.geocoded$latlong <- paste(md.geocoded$lat,md.geocoded$lon,sep="-")
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
                         # "Number Copies: ",md.spdf$SubscriberNoCopies, "<br/>",
                         # "Under 16: ",ifelse(is.na(md.spdf$Under.16),"Unknown",md.spdf$Under.16), "<br/>",
                         # "Over 16: ",ifelse(is.na(md.spdf$Over16),"Unknown",md.spdf$Over16), "<br/>",
                        "Total Household Size: ",ifelse(is.na(md.spdf$CensusNumberOfHouseholdMembers),"Unknown",md.spdf$CensusNumberOfHouseholdMembers), "<br/>",
                        "Enslaved People: ",ifelse(is.na(md.spdf$NumberSlaves),"Unknown",md.spdf$NumberSlaves), "<br/>",
                        "Location: ",md.spdf$PlottedLocation, "<br/>", 
                        "Notes: ",ifelse(is.na(md.spdf$ShinyNote),"N/A",md.spdf$ShinyNote),"<br/>"
                    ) #end html popup

#build Shiny interface
ui <- dashboardPage(
  dashboardHeader(title = "Badin Bible Subscribers"),
  dashboardSidebar(
    checkboxInput("slaves", "Confirmed Slave Owners Only", value = FALSE, width = NULL),
    checkboxInput("priests", "Priests Only", value = FALSE, width = NULL),
  sidebarMenu(
    menuItem("Map", tabName = "map"),
    menuItem("Raw Data", tabName = "rawdata"),
    menuItem("Network", tabName = "network")
  )),
  dashboardBody(
    tabItems(
      tabItem("map",
              fluidRow(
                box(width = 9, status = "primary", solidHeader = TRUE,
                  title = "Subscribers in Rural Maryland",
                  leafletOutput("mymap", height = 500)),
                box(width = 3, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  title = "Household Size", 
                  plotOutput("chartTest", height = 200)),
                box(width = 3, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    title = "Another Output Example", 
                    plotOutput("anotherOutput", height = 200))
                ))))
)

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
       addProviderTiles(providers$Stamen.TonerLite,
                        options = providerTileOptions(noWrap = TRUE)
       ) %>%
       addCircleMarkers(data = points(),color = ~pal(LocationConfidenceLevel), popup = ~popupw) %>%
       addLegend("bottomleft",pal = pal,values=points()$LocationConfidenceLevel, opacity = 1)
   })
   
#define server logic required to draw a histogram
   output$chartTest<-renderPlot(
     hist(points()@data$CensusNumberOfHouseholdMembers)
   )
}

#run the application 
shinyApp(ui = ui, server = server)

#update the application in Shinyapps.io
# library(rsconnect)
# deployApp()
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list=ls(all=TRUE)) #clear memory

library(shiny)
library(rgdal)
library(leaflet)
library(rsconnect)


#Data Prep
md.geocoded <- read.csv("rural_md_geocoded.csv", stringsAsFactors = F)
md.slave.data <- read.csv("enslaved_people_list.csv", stringsAsFactors = F)


#randomizingPoints
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


#end RandomizingPoints




md.spdf <-  SpatialPointsDataFrame(coords = md.geocoded[,c("lon_edit","lat_edit")], data = md.geocoded,
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
pal <- colorFactor(palette = 'Set1', domain =md.geocoded$LocationConfidenceLevel)
pal.slaves <- colorFactor(palette = 'Set1', domain =md.geocoded$SlavOwnerText)
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


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Badin Bible Maryland Viewer"),
   
   # MainPanel row
   fluidRow(
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
        checkboxInput("slaves", "Confirmed Slave Owners Only", value = FALSE, width = NULL),
        checkboxInput("priests", "Priests Only", value = FALSE, width = NULL)
      ),
      mainPanel(leafletOutput("mymap"))
   )),#end MainPanel
   hr()
   
   # Output tables row <-develop further
   ,
   fluidRow(
     column(2, h5("Summary Chart: "))
   ),
   fluidRow(
     column(12,plotOutput("chartTest"))
   )
)

# Define server logic required to draw a histogram
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
    
# Reactive map output    
   output$mymap <- renderLeaflet({
     leaflet() %>%
       addProviderTiles(providers$Stamen.TonerLite,
                        options = providerTileOptions(noWrap = TRUE)
       ) %>%
       addCircleMarkers(data = points(),color = ~pal(LocationConfidenceLevel), popup = ~popupw) %>%
       addLegend("bottomleft",pal = pal,values=points()$LocationConfidenceLevel, opacity = 1)
   })
   output$chartTest<-renderPlot(
     hist(points()@data$CensusNumberOfHouseholdMembers)
   )

}
# pal <- colorFactor(palette = 'Set1', domain =md.geocoded$LocationConfidenceLevel)

# Run the application 
shinyApp(ui = ui, server = server)

# Update the application in Shinyapps.io
# library(rsconnect)
# deployApp()

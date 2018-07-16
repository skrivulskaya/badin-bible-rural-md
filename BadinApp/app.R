#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
md.geocoded <- read.csv("rural_md_geocoded.csv", stringsAsFactors = F)
md.spdf <-  SpatialPointsDataFrame(coords = md.geocoded[,c("lon","lat")], data = md.geocoded,
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
pal <- colorFactor(palette = 'Set1', domain =md.geocoded$LocationConfidenceLevel)
md.spdf$popupw <- paste(sep = "",  "<b>",md.spdf$SubscriberFirstName, " ", md.spdf$SubscriberLastName,"</b><br/>",
                         "Number Copies: ",md.spdf$SubscriberNoCopies, "<br/>",
                         "Under 16: ",ifelse(is.na(md.spdf$Under.16),"Unknown",md.spdf$Under.16), "<br/>",
                         "Over 16: ",ifelse(is.na(md.spdf$Over16),"Unknown",md.spdf$Over16), "<br/>",
                         "Females: ",ifelse(is.na(md.spdf$Females),"Unknown",md.spdf$Females), "<br/>",
                         "Slaves: ",ifelse(is.na(md.spdf$Slaves),"Unknown",md.spdf$Slaves), "<br/>",
                         "Notes: ",md.spdf$Notes,"<br/>"
) #end html popup


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Badin Bible Maryland Viewer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
        checkboxInput("slaves", "Slave Holders Only", value = FALSE, width = NULL)
      ),
      mainPanel(
         leafletOutput("mymap")
      )#end MainPanel
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    points <- eventReactive(c(input$slaves), {
      working.spdf <- md.spdf
      if (input$slaves){
        working.spdf <- working.spdf[which(!is.na(working.spdf$Slaves)),]
      }
      return(working.spdf)
      
    }, ignoreNULL = FALSE)
   output$mymap <- renderLeaflet({
     leaflet() %>%
       addProviderTiles(providers$Stamen.TonerLite,
                        options = providerTileOptions(noWrap = TRUE)
       ) %>%
       addCircleMarkers(data = points(),color = ~pal(LocationConfidenceLevel), popup = ~popupw) %>%
       addLegend("bottomleft",pal = pal,values=points()$LocationConfidenceLevel, opacity = 1)
   })
}
# pal <- colorFactor(palette = 'Set1', domain =md.geocoded$LocationConfidenceLevel)


# Run the application 
shinyApp(ui = ui, server = server)


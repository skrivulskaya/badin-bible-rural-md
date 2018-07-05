rm(list=ls(all=TRUE)) # clear memory

packages<- c("maptools","rgdal","leaflet","htmlwidgets","shiny","ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
setwd("/Users/suzannakrivulskaya/Box Sync/Research Assistant Work/Catholic Bibles Project 2018/badin-bible-rural-md")
latlong <- "+init=epsg:4326"

geocode.it <- FALSE

if (geocode.it){
  #NOTE: This is a fantastically stupid way of doing this. Someday I will change it to a function and make it pretty
  
  md.raw <- read.csv("rural_md.csv",stringsAsFactors = F)
  md.raw$Acc_Loc<-NA
  md.raw$Loc_Long<-NA
  md.raw$Loc_Lat<-NA
  for(i in 1:nrow(md.raw)){
    if (is.na(md.raw$Acc_Loc[i])){ #uses the accuracy variable as a check if it has been done
      result <- tryCatch({
        #Try this, if it fails because of something on Googles end, do the other stuff
        geocode(md.raw$PlottedLocation[i], output = "more", source = "google")
      },warning=function(w){#in case of a warning     
        result$lon <- NA
        result$lat <- NA
        result$loctype <- "FAILED"
        return(result)
      }, error = function(e) { #in case of an error     
        result$lon <- NA
        result$lat <- NA
        result$loctype <- "FAILED"
        return(result)
      }#end error
      
      )#end trycatch
      #Then give it all the values
      md.raw$Loc_Long[i] <- as.numeric(result$lon)
      md.raw$Loc_Lat[i] <- as.numeric(result$lat)
      md.raw$Acc_Loc[i] <- as.character(result$loctype)
    }#end of the if statement for origin geocoding
    
  }#end of the for loop
  if (!file.exists("rural_md_geocoded.csv")){
    write.csv(md.raw,"rural_md_geocoded.csv", row.names = F)
  }# end if statement in output
}else{ #else if already geocoded, just read the saved version
  md.raw <- read.csv("rural_md_geocoded.csv",stringsAsFactors = F)
} 


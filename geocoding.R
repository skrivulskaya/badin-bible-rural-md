rm(list=ls(all=TRUE)) # clear memory

packages<- c("maptools","rgdal","leaflet","htmlwidgets","shiny","ggmap") # list the packages that you'll need
lapply(packages, require, character.only=T) # load the packages, if they don't load you might need to install them first
# setwd("/Users/suzannakrivulskaya/Box Sync/Research Assistant Work/Catholic Bibles Project 2018/badin-bible-rural-md")
setwd("E:\\GIT_Checkouts\\R_Scripts\\badin-bible-rural-md")
latlong <- "+init=epsg:4326"

geocode.it <- TRUE

if (geocode.it){
  #NOTE: This is a slightly less stupid way of doing this. Someday I will change it to a function and make it pretty
  
  md.raw <- read.csv("rural_md.csv",stringsAsFactors = F)

  for(i in 1:nrow(md.raw)){
    result <- geocode(as.character(md.raw$PlottedLocation[i]), output = "latlona", source = "google")
    while(is.na(result[1])){ #checks if the latitude is NA and reruns if it is
      Sys.sleep(2) #Pauses for a minute to let the API Catch up
      result <- geocode(as.character(md.raw$PlottedLocation[i]), output = "latlona", source = "google")
    } 
    md.raw$lon[i] <- as.numeric(result[1])
    md.raw$lat[i] <- as.numeric(result[2])
    md.raw$geoAddress[i] <- as.character(result[3])
  }
  
  if (!file.exists("rural_md_geocoded.csv")){
    write.csv(md.raw,"rural_md_geocoded.csv", row.names = F)
  }# end if statement in output
  }else{ #else if already geocoded, just read the saved version
  md.raw <- read.csv("rural_md_geocoded.csv",stringsAsFactors = F)
} 


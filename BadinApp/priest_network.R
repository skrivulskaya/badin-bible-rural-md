# Building network visualizations using the visNetwork R package
# more info here: https://datastorm-open.github.io/visNetwork/
# and here: https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html
# run this code for adapting for Shiny:
# shiny::runApp(system.file("shiny", package = "visNetwork"))

#clear memory
rm(list=ls(all=TRUE))

#set working directory
setwd("/Users/suzannakrivulskaya/Box Sync/Research Assistant Work/Catholic Bibles Project 2018/badin-bible-rural-md-master/BadinApp")

#load required package
library(visNetwork)

#load data
clergy.net <- read.csv("priest_network.csv", header = T, as.is = T)

#generate html popup to use as node label
clergy.net$popupw <- paste(sep = "", "<b>",clergy.net$ShinyName,"</b><br/>",
                        "Years: ", ifelse(is.na(clergy.net$ShinyDates), "Unknown", clergy.net$ShinyDates),"<br/>",
                        "Bible copies: ", ifelse(is.na(clergy.net$SubscriberNoCopies), "Unknown", clergy.net$SubscriberNoCopies),"<br/>",
                        "Enslaved People: ",ifelse(is.na(clergy.net$NumberSlaves),"Unknown",clergy.net$NumberSlaves), "<br/>",
                        "Notes: ",ifelse(is.na(clergy.net$NetworkNote),"N/A",clergy.net$NetworkNote),"<br/>"
) #end html popup

require(visNetwork, quietly = TRUE)

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
  #define shapes and colors for the clergy
  visGroups(groupname = "Clergy", color = list(background = "lightgray",border="darkgray"), shape = "box", borderWidth = 1) %>% 
  # define shapes and colors for the lay subscribers
  visGroups(groupname = "Lay", shape = "box")

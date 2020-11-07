library(pacman)
p_load(reshape2,jsonlite,tidyverse, tm, topicmodels, parallel, tidytext, plotly, widyr,data.table)
#Parallize this process on 16 threads
cluster <- makeCluster(4)
#Export the jsonlite function stream_in to the cluster
clusterExport(cluster,list("stream_in"))
#Create an empty list for the dataframe for each file
import <- list()
#Run this function on every file in the ./import directory
import <- parLapply(cluster,list.files(path = "/home/ptrt/FrontPage-ndjson/"),function(file) {
  #jsonlite function to convert the ndjson file to a dataframe
  df1 <- stream_in(file(paste0("/home/ptrt/FrontPage-ndjson/",file)))
  #select which columns to keep
  df1$paper <- file
  return(df1)
})
#function called from the data.table library
df1 <- rbindlist(import)
#Now you can stop the cluster
stopCluster(cluster)

df1$text <- str_remove_all(df1$text, "[:punct:]")
df1$text <- str_replace_all(df1$text, "[:digit:]", "")

df1$date <- lubridate::ymd_hms(df1$date)

df1 %>% write_csv("data.csv")

df1 %>% saveRDS("data.rds")

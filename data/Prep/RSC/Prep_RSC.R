
rm(list= ls())

RSC <- read.csv("data/Prep/RSC/data_103.csv", sep = "\t", encoding = "UTF-8", 
                 na.strings = c("NA"), header = TRUE)


rm(list= ls())

OZ<- FD[,c("subject", "order", "item", "condition", "line", "wordnum", "word", "FF", "Gaze", "TotalTime", "freq", "Zipf",
           "word_len", "logFreq")]

colnames(OZ)<- c("sub", "seq", "item", "cond", "line", "word", "wordID", "FFD", "GD", "TVT",
                 "freq", "Zipf", "word_len", "logFreq")

save(OZ, file= "data/OZ.Rda")

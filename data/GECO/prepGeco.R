
rm(list= ls())

load("data/GECO/geco_raw.Rda")

new<- geco[,c("PP_NR", "PART", "TRIAL", "TRIAL_FIXATION_COUNT", "WORD_ID_WITHIN_TRIAL", "WORD_ID", "WORD", "WORD_FIXATION_COUNT",
              "WORD_FIRST_RUN_FIXATION_COUNT", "WORD_FIRST_FIXATION_DURATION", "WORD_GAZE_DURATION", "WORD_GO_PAST_TIME",
              "WORD_TOTAL_READING_TIME", "WORD_SKIP")]

rm(geco)

colnames(new)<- c("sub", "part", "item", "nfix", "word", "wordUnique", "wordID", "nfixAll", "nfix1", "FFD", "GD", "GPT",
                  "TVT", "skip")

geco<- new
rm(new)

geco$FFD<- as.numeric(geco$FFD)
geco$GD<- as.numeric(geco$GD)
geco$TVT<- as.numeric(geco$TVT)
geco$GPT<- as.numeric(geco$GPT)
geco$nfix1<- as.numeric(geco$nfix1)

# add SFD:

geco$SFD<- NULL
for(i in 1:nrow(geco)){
  
  if(!is.na(geco$FFD[i]) & !is.na(geco$GD[i])){
    if(geco$FFD[i]== geco$GD[i]){
      geco$SFD[i]<- geco$FFD[i] 
    }else{
      geco$SFD[i]<- NA
    }
    
  }else{
    geco$SFD[i]<- NA
  }
  
  print(i)
}

#geco$word_len<- nchar(geco$wordID)

save(geco, file= "data/geco.Rda")


# Martin R. Vasilev, 2019

rm(list= ls())

load("data/Provo/OSFdata.Rda")

# let's get of some unnecessary columns:
dat$RECORDING_SESSION_LABEL<- NULL
dat$Word_Cleaned<-NULL
dat$Total_Response_Count<- NULL
dat$Unique_Count<- NULL
dat$IsModalResponse<- NULL
dat$ModalResponse<- NULL
dat$ModalResponseCount<- NULL
dat$POSMatch<- NULL
dat$POSMatchModel<- NULL
dat$InflectionMatch<- NULL
dat$InflectionMatchModel<- NULL
dat$LSA_Context_Score<- NULL
dat$LSA_Response_Match_Score<- NULL
dat$IA_ID<- NULL
dat$IA_LABEL<- NULL
dat$IA_LEFT<- NULL
dat$IA_AREA<- NULL
dat$IA_RIGHT<- NULL
dat$IA_TOP<- NULL
dat$IA_BOTTOM<- NULL
dat$IA_FIRST_FIXATION_INDEX<- NULL
dat$IA_FIRST_FIXATION_VISITED_IA_COUNT<- NULL
dat$IA_FIRST_FIXATION_X<- NULL
dat$IA_FIRST_FIXATION_Y<- NULL
dat$IA_FIRST_FIX_PROGRESSIVE<- NULL
dat$IA_FIRST_FIXATION_TIME<- NULL
dat$IA_FIRST_FIXATION_RUN_INDEX<- NULL
dat$IA_FIRST_RUN_END_TIME<- NULL
dat$IA_FIRST_RUN_FIXATION_.<- NULL
dat$IA_FIRST_RUN_START_TIME<- NULL
dat$IA_FIRST_SACCADE_ANGLE<- NULL
dat$IA_FIRST_SACCADE_AMPLITUDE<- NULL
dat$IA_FIRST_SACCADE_END_TIME<- NULL
dat$IA_FIRST_SACCADE_START_TIME<- NULL
dat$IA_FIXATION_COUNT<- NULL
dat$IA_REGRESSION_IN<- NULL
dat$IA_REGRESSION_IN_COUNT<- NULL
dat$IA_RUN_COUNT<- NULL
dat$IA_REGRESSION_OUT<- NULL
dat$IA_REGRESSION_OUT_COUNT<- NULL
dat$IA_REGRESSION_OUT_FULL<- NULL
dat$IA_REGRESSION_OUT_FULL_COUNT<- NULL
#dat$Word_Unique_ID<- NULL


colnames(dat)<- c("sub", "unique_ID", "item", "word", "sent", "word_sent", "wordID", "word_length",                
                  "cloze", "cloze_model", "Certainty", "POS_CLAWS", "word_type", "Word_POS", "seq",
                  "FFD", 
                  "GD", "nfix1", "TVT", "skip", "GPT")

dat$Certainty<- NULL

# calculate SFD:
dat$SFD<- NULL
for(i in 1:nrow(dat)){
  
  if(!is.na(dat$FFD[i]) & !is.na(dat$GD[i])){
    if(dat$FFD[i]== dat$GD[i]){
      dat$SFD[i]<- dat$FFD[i] 
    }else{
      dat$SFD[i]<- NA
    }
    
  }else{
    dat$SFD[i]<- NA
  }
  

  # fix 0s in TVT while we're at it..
  if(!is.na(dat$TVT[i])){
    if(dat$TVT[i]==0){
      dat$TVT[i]<- NA
    }
    
  }

  print(i)
}


colnames(dat)
dat<- dat[, c("sub", "item", "seq",  "word", "sent", "word_sent", "wordID", "word_length",
               "FFD", "GD", "SFD", "TVT", "GPT", "skip", "nfix1", "cloze","cloze_model",
               "POS_CLAWS", "word_type", "Word_POS", "unique_ID")]

save(dat, file= "data/Provo.Rda")

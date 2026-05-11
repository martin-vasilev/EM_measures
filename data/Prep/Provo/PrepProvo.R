
# Martin R. Vasilev, 2019

rm(list= ls())

# load/ install preprocssing package:
if('EMreading' %in% rownames(installed.packages())==FALSE){
  if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
  install_github('martin-vasilev/EMreading')
}else{
  library(EMreading)
}



load("data/Prep/Provo/OSFdata.Rda")

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
#dat$IA_LEFT<- NULL
dat$IA_AREA<- NULL
#dat$IA_RIGHT<- NULL
dat$IA_TOP<- NULL
dat$IA_BOTTOM<- NULL
dat$IA_FIRST_FIXATION_INDEX<- NULL
dat$IA_FIRST_FIXATION_VISITED_IA_COUNT<- NULL
#dat$IA_FIRST_FIXATION_X<- NULL
dat$IA_FIRST_FIXATION_Y<- NULL
#dat$IA_FIRST_FIX_PROGRESSIVE<- NULL
dat$IA_FIRST_FIXATION_TIME<- NULL
dat$IA_FIRST_FIXATION_RUN_INDEX<- NULL
dat$IA_FIRST_RUN_END_TIME<- NULL
dat$IA_FIRST_RUN_FIXATION_.<- NULL
dat$IA_FIRST_RUN_START_TIME<- NULL
dat$IA_FIRST_SACCADE_ANGLE<- NULL
dat$IA_FIRST_SACCADE_AMPLITUDE<- NULL
dat$IA_FIRST_SACCADE_END_TIME<- NULL
dat$IA_FIRST_SACCADE_START_TIME<- NULL
#dat$IA_FIXATION_COUNT<- NULL
dat$IA_REGRESSION_IN<- NULL
dat$IA_REGRESSION_IN_COUNT<- NULL
dat$IA_RUN_COUNT<- NULL
dat$IA_REGRESSION_OUT<- NULL
dat$IA_REGRESSION_OUT_COUNT<- NULL
dat$IA_REGRESSION_OUT_FULL<- NULL
dat$IA_REGRESSION_OUT_FULL_COUNT<- NULL
#dat$Word_Unique_ID<- NULL


colnames(dat)<- c("sub", "unique_ID", "item", "word", "sent",
                  "word_sent", "wordID", "word_length",                
                  "cloze", "cloze_model", "Certainty", 
                  "POS_CLAWS", "word_type", "Word_POS", "seq",
                  "IA_LEFT", "IA_RIGHT", "FFD", "land_x", 'First_fix_progressive', 
                  "GD", "nfix1", "TVT", 'nfixAll', "skip_1st", "GPT")

dat$Certainty<- NULL

## remove cases where first fix progressive is 0 from first-pass measures:
dat$FFD[which(dat$First_fix_progressive==0)]<- NA
dat$GD[which(dat$First_fix_progressive==0)]<- NA
dat$nfix1[which(dat$First_fix_progressive==0)]<- 0
dat$nfix2<- dat$nfixAll - dat$nfix1

# calculate extra stuff:

library(tidyverse)

dat <- dat %>%
  mutate(
    SFD = if_else(
      !is.na(FFD) & !is.na(GD) & FFD == GD,
      FFD,
      NA_real_
    ),
    
    ppl = (IA_RIGHT - IA_LEFT) / word_length,
    land_pos = floor((land_x - IA_LEFT) / ppl) + 1,
    
    TVT = if_else(TVT == 0, NA_real_, TVT)
  ) %>%
  select(-ppl)
# dat$land_pos<- NA
# dat$SFD<- NULL
# 
# for(i in 1:nrow(dat)){
#   
#   if(!is.na(dat$FFD[i]) & !is.na(dat$GD[i])){
#     if(dat$FFD[i]== dat$GD[i]){
#       dat$SFD[i]<- dat$FFD[i] 
#     }else{
#       dat$SFD[i]<- NA
#     }
#     
#   }else{
#     dat$SFD[i]<- NA
#   }
#   
#   # landing position:
#   ppl= (dat$IA_RIGHT[i] - dat$IA_LEFT[i])/ dat$word_length[i] 
#   dat$land_pos[i]<- floor((dat$land_x[i] -dat$IA_LEFT[i])/ppl)+1
# 
#   # fix 0s in TVT while we're at it..
#   if(!is.na(dat$TVT[i])){
#     if(dat$TVT[i]==0){
#       dat$TVT[i]<- NA
#     }
#     
#   }
# 
#   #print(i)
# }

Provo= dat
colnames(dat)
# dat<- dat[, c("sub", "item", "seq",  "word", "sent", "word_sent", "wordID", "word_length",
#                "FFD", "GD", "SFD", "TVT", "GPT", "skip", "nfix1", "cloze","cloze_model",
#                "POS_CLAWS", "word_type", "Word_POS", "unique_ID")]
# 
## Add frequency:
Provo<- Frequency(Provo, database = "SUBTLEX-UK", PoS= T)


save(Provo, file= "data/Provo.Rda")
write.csv(Provo, "Data/Provo.csv")

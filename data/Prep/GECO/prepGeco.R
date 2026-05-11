
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


load("data/Prep/GECO/geco_raw.Rda")

new<- geco[,c("PP_NR", "PART", "TRIAL", "TRIAL_FIXATION_COUNT", "WORD_ID_WITHIN_TRIAL", "WORD_ID", "WORD", "WORD_FIXATION_COUNT",
              "WORD_FIRST_RUN_FIXATION_COUNT", "WORD_FIRST_FIXATION_DURATION", "WORD_GAZE_DURATION", "WORD_GO_PAST_TIME",
              "WORD_TOTAL_READING_TIME", "WORD_SKIP", 'WORD_FIRST_FIX_PROGRESSIVE')]

rm(geco)

colnames(new)<- c("sub", "part", "item", "nfix", "word", "wordUnique", "wordID", "nfixAll", "nfix1", "FFD", "GD", "GPT",
                  "TVT", "skip_1st", "FIRST_FIX_PROGRESSIVE")

geco<- new
rm(new)

geco$FFD<- as.numeric(geco$FFD)
geco$GD<- as.numeric(geco$GD)
geco$TVT<- as.numeric(geco$TVT)
geco$GPT<- as.numeric(geco$GPT)
geco$nfix1<- as.numeric(geco$nfix1)

### Fix first-pass measures when first fix progressive is 0:
geco$FFD[which(geco$FIRST_FIX_PROGRESSIVE==0)]<- NA
geco$GD[which(geco$FIRST_FIX_PROGRESSIVE==0)]<- NA
geco$nfix1[which(geco$FIRST_FIX_PROGRESSIVE==0)]<- 0
geco$nfix2<- geco$nfixAll - geco$nfix1

### remove few weird observations that are clearly wrong:
remove<- which(!is.element(geco$FIRST_FIX_PROGRESSIVE, c('0', '1', '.')))
geco<- geco[-remove,]

# add SFD:
geco$SFD <- ifelse(
  !is.na(geco$FFD) & !is.na(geco$GD) & geco$FFD == geco$GD,
  geco$FFD, NA)


# add word length information:
geco$wordID<- enc2native(geco$wordID)
geco$wordID <- iconv(geco$wordID, from = "", to = "UTF-8", sub = "")
geco$word_len <- nchar(geco$wordID)

# remove cases with empty "words":
geco<- geco[-which(geco$wordID==''),]

geco<- Frequency(geco, database = "SUBTLEX-UK", PoS = T)


save(geco, file= "data/geco.Rda")
write.csv(geco, "data/geco.csv")

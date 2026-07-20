
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

library(dplyr)
library(stringi)

fix_encoding <- function(x) {
  if (is.character(x)) {
    x <- iconv(x, from = "", to = "UTF-8", sub = "")
    x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE)
  }
  x
}

geco <- geco %>%
  mutate(across(where(is.character), fix_encoding))

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




### GET mono fixation report:

# library(data.table)
# fr <- fread(
#   "C:/Data/GECO/books/book1.csv",
#   sep = ",",
#   header = TRUE,
#   fill = TRUE,
#   quote = "\"",
#   encoding = "UTF-8",
#   data.table = FALSE
# )

### Book 1:

library(readr)
book1 <- read_csv("C:/Data/GECO/books/book1.csv")

book1 <- book1[, !grepl("^\\.\\.\\.", names(book1))]

# char_cols <- names(fr)[sapply(fr, is.character)]
# 
# for(col in char_cols){
#   
#   fr[[col]] <- iconv(
#     fr[[col]],
#     from = "Windows-1252",
#     to = "UTF-8"
#   )
#   
# }

cols2keep<- c("RECORDING_SESSION_LABEL",
              "TRIAL_INDEX","CURRENT_FIX_X" ,
              "CURRENT_FIX_Y", "CURRENT_FIX_DURATION",
              "CURRENT_FIX_INDEX",
              "CURRENT_FIX_INTEREST_AREA_ID",
              "CURRENT_FIX_INTEREST_AREA_LABEL" ,
              "DATA_FILE"#, "identifier", "zin" 
              ) 
book1<- book1[, cols2keep]

write.csv(x = book1, 
          file =  "data/Prep/GECO/raw_fix_book1.csv",
          row.names = F)
book1$book<- 1


### Book 2:

library(readr)
book2 <- read_csv("C:/Data/GECO/books/book2.csv")

book2 <- book2[, !grepl("^\\.\\.\\.", names(book2))]

book2<- book2[, cols2keep]

write.csv(x = book2, 
          file =  "data/Prep/GECO/raw_fix_book2.csv",
          row.names = F)
book2$book<- 2


### Book 3:

library(readr)
book3 <- read_csv("C:/Data/GECO/books/book3.csv")

book3 <- book3[, !grepl("^\\.\\.\\.", names(book3))]

book3<- book3[, cols2keep]

char_cols <- names(book3)[sapply(book3, is.character)]

for(col in char_cols){

  book3[[col]] <- iconv(
    book3[[col]],
    from = "Windows-1252",
    to = "UTF-8"
  )

}


write.csv(x = book3, 
          file =  "data/Prep/GECO/raw_fix_book3.csv",
          row.names = F)
book3$book<- 3



### Book 4:

library(readr)
book4 <- read_csv("C:/Data/GECO/books/book4.csv")

book4 <- book4[, !grepl("^\\.\\.\\.", names(book4))]

book4<- book4[, cols2keep]

char_cols <- names(book4)[sapply(book4, is.character)]

for(col in char_cols){
  
  book4[[col]] <- iconv(
    book4[[col]],
    from = "Windows-1252",
    to = "UTF-8"
  )
  
}


write.csv(x = book4, 
          file =  "data/Prep/GECO/raw_fix_book4.csv",
          row.names = F)
book4$book<- 4

allfix<- rbind(book1, book2, book3, book4)
allfix$wordUnique<- paste(allfix$book,
                          allfix$TRIAL_INDEX, 
                          allfix$CURRENT_FIX_INTEREST_AREA_ID,
                          sep= '-')

write.csv(allfix, file = 'data/Prep/GECO/raw_fix_combined.csv')



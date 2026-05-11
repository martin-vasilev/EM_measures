
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

##

# fix<- preprocFromDA1(data_dir = 'C:/Data/Oz', padding = 5)
# 
# load("data/Prep/Oz/Bold_data_raw.Rda")
# 
# OZ<- wordMeasures(raw_OZ)
# 
# OZ$RS<- NULL
# OZ$RS_type<- NULL
# OZ<- OZ[-which(OZ$blinks_1stPass==1 | OZ$blinks_2ndPass==1),]
# 
# OZ$blinks_1stPass<- NULL
# OZ$blinks_2ndPass<- NULL
# 
# OZ$wordID<- gsub("#####", "", OZ$wordID)
# OZ$word_length<- nchar(as.character(OZ$wordID))
 OZ<- Frequency(OZ, PoS = T)


save(OZ, file= "data/OZ.Rda")

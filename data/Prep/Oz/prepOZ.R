
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

load("data/Prep/Oz/Bold_data_raw.Rda")

OZ<- wordMeasures(raw_OZ)

OZ$RS<- NULL
OZ$RS_type<- NULL
OZ<- OZ[-which(OZ$blinks_1stPass==1 | OZ$blinks_2ndPass==1),]

OZ$blinks_1stPass<- NULL
OZ$blinks_2ndPass<- NULL


save(OZ, file= "data/OZ.Rda")


# load text font data:
load("data/visualisation_data/text_font/raw_fix.Rda")

library(EMreading)

#raw_fix<- raw_fix[which(raw_fix$blink==0 & raw_fix$prev_blink==0 & raw_fix$after_blink==0),]

nsubs<- unique(raw_fix$sub)

dat<- NULL

for(i in 1:length(nsubs)){
  a<- subset(raw_fix, sub== nsubs[i])
  
  nitems<- unique(a$item)
  
  for(j in 1:length(nitems)){
    b<- subset(a, item== nitems[j])
    
    b$regress<- NA
    
    max_word<- 1
    
    max_fixated<- max(b$word, na.rm=T)
    
    if(max_fixated<0){
      dat<- rbind(dat, b)
      next
    }
    
    terminated<- rep(0, max_fixated)
    
    for(k in 1:nrow(b)){
      
      if(!is.na(b$word[k])){
        if(b$word[k]>= max_word & terminated[b$word[k]]==0){
          b$regress[k]<- 0
        }else{
          b$regress[k]<- 1
        }
        # 
        # # check for cases where readers return to word before progressing:
        # if(b$word_num[k]== max_word){
        #  which(b[1:k,]$regress) 
        # }
        
        
        if(b$word[k]> max_word){
          max_word<- b$word[k]
          terminated[1:(b$word[k]-1)]<- 1
        }
        
        if(b$word[k]< max_word){
          terminated[max_word]<- 1
        }
        
      }
      
      
    }
    
    dat<- rbind(dat, b)
    
  } # end of item (j)
  
}  #end of subject (i)


# ### exclude outlier fixations
# library(tidyverse)
# 
# dat2 <- dat %>% filter(fix_dur>=80 & fix_dur<=1000 & blink==0)
# 
# library(EMreading)
# words<- wordMeasures(dat2)

refix_dat<- NULL
subs<- unique(dat$sub)

for(i in 1:length(subs)){
  n<- subset(dat, sub==  subs[i])
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    words<- unique(m$word)
    words<- words[which(!is.na(words))]
    
    for(k in 1:length(words)){
      o<- subset(m, word== words[k] & regress== 0)
      
      if(nrow(o)>0){
        o <- o[order(o$fix_num), ]
        o$refix_order<- 1:nrow(o)
        o$refix_num<- nrow(o)
        refix_dat<- rbind(refix_dat, o)
      }
      
      
    }
    
    
  }
  
  cat(i); cat(' ')
  
}

write.csv(refix_dat, file = 'data/Vasilev_2021_refixation_data.csv')


######### second-pass refixation data:

refix_dat2<- NULL
subs<- unique(dat$sub)

for(i in 1:length(subs)){
  n<- subset(dat, sub==  subs[i])
  nitems<- unique(n$item)
  
  for(j in 1:length(nitems)){
    m<- subset(n, item== nitems[j])
    
    words<- unique(m$word)
    words<- words[which(!is.na(words))]
    
    for(k in 1:length(words)){
      o<- subset(m, word== words[k])
      
      if(nrow(o)>0){
        o <- o[order(o$fix_num), ]
        o$fix_order<- 1:nrow(o)
        sec_pass<- which(o$regress==1)
        o$refix_num_2ndpass<- NA
        
        if(length(sec_pass)>0){
          o$refix_num_2ndpass[sec_pass]<- 1:length(which(o$regress==1))
        }
        
        ## find fixation number terminating first pass:
        which_first<- o[which(o$regress==0),]
        
        last_first<- NA
        
        if(length(which(which_first$regress==0))>0){
          last_first<- max(which_first$fix_num)
        }
        
        
        if(length(last_first)>0 & is.finite(last_first)){
          o$last_first_fix<- last_first
        }else{
          o$last_first_fix<- NA
        }
        
        o$diff_from_last_fix<- o$fix_num - o$last_first_fix
        
        
        #o$refix_num<- nrow(o)
        refix_dat2<- rbind(refix_dat2, o)
      }
      
      
    }
    
    
  }
  
  cat(i); cat(' ')
  
}

write.csv(refix_dat2, file = 'data/Vasilev_2021_refixation_data_2ndpass.csv')








df <- read_csv("data/Vasilev2021_word_data.csv")

df$word_length<- nchar(df$wordID)

library(EMreading)
df<- Frequency(df)

write.csv(df, file = "data/Vasilev2021_word_data.csv")



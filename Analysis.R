
# Martin R. Vasilev, 2019

rm(list= ls())

load("data/Provo.Rda") # Provo corpus
load("data/geco.Rda") # GECO corpus

### Check data:

Provo2<- subset(Provo, !is.na(FFD)) # get rid of NAs for FFD and GD (only remaining NAs for SFD)
Provo3<- subset(Provo2, !is.na(SFD)) # remove NAs for SFD as well

# check just to make sure:
table(is.na(Provo3$FFD))
table(is.na(Provo3$SFD))
table(is.na(Provo3$GD))
table(is.na(Provo3$GPT))

cor(x=Provo2$FFD, y=Provo2$GD) # 0.5950107
cor(x=Provo2$FFD, y=Provo2$TVT) # 0.4057805
cor(x=Provo2$FFD, y=Provo2$GPT) # 0.1609874

cor(x=Provo2$GD, y=Provo2$TVT) # 0.7042744
cor(x=Provo2$TVT, y=Provo2$GPT) # 0.4707271

library(lme4)
L1<- lmer(log(FFD)~ cloze_model +(cloze_model|sub)+ (1|item), data= Provo2)
summary(L1)





# Martin R. Vasilev, 2025

rm(list= ls())

load("data/Provo.Rda") # Provo corpus
load("data/geco.Rda") # GECO corpus
textFont <- read.csv("data/Vasilev2021_word_data.csv")
Oz<- read.csv("data/Oz_words.csv")


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F", "orange") # "Classic & trustworthy"

library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(ggpubr)

geco$word_length<- geco$word_len
geco.c<- subset(geco, nfixAll<100)

# combine all datasets
dat_all <- bind_rows(
  geco     %>% mutate(corpus = "GECO",     sub = as.character(sub)),
  Provo    %>% mutate(corpus = "Provo",    sub = as.character(sub)),
  Oz       %>% mutate(corpus = "Oz",       sub = as.character(sub)),
  textFont %>% mutate(corpus = "Text Font", sub = as.character(sub))
)


geco.c<- geco.c[,c(10, 17, 11, 12, 13)]

colnames(geco.c)<- c("FFD", "SFD", "GD", "GPT", "TFT")

r_corr<- cor(geco.c, use = 'pairwise.complete.obs', method = 'pearson', )

P1<-r_corr%>%ggcorrplot(method = 'square', type = 'upper',
                    title = 'GECO (Cop et al., 2017)', digits = 2, show.diag = F,
                    outline.color = 'black', lab = T)+
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 14))


Provo_c<- Provo[,c('FFD', 'SFD', 'GD', 'GPT', 'TVT')]
colnames(Provo_c)<- c("FFD", "SFD", "GD", "GPT", "TFT" )

r_corr2<- cor(Provo_c, use = 'pairwise.complete.obs', method = 'pearson')

P2<-r_corr2%>%ggcorrplot(method = 'square', type = 'upper',
                        title = 'Provo (Luke & Christianson, 2018)', digits = 2, show.diag = F,
                        outline.color = 'black', lab = T)+
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 14))

textFont_c<- textFont[, c('FFD', 'SFD', 'GD', 'GPT', 'TVT')]
colnames(textFont_c)<- c("FFD", "SFD", "GD", "GPT", "TFT" )
r_corr3<- cor(textFont_c, use = 'pairwise.complete.obs', method = 'pearson')

P3<-r_corr3%>%ggcorrplot(method = 'square', type = 'upper',
                         title = 'Text Font (Vasilev et al., 2021)', digits = 2, show.diag = F,
                         outline.color = 'black', lab = T)+
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 14))


Oz_c<- Oz[, c('FFD', 'SFD', 'GD', 'GPT', 'TVT')]
colnames(Oz_c)<- c("FFD", "SFD", "GD", "GPT", "TFT" )

r_corr4<- cor(Oz_c, use = 'pairwise.complete.obs', method = 'pearson')

P4<-r_corr4%>%ggcorrplot(method = 'square', type = 'upper',
                         title = 'Oz (Slattery & Vasilev, 2019)', digits = 2, show.diag = F,
                         outline.color = 'black', lab = T)+
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 14))



figure1 <- ggarrange(P1, P2, P3, P4, ncol = 2, nrow = 2,
                     common.legend = T)

ggsave(filename = 'Plots/Corr_plot.pdf', plot = figure1,
       width = 8, height = 8)

### First-pass fixation probability:

content <- c(
  "adjective",
  "adverb",
  "interjection",
  "name",
  "noun",
  "number",
  "verb"
)

function_words <- c(
  "conjunction",
  "determiner",
  "marker",
  "preposition",
  "pronoun"
)

df_fp<- dat_all%>%
 group_by(PoS)%>%
 #group_by(corpus, PoS)%>%
  mutate(fix_1st= ifelse(skip_1st==1, 0, 1))%>%
  summarise(M= mean(fix_1st, na.rm= T),
            N= n_distinct(wordID), #n(), #length(unique(sub)),
            SD= sd(fix_1st, na.rm= T),
            SE= SD/sqrt(N),
            CI_lower= M- 1.96*SE,
            CI_upper= M+1.96*SE)%>%
            mutate(
            word_type = case_when(
              PoS %in% content ~ "content",
              PoS %in% function_words ~ "function",
              TRUE ~ "other"
            )
  )

df_fp2 <- dat_all %>%
  mutate(
    fix_2nd = case_when(
      
      # skipped first pass, never fixated later
      skip_1st == 1 & nfixAll == 0 ~ 0,
      
      # skipped first pass, but fixated later
      skip_1st == 1 & nfixAll > 0 ~ 1,
      
      # first-pass fixated, then reread
      skip_1st == 0 & TVT > GD ~ 1,
      
      # first-pass fixated, no rereading
      skip_1st == 0 & TVT == GD ~ 0
    ),
    
    fixated = ifelse(skip_1st == 0, "Yes", "No"),
    fix_1st= ifelse(skip_1st==1, 0, 1)
  ) %>%
  group_by(PoS, fixated) %>%
  summarise(
    M = mean(fix_2nd, na.rm = TRUE),
    N= n_distinct(wordID), #n(), #length(unique(sub)),
    SD= sd(fix_2nd, na.rm= T),
    SE= SD/sqrt(N),
    CI_lower= M- 1.96*SE,
    CI_upper= M+1.96*SE)%>%
  mutate(
    word_type = case_when(
      PoS %in% content ~ "content",
      PoS %in% function_words ~ "function",
      TRUE ~ "other"
    ))

df_fp2

pos_order <- df_fp %>%
  filter(!is.na(PoS),
         PoS != "NA",
         PoS != "",
         PoS != "unclassified") %>%
  arrange(M) %>%
  pull(PoS)

df_fp$PoS  <- factor(df_fp$PoS, levels = pos_order)
df_fp2$PoS <- factor(df_fp2$PoS, levels = pos_order)


library(ggplot2)
library(dplyr)

PoS1<- df_fp %>%
  filter(!is.na(PoS), PoS != "NA", PoS!= '', PoS!= 'unclassified') %>%
  mutate(
    #PoS = reorder(PoS, M, mean)
  ) %>%
  ggplot(aes(x = M, y = PoS, colour = word_type,
             xmin= CI_lower, xmax= CI_upper)) +
  #xlim(-0.2, 1.2)+
  xlim(-0.31, 1)+
  geom_point(size = 2)+
 #            position = position_dodge(width = 0.5)) +
  geom_errorbar(
    width = 0)+
  #   position = position_dodge(width = 0.5), linetype=1
  # )+
  theme_classic(base_size = 22) +
  scale_colour_manual(values = pallete1[c(1,2,5,6)])+
  scale_fill_manual(values = pallete1[c(1,2,5,6)])+
  labs(
    x = "Mean first-pass fixation probability",
    y = "Part of speech",
    colour = "Word type"
  )+
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_rect(
          colour = "black",
          fill = "white",
          linewidth = 0.3
        ))

## second-pass fixation probability based on first-pass fixation probability:

PoS2<- df_fp2 %>%
  filter(!is.na(fixated),!is.na(PoS), PoS != "NA", PoS!= '', PoS!= 'unclassified') %>%
  mutate(
 #   PoS = reorder(PoS, M, mean)
  ) %>%
  ggplot(aes(x = M, y = PoS, colour = fixated,
             xmin= CI_lower, xmax= CI_upper)) +
  #xlim(-0.2, 1.2)+
  xlim(-0.31, 1)+
  geom_point(size = 2,
              position = position_dodge(width = 0.5)) +
  geom_errorbar(
    width = 0,
    position = position_dodge(width = 0.5), linetype=1
  )+
  theme_classic(base_size = 22) +
  scale_colour_manual(values = pallete1[c(5,6)])+
  scale_fill_manual(values = pallete1[c(5,6)])+
 # facet_wrap(~word_type)+
  labs(
    x = "Mean second-pass fixation probability",
    y = "Part of speech",
    colour = "Fixated (first-pass)"
  )+
  theme(legend.position = c(0.75, 0.15),
        legend.background = element_rect(
          colour = "black",
          fill = "white",
          linewidth = 0.3
        ))

library(patchwork)

PoS_all<- PoS1+ PoS2

ggsave(filename = 'Plots/Pos.pdf', plot = PoS_all,
       width= 16, height= 10)

### First-pass refixation probability:

geco$refix_1st<- ifelse(geco$GD!= geco$FFD, 1, 0)
Provo$refix_1st<- ifelse(Provo$GD!= Provo$FFD, 1, 0)
textFont$refix_1st<- ifelse(textFont$GD!= textFont$FFD, 1, 0)
Oz$refix_1st<- ifelse(Oz$GD!= Oz$FFD, 1, 0)









## refixation probability:

# GECO
round(mean(geco$refix_1st, na.rm=T),2)
round(sd(geco$refix_1st, na.rm=T),2)

# Provo:
round(mean(Provo$refix_1st, na.rm=T),2)
round(sd(Provo$refix_1st, na.rm=T),2)

# Provo:
round(mean(textFont$refix_1st, na.rm=T),2)
round(sd(textFont$refix_1st, na.rm=T),2)

# Oz:
round(mean(Oz$refix_1st, na.rm=T),2)
round(sd(Oz$refix_1st, na.rm=T),2)


#### FFD (proportion of word fixation time):

# Geco:
geco$prop_time_FFD<- geco$FFD/ geco$GD
geco$prop_time_SFD<- geco$SFD/ geco$GD
round(mean(geco$prop_time_FFD, na.rm=T),2)
round(mean(geco$prop_time_SFD, na.rm=T),2)
round(sd(geco$prop_time_FFD, na.rm=T),2)

# Provo:
Provo$prop_time_FFD<- Provo$FFD/ Provo$GD
round(mean(Provo$prop_time_FFD, na.rm=T),2)
round(sd(Provo$prop_time_FFD, na.rm=T),2)

# TextFont:
textFont$prop_time_FFD<- textFont$FFD/ textFont$GD
round(mean(textFont$prop_time_FFD, na.rm=T),2)
round(sd(textFont$prop_time_FFD, na.rm=T),2)

# Oz:
Oz$prop_time_FFD<- Oz$FFD/ Oz$GD
round(mean(Oz$prop_time_FFD, na.rm=T),2)
round(sd(Oz$prop_time_FFD, na.rm=T),2)


Provo$prop_time_FFD<- Provo$FFD/ Provo$GD

mean(Provo$prop_time_FFD, na.rm=T)
sd(Provo$prop_time_FFD, na.rm=T)

mean(geco$prop_time_FFD, na.rm= T)
sd(geco$prop_time_FFD, na.rm= T)

mean(Provo$refix_1st, na.rm=T)
sd(Provo$refix_1st, na.rm=T)

pRF1<- Provo %>% 
  group_by(word_length) %>%
  summarise(M= mean(refix_1st, na.rm = T), 
            SD= sd(refix_1st, na.rm = T),
            N= length(unique(sub)))%>%
  filter(!is.na(word_length))%>%
  mutate(SE= SD/sqrt(N),
         upper= M+ SE, lower= M-SE)%>%
  ggplot(aes(x= word_length, y = M, ymin= lower, ymax= upper))+
  geom_line(color= pallete1[1])+
  ylim(0, 1)+
  geom_ribbon(fill= pallete1[1], alpha= .2)+
  labs(x= "Word length\n(in characters)",
       y= "First-pass refixation probability")+
  theme_classic(20)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

RF2<- Provo %>% 
  mutate(zipf_r= round(zipf,1))%>%
  group_by(zipf_r) %>%
  summarise(M= mean(refix_1st, na.rm = T), 
            SD= sd(refix_1st, na.rm = T),
            N= length(unique(sub)))%>%
  filter(!is.na(zipf_r))%>%
  mutate(SE= SD/sqrt(N),
         upper= M+ SE, lower= M-SE)%>%
  ggplot(aes(x= zipf_r, y = M, ymin= lower, ymax= upper))+
  geom_line(color= pallete1[2])+
  ylim(0, 1)+
  geom_ribbon(fill= pallete1[2], alpha= .2)+
  labs(x= "Word frequency (Zipf)\n",
       y= "First-pass refixation probability")+
  theme_classic(20)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

RF3<- Provo %>% 
  mutate(cloze_model= round(cloze_model,1))%>%
  group_by(cloze_model) %>%
  summarise(M= mean(refix_1st, na.rm = T), 
            SD= sd(refix_1st, na.rm = T),
            N= length(unique(sub)))%>%
  filter(!is.na(cloze_model))%>%
  mutate(SE= SD/sqrt(N),
         upper= M+ SE, lower= M-SE)%>%
  ggplot(aes(x= cloze_model, y = M, ymin= lower, ymax= upper))+
  geom_line(color= pallete1[3])+
  ylim(0, 1)+
  geom_ribbon(fill= pallete1[3], alpha= .2)+
  labs(x= "Word predictability\n",
       y= "First-pass refixation probability")+
  theme_classic(20)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

# by landing position:
RF4<- Provo %>% 
  mutate(land_c= land_pos - (word_length/2))%>%
  filter(abs(land_c)< 6)%>%
  group_by(land_c) %>%
  summarise(M= mean(refix_1st, na.rm = T), 
            SD= sd(refix_1st, na.rm = T),
            N= length(unique(sub)))%>%
  mutate(SE= SD/sqrt(N),
  upper= M+ SE, lower= M-SE)%>%
  ggplot(aes(x= land_c, y = M, ymin= lower, ymax= upper))+
  geom_line(color= pallete1[5])+
  ylim(0, 1)+
  geom_ribbon(fill= pallete1[5], alpha= .2)+
  labs(x= "Landing position\n(relative to word centre)",
       y= "First-pass refixation probability")+
  theme_classic(20)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))




library(ggpubr)

# Combine
figureRF <- ggarrange(pRF1, RF2, RF3, RF4, ncol = 4)

# Save using ggsave() – this should work if figureSA is a ggpubr object
ggsave("Plots/refixation_prob.pdf", 
       plot = figureRF, width = 16, height = 8)



###### Analyse durations of first-pass re-fixations:

refix_dat <- read.csv("data/Vasilev_2021_refixation_data.csv")

library(tidyverse)
refix_dat%>%
  group_by(refix_num, refix_order) %>%
  filter(fix_dur>=80 & fix_dur<= 1000)%>%
  summarise(M= mean(fix_dur), n= n(),
            sd= sd(fix_dur))

(length(which(refix_dat$refix_num>=5))/nrow(refix_dat))*100


refix_dat<- subset(refix_dat, refix_num<5)

refix_dat %>% count(refix_num)%>%mutate(prop= (n/ sum(n))*100)

refix_dat$refix_num<- as.character(refix_dat$refix_num)
refix_dat$refix_num[which(refix_dat$refix_num=="1")]<- "0 refixations (79.5%)"
refix_dat$refix_num[which(refix_dat$refix_num=="2")]<- "1 refixation (17.9%)"
refix_dat$refix_num[which(refix_dat$refix_num=="3")]<- "2 refixations (2.1%)"
refix_dat$refix_num[which(refix_dat$refix_num=="4")]<- "3 refixations (0.45%)"
refix_dat$refix_num[which(refix_dat$refix_num=="5")]<- "4 refixations"
refix_dat$refix_num[which(refix_dat$refix_num=="6")]<- "5 refixations"


Refix_dur<- refix_dat%>%
  filter(refix_order<7 )%>%
  filter(fix_dur>=80 & fix_dur<= 1000)%>%
  mutate(refix_order= as.factor(refix_order))%>%
  ggplot(aes(x= refix_order, y= fix_dur,
             group= refix_order, color= refix_order,
             fill= refix_order))+
  geom_boxplot(alpha= .4)+
  facet_wrap(~refix_num, ncol=4)+
  scale_color_manual(values= pallete1[2:5])+
  theme_bw(20)+
  scale_x_discrete(breaks = c("1","2","3","4","5","6"), 
                     labels = c("1st","2nd","3rd","4th","5th","6th"))+
  labs(x= 'Fixation number in the first-pass sequence', y= "Fixation duration (in ms)")+
  theme(legend.position = 'none')


# Combine

figureRF_titled <- annotate_figure(
  figureRF,
  top = text_grob("\na) First-pass refixation probability (Luke & Christianson, 2018)",
                  face = "bold",
                  size = 24,
                  vjust = 0.3,       # moves title up/down
                  hjust = -0.1,
                  x=0)     # centered
)

Refix_dur_titled <- annotate_figure(
  Refix_dur,
  top = text_grob("\n\nb) First-pass refixation durations (Vasilev et al., 2021)",
                  face = "bold",
                  size = 24,
                  vjust = 0.3,
                  hjust = -0.1,
                  x=0)
)

figureRF_final <- ggarrange(figureRF_titled,
                            Refix_dur_titled, nrow=2) 

ggsave("Plots/refixation_prob_combined.pdf", 
       plot = figureRF_final, width = 16, height = 14)



### Second-pass refixation probability

geco$refix_2nd<- ifelse(geco$TVT> geco$GD, 1, 0)
Provo$refix_2nd<- ifelse(Provo$TVT> Provo$GD, 1, 0)
textFont$refix_2nd<- ifelse(textFont$TVT> textFont$GD, 1, 0)
Oz$refix_2nd<- ifelse(Oz$TVT> Oz$GD, 1, 0)


# GECO
round(mean(geco$refix_2nd, na.rm=T),2)
round(sd(geco$refix_2nd, na.rm=T),2)

# Provo:
round(mean(Provo$refix_2nd, na.rm=T),2)
round(sd(Provo$refix_2nd, na.rm=T),2)

# Text font:
round(mean(textFont$refix_2nd, na.rm=T),2)
round(sd(textFont$refix_2nd, na.rm=T),2)

# Oz:
round(mean(Oz$refix_2nd, na.rm=T),2)
round(sd(Oz$refix_2nd, na.rm=T),2)


RF_2nd<- Provo %>% 
  group_by(word_length) %>%
  summarise(M= mean(refix_2nd, na.rm = T), 
            SD= sd(refix_2nd, na.rm = T),
            N= length(unique(sub)))%>%
  mutate(SE= SD/sqrt(N),
         upper= M+ SE, lower= M-SE)%>%
  ggplot(aes(x= word_length, y = M, ymin= lower, ymax= upper))+
  geom_line(color= pallete1[1])+
  ylim(0, 1)+
  geom_ribbon(fill= pallete1[1], alpha= .2)+
  labs(x= "Word length\n(in characters)",
       y= "Second-pass refixation probability")+
  theme_classic(20)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


RF_2nd_F<- Provo %>% 
  mutate(zipf_r= round(zipf,1))%>%
  group_by(zipf_r) %>%
  summarise(M= mean(refix_2nd, na.rm = T), 
            SD= sd(refix_2nd, na.rm = T),
            N= length(unique(sub)))%>%
  mutate(SE= SD/sqrt(N),
         upper= M+ SE, lower= M-SE)%>%
  ggplot(aes(x= zipf_r, y = M, ymin= lower, ymax= upper))+
  geom_line(color= pallete1[2])+
  ylim(0, 1)+
  geom_ribbon(fill= pallete1[2], alpha= .2)+
  labs(x= "Word frequency (Zipf)\n",
       y= "Second-pass refixation probability")+
  theme_classic(20)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))


RF_2nd_C<- Provo %>% 
  mutate(cloze_model= round(cloze_model,1))%>%
  group_by(cloze_model) %>%
  summarise(M= mean(refix_2nd, na.rm = T), 
            SD= sd(refix_2nd, na.rm = T),
            N= length(unique(sub)))%>%
  mutate(SE= SD/sqrt(N),
         upper= M+ SE, lower= M-SE)%>%
  ggplot(aes(x= cloze_model, y = M, ymin= lower, ymax= upper))+
  geom_line(color= pallete1[3])+
  ylim(0, 1)+
  geom_ribbon(fill= pallete1[3], alpha= .2)+
  labs(x= "Word predictability (Zipf)\n",
       y= "Second-pass refixation probability")+
  theme_classic(20)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))



RF_2nd_L<- Provo %>% 
  mutate(land_c= land_pos - (word_length/2))%>%
  filter(abs(land_c)< 6)%>%
  group_by(land_c) %>%
  summarise(M= mean(refix_2nd, na.rm = T), 
            SD= sd(refix_2nd, na.rm = T),
            N= length(unique(sub)))%>%
  mutate(SE= SD/sqrt(N),
         upper= M+ SE, lower= M-SE)%>%
  ggplot(aes(x= land_c, y = M, ymin= lower, ymax= upper))+
  geom_line(color= pallete1[5])+
  ylim(0, 1)+
  geom_ribbon(fill= pallete1[5], alpha= .2)+
  labs(x= "Initial landing position\n(relative to word centre)",
       y= "Second-pass refixation probability")+
  theme_classic(20)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))

library(ggpubr)

# Combine
figureRF_2nd <- ggarrange(RF_2nd, RF_2nd_F, RF_2nd_C,
                          RF_2nd_L, ncol = 4, align = "v")

# Save using ggsave() – this should work if figureSA is a ggpubr object
ggsave("Plots/refixation_prob_2nd.pdf", 
       plot = figureRF_2nd, width = 16, height = 8)



### Show differences between FFD distributions (single vs refixated):

geco %>% 
  mutate(refix_1st= as.factor(refix_1st))%>%
  filter(FFD<=1000)%>%
  ggplot(aes(x= FFD, fill= refix_1st,
             colour=refix_1st, group= refix_1st))+
  geom_density(alpha=.2)+
  theme_bw(18)


# geco %>% group_by(refix_1st)%>%
#   summarise (M= mean(FFD, na.rm=T))


Provo %>% 
  mutate(refix_1st= as.factor(refix_1st))%>%
  filter(FFD<=1000)%>%
  ggplot(aes(x= FFD, fill= refix_1st,
             colour=refix_1st, group= refix_1st))+
  geom_density(alpha=.2)+
  theme_bw(18)

#Provo %>% group_by(refix_1st)%>%
#  summarise (M= mean(FFD, na.rm=T))



Oz %>% 
  mutate(refix_1st= as.factor(refix_1st))%>%
  filter(FFD<=1000)%>%
  ggplot(aes(x= FFD, fill= refix_1st,
             colour=refix_1st, group= refix_1st))+
  geom_density(alpha=.2)+
  theme_bw(18)

#Oz %>% group_by(refix_1st)%>%
#  summarise (M= mean(FFD, na.rm=T))


textFont %>% 
  mutate(refix_1st= as.factor(refix_1st))%>%
  filter(FFD<=1000)%>%
  ggplot(aes(x= FFD, fill= refix_1st,
             colour=refix_1st, group= refix_1st))+
  geom_density(alpha=.2)+
  theme_bw(18)

#textFont %>% group_by(refix_1st)%>%
#  summarise(M= mean(FFD, na.rm=T))

library(dplyr)
library(ggplot2)

means_df <- dat_all %>%
  mutate(refix_1st = as.factor(refix_1st),
         corpus= as.factor(corpus)) %>%
  mutate(corpus= fct_relevel(corpus, 'GECO', 
                             'Provo', 'Text Font', 'Oz'))%>%
  filter(FFD <= 1000) %>%
  group_by(corpus, refix_1st) %>%
  summarise(mean_FFD = mean(FFD, na.rm = TRUE), .groups = "drop") %>%
  group_by(corpus) %>%
  mutate(
    # left for group 1, right for group 2 (extend if >2 groups)
    x_nudge = ifelse(as.numeric(refix_1st) == 1, 125, -125)
  ) %>%
  ungroup()

SFD_dist<- dat_all %>%
  mutate(refix_1st = as.factor(refix_1st),
         corpus= as.factor(corpus)) %>%
  mutate(corpus= fct_relevel(corpus, 'GECO', 
                             'Provo', 'Text Font', 'Oz'))%>%
  filter(FFD <= 1000) %>%
  ggplot(aes(FFD, fill = refix_1st, colour = refix_1st)) +
  geom_density(alpha = .2) +
  ggtitle('a)')+
  geom_vline(data = means_df,
             aes(xintercept = mean_FFD, colour = refix_1st),
             linetype = "dashed",
             linewidth = 0.8) +
  geom_text(data = means_df,
            aes(x = mean_FFD + x_nudge,
                y = Inf,
                label = paste0("M= ", round(mean_FFD, 0)),
                colour = refix_1st),
            vjust = 1.6,
            size = 5,
            show.legend = FALSE) +
  facet_wrap(~ corpus, ncol = 2) +
 # coord_cartesian(clip = "off") +
  theme_bw(24)+
  scale_fill_manual(
    name = "1st-pass refixation",
    values = c("0" = pallete1[2], "1" = pallete1[1]),
    labels = c("0" = "No", "1" = "Yes")
  ) +
  scale_colour_manual(
    name = "1st-pass refixation",
    values = c("0" = pallete1[2], "1" = pallete1[1]),
    labels = c("0" = "No", "1" = "Yes")
  )+theme(
    legend.position = 'top',
    plot.margin = margin(10, 50, 10, 10)
  )+ylab('Probability density')+
  ylim(0, 0.008)


## "Missing" time in SFD, relative to FFD:

dat_mt <- dat_all %>%
  filter(!is.na(FFD), !is.na(refix_1st), !is.na(freq)) %>%
  mutate(
    missing_rel_ffd = ifelse(refix_1st == 1, FFD, 0),
    zlen  = scale(word_length)%>%as.numeric(),
    zfreq = scale(zipf)%>%as.numeric()
  )

dat_mt %>% 
  group_by(corpus)%>%
  summarise(M= mean(missing_rel_ffd),
            sd= sd(missing_rel_ffd))

library(lme4)

# make subject IDs unique across datasets:
textFont$sub<- paste('TF', as.character(textFont$sub), sep= '')
Oz$sub<- paste('Oz', as.character(Oz$sub), sep= '')


m_missing <- lmer(
  missing_rel_ffd ~ word_length + zipf + (1 | sub) + (1 | wordID),
  data = dat_mt,
  REML = FALSE
)

summary(m_missing)

library(ggplot2)

SFD_length<- dat_mt %>%
  mutate(length_capped = pmin(word_length, 15)) %>%
  ggplot(aes(x = length_capped, y = missing_rel_ffd,
             colour = corpus, fill= corpus)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE,
              level= 0.95) +
  labs(
    x = "Word length (char.)",
    y = '"Missing" time in SFD (ms)'
  ) +
  ggtitle('b)')+
 # ylim(0,160)+
  scale_colour_manual(values = pallete1[c(1,3,5,6)])+
  scale_fill_manual(values = pallete1[c(1,3,5,6)])+
  theme_bw(24)+scale_x_continuous(
    breaks = seq(1, 15, 2),
    labels = c(as.character(seq(1, 14, 2)), "15+")
  )+
  coord_cartesian(ylim = c(0, 160))


SFD_freq<- dat_mt %>%
  ggplot(aes(x = zipf, y = missing_rel_ffd,
             colour = corpus, fill= corpus)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, 
              level= 0.95) +
  labs(
    x = "Word frequency (zipf)",
    y = NULL
  ) +
  ggtitle('')+
 # ylim(0,160)+
  theme_bw(24)+
  scale_colour_manual(values = pallete1[c(1,3,5,6)])+
  scale_fill_manual(values = pallete1[c(1,3,5,6)])+
  scale_x_continuous(n.breaks = 8)+
  coord_cartesian(ylim = c(0, 160))

figureSFD_time <- ggarrange(SFD_length,
                            SFD_freq, nrow=1, 
                            common.legend = T,
                            align = "hv") 

#Figure_SFD<- ggarrange(SFD_dist, figureSFD_time, nrow=2, heights = c(1.5,1))


# ggsave("Plots/SFD_combined.pdf", 
#        plot = Figure_SFD, width = 10, height = 12)


# ### models:
# 
# m_sfd <- lmer(
#   SFD ~ zlen + zfreq + (1 | sub) + (1 | wordID),
#   data = dat_mt,
#   REML = FALSE
# )
# 
# summary(m_sfd)
# 
# m_ffd <- lmer(
#   FFD ~ zlen + zfreq + (1 | sub) + (1 | wordID),
#   data = dat_mt,
#   REML = FALSE
# )
# 
# summary(m_ffd)
# 

#### Compare slopes for FFD and SFD:

library(dplyr)
library(tidyr)
library(lme4)

# Create long data with a measure factor (FFD vs SFD)
dat_long <- dat_all %>%
  filter(!is.na(FFD)) %>%
  select(sub, wordID, corpus, refix_1st, word_length, zipf, FFD, SFD) %>%
  pivot_longer(c(FFD, SFD), names_to = "measure", values_to = "time") %>%
  filter(!is.na(time)) %>%
  mutate(length_capped = pmin(word_length, 15)) %>%
  mutate(measure = factor(measure, levels = c("FFD", "SFD")))

# Model: slope differs by measure
library(lme4)

contrasts(dat_long$measure)

m1_length <- lmer(
  time ~ measure * length_capped + (1  | sub) + (1 | wordID),
  data = dat_long
)
summary(m1_length)

m1_freq <- lmer(
  time ~ measure * zipf + (1 | sub) + (1 | wordID),
  data = dat_long
)
summary(m1_freq)

# b <- fixef(m1_length)
# V <- vcov(m1_length)
# 
# diff_term <- "measureSFD:word_length"
# 
# diff_est <- b[[diff_term]]
# diff_se  <- sqrt(V[diff_term, diff_term])
# 
# diff_df <- tibble::tibble(
#   contrast = "SFD − FFD",
#   diff = diff_est,
#   lo = diff_est - 1.96 * diff_se,
#   hi = diff_est + 1.96 * diff_se
# )
# 
# ggplot(diff_df, aes(x = contrast, y = diff)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.1) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   labs(x = NULL, y = "Difference in slopes (ms/letter)") +
#   theme_bw(18)

y_lim <- c(180, 260)
y_breaks <- seq(180, 260, 20)

library(ggeffects)
SFD_length_diff<- plot(ggpredict(model = m1_length,
               terms = c('length_capped', 'measure')))+
  scale_x_continuous(
    breaks = seq(1, 15, 2),
    labels = c(as.character(seq(1, 14, 2)), "15+")
  )+
  coord_cartesian(ylim = y_lim) +
  scale_y_continuous(breaks = y_breaks) +
  theme_bw(24)+
  labs(title= 'c)',
       y= 'Fixation duration (in ms)',
       x= 'Word length (char.)')

SFD_length_freq<- plot(ggpredict(model = m1_freq,
               terms = c('zipf', 'measure')))+
  theme_bw(24)+
  coord_cartesian(ylim = y_lim) +
  scale_y_continuous(breaks = y_breaks) +
  scale_x_continuous(n.breaks = 8)+
  labs(title= '',
       y= 'Fixation duration (in ms)',
       x= 'Word frequency (zipf)')


figureSFDvsFFD <- ggarrange(SFD_length_diff,
                            SFD_length_freq, nrow=1, 
                            common.legend = T,
                            align = "hv") 

Figure_SFD2<- ggarrange(figureSFD_time, figureSFDvsFFD, nrow=2)

Figure_SFD3<- ggarrange(SFD_dist, Figure_SFD2, nrow=1)


ggsave("Plots/SFD_combined.pdf", 
       plot = Figure_SFD3, width = 20, height = 10)


### Refixation duration - GD:

### create "stacked" bar plot with FFD, refixation duration and
# regression time on words:

dat_all <- dat_all %>%
  mutate(
    FFD_time        = FFD,
    Refix_time      = GD - FFD,
    Regression_time = TVT - GD
  )

dat_all %>%
#  group_by(corpus) %>%
  summarise(
    Refix_time_M      = mean(Refix_time, na.rm = TRUE),
    Refix_time_SD   = sd(Refix_time, na.rm = TRUE)  
  )

dat_long_time <- dat_all %>%
  group_by(corpus) %>%
  summarise(
    FFD_time        = mean(FFD_time, na.rm = TRUE),
    Refix_time      = mean(Refix_time, na.rm = TRUE),
    Regression_time = mean(Regression_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(FFD_time, Refix_time, Regression_time),
    names_to = "Component",
    values_to = "Time"
  ) %>%
  mutate(
    Component = factor(
      Component,
      levels = c("Regression_time", "Refix_time", "FFD_time")
    )
  )

fixation_comps<- ggplot(dat_long_time, aes(x = corpus, y = Time, fill = Component)) +
  geom_col(width = 0.7) +
  labs(
    x = "Corpus",
    y = "Mean total fixation\nduration per word",
    fill = "Reading time component",
    title= 'a)'
  ) +
  scale_fill_manual(
    values = c(
      "FFD_time"        = pallete1[2],
      "Refix_time"      = pallete1[3],
      "Regression_time" = pallete1[4]
    ),
    labels = c(
      "FFD_time"        = "First fixation duration (FFD)",
      "Refix_time"      = "Refixation duration (GD - FFD)",
      "Regression_time" = "Regression duration (TFD - GD)"
    )
  ) +
  theme_bw(24)+
  theme(legend.position = 'right',
        legend.direction = "vertical")

refix_time<- dat_all%>% filter(Refix_time>0)
cor(refix_time$FFD, refix_time$Refix_time)

ylims <- c(0, 320)

Refix_length<- dat_all%>% 
  mutate(length_capped = pmin(word_length, 15)) %>%
  ggplot(aes(x= length_capped, y = Refix_time,
             colour= corpus, fill=corpus))+
  geom_smooth(method = 'gam')+
  scale_colour_manual(values = pallete1[c(1,3,5,6)])+
  scale_fill_manual(values = pallete1[c(1,3,5,6)])+
  theme_bw(24)+scale_x_continuous(
    breaks = seq(1, 15, 2),
    labels = c(as.character(seq(1, 14, 2)), "15+")
  )+
  coord_cartesian(ylim = ylims)+
#  ylim(0, 160)+
  labs(
    x = "Word length (char.)",
    y = 'Refixation duration\n(GD - FFD)'
  )+
  ggtitle('c)')+
  theme(legend.position = 'bottom')
  

Refix_freq<- dat_all%>% 
  ggplot(aes(x= zipf, y = Refix_time, colour= corpus, fill= corpus))+
  geom_smooth(method = 'gam')+
  scale_colour_manual(values = pallete1[c(1,3,5,6)])+
  scale_fill_manual(values = pallete1[c(1,3,5,6)])+
  theme_bw(24)+ 
  scale_x_continuous(n.breaks = 8)+
  coord_cartesian(ylim = ylims)+
 # ylim(0, 160)+
  labs(
    x = "Word frequency (Zipf)",
    y = ' '
  ) +
  ggtitle('')+
  theme(legend.position = 'bottom')

figure_Refixdur <- ggarrange(Refix_length,
                             Refix_freq, nrow=1, 
                            common.legend = T,
                            legend = 'bottom') 

plot_dat<- dat_all%>% 
  group_by(corpus,sub)%>%
  summarise(FFD= mean(FFD, na.rm=T),
            Refix_time= mean(Refix_time, na.rm=T),
            GD= mean(GD, na.rm=T),
            Regression_time= mean(Regression_time, na.rm=T))

r2_dat <- plot_dat %>%
  ungroup()%>%
 # group_by(corpus) %>%
  summarise(
    r2 = summary(lm(Refix_time ~ FFD))$r.squared
  )%>%
  mutate(label = paste0("R² = ", round(r2, 2)))

Refix_scatter<- plot_dat%>%
  ggplot(aes(x= FFD, y= Refix_time, colour= corpus))+
  geom_point(size=2)+
  geom_smooth(method = "lm", aes(group = 1), colour = "black")+
  scale_colour_manual(values = pallete1[c(1,3,5,6)])+
  scale_fill_manual(values = pallete1[c(1,3,5,6)])+
  #facet_wrap(~corpus)+
  #geom_smooth(method= 'lm')+
  geom_text(
    data = r2_dat,
    aes(x = Inf,
      y = Inf,
      label = label),
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE,
    size = 8
  ) +
  ggtitle('b)')+
  labs(x= 'FFD', y= "Refixation duration\n(GD - FFD)")+
  theme_bw(24)+
  theme(legend.position= "bottom")

r2_dat_reg <- plot_dat %>%
  ungroup()%>%
  # group_by(corpus) %>%
  summarise(
    r2 = summary(lm(Regression_time ~ GD))$r.squared
  )%>%
  mutate(label = paste0("R² = ", round(r2, 2)))

Refix_scatter2<- plot_dat%>%
  ggplot(aes(x= GD, y= Regression_time, colour= corpus))+
  geom_point(size=2)+
  geom_smooth(method = "lm", aes(group = 1), colour = "black")+
  scale_colour_manual(values = pallete1[c(1,3,5,6)])+
  scale_fill_manual(values = pallete1[c(1,3,5,6)])+
  #facet_wrap(~corpus)+
  #geom_smooth(method= 'lm')+
  geom_text(
    data = r2_dat_reg,
    aes(x = Inf,
        y = Inf,
        label = label),
    hjust = 1.1,
    vjust = 1.2,
    inherit.aes = FALSE,
    size = 8
  ) +
  ggtitle('')+
  labs(x= 'GD', y= "Regression duration\n(TFD - GD)")+
  theme_bw(24)+
  theme(legend.position= "bottom")


figure_GD <- ggarrange(fixation_comps,
                       figure_Refixdur,
                       nrow=1, widths = c(0.6, 1)) 

scatter_panel <- ggarrange(Refix_scatter,Refix_scatter2,
                       nrow=1, common.legend = T,
                       legend = 'bottom') 




# figure_GD2 <- ggarrange(figure_GD,
#                         scatter_panel,
#                        nrow=2, widths = c(1, 1)) 
# 
# ggsave("Plots/GD_combined.pdf", 
#        plot = figure_GD2, width = 16, height = 18)


#### Regression time (TVT):

Regress_length<- dat_all%>% 
  mutate(length_capped = pmin(word_length, 15)) %>%
  ggplot(aes(x= length_capped, y = Regression_time,
             colour= corpus, fill=corpus))+
  geom_smooth(method = 'gam')+
  scale_colour_manual(values = pallete1[c(1,3,5,6)])+
  scale_fill_manual(values = pallete1[c(1,3,5,6)])+
  theme_bw(24)+scale_x_continuous(
    breaks = seq(1, 15, 2),
    labels = c(as.character(seq(1, 14, 2)), "15+")
  )+
 coord_cartesian(ylim = ylims)+
  #  ylim(0, 160)+
  labs(
    x = "Word length (char.)",
    y = 'Regression duration\n(TVT - GD)'
  )+
  ggtitle('d)')+
  theme(legend.position = 'bottom')


Regress_freq<- dat_all%>% 
  ggplot(aes(x= zipf, y = Regression_time, colour= corpus, fill= corpus))+
  geom_smooth(method = 'gam')+
  scale_colour_manual(values = pallete1[c(1,3,5,6)])+
  scale_fill_manual(values = pallete1[c(1,3,5,6)])+
  theme_bw(24)+ 
  scale_x_continuous(n.breaks = 8)+
  coord_cartesian(ylim = ylims)+
  # ylim(0, 160)+
  labs(
    x = "Word frequency (Zipf)",
    y = ' '
  ) +
  ggtitle('')+
  theme(legend.position = 'bottom')

figure_Refixdur2 <- ggarrange(Regress_length,
                             Regress_freq, nrow=1, 
                             common.legend = T,
                             legend = 'bottom') 

figure_GD2<- ggarrange(fixation_comps,
                       scatter_panel,
                       figure_Refixdur,
                       figure_Refixdur2,
                       ncol=1)
                      # common.legend = T,
                      # legend = 'bottom') 


ggsave("Plots/GD_combined.pdf", 
       plot = figure_GD2, width = 15, height = 24)

# TFD------- How many 2nd-pass re-fixations do words receive?

nfix2<- dat_all%>%
  mutate(nfix2= nfixAll - nfix1,
         nfix2 = pmin(nfix2, 5))%>%
  group_by(corpus)%>%
  count(nfix2)%>%
  filter(!is.na(n) &nfix2>=0)%>%
  mutate(
    total = sum(n),              
    prop = n / total,           
    perc = prop * 100,
    nfix2= as.character(nfix2)
  )
head(nfix2) 

#nfix2$nfix2[which(nfix2$nfix2=='5')]<- '5+'

nfix2 <- nfix2 %>%
  mutate(
    nfix2 = factor(nfix2, levels = c(5, 4, 3, 2, 1, 0),
                   labels = c("5+", "4", "3", "2", "1", "0"))
  )

ggplot(nfix2, aes(x = corpus, y = perc, fill = nfix2)) +
  geom_col() +
  theme_bw(24) +
  scale_fill_manual(values = pallete1)+
  labs(
    x = "Corpus",
    y = "Percentage",
    fill = "Number of second-pass fixations"
  )+
  theme(legend.position = 'right')


##### show components contributing to TFD:

dat_all%>% 
  group_by(corpus)%>%
  summarise(M= mean(skip_1st, na.rm= T),
            M2= mean(skip, na.rm=T))

dat_all%>% 
  filter(corpus== "Text Font"| corpus== "Oz")%>%
  group_by(skip_1st)%>%
  summarise(M= mean(TVT, na.rm=T))

dat_all%>% 
  #filter(corpus== "Text Font"| corpus== "Oz")%>%
  mutate(regress_time_nonfixated= ifelse(is.na(FFD)&!is.na(TVT), TVT, 0),
         regress_time_fixated= ifelse(regress_time_nonfixated>0, 0, TVT-GD))%>%
  group_by(corpus)%>%
  summarise(M_nonfix= mean(regress_time_nonfixated, na.rm= T),
            M_fix= mean(regress_time_fixated, na.rm= T))

dat_all<- dat_all%>%
  mutate(regress_time_nonfixated= ifelse(is.na(FFD)&!is.na(TVT), TVT, 0),
         regress_time_fixated= ifelse(regress_time_nonfixated>0, 0, TVT-GD))






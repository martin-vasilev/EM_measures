

##### VISUALISE A TRIAL TO ILLUSTRATE COMMON MEASURES:

rm(list= ls())

load("data/visualisation_data/text_font/raw_fix.Rda")

dat<- subset(raw_fix, sub== 4 & item== 63)
rm(raw_fix)

dat$dur_scaled<- dat$fix_dur/ max(dat$fix_dur)*7

# import text coordinates and raw samples:
coords <- read.csv("data/visualisation_data/text_font/text_trial_coordinates.csv", header=T)
samples <- read.csv("data/visualisation_data/text_font/raw_trial_sample.csv", header=T)

# subset sample to start of first fix after gaze box:

samples<- subset(samples, time> dat$SFIX[1])


###
# Visual trial stimuli boxes:

library(extrafont)
loadfonts(device = "win")

library(tidyverse)
library(ggplot2)

dat_arrows <- dat %>%
  #arrange(fix_num) %>%
  mutate(
    xend = lead(xPos-8),
    yend = lead(yPos - 20),
    ystart = yPos - 20,
    next_fix = lead(word),
    is_back = next_fix < word
  ) %>%
  filter(!is.na(xend))

dat_arrows$is_back<- ifelse(dat_arrows$is_back== TRUE, "Regressive", "Progressive")


dat_arrows$xend[which(dat_arrows$is_back== TRUE)]<- dat_arrows$xend[which(dat_arrows$is_back== TRUE)]+16

P1<- samples %>% ggplot(aes(x= xPos, y = yPos))+
  #ylim(500, 700)+
  # geom_rect(data = coords, aes(xmin = x1,
  #                              xmax = x2,
  #                              ymin = y1,
  #                              ymax = y2),
  #           fill = "transparent", color = "#E9E5E5", size = 0.5,
  #           inherit.aes = FALSE)+
  geom_point(aes(color = "Eye trace"), size = 0.5)+
  scale_color_manual(name = "Legend:", values = c("Eye trace" = "#E4080A"))+
  theme_classic(16)+
  scale_y_reverse()+
  geom_text(data = coords, aes(x = (x1 + x2) / 2,
                               y = (y1 + y2) / 2, label = letter,
                               family = "Consolas"),
            color = "black", size = 6, 
            inherit.aes = FALSE)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  ggtitle('a)')+
  
   geom_point(data= dat, aes(x= xPos, y=yPos -20, size= fix_dur),
              shape = 21, fill = "blue", color = "black", alpha= 0.33)+
  scale_size(range = c(0, 14))+
  geom_text(data= dat, aes(x= xPos, y=yPos -20, label= as.character(fix_num)))+
  labs(size= 'Fixation duration (ms)', linetype = "Type of saccade")+
  geom_curve(data = dat_arrows,
                            aes(x = xPos, y = ystart, xend = xend, yend = yend,
                                linetype = is_back),
                            color = '#67666B',
                            arrow = arrow(length = unit(0.05, "inches"),
                                          type = "closed"),
                            curvature = -0.4,
                            alpha = 0.65)+
  
  # add some annotation labels:
  # skip:
  annotate("text", x = 445, y = 520, label = "skip",color= 'lightgreen')+
  
  # regression:
  annotate("text", x = 690, y = 543, label = "regression",color= 'lightgreen', angle= 15)+
  
  # refixation:
  annotate("text", x = 390, y = 535, label = "refixation",color= 'lightgreen')+
  
  # return-sweep:
  annotate("text", x = 655, y = 580, label = "return-sweep",color= 'lightgreen',angle = 50)+
  
  # undersweep:
  annotate("text", x = 275, y = 628, label = "undersweep",color= 'lightgreen')

  # geom_segment(data = dat_arrows,
  #              aes(x = xPos, y = ystart, xend = xend, yend = yend,
  #                  linetype = is_back,
  #                  color = is_back),
  #              arrow = arrow(length = unit(0.08, "inches"), type = "closed"),
  #              alpha = 0.5) +
  # scale_linetype_manual(values = c("Forward" = "solid", "Backward" = "dotted")) +
  # scale_color_manual(values = c("FALSE" = "black", "TRUE" = "purple",
  #                               "Eye trace" = "#E4080A") )
 # guides(linetype = "none")  # hides linetype legend, optional

ggsave(plot = P1, filename = 'Plots/Trial_visualisation.pdf', width = 10.5, 
       height = 5, device = cairo_pdf)  

#coords %>% 


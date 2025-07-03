

##### VISUALISE A TRIAL TO ILLUSTRATE COMMON MEASURES:

rm(list= ls())

# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

load("data/visualisation_data/text_font/raw_fix.Rda")

dat<- subset(raw_fix, sub== 4 & item== 63)
rm(raw_fix)

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
  geom_point(aes(color = "Eye trace"), size = 0.5,  alpha=.4)+
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



## draw x samples as function of time (first line only):

samples<- samples %>% mutate(time_ms= time- min(time)+1)

s_L1<- subset(samples, time<= dat$EFIX[which(dat$Rtn_sweep==1)-1] )

s_L2<- subset(samples, time>= dat$SFIX[which(dat$Rtn_sweep==1)] )

fix_L1<- subset(dat, EFIX<= dat$EFIX[which(dat$Rtn_sweep==1)-1])
fix_L1$SFIX_ms<- fix_L1$SFIX- fix_L1$SFIX[1]+1
fix_L1$EFIX_ms<- fix_L1$EFIX- fix_L1$SFIX[1]+1


# parse data using the Kliegl and Engbert algoritgm:
library(saccades)

# prepare data format:
ke<- s_L1[,c('xPos', 'yPos', 'item', 'time_ms')]

colnames(ke)<- c("x", "y", "trial", "time")

ke.result<- detect.fixations(ke, lambda = 15, smooth.coordinates = FALSE,
                     smooth.saccades = TRUE)

PA1<- s_L1 %>%
  ggplot(aes(x= time_ms, y= xPos))+
  scale_y_reverse()+
  theme_classic(15)+
  labs(x= 'Time (in milliseconds)', y= 'X position of the eye (in pixels)')+
  
  # Eyelink fixation events:
  geom_rect(data = fix_L1, aes(xmin = SFIX_ms,
                               xmax = EFIX_ms,
                               ymin = 200,
                               ymax = 800),
            fill = "transparent", color = "lightgreen", size = 0.5,
            inherit.aes = FALSE)+
  geom_line()+
  
  ggtitle("Eyelink default saccade detection algorithm")+
  theme(plot.title = element_text(hjust = 0.5))

PA2<- s_L1 %>%
  ggplot(aes(x= time_ms, y= xPos))+
  scale_y_reverse()+
  theme_classic(15)+
  labs(x= 'Time (in milliseconds)', y= 'X position of the eye (in pixels)')+
  
  #EK fixation events:
  geom_rect(data = ke.result, aes(xmin = start,
                                  xmax = end,
                                  ymin = 200,
                                  ymax = 800),
            fill = "transparent", color = "orange", size = 0.5,
            inherit.aes = FALSE)+
  geom_line()+
  
  ggtitle(expression("Engbert & Kliegl (2003) saccade detection algorithm ("*lambda*" = 15)"))+
  theme(plot.title = element_text(hjust = 0.5))
  
library(ggpubr)

PA1 <- PA1 + theme(axis.title.y = element_blank(),
                   axis.title.x = element_blank())
PA2 <- PA2 + theme(axis.title.y = element_blank(),
                   axis.title.x = element_blank())


figure1 <- ggarrange(PA1, PA2, nrow = 2)
figure1 <- annotate_figure(
  figure1,
  left = text_grob("X position of the eye (in pixels)", rot = 90, size = 16)
)

figure1 <- annotate_figure(
  figure1,
  bottom = text_grob("Time (in milliseconds)", rot = 0, size = 16)
)
  
figure1 <- annotate_figure(figure1,
                           top = text_grob("b)", size = 20, hjust = 0.15, x = 0.02))


ggsave(filename = 'Plots/Sacc_alg.pdf', plot = figure1,
         width = 8, height = 8)

figure2<- ggarrange(P1, figure1, nrow = 2)

ggsave(filename = 'Plots/Trial_visualisation_combined.pdf', plot = figure2,
       width = 12, height = 10, device = cairo_pdf)
  



###############################################################################
# # Compare saccade detection algos:
# library(EMreading)
# s<- ExtractSamples(data_list = 'C:/Data/TextFont', maxtrial=100)
# 
# write.csv(s, file = 'C:/Data/TextFont/raw_samples.csv')  

rm(list= ls())

load("data/visualisation_data/text_font/raw_fix.Rda")
samples <- read.csv("C:/Data/TextFont/raw_samples.csv", header=T)


# parse fixations through the E & K algorithm:
ek<- NULL

lambda<- 6:15

for(l in 1:length(lambda)){
  
  nsubs<- unique(samples$sub)
  
  ek<-  NULL
  
  for (i in 1:length(nsubs)){
    a<- subset(samples, sub== nsubs[i])
    
    nitems<- unique(a$item)
    
    cat(sprintf("Subject %i, lambda %i\n\n", nsubs[i], lambda[l]))
    
    for(j in 1:length(nitems)){
      b<- subset(a, item== nitems[j])
      
      c<- subset(raw_fix, sub== nsubs[i] &  item== nitems[j])
      # keep only sample occuring after first SFIX
      b<- subset(b, time>= c$SFIX[1])
      
      # parse fixations through algorithm
      
      b<- b[,c('xPos', 'yPos', 'item', 'time')]
      
      colnames(b)<- c("x", "y", "trial", "time")
      
      if(nrow(b)==0){
        next
      }
      
      b.result<- detect.fixations(b, lambda = lambda[l],
                                  smooth.coordinates = FALSE,
                                  smooth.saccades = TRUE)
      b.result$lambda<- lambda[l]
      b.result$sub<-nsubs[i]
        
      ek<- rbind(ek, b.result)
    }
    
 
    
  }
  write.csv(ek, file = paste('data/saccadic_algorithms/Text_font_EK2003_l',
                             lambda[l], '.csv', sep=''))
  
  
}

#save(ek, file = 'data/saccadic_algorithms/Text_font_EK2003.Rda')
rm(samples)


#load('data/saccadic_algorithms/Text_font_EK2003.Rda')

library(tidyverse)
library(readr)

l6 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l6.csv")
l7 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l7.csv")
l8 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l8.csv")
l9 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l9.csv")
l10 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l10.csv")
l11 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l11.csv")
l12 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l12.csv")
l13 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l13.csv")
l14 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l14.csv")
l15 <- read_csv("data/saccadic_algorithms/Text_font_EK2003_l15.csv")

ek<- rbind(l6, l7, l8, l9, l10, l11, l12, l13, l14, l15)
rm(l6, l7, l8, l9, l10, l11, l12, l13, l14)

#ek<- subset(ek, event== "fixation")

ek.sum<- ek %>%
  group_by(lambda, sub) %>%
  summarise(M= mean(dur))

el.sum<- raw_fix %>% 
  group_by(sub)%>%
  summarise(M= mean(fix_dur, na.rm= T))

mean(el.sum$M)

el<-raw_fix[, c('sub', 'item', 'fix_dur')]

l15<- subset(l15, event== "fixation")
ek_l15<- l15[, c('sub', 'trial', 'dur')]

colnames(ek_l15)<- c('sub', 'item', 'fix_dur')



el$Algorithm<- "Eyelink (default)"
ek_l15$Algorithm <- "Engbert and Kliegl (2003)"#

dat<- rbind(el, ek_l15)


SA1<-dat %>%
  filter(fix_dur <= 1000) %>%
  ggplot(aes(x = fix_dur, fill = Algorithm)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(
    values = c("Engbert and Kliegl (2003)" = pallete1[1],  
               "Eyelink (default)" = pallete1[2]),         
    labels = c(expression("Engbert and Kliegl, 2003 ("*lambda*" = 15)"),
               "Eyelink (default)")
  ) +
  theme_bw(base_size = 16) +
  labs(x = 'Fixation duration (in ms)', y = 'Density')+
  theme(legend.position = 'inside', legend.position.inside = c(0.6, 0.8))+
  ggtitle('a)')

#calculate by-subject/ trial correlations:

dat2<- dat %>% 
  group_by(Algorithm, sub)%>%
  summarise(M= mean(fix_dur))%>%
  pivot_wider(names_from = Algorithm, values_from = M)
  

# Fit the linear model
model <- lm(`Eyelink (default)` ~ `Engbert and Kliegl (2003)`, data = dat2)
r2 <- summary(model)$r.squared

SA2<- dat2 %>%
  ggplot(aes(x= `Engbert and Kliegl (2003)`, y= `Eyelink (default)`))+
  geom_point(color= pallete1[3], size=2)+
  theme_bw(base_size = 16)+
  labs(x= expression("Fixation duration (Engbert and Kliegl, 2003, "*lambda*" = 15)"),
                     y= "Fixation duration (Eyelink default)")+
  geom_smooth(method = 'lm', color= pallete1[1])+
  annotate(
    "text",
    x = Inf, y = -Inf,
    hjust = 1.1, vjust = -0.5,
    label = paste0("R² = ", round(r2, 3)),
    size = 5
  )+
  ggtitle('b)')

# number of fixations per trial:
fun_mean <- function(x, rounding= 2){
  return(data.frame(y=mean(x),label= paste("M= ", round(mean(x,na.rm=T), rounding), sep= '')))}


SA3<- dat%>%
  group_by(Algorithm, sub, item)%>%
  count()%>%
  group_by(Algorithm, sub)%>%
  summarise(M= mean(n))%>%
  
  ggplot(aes(x= Algorithm, y= M, fill = Algorithm, color= Algorithm))+
  theme_bw(16)+
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA, alpha=1) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, fill= NA
  ) +
  geom_point(
    size = 1.7,
    alpha = .3,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) +
  scale_color_manual(
    values = c("Engbert and Kliegl (2003)" = pallete1[1],  
               "Eyelink (default)" = pallete1[2]))+
  scale_fill_manual(
    values = c("Engbert and Kliegl (2003)" = pallete1[1],  
               "Eyelink (default)" = pallete1[2]))+
  scale_x_discrete(
    labels = c(
      "Engbert and Kliegl (2003)" = expression("Engbert and Kliegl, 2003 ("*lambda*" = 15)"),
      "Eyelink (default)" = "Eyelink (default)"
    )
  )+
  labs(y= "Mean number of fixations (per trial)")+
  theme(legend.position = 'none')+
  stat_summary(fun = mean, geom="point",colour="black", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-1.15,
               hjust= 0.75, colour="black", size= 7)+
  ggtitle('c)')
  
library(ggpubr)

# Combine
figureSA <- ggarrange(SA1, SA2, SA3, ncol = 3)

# Save using ggsave() – this should work if figureSA is a ggpubr object
ggsave("Plots/ELvsEK.pdf", plot = figureSA, width = 18, height = 8)


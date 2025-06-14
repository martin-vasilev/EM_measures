
# Martin R. Vasilev, 2025

rm(list= ls())

load("data/Provo.Rda") # Provo corpus
load("data/geco.Rda") # GECO corpus


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(ggpubr)

geco.c<- subset(geco, nfixAll<100)
geco.c<- geco.c[,c(10, 15, 11, 12, 13, 14)]

colnames(geco.c)<- c("FFD", "SFD", "GD", "GPT", "TFT","Skip" )

r_corr<- cor(geco.c, use = 'pairwise.complete.obs', method = 'pearson', )

P1<-r_corr%>%ggcorrplot(method = 'square', type = 'upper',
                    title = 'GECO (Cop et al., 2017)', digits = 2, show.diag = F,
                    outline.color = 'black', lab = T)


Provo_c<- Provo[,c(9, 11, 10,13, 12, 14)]
colnames(Provo_c)<- c("FFD", "SFD", "GD", "GPT", "TFT","Skip" )

r_corr2<- cor(Provo_c, use = 'pairwise.complete.obs', method = 'pearson')

P2<-r_corr2%>%ggcorrplot(method = 'square', type = 'upper',
                        title = 'Provo (Luke & Christianson, 2018)', digits = 2, show.diag = F,
                        outline.color = 'black', lab = T)

figure1 <- ggarrange(P1, P2, ncol = 2)

ggsave(filename = 'Plots/Corr_plot.pdf', plot = figure1,
       width = 8, height = 4)


### First-pass refixation probability:







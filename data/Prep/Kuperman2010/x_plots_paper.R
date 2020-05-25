# =========================================================================================
# Program x_plots.R
# =========================================================================================
#
# Word position effect
#
# M. Dambacher, March 2009
# =========================================================================================



rm(list=ls())
library("ggplot2")
source("parseq_fast.R")



# ------------------------------------------------------------------------------
# PSC
# ------------------------------------------------------------------------------
# --- Load data ---
load("psc.data.rda")

a <- dx[,c("id", "sn", "wn", "nw", "wid", "wrd", "dur", "ao", "o")]


# --- Set up b ---
b <- rep(0,nrow(a))
b <- data.frame(b)
b$cnd   <- a$nw
b$nw    <- a$nw
b$wn    <- a$wn
b$relwn <- a$wn/a$nw
b$dur   <- exp(a$dur)
b$sub   <- a$id
b$id    <- a$wrd
b$exp   <- "PSC"
b       <- b[,-1]
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# DEMONIC
# ------------------------------------------------------------------------------
load("demonic.single.rda") 
	#DEMONIC SFD, data set after trimming (sentences with fewer than 16 words, extra short and long fixations removed, etc)
	
b1       <- rep(0,nrow(single))
b1       <- data.frame(b1)
b1$cnd   <- single$TotalWordsinSentence
b1$nw    <- single$TotalWordsinSentence
b1$wn    <- single$NumWordInSentence
b1$relwn <- single$NumWordInSentence/single$TotalWordsinSentence
b1$dur   <- exp(single$GazeDur)
#b1$sn   <- single$TrialID
b1$sub   <- single$Subject
b1$id    <- single$Word
b1$exp   <- "DEMONIC"
b1       <- b1[b1$nw < 16,]
b1       <- b1[,-1]
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# DMORPH
# ------------------------------------------------------------------------------
load("affix.rda")
	#DMORPH SFD, dataset after trimming.

b2       <- rep(0,nrow(affpos1))
b2       <- data.frame(b2)
b2$cnd   <- affpos1$TotalWordsinSentence
b2$nw    <- affpos1$TotalWordsinSentence
b2$wn    <- affpos1$NumWordInSentence
b2$relwn <- affpos1$NumWordInSentence/affpos1$TotalWordsinSentence
b2$dur   <- exp(affpos1$FixDur)
#b2$sn   <- affpos1$TrialID
b2$sub   <- affpos1$Subject
b2$id    <- affpos1$Word
b2$exp   <- "DMORPH"
b2       <- b2[b2$nw < 16,]
b2       <- b2[,-1]
# ------------------------------------------------------------------------------




# ------------------------------------------------------------------------------
# Plot data
# ------------------------------------------------------------------------------

# --- Merge Data ---
b.all = rbind(b, b1, b2)

  # --- Position locked to sentence-final word ---
  maxnw <- max(b.all$nw)
  b.all$finposnum <- NA
  b.all$finposlab <- NA
  labelvec <- NA
  for (i in 0:maxnw-1) {
    id <- which(b.all$wn==(b.all$nw-i))
    label = paste("-", as.character(i), sep="")
    b.all$finposnum[id] <- maxnw-i
    b.all$finposlab[id] <- label
    labelvec[i] <- label
    }
    b.all$finpos <- b.all$finposnum -maxnw
    #label <- intersect(labelvec,(unique(b.all$finposlab)))
    #label <- label[length(label):1]
    #head(b.all)
  

  
  
  # ----------------------------------------------

dat.m <- melt(b.all, id.var = c("sub", "id", "cnd", "nw", "wn", "exp", "relwn", "finpos"), measure.var = "dur", variable_name = "dur")
# -----

linecol = rgb(.25,.25,.25)
# --- Relative Word Position ---
dataR <- cast(dat.m, relwn+exp~dur, mean)
dataR$exp <- factor(dataR$exp, levels=c("PSC", "DEMONIC", "DMORPH"))


prel <- qplot(relwn, dur,  data=dataR, geom=c("smooth"), facets = exp~.,
         ylim=c(180,275),
         xlab="Relative Word Position",
         ylab="Single Fixation Duration [ms]") +
         geom_smooth(col=linecol, size=.8, stat="smooth") +
         geom_point(data=dataR, size=1, stat="identity")


pdf("relwn.pdf", he = 9, wi= 3)
  print(prel)
dev.off()
# -----


# --- Absolute Word Position ---
dataA <- cast(dat.m, wn+exp~dur, mean)
dataA$exp <- factor(dataA$exp, levels=c("PSC", "DEMONIC", "DMORPH"))
pabs <- qplot(wn, dur, data=dataA, geom=c("smooth"), facets = exp~.,
        ylim=c(180,275),
        xlab="Word Position",
        ylab="Single Fixation Duration [ms]") +
        geom_smooth(data=dataA, col=linecol, size=.8, stat="smooth") +
        geom_point(data=dataA, size=1, stat="identity")

pdf("abswn.pdf", he = 9, wi= 3)
  print(pabs)
dev.off()
# -----


# --- Absolute Word Position locked to sentence-final word---
dataF <- cast(dat.m, finpos+exp~dur, mean)
dataF$exp <- factor(dataF$exp, levels=c("PSC", "DEMONIC", "DMORPH"))


unipos <- unique(dataF$finpos)
pfin <- qplot(finpos, dur, data=dataF, geom=c("smooth"), facets = exp~.,
        ylim=c(180,275),
        xlab="Position from Final Word",
        ylab="Single Fixation Duration [ms]") +
        geom_smooth(data=dataF, col=linecol, size=.8, stat="smooth") +
        geom_point(data=dataF, size=1, stat="identity") #+
        #scale_x_continuous("finpos",
        #                   breaks=unipos[seq(1,length(unipos), by=2)],
        #                   labels=label[seq(1,length(label), by=2)])

pdf("finwn.pdf", he = 9, wi= 3)
  print(pfin)
dev.off()
# -----
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# Dundee
# ------------------------------------------------------------------------------
rm(list=ls())
library(ggplot2)

linecol = rgb(.25,.25,.25)

data.dundeeS <- read.table("Dundee_relwn_sent.txt", header=TRUE, sep="\t", dec=".")
pdf("qplot-dundee-relwnSent-horiz.pdf", he = 7, wi= 9)
par(cex = 1)
qplot(relwnS, dur,  data=data.dundeeS,
      ylim=c(170,260),
      xlab="Relative Word Position in Sentence, by Sentence Length",
      ylab="Single Fixation Duration [ms]") +
      geom_smooth(col=linecol, size=.8, stat="smooth") +
      geom_point(data=data.dundeeS, size=1, stat="identity") +
      facet_wrap(~ nwS, ncol = 6)
dev.off()


data.dundeeL <- read.table("Dundee_relwn_line.txt", header=TRUE, sep="\t", dec=".")
pdf("qplot-dundee-relwnLine-horiz.pdf", he = 4, wi= 8)
par(cex = 1)
qplot(relwn, dur,  data=data.dundeeL, facets = .~lmod,
      ylim=c(170,260),
      xlab="Relative Word Position in Line",
      ylab="Single Fixation Duration [ms]") +
      geom_smooth(col=linecol, size=.8, stat="smooth") +
      geom_point(data=data.dundeeL, size=1, stat="identity")
dev.off()














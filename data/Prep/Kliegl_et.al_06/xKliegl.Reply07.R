# Kliegl (2007). Reply to Rayner et al., JEP:General

#  SINGLE-FIXATION DURATIONS

# This script contains two parts:
#  I Tables and Figures-- uses dataframe  "bx.rda"
#     -- bx contains variables in original metric

# II Linear Mixed Models-- uses dataframe "dx.rda"
#     -- dx contains variables in centered within subject metric

################################################################
options(digits=2)


#################################################################
#    Tables and Figures
#################################################################
# Plots of main effects
# Generic utility for getting info about dichotomous factors
# r. kliegl, 15/10/2006

rm(list=ls())


library(reshape)
load("bx.rda")
b <- bx
N <- nrow(b)

# prepare for reshape
b.rs <- melt(b, id=c("x", "x1", "x2","s1", "s2"), measure=c("dur"))

# means, sd, n, % for table 4 (Supplement)
M3a.tab <- cast(b.rs, x1+x+s1 ~ variable,
            function(x) c( M=round(mean(x)), SD=round(sd(x)), N=length(x), p=round(length(x)/N*100) )    )

M3b.tab <- cast(b.rs, x2+x+s2 ~ variable, 
            function(x) c( M=round(mean(x)), SD=round(sd(x)), N=length(x), p=round(length(x)/N*100) )     )  

# means and ci's for figure 2a and 2b (Reply)
library(lattice)

# ... figure 2a
M2a.fig <- cast(b.rs, x1+x+s1 ~ variable,
            function(x) c( M=round(mean(x)), CI=3*(sd(x)/length(x-1)^.5) )  )

M2a <- data.frame(unclass(M2a.fig)) 
names(M2a$x) <- "Lexical status of word n"
levels(M2a$x) <- c("content word n", "function word n")

names(M2a$x1) <- "Lexical status of word n-1"
levels(M2a$x1) <- c("content word n-1", "function word n-1")

names(M2a$s1) <- "Skipping status of word n-1"
levels(M2a$s1) <- c("fixated", "skipped")

M2a$lower <- M2a$dur_M - 3
M2a$upper <- M2a$dur_M + 3
M2a$S1 <- as.numeric(M2a$s1)

library(Hmisc)
xYplot(Cbind(dur_M, lower, upper) ~ S1|x1, group=x, data=M2a, 
  nx=FALSE, asp=1, type="b", pch=c(16,1), lty=1:2, col=1, cex=1.25, keys="lines",
  xlab="Skipping status of word n-1", 
  ylab="Single-fixation duration [ms]",
  xlim=c(0.5,2.5), ylim=c(180,230),
  scales = list(x = list(at=seq(1, 2, by=1), labels = levels(M2a$s1)))  )

# ... figure 2b
M2b.fig <- cast(b.rs, x2+x+s2 ~ variable,
            function(x) c( M=round(mean(x)), CI=3*(sd(x)/length(x-1)^.5) )  )

M2b <- data.frame(unclass(M2b.fig)) 
names(M2b$x) <- "Lexical status of word n"
levels(M2b$x) <- c("content word n", "function word n")

names(M2b$x2) <- "Lexical status of word n+1"
levels(M2b$x2) <- c("content word n+1", "function word n+1")

names(M2b$s2) <- "Skipping status of word n+1"
levels(M2b$s2) <- c("fixated", "skipped")

M2b$lower <- M2b$dur_M - 3
M2b$upper <- M2b$dur_M + 3
M2b$S2 <- as.numeric(M2b$s2)

library(Hmisc)
xYplot(Cbind(dur_M, lower, upper) ~ S2|x2, group=x, data=M2b, 
  nx=FALSE, asp=1, type="b", pch=c(16,1), lty=1:2, col=1, cex=1.25, keys="lines",
  xlab="Skipping status of word n+1", 
  ylab="Single-fixation duration [ms]",
  xlim=c(0.5,2.5), ylim=c(180,230),
  scales = list(x = list(at=seq(1, 2, by=1), labels = levels(M2b$s2)))  )


###########################
# Lattice option w/o errors, but nice labeling
xyplot(dur_M ~ s1|x1, group=x, data=M2a, 
  asp=1, type="b", pch=c(16,1), col=1, cex=1.25,
  xlab="Skipping status of word n-1", 
  ylab="Single-fixation duration [ms]",
  ylim=c(180,230)     )

trellis.focus("panel", 1,1) 
  panel.text(x=2, y=222, labels="content word n")
  panel.text(x=2, y=206, labels="function word n") 
trellis.unfocus()

trellis.focus("panel", 2,1) 
  panel.text(x=1, y=186, labels="content word n")
  panel.text(x=1, y=198, labels="function word n") 
trellis.unfocus()

#################################################################
#    LINEAR MIXED MODELS
#################################################################
# Centering frequencies within subjects AND within lexical status
#    --- frequency centered within content words is in variable g;
#	 --- frequency centered within function words is in variable h

# r. kliegl, october 2006

# abbreviated version, starts with transformed variables
# r. kliegl, january 2007
#
rm(list=ls())

load("dx.rda")  # single-fixation durations, (dxx.rda=multiple-fix)

# ... pred square
dx$p1sq <- dx$p1*dx$p1

library(lme4)
# Note: Model count corresponds to Online Supplement 
# not reliable, t<3:+g:g1 +l:g2 +l:h2 +l:h (t=2.6)
d0 <- lmer(dur ~ ao1+o+oq+ao +p +l+l67 +x+g+gsq+gcb +h+hsq+hcb 
                +x1+g1+h1+p1+p1sq+l1+s1 +x2+g2+h2+p2+s2
                +l:g +h:h1 +h:g1 +g:h1 +l:p2 
                +(1|id), data=dx)

# Model 1                
d1 <- lmer(dur ~ ao1+o+oq+ao +p +l+l67 +x+g+gsq+gcb +h+hsq+hcb 
                +x1+g1+h1+p1+p1sq+l1+s1 +x2+g2+h2+p2+s2
                +l:g +h:h1 +h:g1 +g:h1 +l:p2 
                +x*x1*s1 +x*x2*s2+(1|id), data=dx)

# including the full factorial                 
d1.a <- lmer(dur ~ ao1+o+oq+ao +p +l+l67 +x+g+gsq+gcb +h+hsq+hcb 
                +x1+g1+h1+p1+p1sq+l1+s1 +x2+g2+h2+p2+s2
                +l:g +h:h1 +h:g1 +g:h1 +l:p2  
                +x*x1*s1*x2*s2+(1|id), data=dx)

# possible suppressor, s1:x2, not in the model              
d1.b <- lmer(dur ~ ao1+o+oq+ao +p +l+l67 +x+g+gsq+gcb +h+hsq+hcb 
                +x1+g1+h1+p1+p1sq+l1+s1 +x2+g2+h2+p2+s2
                +l:g +h:h1 +h:g1 +g:h1 +l:p2 
                +x*x1*s1 +x*x2*s2
                +x*x1*s2 +x1*x2*s2+(1|id), data=dx)
                
anova(d0,d1,d1.b,d1.a)

# Model 2, p interactions
d2 <- lmer(dur ~ ao1+o+oq+ao +p +l+l67 +x+g+gsq+gcb +h+hsq+hcb 
                +x1+g1+h1+p1+p1sq+l1+s1 +x2+g2+h2+p2+s2
                +l:g +h:h1 +h:g1 +g:h1 +l:p2 
                +x*x1*s1 +x*x2*s2 
                +  p:(x +x1+x2+s1+s2 +x:x1 +x:s1 +x1:s1 +x:x2 +x:s2 + x2:s2 )
                + p1:(x +x1+s1  +x:x1 +x:s1 +x1:s1 )
                +p1sq:(x +x1+s1  +x:x1 +x:s1 +x1:s1 )
                + p2:(x +x2+s2  +x:x2 +x:s2 +x2:s2 )
                +(1|id), data=dx)

# Model 2.1, iteration: 1, remove if t < 3
d2.1 <- lmer(dur ~ ao1+o+oq+ao +p +l+l67 +x+g+gsq+gcb +h+hsq+hcb 
                +x1+g1+h1+p1+p1sq+l1+s1 +x2+g2+h2+p2+s2
                +l:g +h:h1 +h:g1 +g:h1 +l:p2 
                +x*x1*s1 +x*x2*s2 
                +  p:(x+x1+s1 +x:x1 )
                + p1:(x+x1+s1 +x:s1 +x1:s1 )
               +p1sq:(x+x1+s1 +x:s1 +x1:s1 )
                + p2:(x+x2  +x:x2 )
                +(1|id), data=dx)


# Model 2a, CW-f interactions, taking it from d2.1                  
d2a  <- update(d2.1, . ~ . 
                   +  g:(x1+x2+s1+s2 +x1:s1 + x2:s2 )
                   +gsq:(x1+x2+s1+s2 +x1:s1 + x2:s2 )        
                   +gcb:(x1+x2+s1+s2 +x1:s1 + x2:s2 )
                   + g1:(x +s1 +x:s1 )
                   + g2:(x +s2 +x:s2 )
                   , data=dx)

# Model 2a, iteration: 1, removing >=2-factor interactions,
# t < 2 (+gcb:x2=2.8, g:x2=-2.1)
d2a.1 <- update(d2.1, . ~ . 
                   +  g:(x1+s1 )
                   +gsq:(x1+s1 )        
                   +gcb:(x1+s1 )
                   + g1:(x +s1 )
                   + g2:s2
                   , data=dx)

# Model 2b, FW-f interactions, taking it from d2.1
d2b <- update(d2.1, . ~ . 
                   +  h:(x1+x2+s1+s2 +x1:s1 + x2:s2 )
                   +hsq:(x1+x2+s1+s2 +x1:s1 + x2:s2 )        
                   +hcb:(x1+x2+s1+s2 +x1:s1 + x2:s2 )
                   + h1:(x +s1 +x:s1 )
                   + h2:(x +s2 +x:s2 )
                   , data=dx)

# Model 2b, iteration: 1, removing >=2-factor interactions,
# t < 2.5 (+gcb:x2=2.6, g:x2=-2.1)
d2b.1 <- update(d2.1, . ~ . 
                   +  h:(x1+x2+s1+s2 +x1:s1 + x2:s2 )
                   +hsq:s2        
                   +hcb:s2
                   + h1:(x +s1 +x:s1 )
                   + h2:s2
                   , data=dx)

# Adding the significant interactions of d2a.1 and d2b.1
d3 <- update(d2.1, .~. 
                   +  g:(x1+s1 )
                   +gsq:(x1+s1 )        
                   +gcb:(x1+s1 )
                   + g1:(x +s1 )
                   + g2:s2
                   +  h:(x1+x2+s1+s2 +x1:s1 + x2:s2 )
                   +hsq:s2        
                   +hcb:s2
                   + h1:(x +s1 +x:s1 )
                   + h2:s2
                   , data=dx)

# Allowing for sentence differences
d4 <- update(d3, .~. + (1|sn), data=dx)

# Allowing for individual differences in three effects, but no correlation
#d5 <- update(d4, .~. + (0+o|id) +(0+p|id) + (0+g|id),
#                 data=dx, control=list( niterEM=0, gradient=FALSE) ) 

# New specification, needed due to changes in lmer (r kliegl, 21 Jan 2011)
d5 <- update(d4, .~. + (0+o|id) +(0+p|id) + (0+g|id),
                 data=dx, verbose=TRUE) 

anova(d3, d4, d5)  

d6 <- update(d4, .~. - (1|id) + (1 + o + p + g|id),
                 data=dx, verbose=TRUE)

anova(d3, d4, d5, d6) # No further improvement from d5 to d6 with covariance parameters
require('dplyr')
require('lme4')

## clear workspace - delete / out-comment it, if you do not like that
rm(list=ls())

## read in the data
exp1_pt <- read.csv('fl_exp1_pt.csv', header = TRUE)
exp2_pt <- read.csv('fl_exp2_pt.csv', header = TRUE)
exp1_t <- read.csv('fl_exp1_t.csv', header = TRUE)
exp2_t <- read.csv('fl_exp2_t.csv', header = TRUE)

## merge the dfs
exp1 <- merge(exp1_pt, exp1_t) 
exp2 <- merge(exp2_pt, exp2_t) 
rm(exp1_pt, exp2_pt, exp1_t, exp2_t)

## get size of the raw-data
exp1.nrow.raw <- nrow(exp1)
exp2.nrow.raw <- nrow(exp2)

## only include trials in which the incoming saccade on the target word
## came from the pretarget word (i.e., which are labeled 'valid')
exp1 <- subset(exp1, exp1$valid==1)
exp2 <- subset(exp2, exp2$valid==1)

## exceedingly large saccade durations (of the saccade triggering the 
## display change) indicate a that the display change actually occurred
## during a fixation, that is, the participant 'drifted' over the boundary
## (however, these cases are extremely rare)
exp1 <- subset(exp1, exp1$t_saccDur < 70)
exp2 <- subset(exp2, exp2$t_saccDur < 70)

## exclude the cases where the pretarget word was initially skipped, but
## later fixated - not doing this would yield some 2nd-pass reading of the
## target words (which the preprocessing script doesn't capture as 'invalid')
exp1 <- subset(exp1, exp1$pt_skip==0)
exp2 <- subset(exp2, exp2$pt_skip==0)

## assign NA to outliers
outlier  <- function(dat, minFFD, maxFFD, maxGD) {
  dat$pt_ffd.c <- ifelse(dat$pt_ffd < minFFD | dat$pt_ffd > maxFFD, NA, dat$pt_ffd)
  dat$pt_gaze.c <- ifelse(dat$pt_gaze < minFFD | dat$pt_gaze > maxGD, NA, dat$pt_gaze)
  dat$t_ffd.c <- ifelse(dat$t_ffd < minFFD | dat$t_ffd > maxFFD, NA, dat$t_ffd)
  dat$t_gaze.c <- ifelse(dat$t_gaze < minFFD | dat$t_gaze > maxGD, NA, dat$t_gaze)
  dat$pt_sfd <- ifelse(dat$pt_Nfix == 1, dat$pt_ffd.c, NA)
  dat$t_sfd <- ifelse(dat$t_Nfix == 1, dat$t_ffd.c, NA)
  return(dat)
}

exp1 <- outlier(exp1, minFFD = 80, maxFFD = 600, maxGD = 1250)
exp2 <- outlier(exp2, minFFD = 80, maxFFD = 600, maxGD = 1250)

## get size of the clean data and print the data loss and the number of
## valid observation per condition per experiment
exp1.nrow.clean <- nrow(exp1)
exp2.nrow.clean <- nrow(exp2)
cat("\nData loss in Exp1 is", 100 - exp1.nrow.clean / exp1.nrow.raw * 100, "%\n") 
cat("Data loss in Exp2 is", 100 - exp2.nrow.clean / exp2.nrow.raw * 100, "%\n") 
cat("\nValid observations per condition:\nExp1:\n")
print(aggregate(t_ffd.c ~ load + prev, data=exp1, FUN='length'))
cat("Exp2:\n")
print(aggregate(t_ffd.c ~ load + prev, data=exp2, FUN='length'))

## do some aggregation and the computation of confidence intervals
## for which we remove the "between-subject variability" as suggested
## by Cousineau, D. (2005). Confidence intervals in within-subject
## designs: A simpler solution to Loftus and Masson’s method. Tutorial
## in Quantitative Methods for Psychology, 1(1), 4–45. 
source('fl_aggregate.R')
m.exp1 <- fl.agg(exp1)
m.exp2 <- fl.agg(exp2)

## plot the data - creates tiff-files (uncomment if you want to 
## reproduce the figures)
source('syn_plot_pt.R')
source('syn_plot_t.R')

## display change awareness (dca)
#### the sbj who were unaware of the display changes in Exp. 1
exp1.dca <- c('v01', 'v05', 'v07', 'v09', 'v10', 'v11', 'v12', 'v13', 'v15',
              'v16', 'v22', 'v24', 'v25', 'v33', 'v34', 'v35', 'v39', 'v40',
	      'v42', 'v43', 'v44', 'v45', 'v48', 'v52', 'v54', 'v60')

for (v in unique(exp1$sbj)) {
  if (v %in% exp1.dca) {
    exp1$dca[exp1$sbj == v] <- 'U' 
  } else { 
    exp1$dca[exp1$sbj == v] <- 'A'
  }
}  
exp1$dca <- factor(exp1$dca )

##### the sbj who were unaware of the display changes in Exp. 2
exp2.dca <- c('v01', 'v03', 'v04', 'v32', 'v37', 'v38', 'v40', 'v43', 'v48',
              'v49', 'v50', 'v53', 'v59', 'v60')

for (v in unique(exp2$sbj)) {
  if (v %in% exp2.dca) {
    exp2$dca[exp2$sbj == v] <- 'U'
  } else { 
    exp2$dca[exp2$sbj == v] <- 'A'
  }
}  
exp2$dca <- factor(exp2$dca )

## LMM PRETARGET EXPERIMENT 1
#### FFD
exp1$dv <- log(exp1$pt_ffd.c)
exp1_pt_ffd <- filter(exp1, !is.na(dv))

contrasts(exp1_pt_ffd$load) <- contr.helmert(2)
contrasts(exp1_pt_ffd$prev) <- contr.helmert(3)

mm1 <- model.matrix(dv ~ load*prev, data=exp1_pt_ffd)
exp1_pt_ffd$l  <- mm1[,2]
exp1_pt_ffd$c1 <- mm1[,4] # D0 - (D33 + D66)/2
exp1_pt_ffd$c2 <- mm1[,3] # D33-66

m.exp1.pt_ffd  <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                       REML=FALSE, data=exp1_pt_ffd)

#### SFD
exp1$dv <- log(exp1$pt_sfd)
exp1_pt_sfd <- filter(exp1, !is.na(dv))

contrasts(exp1_pt_sfd$load) <- contr.helmert(2)
contrasts(exp1_pt_sfd$prev) <- contr.helmert(3)

mm1 <- model.matrix(dv ~ load*prev, data=exp1_pt_sfd)
exp1_pt_sfd$l  <- mm1[,2]
exp1_pt_sfd$c1 <- mm1[,4] # D0 - (D33 + D66)/2
exp1_pt_sfd$c2 <- mm1[,3] # D33-66

m.exp1.pt_sfd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                      REML=FALSE, data=exp1_pt_sfd)

#### GD
exp1$dv <- log(exp1$pt_gaze.c)
exp1_pt_gd <- filter(exp1, !is.na(dv))

contrasts(exp1_pt_gd$load) <- contr.helmert(2)
contrasts(exp1_pt_gd$prev) <- contr.helmert(3)

mm1 <- model.matrix(dv ~ load*prev, data=exp1_pt_gd)
exp1_pt_gd$l  <- mm1[,2]
exp1_pt_gd$c1 <- mm1[,4] # D0 - (D33 + D66)/2
exp1_pt_gd$c2 <- mm1[,3] # D33-66

m.exp1.pt_gd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                     REML=FALSE, data=exp1_pt_gd)

## LMM PRETARGET EXPERIMENT 2 
#### FFD
exp2$dv <- log(exp2$pt_ffd.c)
exp2_pt_ffd <- filter(exp2, !is.na(dv))

contrasts(exp2_pt_ffd$load) <- contr.helmert(2)
contrasts(exp2_pt_ffd$prev) <- contr.helmert(3)

mm2 <- model.matrix(dv ~ load*prev, data=exp2_pt_ffd)
exp2_pt_ffd$l  <- mm2[,2]
exp2_pt_ffd$c1 <- mm2[,4] # D0 - (D33 + D66)/2
exp2_pt_ffd$c2 <- mm2[,3] # D33-66

m.exp2.pt_ffd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                      REML=FALSE, data=exp2_pt_ffd)

#### SFD
exp2$dv <- log(exp2$pt_sfd)
exp2_pt_sfd <- filter(exp2, !is.na(dv))

contrasts(exp2_pt_sfd$load) <- contr.helmert(2)
contrasts(exp2_pt_sfd$prev) <- contr.helmert(3)

mm2 <- model.matrix(dv ~ load*prev, data=exp2_pt_sfd)
exp2_pt_sfd$l  <- mm2[,2]
exp2_pt_sfd$c1 <- mm2[,4] # D0 - (D33 + D66)/2
exp2_pt_sfd$c2 <- mm2[,3] # D33-66

m.exp2.pt_sfd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                      REML=FALSE, data=exp2_pt_sfd)
   
#### GD
exp2$dv <- log(exp2$pt_gaze.c)
exp2_pt_gd <- filter(exp2, !is.na(dv))

contrasts(exp2_pt_gd$load) <- contr.helmert(2)
contrasts(exp2_pt_gd$prev) <- contr.helmert(3)

mm2 <- model.matrix(dv ~ load*prev, data=exp2_pt_gd)
exp2_pt_gd$l  <- mm2[,2]
exp2_pt_gd$c1 <- mm2[,4] # D0 - (D33 + D66)/2
exp2_pt_gd$c2 <- mm2[,3] # D33-66

m_exp2.pt_gd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                 REML=FALSE, data=exp2_pt_gd)

## LMM TARGET EXPERIMENT 1
#### FFD
exp1$dv <- log(exp1$t_ffd.c)
exp1_ffd <- filter(exp1, !is.na(dv))

contrasts(exp1_ffd$load) <- contr.helmert(2)
contrasts(exp1_ffd$prev) <- contr.helmert(3)

mm1 <- model.matrix(dv ~ load*prev, data=exp1_ffd)
exp1_ffd$l  <- mm1[,2]
exp1_ffd$c1 <- mm1[,4] # D0 - (D33 + D66)/2
exp1_ffd$c2 <- mm1[,3] # D33-66

m.exp1.t_ffd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                     REML=FALSE, data=exp1_ffd)
#### SFD
exp1$dv <- log(exp1$t_sfd)
exp1_sfd <- filter(exp1, !is.na(dv))

contrasts(exp1_sfd$load) <- contr.helmert(2)
contrasts(exp1_sfd$prev) <- contr.helmert(3)

mm1.sfd <- model.matrix(dv ~ load*prev, data=exp1_sfd)
exp1_sfd$l  <- mm1.sfd[,2]
exp1_sfd$c1 <- mm1.sfd[,4] # D0 - (D33 + D66)/2
exp1_sfd$c2 <- mm1.sfd[,3] # D33-66

m.exp1.t_sfd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                     REML=FALSE, data=exp1_sfd)

# compute correlations which serves the estimation of effect sizes, which we retrieved from: 
# https://www.psychometrica.de/effect_size.html
agg_exp1_sfd <- aggregate(t_sfd ~ sbj + load, FUN="mean", data=exp1_sfd)
cor(agg_exp1_sfd$t_sfd[agg_exp1_sfd$load=="L"], agg_exp1_sfd$t_sfd[agg_exp1_sfd$load=="H"])

#### GD
exp1$dv <- log(exp1$t_gaze.c)
exp1_gd <- filter(exp1, !is.na(dv))

contrasts(exp1_gd$load) <- contr.helmert(2)
contrasts(exp1_gd$prev) <- contr.helmert(3)

mm1.gd <- model.matrix(dv ~ load*prev, data=exp1_gd)
exp1_gd$l  <- mm1.gd[,2]
exp1_gd$c1 <- mm1.gd[,4] # D0 - (D33 + D66)/2
exp1_gd$c2 <- mm1.gd[,3] # D33-66

m.exp1.t_gd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                    REML=FALSE, data=exp1_gd)
# compute correlations which serves the estimation of effect sizes
agg_exp1_gd <- aggregate(t_gaze.c ~ sbj + load, FUN="mean", data=exp1_gd)
cor(agg_exp1_gd$t_gaze.c[agg_exp1_gd$load=="L"], agg_exp1_gd$t_gaze.c[agg_exp1_gd$load=="H"])

## LMM TARGET EXPERIMENT 2
#### FFD
exp2$dv <- log(exp2$t_ffd.c)
exp2_ffd <- filter(exp2, !is.na(dv))

contrasts(exp2_ffd$load) <- contr.helmert(2)
contrasts(exp2_ffd$prev) <- contr.helmert(3)

mm2 <- model.matrix(dv ~ load*prev, data=exp2_ffd)
exp2_ffd$l  <- mm2[,2]
exp2_ffd$c1 <- mm2[,4] # D0 - (D33 + D66)/2
exp2_ffd$c2 <- mm2[,3] # D33-66

m.exp2.t_ffd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                     REML=FALSE, data=exp2_ffd)

# compute correlations which serves the estimation of effect sizes
agg_exp2_ffd <- aggregate(t_ffd.c ~ sbj + prev, FUN="mean", data=exp2_ffd)
agg_exp2_ffd_val <- subset(agg_exp2_ffd, agg_exp2_ffd$prev == 'D0')
agg_exp2_ffd_inv <- aggregate(t_ffd.c ~ sbj, FUN="mean", data=agg_exp2_ffd)
cor(agg_exp2_ffd_val$t_ffd.c, agg_exp2_ffd_inv$t_ffd.c)

#### SFD
exp2$dv <- log(exp2$t_sfd)
exp2_sfd <- filter(exp2, !is.na(dv))

contrasts(exp2_sfd$load) <- contr.helmert(2)
contrasts(exp2_sfd$prev) <- contr.helmert(3)

mm2.sfd <- model.matrix(dv ~ load*prev, data=exp2_sfd)
exp2_sfd$l  <- mm2.sfd[,2]
exp2_sfd$c1 <- mm2.sfd[,4] # D0 - (D33 + D66)/2
exp2_sfd$c2 <- mm2.sfd[,3] # D33-66

m.exp2.t_sfd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                     REML=FALSE, data=exp2_sfd)

# compute correlations which serves the estimation of effect sizes
agg_exp2_sfd <- aggregate(t_sfd ~ sbj + load, FUN="mean", data=exp2_sfd)
cor(agg_exp2_sfd$t_sfd[agg_exp2_sfd$load=="L"], agg_exp2_sfd$t_sfd[agg_exp2_sfd$load=="H"])

#### GD
exp2$dv <- log(exp2$t_gaze.c)
exp2_gd <- filter(exp2, !is.na(dv))

contrasts(exp2_gd$load) <- contr.helmert(2)
contrasts(exp2_gd$prev) <- contr.helmert(3)

mm2.gd <- model.matrix(dv ~ load*prev, data=exp2_gd)
exp2_gd$l  <- mm2.gd[,2]
exp2_gd$c1 <- mm2.gd[,4] # D0 - (D33 + D66)/2
exp2_gd$c2 <- mm2.gd[,3] # D33-66

m.exp2.t_gd <- lmer(dv ~ 1 + l * (c1 + c2) + (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                    REML=FALSE, data=exp2_gd)


# compute correlations which serves the estimation of effect sizes
agg_exp2_gd <- aggregate(t_gaze.c ~ sbj + prev, FUN="mean", data=exp2_gd)
agg_exp2_gd_val <- subset(agg_exp2_gd, agg_exp2_gd$prev == 'D0')
agg_exp2_gd_inv <- aggregate(t_gaze.c ~ sbj, FUN="mean", data=agg_exp2_gd)
cor(agg_exp2_gd_val$t_gaze.c, agg_exp2_gd_inv$t_gaze.c)

agg_exp2_gd <- aggregate(t_gaze.c ~ sbj + load, FUN="mean", data=exp2_gd)
cor(agg_exp2_gd$t_gaze.c[agg_exp2_gd$load=="L"], agg_exp2_gd$t_gaze.c[agg_exp2_gd$load=="H"])

## Add display change awareness to the models
#### EXPERIMENT 1
###### FFD
exp1$dv <- log(exp1$t_ffd.c)
exp1_ffd <- filter(exp1, !is.na(dv))

mm1.ffd.dca <- model.matrix(dv ~ dca*load*prev, data=exp1_ffd)

contrasts(exp1_ffd$dca) <- contr.helmert(2)
contrasts(exp1_ffd$load) <- contr.helmert(2)
contrasts(exp1_ffd$prev) <- contr.helmert(3)

mm1.ffd <- model.matrix(dv ~ dca*load*prev, data=exp1_ffd)
exp1_ffd$dca  <- mm1.ffd[,2]
exp1_ffd$l  <- mm1.ffd[,3]
exp1_ffd$c1 <- mm1.ffd[,5] # D0 - (D33 + D66)/2
exp1_ffd$c2 <- mm1.ffd[,4] # D33-66

m.exp1.ffd.dca <- lmer(dv ~ 1 + dca * l * (c1 + c2) + 		 
	              (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                      REML=FALSE, data=exp1_ffd)

###### SFD
exp1$dv <- log(exp1$t_sfd)
exp1_sfd <- filter(exp1, !is.na(dv))

mm1.sfd.dca <- model.matrix(dv ~ dca*load*prev, data=exp1_sfd)

contrasts(exp1_sfd$dca) <- contr.helmert(2)
contrasts(exp1_sfd$load) <- contr.helmert(2)
contrasts(exp1_sfd$prev) <- contr.helmert(3)

mm1.sfd <- model.matrix(dv ~ dca*load*prev, data=exp1_sfd)
exp1_sfd$f_dca  <- mm1.sfd[,2]
exp1_sfd$l  <- mm1.sfd[,3]
exp1_sfd$c1 <- mm1.sfd[,5] # D0 - (D33 + D66)/2
exp1_sfd$c2 <- mm1.sfd[,4] # D33-66

m.exp1.sfd.dca <- lmer(dv ~ 1 + f_dca * l * (c1 + c2) + 		 
	  	      (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                      REML=FALSE, data=exp1_sfd)

agg_exp1_t_sfd <- aggregate(t_sfd ~ sbj + dca + prev, FUN=mean, data=exp1_sfd)
mean(agg_exp1_t_sfd$t_sfd[agg_exp1_t_sfd$dca == 'U'  & agg_exp1_t_sfd$prev == 'DP'])
mean(agg_exp1_t_sfd$t_sfd[agg_exp1_t_sfd$dca == 'A'  & agg_exp1_t_sfd$prev == 'DP'])

mean(agg_exp1_t_sfd$t_sfd[agg_exp1_t_sfd$dca == 'U'  & agg_exp1_t_sfd$prev == 'SP'])
mean(agg_exp1_t_sfd$t_sfd[agg_exp1_t_sfd$dca == 'A'  & agg_exp1_t_sfd$prev == 'SP'])

###### GD
exp1$dv <- log(exp1$t_gaze.c)
exp1_gd <- filter(exp1, !is.na(dv))

mm1.gd.dca <- model.matrix(dv ~ dca*load*prev, data=exp1_gd)

contrasts(exp1_gd$dca) <- contr.helmert(2)
contrasts(exp1_gd$load) <- contr.helmert(2)
contrasts(exp1_gd$prev) <- contr.helmert(3)

mm1.gd <- model.matrix(dv ~ dca*load*prev, data=exp1_gd)
exp1_gd$f_dca  <- mm1.gd[,2]
exp1_gd$l  <- mm1.gd[,3]
exp1_gd$c1 <- mm1.gd[,5] # D0 - (D33 + D66)/2
exp1_gd$c2 <- mm1.gd[,4] # D33-66

m.exp1.gd.dca <- lmer(dv ~ 1 + f_dca * l * (c1 + c2) + 		 
		     (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                     REML=FALSE, data=exp1_gd)

# compute the means which we reported in the ms for the interaction DCA X TYPE OF PREVIEW
agg_exp1_t_gd <- aggregate(t_gaze.c ~ sbj + dca + prev, FUN=mean, data=exp1_gd)
mean(agg_exp1_t_gd$t_gaze.c[agg_exp1_t_gd$dca == 'U'  & agg_exp1_t_gd$prev == 'DP'])
mean(agg_exp1_t_gd$t_gaze.c[agg_exp1_t_gd$dca == 'A'  & agg_exp1_t_gd$prev == 'DP'])

mean(agg_exp1_t_gd$t_gaze.c[agg_exp1_t_gd$dca == 'U'  & agg_exp1_t_gd$prev == 'SP'])
mean(agg_exp1_t_gd$t_gaze.c[agg_exp1_t_gd$dca == 'A'  & agg_exp1_t_gd$prev == 'SP'])

#### EXPERIMENT 2
###### FFD
exp2$dv <- log(exp2$t_ffd.c)
exp2_ffd <- filter(exp2, !is.na(dv))

mm2.ffd.dca <- model.matrix(dv ~ dca*load*prev, data=exp2_ffd)

contrasts(exp2_ffd$dca) <- contr.helmert(2)
contrasts(exp2_ffd$load) <- contr.helmert(2)
contrasts(exp2_ffd$prev) <- contr.helmert(3)

mm2.ffd <- model.matrix(dv ~ dca*load*prev, data=exp2_ffd)
exp2_ffd$dca  <- mm2.ffd[,2]
exp2_ffd$l  <- mm2.ffd[,3]
exp2_ffd$c1 <- mm2.ffd[,5] # D0 - (D33 + D66)/2
exp2_ffd$c2 <- mm2.ffd[,4] # D33-66

m.exp2.ffd.dca <- lmer(dv ~ 1 + dca * l * (c1 + c2) + 		 
		      (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                      REML=FALSE, data=exp2_ffd)

###### SFD
exp2$dv <- log(exp2$t_sfd)
exp2_sfd <- filter(exp2, !is.na(dv))

mm2.sfd.dca <- model.matrix(dv ~ dca*load*prev, data=exp2_sfd)

contrasts(exp2_sfd$dca) <- contr.helmert(2)
contrasts(exp2_sfd$load) <- contr.helmert(2)
contrasts(exp2_sfd$prev) <- contr.helmert(3)

mm2.sfd <- model.matrix(dv ~ dca*load*prev, data=exp2_sfd)
exp2_sfd$f_dca  <- mm2.sfd[,2]
exp2_sfd$l  <- mm2.sfd[,3]
exp2_sfd$c1 <- mm2.sfd[,5] # D0 - (D33 + D66)/2
exp2_sfd$c2 <- mm2.sfd[,4] # D33-66

m.exp1.sfd.dca <- lmer(dv ~ 1 + f_dca * l * (c1 + c2) + 		 
		      (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                      REML=FALSE, data=exp2_sfd)

###### GD
exp2$dv <- log(exp2$t_gaze.c)
exp2_gd <- filter(exp2, !is.na(dv))

mm2.gd.dca <- model.matrix(dv ~ dca*load*prev, data=exp2_gd)

contrasts(exp2_gd$dca) <- contr.helmert(2)
contrasts(exp2_gd$load) <- contr.helmert(2)
contrasts(exp2_gd$prev) <- contr.helmert(3)

mm2.gd <- model.matrix(dv ~ dca*load*prev, data=exp2_gd)
exp2_gd$dca  <- mm2.gd[,2]
exp2_gd$l  <- mm2.gd[,3]
exp2_gd$c1 <- mm2.gd[,5] # D0 - (D33 + D66)/2
exp2_gd$c2 <- mm2.gd[,4] # D33-66

m.exp1.gd.dca <- lmer(dv ~ 1 + dca * l * (c1 + c2) + 		 
		     (1 + l + c1 || sbj) + (1 + l + c1 || stim),
                     REML=FALSE, data=exp2_gd)

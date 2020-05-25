##########################################################
# R code to reproduce analyses reported in 
# Russian Sentence Corpus: Benchmark measures of eye movements in reading in Russian, 
# submitted to the Behavior Research Methods
# Written by Anna Laurinavichyute
# Email: annlaurin@gmail.com
# Please email me if you see any errors or have any questions
# Last update: 08.02.2018
##########################################################

rm(list=ls())
if(!require(MASS)){install.packages('MASS')} 
if(!require(lme4)){install.packages('lme4')} 
if(!require(sjPlot)){install.packages('sjPlot')} 
source('vif.R')

data <- read.csv("data.csv", sep = "\t", encoding = "UTF-8", 
                  na.strings = c("NA"), header = TRUE)

# To select target words
# data <- droplevels(data[data$target.word == 1,])

# Center and not scale word length
data$len.scaled <- scale(data$wl_nocomma, center = TRUE, scale = F)

# Conflate several parts of speech tags into more general
levels(data$POS)[levels(data$POS) == "SPRO" ] <- "S"
levels(data$POS)[levels(data$POS) == "ANUM" ] <- "A"
levels(data$POS)[levels(data$POS) == "ADVPRO" ] <- "ADV"
levels(data$POS)[levels(data$POS) == "APRO" ] <- "A"
levels(data$POS)[levels(data$POS) == "CONJ" ] <- "PR"
levels(data$POS)[levels(data$POS) == "PART" ] <- "PR"
levels(data$POS)[levels(data$POS) == "NUM" ] <- "PR"

# Make verb the basis of comparison
data$POS <- relevel(data$POS, "V")


###############################################################################
#                         Linear mixed effects models
###############################################################################

# ------------------------ First fixation duration ----------------------------

data.ffd <- data[data$twoplus_fix == 1 & data$IA_SKIP == 0,]
data.ffd <- data.ffd[! is.na(data.ffd$logfreq),]

# Simple model by Kliegl'04
ffd_K04.mod <- lmer(log(IA_FIRST_FIXATION_DURATION) ~    
                  logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                  (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                data = data.ffd, 
                control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(ffd_K04.mod))
summary(ffd_K04.mod)
vif.mer(ffd_K04.mod)

# Simple model by Kliegl'06
ffd_K06.mod <- lmer(log(IA_FIRST_FIXATION_DURATION) ~    
                  logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                  next.len.scaled + next.logfreq + next.logit.acc +
                  prev.len.scaled + prev.logfreq + prev.logit.acc +
                  landing + ao +
                  (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                data = data.ffd, REML = FALSE,
                control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(ffd_K06.mod))
summary(ffd_K06.mod)
vif.mer(ffd_K06.mod)


# Extended model based on model by Kliegl'06
ffd.mod <- lmer(log(IA_FIRST_FIXATION_DURATION) ~    
                  logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                  next.len.scaled + next.logfreq + next.logit.acc +
                  prev.len.scaled + prev.logfreq + prev.logit.acc +
                  ao + landing +
                  rel.position + POS + ambig + base.form + 
                  (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                data = data.ffd, REML = FALSE,
                control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(ffd.mod))
summary(ffd.mod)
vif.mer(ffd.mod)

# Stepwise model comparison
null <- update(ffd.mod, . ~ . - (rel.position + POS + ambig + base.form))
nullRP <- update(ffd.mod, . ~ . - (POS + ambig + base.form))
nullRPPOS <- update(ffd.mod, . ~ . - (ambig + base.form))
nullRPPOSAMB <-  update(ffd.mod, . ~ . - base.form)

anova(null, nullRP)
anova(nullRP, nullRPPOS)
anova(nullRPPOS, nullRPPOSAMB)
anova(nullRPPOSAMB, ffd.mod)


# ------------------------- Single fixation duration --------------------------

data.sfd <- data[data$one_fix == 1 & data$IA_SKIP == 0,]
data.sfd <- data.sfd[! is.na(data.sfd$logfreq),]

# Simple model by Kliegl'04
sfd_K04.mod <- lmer(log(IA_FIRST_FIXATION_DURATION) ~    
                  logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                  (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                data = data.sfd, 
                control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(sfd_K04.mod))
summary(sfd_K04.mod)
vif.mer(sfd_K04.mod)

# Simple model by Kliegl'06
sfd_K06.mod <- lmer(log(IA_FIRST_FIXATION_DURATION) ~    
                  logfreq + logit.acc + len.scaled +
                  next.len.scaled + next.logfreq + next.logit.acc +
                  prev.len.scaled + prev.logfreq + prev.logit.acc +
                  ao + landing +
                  (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                data = data.sfd, REML = FALSE,
                control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(sfd_K06.mod))
summary(sfd_K06.mod)
vif.mer(sfd_K06.mod)


# Extended model based on model by Kliegl'06
sfd.mod <- lmer(log(IA_FIRST_FIXATION_DURATION) ~    
                  logfreq + logit.acc + len.scaled +
                  next.len.scaled + next.logfreq + next.logit.acc +
                  prev.len.scaled + prev.logfreq + prev.logit.acc +
                  ao + landing +
                  rel.position + POS + ambig + base.form + 
                  (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                data = data.sfd, REML = FALSE,
                control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(sfd.mod))
summary(sfd.mod)
vif.mer(sfd.mod)

# Stepwise model comparison
null <- update(sfd.mod, . ~ . - (rel.position + POS + ambig + base.form))
nullRP <- update(sfd.mod, . ~ . - (POS + ambig + base.form))
nullRPPOS <- update(sfd.mod, . ~ . - (ambig + base.form))
nullRPPOSAMB <-  update(sfd.mod, . ~ . - base.form)

anova(null, nullRP)
anova(nullRP, nullRPPOS)
anova(nullRPPOS, nullRPPOSAMB)
anova(nullRPPOSAMB, sfd.mod)



# ------------------------------ Gaze duration --------------------------------

data.gd <- data[data$IA_FIRST_RUN_DWELL_TIME != 0 & 
                   !(is.na(data$IA_FIRST_RUN_DWELL_TIME)),]
data.gd <- data.gd[! is.na(data.gd$logfreq),]

# Kliegl'04
gd_K04.mod <- lmer(log(IA_FIRST_RUN_DWELL_TIME) ~    
                 logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                 (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
               data = data.gd, 
               control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(gd_K04.mod))
summary(gd_K04.mod)
vif.mer(gd_K04.mod)

# Kliegl'06
gd_K06.mod <- lmer(log(IA_FIRST_RUN_DWELL_TIME) ~    
                 logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                 next.len.scaled + next.logfreq + next.logit.acc +
                 prev.len.scaled + prev.logfreq + prev.logit.acc +
                 landing + ao +
                 (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
               data = data.gd,  REML = FALSE,
               control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(gd_K06.mod))
summary(gd_K06.mod)
vif.mer(gd_K06.mod)


# Extended model based on model by Kliegl'06
gd.mod <- lmer(log(IA_FIRST_RUN_DWELL_TIME) ~    
                 logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                 next.len.scaled + next.logfreq + next.logit.acc +
                 prev.len.scaled + prev.logfreq + prev.logit.acc +
                 ao + landing +
                 rel.position + POS + ambig + base.form + 
                 (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
               data = data.gd,  REML = FALSE,
               control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(gd.mod))
summary(gd.mod)
vif.mer(gd.mod)

# Stepwise model comparison
null <- update(gd.mod, . ~ . - (rel.position + POS + ambig + base.form))
nullRP <- update(gd.mod, . ~ . - (POS + ambig + base.form))
nullRPPOS <- update(gd.mod, . ~ . - (ambig + base.form))
nullRPPOSAMB <-  update(gd.mod, . ~ . - base.form)

anova(null, nullRP)
anova(nullRP, nullRPPOS)
anova(nullRPPOS, nullRPPOSAMB)
anova(nullRPPOSAMB, gd.mod)


# ---------------------------- Total viewing time -----------------------------

data.tt <- data[data$IA_DWELL_TIME != 0,]
data.tt <- data.tt[! is.na(data.tt$logfreq),]

#Kliegl'04
tt_K04.mod <- lmer(log(IA_DWELL_TIME) ~    
                 logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                 (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
               data = data.tt, 
               control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(tt_K04.mod))
summary(tt_K04.mod)
vif.mer(tt_K04.mod)

# Kliegl'06
tt_K06.mod <- lmer(log(IA_DWELL_TIME) ~    
                 logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                 next.len.scaled + next.logfreq + next.logit.acc +
                 prev.len.scaled + prev.logfreq + prev.logit.acc +
                 landing + ao +
                 (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
               data = data.tt,  REML = FALSE,
               control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(tt_K06.mod))
summary(tt_K06.mod)
vif.mer(tt_K06.mod)


# Extended model based on model by Kliegl'06
tt.mod <- lmer(log(IA_DWELL_TIME) ~    
                 logfreq + logit.acc + poly(len.scaled, degree = 2, raw = T) + 
                 next.len.scaled + next.logfreq + next.logit.acc +
                 prev.len.scaled + prev.logfreq + prev.logit.acc +
                 landing + ao +
                 rel.position +  POS + ambig + base.form + 
                 (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
               data = data.tt,  REML = FALSE,
               control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(tt.mod))
summary(tt.mod)
vif.mer(tt.mod)

# Stepwise model comparison
null <- update(tt.mod, . ~ . - (rel.position + POS + ambig + base.form))
nullRP <- update(tt.mod, . ~ . - (POS + ambig + base.form))
nullRPPOS <- update(tt.mod, . ~ . - (ambig + base.form))
nullRPPOSAMB <-  update(tt.mod, . ~ . - base.form)

anova(null, nullRP)
anova(nullRP, nullRPPOS)
anova(nullRPPOS, nullRPPOSAMB)
anova(nullRPPOSAMB, tt.mod)



###############################################################################
#                                Comparison table 
###############################################################################

sjt.lmer(ffd.mod, sfd.mod, gd.mod, tt.mod,
         digits.est = 3, digits.ci = 3, digits.se = 3,
         separate.ci.col = FALSE,
         show.icc = F,
         sep.column = F, cell.spacing = 0.07,
         show.se = T, show.ci = F,
         string.est = "Estimate",
         file = "table.html",
         remove.spaces = T,
         depvar.labels = c("log FFD", "log SFD", "log GD", "log TT"),
         pred.labels = c("Log frequency", "Logit predictability",
                             "Length", "Length squared",
                             "n+1 length", "n+1 log frequency",
                             "n+1 predictability", "n-1 length",
                             "n-1 log frequency", "n-1 predictability",
                             "Word position in the sentence",
                             "Adjective", "Adverb", "Function word",
                             "Noun", "Ambiguity", "Base form",
                             "Incoming saccade amplitude"))



###############################################################################
#                         Mixed effects logistic models
###############################################################################

#
#                                   CAUTION!!!
#                   These models take very long to converge!
#


# --------------------- Probability of skipping the word  ---------------------

skip.mod <- glmer(IA_SKIP ~ logfreq + logit.acc + 
                    poly(len.scaled, degree = 2, raw = TRUE) + 
                    (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                  data = data, family = binomial,
                  control = glmerControl(optimizer = "bobyqa"))
summary(skip.mod)
vif.mer(skip.mod)

# ----------- Probability of having only one fixation on the word  ------------

onefix.mod <- glmer(one_fix ~ logfreq + logit.acc + 
                      poly(len.scaled, degree = 2, raw = TRUE) + 
                      (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                    data = data, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(onefix.mod)
vif.mer(onefix.mod)

# -----Probability of having two or more fixations on the word  ---------------

twofix.mod <- glmer(twoplus_fix ~ logfreq + logit.acc +
                      poly(len.scaled, degree = 2, raw = TRUE) + 
                      (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                    data = data, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))
summary(twofix.mod)
vif.mer(twofix.mod)


# -------Probability the word being an origin of a regressive saccade ---------

RO.mod <- glmer(IA_REGRESSION_OUT_FULL ~ logfreq + logit.acc + 
                  poly(len.scaled, degree = 2, raw = TRUE) + 
                  (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                data = data, family = binomial,
                control = glmerControl(optimizer = "bobyqa"))
summary(RO.mod)
vif.mer(RO.mod)


# ------- Probability the word being a goal of a regressive saccade -----------

RG.mod <- glmer(IA_REGRESSION_IN ~ logfreq + logit.acc + 
                  poly(len.scaled, degree = 2, raw = TRUE) + 
                  (1 | DATA_FILE) + (1 | item.id) + (1 | word.id), 
                data = data, family = binomial,
                control = glmerControl(optimizer = "bobyqa"))
summary(RG.mod) 
vif.mer(RG.mod)


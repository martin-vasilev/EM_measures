# =========================================================================================
# Program x_analyses.R
# =========================================================================================
# PSC data
# Word position effect
#
# M. Dambacher, March 2009
# =========================================================================================

rm(list=ls())
library(lme4)

# --- INPUT EXPERIMENT 1 ------------------------------------------------------------------
load("psc.data.rda")
# ------------------------------------------------------------------------------


# --- LMER -------------------------------------------------
psc.rel = lmer(dur ~ poly(relwn,3) +  f   + l   + p   + x +
                                      f1  + l1  + p1 + x1 +
                                      f2  + l2  + p2 + x2 +
                                      ao1 + ao + o + oq +
                                      (1 | id) + (1 | sn) + (1 | wid),
                                      data = dx)
print(psc.rel, corr = F)
#key to variables:	relwn - relative word position
#				f, l, p, x - frequency, length, cloze probability and lexical status (function/content) for word N
#				f1, l1, p1, x1 - frequency, length, cloze probability and lexical status (function/content) for word N-1
#				f2, l2, p2, x2 - frequency, length, cloze probability and lexical status (function/content) for word N+1
#				ao1 - incoming saccade amplitude
#				ao - outgoing saccade amplitude
#				o, oq - viewing position (linear and quadratic components)
#				id, sn, wid - subject id, sentence id, word id.
# ------------------------------------------------------------------------------


# --- INPUT EXPERIMENT 2 ------------------------------------------------------------------
load("demonic.rda")
# ------------------------------------------------------------------------------


# --- LMER -------------------------------------------------
single.lmer = lmer(GazeDur ~ poly(WordPos,3) +	WordFreq + WordLength + ClozeProb + LexStatus + 
								FreqPrev + LenPrev + LexPrev + ClozePrev + 
								FreqNext + LenNext + LexNext + ClozeNext + 
								PrecSaccade + FolSaccade + 
								(1 | Subject) + (1 | Word) + (1 | TrialID), 
								data = single)
print(single.lmer, corr = F)

# ------------------------------------------------------------------------------


# --- INPUT EXPERIMENT 3 ------------------------------------------------------------------
load("dmorph.rda")
# ------------------------------------------------------------------------------


# --- LMER -------------------------------------------------

aff.lmer = lmer(FixDur ~ poly(WordPos, 3) +	WordFreq + WordLength + LexStatus + 
								FreqPrev + LenPrev + LexPrev + 
								LenNext + FreqNext + LexNext + 
								PrecSaccade + FolSaccade +  
								(1 | Subject) + (1 | Word) + (1 | TrialID), 
								data = affpos1)
print(aff.lmer, corr = F)



# ------------------------------------------------------------------------------


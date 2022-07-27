library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)

# read in EMA data
dat = read.delim("./CleanedData_merged_addedCategoryCodes_addprev+next.txt", stringsAsFactors=F)

# add gender and age for covariates
qualtrics = read.delim("./DiscrimStudy_Qualtrics_clean_fixedGender_HSdiv_withScaleTotals_deindentified.txt") %>% 
  rename(Subject = SubID)

dat = left_join(dat, select(qualtrics, Subject, Gender, Age), by = "Subject")

# Make day of week a factor
dat$DayWeek.d = factor(dat$DayWeek.d)

# effect code Gender
dat$Gender.e = ifelse(dat$Gender == 2, -1, # female = -1; male = 1; trans/non-binary = 0
                      ifelse(dat$Gender == 1, 1, 0))

# center age around median (20 y/o)
median(dat$Age, na.rm=T)
dat$Age.center = dat$Age - 20

# 1. Test effect of discrimination on mood (same prompt) --------------------------------

# Effect of discrim on immediate affect, controlling for day of the week, n-1 prompt, gender, age

# PA
PA_same.model = lmer(PA_agg ~ DiscrimCheck.d + DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat)
PA_same.model.res = lmer(PA_agg ~ DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat)
PA_same = summary(PA_same.model) # summary
r.squaredGLMM(PA_same.model) # R squared
PA_same_CIs = confint(PA_same.model) # CIs
# effect size
(r.squaredGLMM(PA_same.model)[2] - r.squaredGLMM(PA_same.model.res)[2])/(1-r.squaredGLMM(PA_same.model)[2])

# NA
NA_same.model = lmer(NA_agg ~ DiscrimCheck.d + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) 
NA_same.model.res = lmer(NA_agg ~ DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) 
NA_same = summary(NA_same.model)
r.squaredGLMM(NA_same.model)
NA_same_CIs = confint(NA_same.model)
# effect size
(r.squaredGLMM(NA_same.model)[2] - r.squaredGLMM(NA_same.model.res)[2])/(1-r.squaredGLMM(NA_same.model)[2])

# Depression
Dep_same.model = lmer(Dep_agg ~ DiscrimCheck.d + DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat)
Dep_same.model.res = lmer(Dep_agg ~ DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat)
Dep_same = summary(Dep_same.model)
r.squaredGLMM(Dep_same.model)
Dep_same_CIs = confint(Dep_same.model)
# effect size
(r.squaredGLMM(Dep_same.model)[2] - r.squaredGLMM(Dep_same.model.res)[2])/(1-r.squaredGLMM(Dep_same.model)[2])

# Anxiety
Anx_same.model = lmer(Anx_agg ~ DiscrimCheck.d + DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat)
Anx_same.model.res = lmer(Anx_agg ~ DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat)
Anx_same = summary(Anx_same.model)
r.squaredGLMM(Anx_same.model)
Anx_same_CIs = confint(Anx_same.model)
# effect size
(r.squaredGLMM(Anx_same.model)[2] - r.squaredGLMM(Anx_same.model.res)[2])/(1-r.squaredGLMM(Anx_same.model)[2])

# unconditional ICCs (same prompt)
lmer(PA_agg ~ 1 + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
.5304/(.1567+.5304+.2409) # ICC for subject is .57
.1567/(.1567+.5304+.2409) # ICC for day is .17

lmer(NA_agg ~ 1 + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
.2345/(.0568+.2345+.0951) # ICC for subject is .61
.0568/(.0568+.2345+.0951) # ICC for day is .15

lmer(Dep_agg ~ 1 + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
.4330/(.1101+.4330+.1863) # ICC for subject is .59
.1101/(.1101+.4330+.1863) # ICC for day is .15

lmer(Anx_agg ~ 1 + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
.3700/(.0983+.3700+.2444) # ICC for subject is .52
.0983/(.0983+.3700+.2444) # ICC for day is .14


# 2. Test effect of discrimination on mood (next prompt) --------------------------------

# PA
PA_next.model = lmer(PA_agg_nextPrompt ~ DiscrimCheck.d + DiscrimCheck_nextPrompt.d + DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat)
PA_next.model.res = lmer(PA_agg_nextPrompt ~ DiscrimCheck_nextPrompt.d + DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat)
PA_next = summary(PA_next.model)
r.squaredGLMM(PA_next.model)
PA_next_CIs = confint(PA_next.model)
# effect size
(r.squaredGLMM(PA_next.model)[2] - r.squaredGLMM(PA_next.model.res)[2])/(1-r.squaredGLMM(PA_next.model)[2])

# NA
NA_next.model = lmer(NA_agg_nextPrompt ~ DiscrimCheck.d + DiscrimCheck_nextPrompt.d + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) 
NA_next.model.res = lmer(NA_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) 
NA_next = summary(NA_next.model)
r.squaredGLMM(NA_next.model)
NA_next_CIs = confint(NA_next.model)
# effect size
(r.squaredGLMM(NA_next.model)[2] - r.squaredGLMM(NA_next.model.res)[2])/(1-r.squaredGLMM(NA_next.model)[2])

# Depression
Dep_next.model = lmer(Dep_agg_nextPrompt ~ DiscrimCheck.d + DiscrimCheck_nextPrompt.d + DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) 
Dep_next.model.res = lmer(Dep_agg_nextPrompt ~ DiscrimCheck.d + DiscrimCheck_nextPrompt.d + DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) 
Dep_next = summary(Dep_next.model)
r.squaredGLMM(Dep_next.model)
Dep_next_CIs = confint(Dep_next.model)
# effect size
(r.squaredGLMM(Dep_next.model)[2] - r.squaredGLMM(Dep_next.model.res)[2])/(1-r.squaredGLMM(Dep_next.model)[2])

# Anxiety
Anx_next.model = lmer(Anx_agg_nextPrompt ~ DiscrimCheck.d + DiscrimCheck_nextPrompt.d + DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) 
Anx_next.model.res = lmer(Anx_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) 
Anx_next = summary(Anx_next.model)
r.squaredGLMM(Anx_next.model)
Anx_next_CIs = confint(Anx_next.model)
# effect size
(r.squaredGLMM(Anx_next.model)[2] - r.squaredGLMM(Anx_next.model.res)[2])/(1-r.squaredGLMM(Anx_next.model)[2])

# unconditional ICCs (next prompt)
lmer(PA_agg_nextPrompt ~ 1 + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
.5357/(.1737+.5357+.2257) # ICC for subject is .57
.1737/(.1737+.5357+.2257) # ICC for day is .19

lmer(NA_agg_nextPrompt ~ 1 + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
.2347/(.0591+.2347+.0937) # ICC for subject is .61
.0591/(.0591+.2347+.0937) # ICC for day is .15

lmer(Dep_agg_nextPrompt ~ 1 + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
.4123/(.1097+.4123+.1911) # ICC for subject is .58
.1097/(.1097+.4123+.1911) # ICC for day is .15

lmer(Anx_agg_nextPrompt ~ 1 + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
.355/(.1087+.3550+.2339) # ICC for subject is .51
.1087/(.1087+.3550+.2339) # ICC for day is .16

# 3. Test effect of discrimination on mood (next day) ---------------------

nextDat = read.delim("./CleanedData_merged_addedCategoryCodes_addprev+nextDay.txt", sep="\t")

# convert to factors
nextDat$DayWeek = factor(nextDat$DayWeek)
nextDat$Subject = factor(nextDat$Subject)
nextDat$DayStudy = factor(nextDat$DayStudy)

# PA
PA_nextday.model = lmer(PA_agg_nextDay ~ DiscrimCheck_day.d + DiscrimCheck_nextDay.d + DayWeek + PA_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDat) 
PA_nextday = summary(PA_nextday.model)
r.squaredGLMM(PA_nextday.model) #R squared

# NA
NA_nextday.model = lmer(NA_agg_nextDay ~ DiscrimCheck_day.d + DiscrimCheck_nextDay.d + DayWeek + NA_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDat) 
NA_nextday = summary(NA_nextday.model)
r.squaredGLMM(NA_nextday.model) #R squared

# Depression
Dep_nextday.model = lmer(Dep_agg_nextDay ~ DiscrimCheck_day.d + DiscrimCheck_nextDay.d + DayWeek + Dep_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDat)
Dep_nextday = summary(Dep_nextday.model)
r.squaredGLMM(Dep_nextday.model) #R squared

# Anxiety
Anx_nextday.model = lmer(Anx_agg_nextDay ~ DiscrimCheck_day.d + DiscrimCheck_nextDay.d + DayWeek + Anx_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDat)
Anx_nextday = summary(Anx_nextday.model)
r.squaredGLMM(Anx_nextday.model) #R squared

# unconditional ICCs (next day)
lmer(PA_agg_nextDay ~ 1 + (1|Subject), nextDat) %>% 
  summary()
.5469/(.5469+.2515) # ICC for subject is .68

lmer(NA_agg_nextDay ~ 1 + (1|Subject), nextDat) %>% 
  summary()
.2406/(.2406+.0977) # ICC for subject is .71

lmer(Dep_agg_nextDay ~ 1 + (1|Subject), nextDat) %>% 
  summary()
.4395/(.4395+.1914) # ICC for subject is .70

lmer(Anx_agg_nextDay ~ 1 + (1|Subject), nextDat) %>% 
  summary()
.3749/(.3749+.2038) # ICC for subject is .65

# Ancillary Analyses --------------------------------------------------------------------------


# Robustness check ----------------------------------------------------------------------------

# number of discim events reported by each subject

numEvents = select(dat, Subject, DiscrimCheck.d) %>% 
  group_by(Subject) %>% 
  summarize_all(sum) %>% 
  as.data.frame()

numEvents[numEvents$DiscrimCheck.d >= 15,] # 3 subjects  with 15 or more reported events

dat.noout = filter(dat, !(Subject %in% c(6, 20, 49)))

# Effect of discrim on same prompt outcomes, not including outlier Ps

PA.same.robust = lmer(PA_agg ~ DiscrimCheck.d + DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat.noout) 
summary(PA.same.robust)
confint(PA.same.robust)

NA.same.robust = lmer(NA_agg ~ DiscrimCheck.d + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat.noout) 
summary(NA.same.robust)
confint(NA.same.robust)

Dep.same.robust = lmer(Dep_agg ~ DiscrimCheck.d + DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat.noout) 
summary(Dep.same.robust)
confint(Dep.same.robust)

Anx.same.robust = lmer(Anx_agg ~ DiscrimCheck.d + DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat.noout) 
summary(Anx.same.robust)
confint(Anx.same.robust)

# Effect of discrim on next prompt outcomes, not including outlier Ps

PA.next.robust = lmer(PA_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat.noout)
summary(PA.next.robust)
confint(PA.next.robust)

lmer(NA_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat.noout) %>% 
  summary()
lmer(Dep_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat.noout) %>% 
  summary()
lmer(Anx_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat.noout) %>% 
  summary()

# Effect of discrim on next day outcomes, not including outlier Ps
nextDay.noout = filter(nextDat, !(Subject %in% c(6, 20, 49)))

# PA
lmer(PA_agg_nextDay ~ DiscrimCheck_day.d + DiscrimCheck_nextDay.d + DayWeek + PA_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDay.noout) %>% 
  summary()

# NA
lmer(NA_agg_nextDay ~ DiscrimCheck_day.d + DiscrimCheck_nextDay.d + DayWeek + NA_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDay.noout) %>% 
  summary()

# Depression
lmer(Dep_agg_nextDay ~ DiscrimCheck_day.d + DiscrimCheck_nextDay.d + DayWeek + Dep_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDay.noout) %>% 
  summary()

# Anxiety
lmer(Anx_agg_nextDay ~ DiscrimCheck_day.d + DiscrimCheck_nextDay.d + DayWeek + Anx_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDay.noout) %>% 
  summary()


# Examining attribution to race ---------------------------------------------------------------

# create continuous attibution to race variable
dat$DiscrimAttrib = dat$DiscrimCheck_Cause
dat$DiscrimAttrib[dat$DiscrimCheck_Cause == "0,1"] = .5
dat$DiscrimAttrib[dat$DiscrimCheck_Cause == "1,2"] = 1.5
dat$DiscrimAttrib[dat$DiscrimCheck_Cause == "2,3"] = 2.5
dat$DiscrimAttrib[dat$DiscrimCheck_Cause == "3,4"] = 3.5
dat$DiscrimAttrib[dat$DiscrimCheck_Cause == "1,4"] = 1
dat$DiscrimAttrib[dat$DiscrimCheck_Cause == "2,4"] = 2
dat$DiscrimAttrib[dat$DiscrimCheck_Cause == "0,2"] = 2
dat$DiscrimAttrib[dat$DiscrimCheck_Cause == "1,2,4"] = 1.5
dat$DiscrimAttrib[dat$DiscrimCheck_Cause == "4"] = NA
dat$DiscrimAttrib = as.numeric(dat$DiscrimAttrib)
hist(dat$DiscrimAttrib)
mean(dat$DiscrimAttrib, na.rm=T)

# center around "Probably because of my race but not certain"
dat$DiscrimAttrib = dat$DiscrimAttrib - 2

# same prompt: add attribution as moderator
lmer(PA_agg ~ DiscrimCheck.d + DiscrimCheck.d:DiscrimAttrib + DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(NA_agg ~ DiscrimCheck.d + DiscrimCheck.d:DiscrimAttrib + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(Dep_agg ~ DiscrimCheck.d + DiscrimCheck.d:DiscrimAttrib + DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(Anx_agg ~ DiscrimCheck.d + DiscrimCheck.d:DiscrimAttrib + DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()

## confidence intervals for effect on NA
lmer(NA_agg ~ DiscrimCheck.d + DiscrimCheck.d:DiscrimAttrib + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  confint()

# next prompt: add attribution as moderator
lmer(PA_agg_nextPrompt ~ DiscrimCheck.d + DiscrimCheck.d:DiscrimAttrib + DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(NA_agg_nextPrompt ~ DiscrimCheck.d + DiscrimCheck.d:DiscrimAttrib + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(Dep_agg_nextPrompt ~ DiscrimCheck.d + DiscrimCheck.d:DiscrimAttrib + DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(Anx_agg_nextPrompt ~ DiscrimCheck.d + DiscrimCheck.d:DiscrimAttrib + DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()




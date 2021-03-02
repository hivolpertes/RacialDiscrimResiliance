library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

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

# Models
lmer(PA_agg ~ DiscrimCheck.d + DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(NA_agg ~ DiscrimCheck.d + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(Dep_agg ~ DiscrimCheck.d + DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(Anx_agg ~ DiscrimCheck.d + DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()

# 2. Test effect of discrimination on mood (next prompt) --------------------------------

# controlling for day of week, n-1 prompt, gender, age
lmer(PA_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + PA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(NA_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + NA_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(Dep_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + Dep_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()
lmer(Anx_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + Anx_agg_prevPrompt + Gender.e + Age.center + (1|Subject) + (1|Subject:DayStudy), dat) %>% 
  summary()


# 3. Test effect of discrimination on mood (next day) ---------------------

nextDat = read.delim("./CleanedData_merged_addedCategoryCodes_addprev+nextDay.txt", sep="\t")

# convert to factors
nextDat$DayWeek = factor(nextDat$DayWeek)
nextDat$Subject = factor(nextDat$Subject)
nextDat$DayStudy = factor(nextDat$DayStudy)

# controlling for day of week, n-1 day, gender, age
lmer(PA_agg_nextDay ~ DiscrimCheck_day.d + DayWeek + PA_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDat) %>% 
  summary()
lmer(NA_agg_nextDay ~ DiscrimCheck_day.d + DayWeek + NA_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDat) %>% 
  summary()
lmer(Dep_agg_nextDay ~ DiscrimCheck_day.d + DayWeek + Dep_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDat) %>% 
  summary()
lmer(Anx_agg_nextDay ~ DiscrimCheck_day.d + DayWeek + Anx_agg_prevDay + Gender.e + Age.center + (1|Subject), nextDat) %>% 
  summary()


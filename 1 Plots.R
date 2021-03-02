library(tidyverse)
library(lme4)
library(emmeans)


# Figure 1: Histogram

dat = read.delim("./CleanedData_merged_addedCategoryCodes.txt", stringsAsFactors=F)
# DiscrimCheck.d = 0 -> no discrim reported on that prompt
# DiscrimCheck.d = 1 -> discrim reported on that prompt
num = group_by(dat, Subject) %>% 
  count(DiscrimCheck.d)
# histogram of number of events
discrim.events = filter(num, DiscrimCheck.d == 1)
# add participants who reported 0 events (row not included in num)
noeventsubs = count(num, Subject) %>% 
  filter(n ==1)
discrim.events = rbind(discrim.events, 
                       data.frame(Subject = noeventsubs$Subject,
                                  DiscrimCheck.d = 1,
                                  n = 0))

ggplot(discrim.events, aes(n)) +
  geom_histogram(fill = "white", color = "black", bins=20) +
  theme_bw() +
  scale_x_continuous(name="Number of events reported", breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  scale_y_continuous(name="Number of participants", breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_text(size=20),
        strip.text = element_text(size=16))

ggsave("./5 TigerAware/HistogramEventFrequency.jpg", width = 10, height = 7, unit="in")

# Figure 2: Immediate and lagged effects

dat = read.delim("./CleanedData_merged_addedCategoryCodes_addprev+next.txt", stringsAsFactors=F)

# Same prompt -------------------------------------------------------------

# Models
mod.PAsameP = lmer(PA_agg ~ DiscrimCheck.d + DayWeek.d + PA_agg_prevPrompt + (1|Subject) + (1|Subject:DayStudy), dat) 
mod.NAsameP = lmer(NA_agg ~ DiscrimCheck.d + DayWeek.d + NA_agg_prevPrompt + (1|Subject) + (1|Subject:DayStudy), dat) 
mod.DepsameP = lmer(Dep_agg ~ DiscrimCheck.d + DayWeek.d + Dep_agg_prevPrompt + (1|Subject) + (1|Subject:DayStudy), dat) 
mod.AnxsameP = lmer(Anx_agg ~ DiscrimCheck.d + DayWeek.d + Anx_agg_prevPrompt + (1|Subject) + (1|Subject:DayStudy), dat) 

# Estimated means
pred.PAsameP = emmeans(mod.PAsameP, poly ~ DiscrimCheck.d| DiscrimCheck.d, adjust="none")$emmeans %>% as.data.frame()
pred.NAsameP = emmeans(mod.NAsameP, poly ~ DiscrimCheck.d| DiscrimCheck.d, adjust="none")$emmeans %>% as.data.frame()
pred.DepsameP = emmeans(mod.DepsameP, poly ~ DiscrimCheck.d| DiscrimCheck.d, adjust="none")$emmeans %>% as.data.frame()
pred.AnxsameP = emmeans(mod.AnxsameP, poly ~ DiscrimCheck.d| DiscrimCheck.d, adjust="none")$emmeans %>% as.data.frame()

sameP.outcomes = rbind(pred.PAsameP, 
                       pred.NAsameP,
                       pred.DepsameP,
                       pred.AnxsameP)
sameP.outcomes$Outcome = c(rep("Positive Affect", 2),
                           rep("Negative Affect", 2),
                           rep("Depression", 2),
                           rep("Anxiety", 2))
sameP.outcomes$Prompt = "N"

# Next prompt -------------------------------------------------------------

# Models
mod.PAnextP = lmer(PA_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + PA_agg_prevPrompt + (1|Subject) + (1|Subject:DayStudy), dat)
mod.NAnextP = lmer(NA_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + NA_agg_prevPrompt + (1|Subject) + (1|Subject:DayStudy), dat) 
mod.DepnextP = lmer(Dep_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + Dep_agg_prevPrompt + (1|Subject) + (1|Subject:DayStudy), dat) 
mod.AnxnextP = lmer(Anx_agg_nextPrompt ~ DiscrimCheck.d + DayWeek.d + Anx_agg_prevPrompt + (1|Subject) + (1|Subject:DayStudy), dat) 

# Estimated means
pred.PAnextP = emmeans(mod.PAnextP, poly ~ DiscrimCheck.d| DiscrimCheck.d, adjust="none")$emmeans %>% as.data.frame()
pred.NAnextP = emmeans(mod.NAnextP, poly ~ DiscrimCheck.d| DiscrimCheck.d, adjust="none")$emmeans %>% as.data.frame()
pred.DepnextP = emmeans(mod.DepnextP, poly ~ DiscrimCheck.d| DiscrimCheck.d, adjust="none")$emmeans %>% as.data.frame()
pred.AnxnextP = emmeans(mod.AnxnextP, poly ~ DiscrimCheck.d| DiscrimCheck.d, adjust="none")$emmeans %>% as.data.frame()

nextP.outcomes = rbind(pred.PAnextP, 
                       pred.NAnextP,
                       pred.DepnextP,
                       pred.AnxnextP)
nextP.outcomes$Outcome = c(rep("Positive Affect", 2),
                           rep("Negative Affect", 2),
                           rep("Depression", 2),
                           rep("Anxiety", 2))
nextP.outcomes$Prompt = "N + 1"


# Plot --------------------------------------------------------------------

together.prompt = rbind(select(sameP.outcomes, -asymp.LCL, -asymp.UCL), 
                        select(nextP.outcomes, -lower.CL, -upper.CL))
together.prompt$Prompt = factor(together.prompt$Prompt)
together.prompt$DiscrimCheck.d = factor(together.prompt$DiscrimCheck.d)

ggplot(together.prompt, aes(Prompt, emmean, color = DiscrimCheck.d, shape = DiscrimCheck.d)) +
  geom_point(position=position_dodge(0), size = 5) +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.3,
                position=position_dodge(0)) +
  facet_wrap(~Outcome) +
  theme_bw() +
  xlab("Prompt") +
  ylab("Response") +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=22),
        strip.text = element_text(size=20),
        panel.grid = element_blank())

ggsave("./EstimatedMeans_PromptToPromptEffect_wide.jpg", width=11, height=6.6, units="in") # errorbar width = .3


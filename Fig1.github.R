###make plots for fasting presentation

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Fasting_data/")
library(data.table)
library(tidyverse)
library(survival)
library(survminer)
library(gmodels)
library(patchwork)

#startle response of f1 crosses
#we can compare accross sexes, try to replicate the figure in zhang 2018
survminer = readRDS("maysurvival data")#survival data from our Shang 2018 replcation study
survminer$sex = gsub("m", "male", survminer$sex)
survminer$sex = gsub("f", "female", survminer$sex)
##survival curves

#fix treatments
survminer$treatment = gsub("c", "control", survminer$treatment)
survminer$treatment = gsub("f", "fasted", survminer$treatment)
# write.csv(survminer, file = "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/March_2024_objects/C2.W118data.csv")
# setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/March_2024_objects/")
table(survminer$replicateindex)
dt <- survminer [sex == "female"]
fit <- survfit(Surv(hours,deathstatus)~treatment, data = dt)
#find test statics
stats = coxph(Surv(hours,deathstatus)~treatment, data = dt)
stats
summary(stats)
plot.f = ggsurvplot(fit,
                    pval = F, conf.int = TRUE,
                    risk.table = F, # Add risk table
                    risk.table.col = "strata", # Change risk table color by groups
                    linetype = "strata", # Change line type by groups
                    surv.median.line = "hv",
                    ggtheme = theme_bw(), # Change ggplot2 theme
                    pval.method = T,
                    font.tickslab = 13,
                    font.x = 15,
                    font.y = 15,
                    
                    #legend = c(.85,.8))
                    legend = "none")
plot.f
plot.f = plot.f$plot
#how different are the two groups
t.test(survminer[sex == "male"][treatment == "fasted"]$hours, survminer[sex == "female"][treatment == "fasted"]$hours)
dt <- survminer [sex == "male"]
fit <- survfit(Surv(hours,deathstatus)~treatment, data = dt)
stats = coxph(Surv(hours,deathstatus)~treatment, data = dt)
stats
plot.m = ggsurvplot(fit,
                    pval = F, conf.int = TRUE,
                    risk.table = F, # Add risk table
                    risk.table.col = "strata", # Change risk table color by groups
                    linetype = "strata", # Change line type by groups
                    surv.median.line = "hv", # Specify median survival
                    ggtheme = theme_bw(), # Change ggplot2 theme
                    pval.method = T,
                    legend = c(.70,.8),
                    font.x = 15,
                    font.y = 15,
                    font.tickslab = 13,
                    legend.title = "Treatment",
                    pval.coord = c(74, 0.50),
                    pval.method.coord = c(74,0.60)
)


plot.m
plot.m = plot.m$plot


colors = c("purple", "orange")
barplot = survminer %>%
  
  group_by(sex, treatment,# inversion.st
  ) %>%
  summarise(mean = ci(hours)[1],
            uci = ci(hours)[2],
            lci = ci(hours)[3]
  ) %>%
  ggplot(aes(
    x=treatment,
    y=mean,
    ymin=lci,
    ymax=uci,
    color =  sex
  )) +
  scale_color_manual(values = colors) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5)) +
  geom_point(position=position_dodge(width = 0.5)) +
  #facet_grid(knockout~., scales = "free_y")+
  xlab("Treatment") +
  ylab("Starv. resistance (hours)") +
  theme_bw() +
  theme(
    legend.position = c(.2, .8),
    legend.key.size = unit(.3, "inch"),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )
barplot
#plot the half lives of flies as box plots

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/")
ggsave((plot.m + plot.f + barplot) + plot_annotation(
  tag_levels = c("A")
), file =  "fig1.pdf", height = 4.5, width = 10)

#save all files
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/")
dt.part1 = dt
dt.part2 = dt3
fatdata
dt.part3 = dt
saveRDS(dt.part3, "Fig3data")

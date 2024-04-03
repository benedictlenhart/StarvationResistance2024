#create new figure three for pasting publication


library(data.table)
library(tidyverse)
library(gmodels)
library(survival)
library(survminer)
library(patchwork)
library(lme4)
library(devtools)
library(ggrepel)
library(RColorBrewer)
#devtools::install_github('cttobin/ggthemr')

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Fasting_data/")
color = brewer.pal(n =4, name = "Set2")
colors = rep(color, 2)
linetypes = c(rep("solid", 4), rep("dashed", 4))

#show SR improvement by latidude/ isofemales, and by inbred lines

isow1 = readRDS("w1survivaldata")
isow2 = readRDS("w2survivaldata")
isow2$week = 2
isow3 = readRDS("w3survivaldata")
dt = rbind(isow1, isow2, isow3)

dt$treatment = gsub("c", "control", dt$treatment)
dt$treatment = gsub("f", "fasted", dt$treatment)
dt$fullid = paste(dt$location, dt$season, sep = "-")
write.csv(dt, file = "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/March_2024_objects/Isofemalelinerawdata.csv")
#dt <- isofem.large
fit <- survfit(Surv(hours,deathstatus)~treatment, data = dt)
stats = coxph(Surv(hours,deathstatus)~treatment, data = dt[season == "summer"][location == "Pennsylvania"])
stats
plot = ggsurvplot(fit,
                  pval = F, conf.int = TRUE,
                  risk.table = F, # Add risk table
                  risk.table.col = "strata", # Change risk table color by groups
                  linetype = "strata", # Change line type by groups
                  surv.median.line = "hv", # Specify median survival
                  ggtheme = theme_bw(), # Change ggplot2 theme
                  pval.method = T,
                  legend = c(0.8,0.8),
                  font.tickslab = 13,
                  legend.title = "Treatment",
                  font.x = 15,
                  font.y = 15,
                  palette = c("#E7B800", "#2E9FDF", "darkorange1", "blue1"))
plot

survivalcurve1 = plot$plot
dt2 = dt %>% 
  mutate(newcolumn = tstrsplit(fullid, "-")) 

s%>% 
  mutate(newcolumn = newcolumn[2])

#plot survival curve of different lines
fit <- survfit(Surv(hours,deathstatus)~treatment+ fullid, data = dt)
stats = coxph(Surv(hours,deathstatus)~treatment + fullid, data = dt)
stats
plot = ggsurvplot(fit,
                  pval = F, conf.int = F,
                  risk.table = F, # Add risk table
                  risk.table.col = "strata", # Change risk table color by groups
                  linetype = linetypes, # Change line type by groups
                  #surv.median.line = "hv", # Specify median survival
                  ggtheme = theme_bw(), # Change ggplot2 theme
                  pval.method = T,
                  legend = "none",
                  font.tickslab = 13,
                  font.x = 15,
                  font.y = 15,
                  xlim = c(0,150),
                  palette = colors )
plot

survivalcurve2 = plot$plot




seasonlocation= dt %>%
  # filter(week == 1) %>% 
  group_by(season, treatment, location,fullid) %>%
  summarise(mean.hour = ci(hours)[1],
            uci = ci(hours)[2],
            lci = ci(hours)[3]
  ) %>%
  ggplot(aes(
    x=treatment,
    y=mean.hour,
    ymin=lci,
    ymax=uci,
    color =  fullid
  )) +  
  xlab("Treatment") +
  ylab("Survival Duration") +
  scale_color_manual(values = color) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = T) +
  geom_point(position=position_dodge(width = 0.5), show.legend = T) +
  geom_line(position=position_dodge(width = 0.5), aes(x = treatment, y = mean.hour, group = fullid, color = fullid)) +
  # facet_grid(.~location ,  scales = "free_y")+
  theme_bw() +
  theme(
    legend.position = c(.8, 0.8),
    #legend.title = element_text("Season/Location"),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
    # strip.background = element_blank(),
    # strip.text.x = element_blank(),
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank()
  )
seasonlocation

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/")
ggsave((survivalcurve1 + survivalcurve2) / (
  (seasonlocation + weekeffect) 
 # plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  ) +
         
         
         
         plot_annotation(
  tag_levels = c("A")
) , file = "figure3.1.pdf",
width = 11, height = 11)
####
#location stats
####
t.test(dt[season == "summer"][location == "Pennsylvania"][treatment == "f"]$hours , dt[season == "summer"][location == "Pennsylvania"][treatment == "c"]$hours)

#############################
##Supplement- week effect###
############################
# 
# dt$treatment = gsub("c", "control", dt$treatment)
# dt$treatment = gsub("f", "fasted", dt$treatment)
weekeffect = 
  ggplot(data = dt, aes(x = treatment, y = hours, color = week)) + geom_boxplot(outlier.shape =  NA) +
  xlab("Treatment")+
  ylab("Starvation resistance(hours)") +
  theme_bw() +
  theme(
    legend.position = c(0.85,0.85),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
    
  )
weekeffect
ggsave(weekeffect,file =  "weekeffectplog.pdf")
ggsave(seasonlocation, file = "seasonlocation.pdf")

#figure for fastign iso feamle lines
library(data.table)
library(tidyverse)
library(gmodels)
library(survival)
library(survminer)
library(patchwork)
library(lme4)
library(devtools)
library(ggrepel)
library(installr)
library(RColorBrewer)
#choose 7 colors for lies
color = brewer.pal(n = 7, name = "Set2")
colors = rep(color, 2)
linetypes = c(rep("solid", 7), rep("dashed", 7))
#try to do something similiar with inbred lines
setwd("/Users/supad/OneDrive/Documents/Bergland Research/Diamonds/October fasting assay/")

dt3 = readRDS("inbred.data.manual")
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Fasting_data/")

#fix labels
dt3 = dt3 %>% 
  mutate(lineid = case_when(grepl("r", lineid) == T ~ "Bahamas-1",
                            grepl("d", lineid) == T ~ "USA-Southeast-1",
                            grepl("a", lineid) == T ~ "USA-Southeast-2",
                            grepl("i", lineid) == T ~ "USA-Maine-1",
                            grepl("l", lineid) == T ~ "Netherlands-1",
                            grepl("q", lineid) == T ~ "Bahamas-2",
                            T ~ "Netherlands-2"
  ))


# write.csv(survminer, file = "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/March_2024_objects/Inbredrawdata.csv")
fit <- survfit(Surv(hours,death.status)~group + lineid, data = dt3)
stats = coxph(Surv(hours,death.status)~group + lineid, data = dt3)
stats
summary(stats)
plot = ggsurvplot(fit,
                  pval = F, 
                  risk.table = F, # Add risk table
                  risk.table.col = "strata", # Change risk table color by groups
                  linetype = linetypes, # Change line type by treatment
                  #surv.median.line = "hv", # Specify median survival
                  ggtheme = theme_bw(), # Change ggplot2 theme
                  pval.method = F,
                  conf.int = FALSE,
                  font.x = 15,
                  font.y = 15,
                  font.tickslab = 13,
                  #xlim = c(18.5, 60),
                  legend = "none",
                  palette = colors)

plot

survivalcurve1 = plot$plot

#try cox proportional hazards
plot = ggsurvplot(fit,
                  pval = F, 
                  risk.table = F, # Add risk table
                  risk.table.col = "strata", # Change risk table color by groups
                  linetype = "strata", # Change line type by groups
                  #surv.median.line = "hv", # Specify median survival
                  ggtheme = theme_bw(), # Change ggplot2 theme
                  pval.method = F,
                  font.x = 15,
                  font.y = 15,
                  legend.title = "Treatment",
                  font.tickslab = 13,
                  legend = c(0.8,0.8),
                  conf.int = T)

plot

survivalcurve2 = plot$plot
#look for significant gxe relationshiwp
dt3
modeladd = lm(hours ~ lineid + group, data = dt3)
modelmult = lm(hours ~ lineid + group + lineid * group, data = dt3)
summary(modelmult)
aovresult = anova(modeladd, modelmult, method)
summary(aovresult)
summary(testmodel)$adj.r.squared
#now we're going to try and see this but with improvement
var2 = dt3%>%
  
  group_by(lineid,group,# inversion.st 
  ) %>%
  summarise(mean = ci(hours)[1],
            uci = ci(hours)[2],
            lci = ci(hours)[3],
  ) %>%
  ggplot(aes(
    x=group,
    y=mean,
    ymin=lci,
    ymax=uci, 
    color = lineid
  )) +  
  xlab("Treatment") +
  ylab("Starv. resistance (hours)") +
  scale_color_manual(values = color) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  geom_point(position=position_dodge(width = 0.5), show.legend = T) + theme_bw() +
  geom_line(position=position_dodge(width = 0.5), aes(x = group, y = mean, group = lineid, color = lineid)) +
  theme(
    legend.position =  c(.8,-.2),
    legend.title = element_blank(),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15),
    #legend.key.size = unit(3,"line"),
    #legend.key.height = unit(4,"cm"),
    legend.direction = "horizontal"
    
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))
#look for significant differences between slopes
var2
mod.full = lm(formula = (hours ~ lineid * group), data = dt2)

mod.add1 = lm(formula = (hours ~ lineid + group), data = dt2)
#use random effects

bastats = anova(mod.full, mod.add1 ,  test="Chisq")
bastats

#look at variation in protein and fat
setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Fasting_data/")


fatdata = readRDS("octoberassayconcentrationdata")

write.csv(survminer, file = "/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/March_2024_objects/Fatconcentrationrawdata.csv")
# fatdata$treatment = gsub("c", "control", fatdata$treatment)
# fatdata$treatment = gsub("f", "fasted", fatdata$treatment)
fmerge = fatdata %>% 
  filter(timepoint == 2) %>% 
  mutate(replicateid = lineid.x) %>% 
  select( replicateid, fatbyprotein)
smerge = dt3 %>% 
  select(replicateid, hours,group) %>% 
  merge(., fmerge, by = c("replicateid"))
ggplot(smerge, aes(x = fatbyprotein, y = hours, color = group)) + geom_point()
cor.test(smerge$fatbyprotein, smerge$hours, method = "pearson")
cor(smerge$fatbyprotein, smerge$hours, method = "pearson")
fatdata$treatment = gsub("c", "control", fatdata$treatment)
fatdata$treatment = gsub("f", "fasted", fatdata$treatment)
#check correlation

var3 = fatdata%>%
  filter(timepoint == 2) %>% 
  filter(line != "O") %>% 
  group_by(line,treatment# inversion.st 
  ) %>%
  summarise(mean = ci(fatbyprotein)[1],
            uci = ci(fatbyprotein)[2],
            lci = ci(fatbyprotein)[3]
  ) %>%
  ggplot(aes(
    x=treatment,
    y=mean,
    ymin=lci,
    ymax=uci, 
    color = line
  )) +  
  xlab("Treatment") +
  ylab("Fat concentration") +
  scale_color_manual(values = color) +
  geom_errorbar(width = 0.1, position=position_dodge(width = 0.5), show.legend = F) +
  geom_point(position=position_dodge(width = 0.5), show.legend = F) + theme_bw() + 
  geom_line(position=position_dodge(width = 0.5), aes(x = treatment, y = mean, group = line, color = line, ),show.legend = F) +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15)
  )

var3



#check statistics
t.test(fatdata[timepoint == 2][treatment == "control"]$fatbyprotein, fatdata[timepoint == 2][treatment == "fasted"]$fatbyprotein)
t.test(fatdata[timepoint == 1][treatment == "fasted"]$fatbyprotein, fatdata[timepoint == 2][treatment == "fasted"]$fatbyprotein)

setwd("/Users/supad/OneDrive/Documents/Bergland Research/R_data_objects/Jan_2024_objects/")
ggsave(
  ( (survivalcurve2 | survivalcurve1) /((var2| var3) + 
                                          plot_layout(guides = "collect") & theme(legend.position = 'bottom')) ) + 
    plot_annotation(
      tag_levels = c("A")
    ), 
  file = "figure2.pdf",
  width = 10, height = 10)



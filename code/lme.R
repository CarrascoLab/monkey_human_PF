library(lme4)
library(performance)
library(lmerTest)
library(psycho)
library(MuMIn)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(export)
library(emmeans)
library(insight)
library(tidyr)

### CODE TO RUN LINEAR MIXED EFFECTS. MODEL ON THE MONKEY HUMAN PERFORMANCE FIELDS STUDY ###
# CODED BY: Ekin Tünçok, 2023-2024

# make sure to set this directory to where the Git repository lies:
directory = "/Volumes/purplab/EXPERIMENTS/1_Current_Experiments/Ekin/monkeyPF"
setwd(directory)

# comparing Asymmetry ratio or dprime data:
file_name = "data"
location_info = "all_locs"

# load the data_tbl in the CSV format:
data_tbl = read.csv((sprintf("%s/data_to_analyze/%s_botheyes_%s.csv", directory, file_name, location_info)), header = T)

# factorize the group levels:
data_tbl$group <- factor(
  data_tbl$group, ordered = FALSE,
  levels = c('1', '2')
)

# organize the data table into contrasts of interest:
data_tbl <- gather(data_tbl, key = "location", value = "sensitivity", RHM,IC_NE,UVM,IC_NW,LHM,IC_SW, LVM,IC_SE)

data_tbl$location <- (factor(data_tbl$location,levels = c('RHM','IC_NE','UVM','IC_NW','LHM','IC_SW', 'LVM','IC_SE')))
data_tbl <- cbind(data_tbl, cardinality = 0)
data_tbl$cardinality[data_tbl$location == 'IC_NE' | data_tbl$location == 'IC_NW' | data_tbl$location == 'IC_SE' | data_tbl$location == 'IC_SW'] = 'IC'
data_tbl$cardinality[data_tbl$location == 'RHM' | data_tbl$location == 'LHM'] = 'H'
data_tbl$cardinality[data_tbl$location == 'LVM'] = 'L'
data_tbl$cardinality[data_tbl$location == 'UVM'] = 'U'

data_tbl$cardinality <- factor(
  data_tbl$cardinality, ordered = FALSE,
  levels = c('H','L','U','IC')
)

##
loc_model_cont = C(data_tbl$location, contr.treatment, base = 3);
lm.full <- lmer(sensitivity ~ group*loc_model_cont + (1|id),REML=FALSE, data=data_tbl)
lm.base <- lmer(sensitivity ~ group+loc_model_cont+ (1|id), REML=FALSE, data=data_tbl)

summary(lm.full)
qqnorm(resid(lm.full)) # looks good

rescompmod <- anova(lm.full, lm.base)
print(rescompmod)
print(r.squaredGLMM(lm.base))
print(r.squaredGLMM(lm.full))
confidence_ints <- confint(lm.full)

con <- list(
  Human_LvsU = c(0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0,0),
  Human_HvsV =c(0.5,0,0,0,-0.5,0,0,0,0.5,0,0,0,-0.5,0,0,0),
  Monkey_LvsU =c(0,0,0,0,0,-1,0,0,0,0,0,0,0,1,0,0),
  Monkey_HvsV =c(0,0.5,0,0,0,-0.5,0,0,0,0.5,0,0,0,-0.5,0,0),
  Lower_MvsH =c(0,0,0,0,-1,1,0,0,0,0,0,0,0,0,0,0),
  Upper_MvsH =c(0,0,0,0,0,0,0,0,0,0,0,0,-1,1,0,0)
)

emm <- emmeans(lm.full, list(~ group * loc_model_cont), contr = con, adjust = "none", level = 0.95)
print(emm)

confint(emm$` of group, loc_model_cont`)

#visualize
table_stats <- tab_model(lm.full, show.re.var= TRUE, 
               pred.labels =c("(Intercept)", "Macaques", "RHM_vs_UVM", "ICNE_vs_UVM", 
                              "ICNW_vs_UVM", "LHM_vs_UVM", "ICSW_vs_UVM", "LVM_vs_UVM",
                              "ICSE_vs_UVM", "macaque:RHM_vs_UVM", "macaque:ICNE_vs_UVM", 
                              "macaque:ICNW_vs_UVM", "macaque:LHM_vs_UVM", "macaque:ICSW_vs_UVM", "macaque:LVM_vs_UVM",
                              "macaque:ICSE_vs_UVM"),
               dv.labels= "Sensitivity (d')")

show(table_stats)

set_theme(base = theme_light())
pp1 <-plot(emm$` of group, loc_model_cont`, horizontal=FALSE, colors=c("black","dark grey","grey","red")) + theme_bw()+coord_flip()

graph2ppt(file="ggplot2_plot.pptx", width=5, height=5) 


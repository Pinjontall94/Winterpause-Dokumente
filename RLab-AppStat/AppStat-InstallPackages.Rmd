################################################################################ 
# R packages Applied Statistics 2025-10
################################################################################ 

p <- c("GAD",
       "car",
       "dplyr",
       "emmeans",
       "ggplot2",
       "knitr",
       "lattice",
       "lme4",
       "lmerTest",
       "MASS",
       "MCPMod",
       "multcomp",
       "multcompView",
       "mvtnorm",
       "nlme",
       "pbkrtest",
       "shiny",
       "sommer")

repos <- "https://ftp.gwdg.de/pub/misc/cran/"
install.packages ( p, dependencies = T, repos = repos)
  

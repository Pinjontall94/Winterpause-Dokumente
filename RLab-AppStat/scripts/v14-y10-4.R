################################################################################
# v14-y10-4.R
################################################################################
# library(knitr); stitch("v14-y10-4.R",template="mf.Rnw")

met <- read.table ( "v14-y10-DE-data.csv", header=T, sep=";", dec="," )
met$gen <- reorder(met$gen, met$GYLD, mean)
suppressMessages(library("emmeans"))
suppressMessages(library("multcomp"))
emm_options(msg.interaction=FALSE,msg.nesting=FALSE)
m.fixed  <- lm (GYLD ~ gen*loc*year + year:loc:rep, data = met)
l <- emmeans ( m.fixed, "gen",mode="a")
cld(l,sort=TRUE,reversed=TRUE,alpha=0.10, adjust="none")
plot(l,comparisons=TRUE,alpha=0.10,adjust="none")

       

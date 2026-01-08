########################################################################
# Efficient data analysis and documentation with R
# Writing results to an Excel file
# v14-y10-7.R: Exercises
########################################################################

met <- read.table ( "v14-y10-DE-data.csv", header=T, sep=";", dec="," )
met$gen <- reorder(met$gen, met$GYLD, mean)
suppressMessages(library("emmeans"))
suppressMessages(library("openxlsx"))
emm_options(msg.interaction=FALSE,msg.nesting=FALSE)

# 1.)

m.fixed  <- lm (GYLD ~ gen*loc*year + year:loc:rep, data = met)
anova(m.fixed)


l <- emmeans ( m.fixed, "gen",mode="a")
EMM <- CLD(l, sort=TRUE, reversed=TRUE, alpha=0.10, adjust="none")

# Writing a table to a workbook

wb <-createWorkbook()
addWorksheet (wb,"Table")
writeData (wb,"Table",EMM,colNames=TRUE, rowNames=FALSE)
saveWorkbook(wb,"EMMeansGYLD.xlsx",overwrite = TRUE)

# Formatting of the table

wb <-createWorkbook()
addWorksheet ( wb, "GYLD" )
writeData ( wb, "GYLD","EMMeans for Grain Yield", startRow=1)
bold <- createStyle ( textDecoration="bold" )
addStyle( wb, "GYLD", bold, 1, 1)
writeData ( wb, "GYLD", EMM,colNames=TRUE, rowNames=FALSE, startRow=3)
twoDigits <- createStyle(numFmt="0.00")
addStyle ( wb, "GYLD", twoDigits, 1:(3+nrow(EMM)), c(2:3,5:6),
           gridExpand=TRUE, stack=TRUE)

# Writing a figure to a workbook

par (  mar=c(6,4,1,1))
png("plot.png", height=1000, width=1500, res=300, pointsize=12)
plot(l,comparisons=TRUE,alpha=0.10,adjust="none")
r<-dev.off()
# addWorksheet ( wb, "GYLDx" )
insertImage(wb, "GYLD", "plot.png", width = 5, height = 5,
            startRow=(5+nrow(EMM)))

saveWorkbook(wb,"EMMeansGYLD.xlsx",overwrite = TRUE)

#############################################################################
# Variance components
#############################################################################

library("sommer")
met <- read.table ( "v14-y10-DE-data.csv", header=T, sep=";", dec="," )
met$env <- paste(met$year,met$loc)

system.time(
m.vc2  <- mmer ( cbind(GYLD,TKW,PLH) ~ 1,
                 random = ~ gen + loc + year 
                            + gen:loc + gen:year + loc:year
                            + gen:loc:year
                            + year:loc:rep,
               rcov=~vs(at(env),units),
               data = met,verbose=F)
)
    
summary(m.vc1)$varcomp

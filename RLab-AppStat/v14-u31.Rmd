################################################################
# MK-002-EN Applied Statistics / MK-002 Angewandte Statistik / #
# MK-002-EN-DI Applied Statistics                              #
# *** Balanced multi-factor ANOVA ***                          #
################################################################

# 1 Generating factor levels for manual data input
##################################################

yld <- c ( 69,91,84,76,  66,56,58,60,  8,4,18,10, 44,24,30,22, 
           118,96,80,106, 132,88,101,119)
var <- gl ( n=2, k=12, length=24, labels=c("a","b") )
cli <- gl ( n=3, k=4,  length=24, labels=c("I","II","III") )
rep <- gl ( n=4, k=1,  length=24, labels=c("r1","r2","r3","r4"))
d2 <-data.frame(var,cli,rep,yld)


# 2 Two-factor ANOVA: Climate experiment
########################################

library("GAD" )
cl <- read.table ( "v14-u31-01.csv", header=T, sep=";", dec=",",
                  stringsAsFactors  = T)
str(cl)

# Stripcharts:
stripchart ( yield ~ climate:variety, 
             vertical=T, col="blue", 
             data=cl )

# Climate and variety are fixed factors:
cl$C  <- as.fixed (cl$climate)
cl$V  <- as.fixed (cl$variety)

# Note the difference:
class(cl$climate)
class(cl$C)

# Linear model:
m.1 <- lm ( yield ~ C + V + C:V, data=cl)
gad ( m.1 )

# Interaction plots:
with ( cl, interaction.plot ( climate, variety, yield, type="b" ))
with ( cl, interaction.plot ( variety, climate, yield, type="b" ))

# Tables of means and effects:
model.tables ( aov(m.1), "means" )
model.tables ( aov(m.1), "effects" )

# MSE and DFE from the ANOVA table:
mse <- gad(m.1)$anova$'Mean Sq'[4]
dfe <- gad(m.1)$anova$'Df'[4]

# How do you know where these values come from?
str(gad(m.1))
gad(m.1)$anova$'Mean Sq'      # column "Mean SQ"
gad(m.1)$anova$'Mean Sq'[4]   # 4th element of column "Mean Sq"

# SED, LSD, HSD
alpha <- 0.05
n.v   <- 12 ; k.v  <- 2
n.c   <-  8 ; k.c  <- 3
n.cv  <-  4 ; k.cv <- 6
sed.c  <- sqrt ( 2*mse / n.c )  
sed.v  <- sqrt ( 2*mse / n.v )  
sed.cv <- sqrt ( 2*mse / n.cv ) 

t.val <- qt( 1-alpha/2,dfe )
lsd.c  <- t.val * sed.c   
lsd.v  <- t.val * sed.v   
lsd.cv <- t.val * sed.cv  

(hsd.v  <- qtukey( 1-alpha,k.v ,dfe ) * sed.v / sqrt(2) )
(hsd.c  <- qtukey( 1-alpha,k.c ,dfe ) * sed.c / sqrt(2) )
(hsd.cv <- qtukey( 1-alpha,k.cv ,dfe ) * sed.cv/ sqrt(2) )

# Different values for climate, variety and the combinations:
sed.c 
sed.v 
sed.cv

lsd.c 
lsd.v 
lsd.cv

hsd.c 
hsd.v 
hsd.cv


# 3 Hierarchical ANOVA: Soils experiment
########################################

# Data input
soils <- read.table ( "v14-u31-02a.csv", header=T, sep=";", dec=",",
                     stringsAsFactors = T)
str(soils)

# Stripchart:
stripchart ( yield ~ village:region,
            vertical=T, col="blue",
            data=soils )

# Region is fixed, village is random:
soils$R  <- as.fixed (soils$region)
soils$V  <- as.random(soils$village)
m <- lm ( yield ~ R + R:V, data=soils )
gad (m)
estimates (m)

# Means and effects
model.tables(aov(m),"means")
model.tables(aov(m),"effects")

# LSD
(va <- data.frame(gad(m)$anova ) )
(dfe <- va[2,1])
(mse <- va[2,3])

alpha <- 0.05
b <- 2 ; n <- 4
sed <- sqrt(2*mse/(b*n)) 
lsd <- sed * qt( 1-alpha/2,dfe ) 

sed
lsd
model.tables(aov(m),"means",cterms="R")  # means only for the regions


# Analysis as a single-factor ANOVA:
str(soils)

soils$RV <- paste("r",soils$R,soils$V,sep="")
soils$RV <- factor(soils$RV)
levels (soils$RV)
soils$RV <- ordered(soils$RV,levels =
                      c("rIv1","rIv2","rIIv1","rIIv2","rIIIv1","rIIIv2" ))
str(soils)

soils$V  <- as.fixed(soils$village)
m.4 <- lm ( yield ~ RV , data=soils )
gad (m.4)

# Treatment means and HSD:
model.tables(aov(m.4),"means")

va <- data.frame(gad(m.4)$anova)
(dfe <- va[2,1])
(mse <- va[2,3])

alpha <- 0.05
n <- 4 
a <- 6
sem <- sqrt(mse/(n))
sed <- sqrt(2) * sem 
hsd <- qtukey( 1-alpha,a,dfe ) * sem 

model.tables(aov(m.4),"means")
sed
hsd

# Display pairwise differences:
attach(soils)
m <- tapply(soils$yield,soils$RV,mean)
detach(soils)
source("v14-u31-00.R")
comp.lsmeans(m,hsd)

# Function:
comp.lsmeans <- function ( lsm, lsd, description.length=4,
                           description.precision=2)
{
  sorted <- sort(lsm,decreasing=T)
  l <- length(sorted)
  cmp  <- NULL
  grps <- 0
  for (i in 1:l) {
    tmp <- matrix ('',ncol=1,nrow=l)
    for (j in i:l) 
      tmp[j] <- if (abs(sorted[i]-sorted[j]) < lsd) 1+grps else ""
    if (i==1) 
    { add <- 1 } 
    else 
    { add <- 0 
    for (j in i:l) 
    { if ( (tmp[j,1]!="") && (cmp[j,grps]=="")) add <- 1 }
    }
    if (add) 
    {colnames(tmp)<-names(sorted[i]) 
    cmp<-cbind(cmp,tmp); grps=grps+1;}
  }
  rownames(cmp) <- paste (format(names(sorted),digits=description.length),
                          round(sorted,description.precision))
  cmp
}


# 4 Mixed model ANOVA: Rapeseed experiment
###########################################

# Data input
raps <- read.table ( "v14-u31-06.csv", header=T, sep=";", dec=",",
                    stringsAsFactors = T)
str(raps)

# Genotype and environment are fixed, replication is random
raps$G  <- as.fixed(raps$geno)
raps$E  <- as.fixed(raps$env)
raps$R  <- as.random(raps$rep)
m <- lm ( hgt ~ G*E + E:R , data=raps )
gad (m)
estimates(m)

# Treatment means for genotypes:
model.tables(aov(m),"means",cterms="G")

# Estimate standard error and dfe:
library("lme4")
library("emmeans")

m2 <- lmer (hgt ~ G + E + G:E + (1|E:R), data=raps)
(sem <- data.frame(emmeans(m2,"G"))$SE[1])
(dfe <- data.frame(emmeans(m2,"G"))$df[1])

# LSD:
alpha <- 0.10
sed <- sqrt(2) * sem
( lsd <- sed * qt( 1-alpha/2,dfe ) )

# Treatment means for GxE interaction:
model.tables(aov(m),"means",cterms="G:E")

# Estimate standard error and dfe:
(sem <- data.frame(emmeans(m2,"E", by="G"))$SE[1])

# LSD:
alpha <- 0.10
sed <- sqrt(2) * sem
( lsd <- sed * qt( 1-alpha/2,dfe ) )

# alternative:
snk.test ( m, anova.tab=gad(m),
           term ='G', among ='G', within ='E')
snk.test ( m, anova.tab=gad(m),
           term ='G:E', among ='G', within ='E')


# 5 Cleaning up
###############

rm(list = ls())
            

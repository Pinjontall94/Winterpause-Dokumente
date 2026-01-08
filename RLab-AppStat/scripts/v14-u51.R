################################################################
# MK-002-EN Applied Statistics / MK-002 Angewandte Statistik / #
# MK-002-EN-DI Applied Statistics                              #
# *** Mixed linear models ***                                  #
################################################################


library("lmerTest"); library("emmeans"); library("multcomp")

# 1 Extending standard experimental designs
###########################################

# 1.1 Split plot: Oats
######################

# Data preparation:
oats <- read.table ("v14-u51-01.csv", header=T, sep=";", dec=",",
                    stringsAsFactors = T) 
str(oats)

# Graphical displays:
par( mar=c(10,4,1,1) )          # Margins of the plot: bltr. bottom is 10
boxplot( Yield ~ Nitro*Variety,
         col="lightblue",
         range = 0,             # Whisker from minimum to maximum
         las = 2,               # Vertical axis tick marks
         data=oats )
grid()

with (oats,
      interaction.plot(Nitro,Variety,Yield, type="b",lwd=2 ) )

# Test of fixed and random effects
mm <- lmer ( Yield ~ Variety + Nitro + Variety:Nitro
                    + (1|Block) + (1|Block:Variety),
            data=oats )

anova(mm) # Tests of fixed effects
ranova(mm) # Tests of random effects

# Estimation of variance components for the random effects
print ( VarCorr(mm) , comp="Variance")

# Treatment means, pairwise tests, and grouping of treatments
e.N <- emmeans ( mm, ~Nitro )
cld ( e.N, adjust="hochberg")
test ( contrast ( e.N , "pairwise"),
       adjust="hochberg" )

e.V <- emmeans ( mm, ~Variety )
cld ( e.V, adjust="hochberg")
test ( contrast ( e.V , "pairwise"),
       adjust="hochberg" )

e.VN <- emmeans ( mm, ~Variety:Nitro )
cld ( e.VN, adjust="hochberg", alpha=0.10, sort=F)
test ( contrast ( e.VN , "pairwise"),
       adjust="hochberg")

# Analysis of residuals
source("v14-u11-00.R") # Some diagnostic plots, see Appendix
check.residuals(mm)


# 1.2 Balanced lattice in six replications: Barley
##################################################

# Data preparation:
barley <- read.table("v14-u51-02.csv", header=T, sep=";", dec=",",
                     stringsAsFactors = T)
str (barley)

# Classical lattice analysis
m.1 <-lmer ( yield ~ Variety # Variety fixed
             + (1|Rep) + (1|RowBlk), # Random lattice effects
             data = barley )
print ( VarCorr(m.1) , comp="Variance" ) # Variance components

# Added column effect
m.2 <-lmer ( yield ~ Variety # Variety fixed
             + (1|Rep) + (1|RowBlk) # Random lattice effects
             + (1|ColBlk), # Column effect
             control=lmerControl(optimizer="Nelder_Mead"), # Convergence
             data = barley )
print ( VarCorr(m.2) , comp="Variance" ) # Variance components

# Test of fixed and random effects
anova (m.2)
ranova (m.2)

# Adjusted treatment means, pairwise tests, and grouping of treatments
e.V <- emmeans ( m.2, ~Variety )
cld ( e.V, adjust="hochberg" )
test ( contrast (e.V , "pairwise"),
       adjust="hochberg" )

# One sided comparisons with a control
levels(barley$Variety) # Look up treatment number of the reference
test ( contrast ( e.V, "trt.vs.ctrl",ref=4),
       side = ">",
       adjust="none")


# 1.3 Alpha lattice with two factors: Rape seed
###############################################

# Data preparation:
raps <- read.table ( file="v14-u51-03.csv", sep=";",header=T,
                     stringsAsFactors = T)
str(raps)

# Test the fixed and random effects
fm <- lmer ( hgt ~ geno + env + geno:env +
               (1|rep:env) + (1|blk:rep:env) , data=raps )
anova (fm)
ranova (fm)

# Variance components
print ( VarCorr(fm) , comp="Variance" )

# Efficiency of the lattice
m.rcbd <- lmer ( hgt ~ geno + env + geno:env + # Analyse as
                   (1|rep:env), # randomized complete
                 data=raps ) # block design
print ( VarCorr(m.rcbd) , comp="Variance" ) # Error in the RCBD
(lattice.efficiency = 89.98 / 59.59 * 100) # Ratio of errors

# Interaction plot of the adjusted entry means
l <- data.frame ( summary ( emmeans(fm,~geno ) ) ) # Means of genotypes
idx <- l[order(l$emmean),]$geno                    # Sort the levels of
raps$geno <- factor ( raps$geno , levels = idx)    # genotypes

fm <- lmer ( hgt ~ geno + env + geno:env +         # Refit model with
               (1|rep:env) + (1|blk:rep:env) ,     # ordered data
             data=raps )                           # set

lsmip ( fm, env ~ geno) # Interaction plot on basis of emmeans

# Difference between genotypes under stress
e.ge <- emmeans ( fm, ~geno:env, at=list(env="s") )
cld ( e.ge, adjust="none", alpha=0.1)
# For comparison (have a look at the bottom of the output)
cld ( e.ge, adjust="FDR", alpha=0.1)

# Detach packages
detach(package:lmerTest)
detach(package:multcomp)


# 2 Heterogeneous error variances
#################################

# 2.1 Series of alpha lattices at six locations
###############################################

library("lmerTest"); library("nlme")

# Data preparation:
met <- read.table("v14-u51-04.csv",sep=";",dec=",",header=T,
                  stringsAsFactors = T)
str(met)

# Lattice analysis with homogeneous error variances at the locations (counties)

mm <- lmer ( yield ~ gen + county + gen:county
            + (1|county:rep) + (1|county:rep:block), data=met )
print ( VarCorr(mm) , comp="Variance" )

# The same with function lme() from package nlme:
met$countyrep <- met$county:met$rep
levels(met$countyrep)
m.lme1 <- lme ( fixed = yield ~ gen + county + gen:county,
                random = ~ 1|countyrep/block,
                data = met )
print ( VarCorr(m.lme1) )

# Heterogeneous error variances at the counties:
m.lme2 <- lme ( fixed = yield ~ gen + county + gen:county,
                random = ~ 1|countyrep/block,
                weights = varIdent (form=~1|county), # Heteroscedacity
                data = met )
print ( VarCorr(m.lme2) )

# The ratios of the standard deviations at the counties:
m.lme2$modelStruct$varStruct

# Error variances at the counties:
vc <- data.frame(VarCorr(m.lme2)[,])
v.e <- as.numeric(as.character(vc["Residual"==rownames(vc),"Variance"]))
vw <- varWeights(m.lme2$modelStruct$varStruct)
tt <- unique(data.frame(comp=names(vw),w=vw))
tt$var <- 1/tt$w/tt$w * v.e
tt[,-2]

# With sommer:
library("sommer")
m2 <- mmer ( fixed = yield ~ gen + county + gen:county ,
             random = ~ county:rep + county:rep:block ,
             rcov = ~ vsr(dsr(county),units), # Heteroscedasticity
             data=met, verbose = FALSE)
summary(m2)$varcomp
detach(package:sommer)

# Are the error variances of the models significantly different?
anova ( m.lme1, m.lme2 ) # Compare homoscedastic with heteoscedastic

# Test fixed effects
anova ( m.lme2 ) # Test fixed effects

# Adjusted entry means and comparisons
library("emmeans")
library("multcomp")
e.g <- emmeans ( m.lme2, ~ gen )
cld (e.g, adjust="none", alpha=0.1)

# Sorted plot of the emmeans:
emt <- summary(e.g)
met$gen <- factor(met$gen,levels=emt$gen[order(emt$emmean)])
m.lme2 <- lme ( fixed = yield ~ gen + county + gen:county,
                random = ~ 1| countyrep/block,
                weights = varIdent (form=~1|county),
                data = met )
e.g <- emmeans ( m.lme2, ~ gen )

plot ( e.g, comparisons=TRUE, CIs=FALSE, adjust="none", alpha=0.1)
# Do the arrows overlap?

# Comparison of the treatments in county #5:
e.c5 <- emmeans ( m.lme2, ~ gen, at=list(county="C5") ) # County #5
cld ( e.c5, adjust="none", alpha=0.1 )

# Reorder factor levels, refit the model and plot emmeans:
emt <- summary(e.c5)
met$gen <- factor(met$gen,levels=emt$gen[order(emt$emmean)])
m.lme2 <- lme ( fixed = yield ~ gen + county + gen:county,
                random = ~ 1| countyrep/block,
                weights = varIdent (form=~1|county),
                data = met )
e.c5 <- emmeans ( m.lme2, ~ gen, at=list(county="C5") ) # County #5
plot ( e.c5, comparisons=TRUE, intervals=FALSE, adjust="none", alpha=0.1)


# 3 Cleaning up
###############

rm(list = ls())
            

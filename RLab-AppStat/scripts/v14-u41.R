################################################################
# MK-002-EN Applied Statistics / MK-002 Angewandte Statistik / #
# MK-002-EN-DI Applied Statistics                              #
# *** Experimental Designs ***                                 #
################################################################

library("lme4"); library("emmeans");  
library("multcomp"); library("multcompView")
options ( contrasts = c("contr.sum","contr.sum") )

# 1 Randomized complete block design: Cotton
############################################

# Data preparation:
cotton <- read.table ("v14-u41-01.csv", header=T, sep=";", dec=",",
                      stringsAsFactors = T) 
str(cotton)

# Analysis of variance:
boxplot( Strength ~ Fert , data=cotton )
m1 <- aov ( Strength ~ Blk + Fert, data=cotton )
summary ( m1 )

# Means:
model.tables ( m1, "means" )

# Least significant differences:
va  <- anova ( m1 )             # Analysis of variance
b <- 3; t <- 5                  # Number of blocks and treatments
sed <- sqrt ( 2 * va[3,3] / b)  # Standard error of a difference of means
dfe <- va[3,1]                  # Degrees of freedom residuals
lsd <- qt(0.975,dfe) * sed                 # Least significant difference
hsd <- qtukey(0.95,t,dfe) * sed / sqrt(2)  # Honestly significant diff.
sed; lsd; hsd

# Pairwise comparisons, unadjusted:
e.F <- emmeans ( m1, ~Fert )
cld ( e.F, adjust="none" )
test( contrast (e.F , "pairwise"), adjust="none")

# Pairwise comparisons, adjusted:
cld ( e.F, adjust="tukey" )
cld ( e.F, adjust="tukey", alpha=0.1 ) # changing the significance level
test( contrast (e.F , "pairwise"), adjust="tukey")


# 2 Latin square: Sugar beets
#############################

# Data input:
sugar <- read.table ( "v14-u41-02.csv", header=T, sep=";", dec=",",
                      stringsAsFactors = T)
str(sugar)
head(sugar)

boxplot ( Yield ~ Variety, data=sugar )

# Anaylsis of variance:
m2 <- aov ( Yield ~ Row + Col + Variety, data=sugar )
summary ( m2 )

# Least significant differences:
r   <- 6                         # 6 rows, 6 columns, 6 treatments
va  <- anova ( m2 ) 
sed <- sqrt ( 2* va[4,3] / r)    # va[4,3]: MS residuals
dfe <- va[4,1]                   # df residuals
lsd <- qt(0.975,dfe) * sed 
hsd <- qtukey(0.95,r,dfe) * sed / sqrt(2) 
sed; lsd; hsd

# Means and pairwise comparions:
e.V <- emmeans(m2, ~Variety)
cld ( e.V, adjust="none" )
test( contrast ( e.V, "pairwise"), adjust="none" )


# 3 Simple lattice: Soy beans
#############################

# Data input:
soy <- read.table ( "v14-u41-03.csv", header=T, sep=";", dec=",",
                    stringsAsFactors = T)
str(soy)

# Analysis of variance:
soy$Block.in.Rep <- soy$Block:soy$Rep   # Sequence of model terms!!
m3 <- aov ( Yield ~ Rep + Block.in.Rep + Variety , data=soy)
summary(m3)

# Least significant differences:
r <- 2; k <- 5                    # 2 replications, blocksize k = 5
va <- anova ( m3 )
msb <- va[2,3]                    # mean squares of the block
mse <- va[4,3]; dfe <- va[4,1]    # mean squares and degrees of freedom of residuals
w <- (msb-mse) / (k*(r-1)*msb)

# Different values for first and second associates:
sed.l1 <- sqrt( 2*mse/r*(1+(r-1)*w)); lsd.l1 <- qt(0.975,dfe) * sed.l1 
sed.l0 <- sqrt( 2*mse/r*(1+r*w))    ; lsd.l0 <- qt(0.975,dfe) * sed.l0 

sed.l1; sed.l0
lsd.l1; lsd.l0

# Calculation of adjusted entry means using a mixed model:
m3a <- lmer(Yield ~   (1|Rep) + (1|Block.in.Rep) + Variety , data=soy)
# (1|...) means random factor
emmeans(m3a, ~Variety)


# 4 Randomized complete block design with two factors: Cowpeas
##############################################################

peas <- read.table ( "v14-u41-04.csv", header=T, sep=";", dec=",",
                     stringsAsFactors = T)
str(peas)

# Analysis of variance:
boxplot ( Yield ~ Spacing:Variety , data=peas )
m4 <- aov ( Yield ~ Block + Spacing + Variety + Spacing:Variety ,
            data=peas)
summary(m4)

# Plot interaction:
with ( peas, interaction.plot ( Spacing, Variety, Yield, type="b"))

# Least significant differences:
r <- 4; a <- 3; b <- 3
va <- anova ( m4 ) 
mse <- va[5,3]; mse
dfe <- va[5,1]; dfe
sed.a <- sqrt( 2*mse/(r*b)) 
sed.b <- sqrt( 2*mse/(r*a)) 
sed.ab <- sqrt( 2*mse/(r)) 
lsd.a <- qt(0.975,dfe) * sed.a 
lsd.b <- qt(0.975,dfe) * sed.b 
lsd.ab <- qt(0.975,dfe) * sed.ab

sed.a; sed.b; sed.ab  # different values for comparisons
lsd.a; lsd.b; lsd.ab  # between main factors and treatment combinations

# Means and pairwise comparisons:
e.V1 <- emmeans(m4, ~Spacing:Variety)
e.V1

e.V2 <- emmeans(m4, ~Spacing|Variety)         # different display
cld ( e.V2, adjust="none", sort=F )
test( contrast ( e.V2, "pairwise"), adjust="none" )

e.V3 <- emmeans(m4, ~Variety|Spacing)
cld ( e.V3, adjust="none", sort=F )
test( contrast ( e.V3, "pairwise"), adjust="none" )  # different display


# 5 Split plot:  Alfalfa
########################

# Data input:
alf <- read.table ( "v14-u41-05.csv", header=T, sep=";", dec=",",
                    stringsAsFactors = T)
str(alf)

# Analysis of variance:
m5 <- aov(Yield ~ Variety + Date + Date:Variety 
          + Error(Block/Variety), data=alf)
summary (m5)

# Plot interaction
par (mfrow=c(1,2))
with ( alf, interaction.plot (  Variety, Date, Yield, type="b" ))
with ( alf, interaction.plot (  Date, Variety, Yield, type="b" ))

# Least significant differences
r <- 6 ; a <- 3 ; b <- 4                 # 6 blocks, 3 varieties, 4 dates
ms.me <- summary(m5)[[2]][[1]][2,3]
ms.se <- summary(m5)[[3]][[1]][3,3]
df.me <- summary(m5)[[2]][[1]][2,1]
df.se <- summary(m5)[[3]][[1]][3,1]
df.ia <- ( ms.me + (b-1)*ms.se ) ^2 /    # Satterthwaite Approximation
  ( ms.me ^2 / df.me  +  ((b-1)*ms.se)^2 / df.se ) 
sed.1 <- sqrt(2*ms.me/r/b); sed.2 <- sqrt(2*ms.se/r/ a)
sed.3 <- sqrt(2*ms.se/r  ); sed.4 <- sqrt(2*(ms.me+(b-1)*ms.se)/r/b)
lsd.1 <- qt(0.975,df.me) * sed.1 ;  lsd.2 <- qt(0.975,df.se) * sed.2 
lsd.3 <- qt(0.975,df.se) * sed.3 ;  lsd.4 <- qt(0.975,df.ia) * sed.4 

sed.1; sed.2; sed.3; sed.4
lsd.1; lsd.2; lsd.3; lsd.4

# Pairwise comparisons
e.V <- emmeans(m5, ~Variety)
cld ( e.V, adjust="none",sort=F)
test( contrast ( e.V, "pairwise"), adjust="none" )

e.D <- emmeans(m5, ~Date)
cld ( e.D, adjust="none",sort=F)
test( contrast ( e.D, "pairwise"), adjust="none" )

e.DV <- emmeans(m5, ~Date|Variety)
cld ( e.DV, adjust="none",sort=F )
test( contrast ( e.DV, "pairwise"), adjust="none" )

e.VD <- emmeans(m5, ~Variety|Date)
cld ( e.VD, adjust="none",sort=F )
test( contrast ( e.VD, "pairwise"), adjust="none" )


# 6 Cleaning up
###############

rm(list = ls())
            

###############################################################################
# Efficient data analysis and documentation with R
# Carrying out an analysis for several factor levels
# v14-y10-3.R
###############################################################################

met <- read.table ( "v14-y10-DE-data.csv", header=T, sep=";", dec="," )

suppressMessages(library("lme4"))
met$env <- met$year:met$loc
Environments <- levels(met$env)

Environments

# Heritability for one environment

E <- Environments[1]
SL <- subset(met,met$env==E)
m.SL  <- lmer ( GYLD ~ (1|gen) + (1|rep), data = SL)
s <- summary(m.SL)
v.e <- s$sigma^2
v.g <- as.numeric(s$varcor$gen)
hsq <- v.g/(v.g+v.e)

hsq

# Heritability for all environments

n <- length(Environments)
SLhsq <- vector("numeric",n)
names(SLhsq) <- Environments
for (i in 1:n){
  SL <- subset(met,met$env==Environments[i])
  m.SL  <- lmer ( GYLD ~ (1|gen) + (1|rep), data = SL)
  s <- summary(m.SL)
  v.e <- s$sigma^2
  v.g <- as.numeric(s$varcor$gen)
  SLhsq[i] <- v.g/(v.g+v.e)
}

data.frame(SLhsq)

# Heritability for all environments for one trait

Trait <- "TKW"
SLhsq <- vector("numeric",n)
names(SLhsq) <- Environments
for (i in 1:n){
  SL <- subset(met,met$env==Environments[i])
  eval(parse(text=paste(
    "m.SL  <- lmer (", Trait ,"~ (1|gen) + (1|rep), data = SL)")))
  s <- summary(m.SL)
  v.e <- s$sigma^2
  v.g <- as.numeric(s$varcor$gen)
  SLhsq[i] <- v.g/(v.g+v.e)
}

data.frame(SLhsq)

# Heritability for all environments for three traits

Environments <- levels(met$env)
Traits       <- c("GYLD","TKW","PLH")

nEnv <- length(Environments)
nTrt <- length(Traits)

Heritability <- matrix(nrow=nEnv,ncol=nTrt)
rownames(Heritability) <- Environments
colnames(Heritability) <- Traits

for (j in 1:nTrt) {
  Trait <- Traits[j]
  for (i in 1:nEnv){
    SL <- subset(met,met$env==Environments[i])
    eval(parse(text=paste(
      "m.SL  <- lmer (", Trait ,"~ (1|gen) + (1|rep), data = SL)")))
    s <- summary(m.SL)
    v.e <- s$sigma^2
    v.g <- as.numeric(s$varcor$gen)
    Heritability[i,j] <- v.g/(v.g+v.e)
  }
}

Heritability

#plot the Heritabilities

library("reshape2")
pdta1 <-data.frame( Result,env=Environments )
pdta2 <- melt(pdta1, id.vars = c("env"))
pdta2$env <- reorder(pdta2$env, pdta2$value, mean)


par (  mar=c(9,4,1,1))
with(pdta2,
 interaction.plot(env,variable,value,col=2:4,las = 2,
 lwd=2,ylab="Single environment heritability",xlab=""))
grid()





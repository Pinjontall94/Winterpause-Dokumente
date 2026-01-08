###############################################################################
# Efficient data analysis and documentation with R
# v14-y10-2.R
###############################################################################

met <- read.table ( "v14-y10-DE-data.csv", header=T, sep=";", dec="," )
str(met)
table(met$year,met$loc)

# Comparison of the yield of the genotypes

suppressMessages(library("emmeans"))
emm_options(msg.interaction=FALSE,msg.nesting=FALSE)

m.fixed  <- lm (GYLD ~ gen*loc*year + year:loc:rep, data = met)
l <- emmeans ( m.fixed, "gen",mode="a")
CLD(l,sort=TRUE,reversed=TRUE,alpha=0.10, adjust="none")

# Writing a R command in a text string

Trait <- "GYLD"
paste("m.fixed <- lm (",Trait,"~ gen*loc*year + year:loc:rep, data = met)")

# Use eval and parst to evaluate the command in a text string

eval(parse(text=paste(
 "m.fixed  <- lm (",Trait,"~ gen*loc*year + year:loc:rep, data = met)")))

# The analysis for TKW

Trait <- "TKW"
eval(parse(text=paste(
  "m.fixed  <- lm (",Trait,"~ gen*loc*year + year:loc:rep, data = met)")))
l <- emmeans ( m.fixed, "gen",mode="a")
CLD(l,sort=TRUE,reversed=TRUE,alpha=0.10, adjust="none")

# And for three variables

Traits <- c("GYLD","TKW","PLH")
for (Trait in Traits) {
  eval(parse(text=paste(
    "m.fixed  <- lm (",Trait,"~ gen*loc*year + year:loc:rep, data = met)")))
  l <- emmeans ( m.fixed, "gen",mode="a")
  comp <- CLD(l,sort=TRUE,reversed=TRUE,alpha=0.10, adjust="none")
  print(Trait)
  print(comp)
}

# Save the results instead of printing

Traits <- c("GYLD","TKW","PLH")
Result <- vector(mode="list",length=length(Traits))
names(Result) <- Traits
for (Trait in Traits) {
  eval(parse(text=paste(
    "m.fixed  <- lm (",Trait,"~ gen*loc*year + year:loc:rep, data = met)")))
  l <- emmeans ( m.fixed, "gen",mode="a")
  Result[[Trait]] <- CLD(l,sort=TRUE,reversed=TRUE,alpha=0.10, adjust="none")
}

Result

detach(package:emmeans)

       
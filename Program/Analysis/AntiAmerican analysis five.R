#Author: Norberto R. Morales
#Project: Anti-American Public Opinion
#Purpose: HLM-Bayesian Statistical Analyses (Past Five Years)
#Last Updated: 06/25/2014
# Machine(Windows 7): S/No: 0039130478
############################################################################
## Employ Hierchierchical/Multi-Level Model with Bayesian Priors.
## WinBugs/OpenBugs Scripts are used with R.
############################################################################
rm(list = ls()) #clear workspace

#libraries:

# install.packages("R2WinBUGS")
# install.packages("MASS")
# install.packages("poLCA") 
# install.packages("ltm") 

library(car)
library(R2WinBUGS)
library(MASS)
library(poLCA)
library(ltm)

# set working directory
#setwd("")
setwd("C:/Users/Antigone/Dropbox/Papers/anti-american_paper_data")
getwd()

load("Data/cnames.Rda")
load("Data/cvars.Rda")
load("Data/dta.Rda")
load("Data/edmat.Rda")
load("Data/gap07.Rda")
load("Data/mus.Rda")
load("Data/sesmat.Rda")

setwd("D:/WinBUGS14")

## 
## Analysis of 2007 survey: HLM using WinBugs/OpenBugs
## 

y <- mus$us.scale               # Anti-Americanism scale: 1=most AA, 0=least AA
pious <- mus$pious              # Individual: 1=highly religious, 0=less religious
musid<- mus$id.fund             # Identify with fundamentalists: 1=identify, 0=does not identify
news <- mus$news                # Follow int'l news: 1=frequently, 0=only when important
age <- mus$age/100
male <- mus$male
ses <- mus$ses
ed2 <- mus$ed2
ed3 <- mus$ed3
satis <- mus$satisfied
z1 <- cvars$struggle            # Country: percent seeing a reformer-Islamist struggle
z2 <- cvars$pctmus              # Country: percent Muslim in country
z3 <- cvars$log.gdppc           # Country: log of per capita GDP
z4 <- cvars$fivedisp             # Country: MID- Militarized disputes
z5 <- cvars$fiveexp            # Country: EXP- U.S. Military Aid to Disputing Neighbors
z6 <- cvars$fivevote            # Country: UN- UN Vote distance with U.S.

## Alternative DV, using just the 4-item Anti-Americanism question (appendix)
#y <- (mus$us-1)/3               #  Anti-Americanism: 1=most AA, 0=least AA

## Alternative IV for piety; does respondent support 1=fundamentalists or 0=modernizers
#pious <- mus$id.fund

cnum <- mus$cnum2
N <- nrow(mus)
ncountry <- max(cnum)

################################################################################
# Models 1-4:
################################################################################

# Model 1 estimates
inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(7,-.05,.05),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.pious=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm.txt"
bugs.model <- bugs(data=c("y","pious","news", "z1","z2","z3", "z4", "z5","z6",
                          "cnum","N","ncountry"),
                   inits=inits,
                   parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                        "mu.country","mu.pious","mu.news","sd.b","sd.y"),
                   model.file=model.file,
                   n.chains=3,
                   n.iter=500,
                   debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)

round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)

# Model 2 estimates

#Model 2, with z1 as percent religious instead of percent seeing a struggle
z1 <- cvars$pctrel              # Country: percent or total religious

inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.pious=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm.txt"
bugs.model <- bugs(data=c("y","pious","news","z1","z2","z3", "z4", "z5","z6",
                          "cnum","N","ncountry"),
                   inits=inits,
                   parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                        "mu.country","mu.pious","mu.news","sd.b","sd.y"),
                   model.file=model.file,
                   n.chains=3,
                   n.iter=500,
                   debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)

round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)

# Model 3 estimates

z1 <- cvars$struggle            # Country: percent seeing a reformer-Islamist struggle

inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.musid=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.musid=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm-three.txt"
bugs.model <- bugs(data=c("y","musid","news","z1","z2","z3","z4", "z5","z6",
                          "cnum","N","ncountry"),
                   inits=inits,
                   parameters.to.save=c("g.country","b.country","b.musid","b.news",
                                        "mu.country","mu.musid","mu.news","sd.b","sd.y"),
                   model.file=model.file,
                   n.chains=3,
                   n.iter=500,
                   debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)

round(bugs.model$mean$mu.musid,2)
round(bugs.model$sd$mu.musid,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.musid)[order(bugs.model$mean$b.musid),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.musid+bugs.model$mean$mu.news,2)

# Model 4 estimates

#Model 4, with z1 as percent religious instead of percent seeing a struggle
z1 <- cvars$pctrel              # Country: percent or total religious

inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.musid=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.musid=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm-three.txt"
bugs.model <- bugs(data=c("y","musid","news","z1","z2","z3", "z4", "z5","z6",
                          "cnum","N","ncountry"),
                   inits=inits,
                   parameters.to.save=c("g.country","b.country","b.musid","b.news",
                                        "mu.country","mu.musid","mu.news","sd.b","sd.y"),
                   model.file=model.file,
                   n.chains=3,
                   n.iter=500,
                   debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)

round(bugs.model$mean$mu.musid,2)
round(bugs.model$sd$mu.musid,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.musid)[order(bugs.model$mean$b.musid),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.musid+bugs.model$mean$mu.news,2)

################################################################################
# Models 5-6:
################################################################################

ed2init <- NA
ed2init[is.na(ed2)] <- rbinom(sum(is.na(ed2)),1,0.5)
ed3init <- NA
ed3init[is.na(ed3)] <- rbinom(sum(is.na(ed3)),1,0.5)
newsinit <- NA
newsinit[is.na(news)] <- rbinom(sum(is.na(news)),1,0.5)
piousinit <- NA
piousinit[is.na(pious)] <- rbinom(sum(is.na(pious)),1,0.6)
sesinit <- NA
sesinit[is.na(ses)] <- rbeta(sum(is.na(ses)),3,5)
z1 <- cvars$struggle            # Country: percent seeing a reformer-Islamist struggle
z2 <- cvars$pctmus              # Country: percent Muslim in country
z3 <- cvars$log.gdppc           # Country: log of per capita GDP
z4 <- cvars$fivedisp             # Country: MID- Militarized disputes
z5 <- cvars$fiveexp            # Country: EXP- U.S. Military Aid to Disputing Neighbors
z6 <- cvars$fivevote            # Country: UN- UN Vote distance with U.S.

# Model 5 estimates

inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           b.age=runif(ncountry,0,0.1),
                           b.male=runif(ncountry,-1,1),
                           b.ses=runif(ncountry,-1,1),
                           b.ed2=runif(ncountry,-1,1),
                           b.ed3=runif(ncountry,-1,1),
                           b.satis=runif(ncountry,-1,1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,9),
                           ed2=ed2init,
                           ed3=ed3init,
                           news=newsinit,
                           pious=piousinit,
                           ses=sesinit,
                           p.pious=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm-two.txt"
bugs.model <- bugs(data=c("y","z1","z2","z3","z4","z5", "z6","pious","news",
                          "age","male","ses","ed2","ed3","satis",
                          "cnum","N","ncountry"),
                   inits=inits,
                   parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                        "b.age","b.male","b.ses","b.ed2","b.ed3","b.satis",
                                        "sd.b","sd.y","mu.pious","mu.news",
                                        "mu.age","mu.male","mu.ses","mu.ed2","mu.ed3","mu.satis"),
                   model.file=model.file,
                   n.chains=3,
                   n.iter=500,
                   debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)

round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

round(bugs.model$mean$mu.age,2)
round(bugs.model$sd$mu.age,2)

round(bugs.model$mean$mu.male,2)
round(bugs.model$sd$mu.male,2)

round(bugs.model$mean$mu.ses,2)
round(bugs.model$sd$mu.ses,2)

round(bugs.model$mean$mu.ed2,2)
round(bugs.model$sd$mu.ed2,2)

round(bugs.model$mean$mu.ed3,2)
round(bugs.model$sd$mu.ed3,2)

round(bugs.model$mean$mu.satis,2)
round(bugs.model$sd$mu.satis,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)

# Model 6 estimates

#Model 6, with z1 as percent religious instead of percent seeing a struggle
z1 <- cvars$pctrel              # Country: percent or total religious

inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           b.age=runif(ncountry,0,0.1),
                           b.male=runif(ncountry,-1,1),
                           b.ses=runif(ncountry,-1,1),
                           b.ed2=runif(ncountry,-1,1),
                           b.ed3=runif(ncountry,-1,1),
                           b.satis=runif(ncountry,-1,1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,9),
                           ed2=ed2init,
                           ed3=ed3init,
                           news=newsinit,
                           pious=piousinit,
                           ses=sesinit,
                           p.pious=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm-two.txt"
bugs.model <- bugs(data=c("y","z1","z2","z3","z4","z5", "z6","pious","news",
                          "age","male","ses","ed2","ed3","satis",
                          "cnum","N","ncountry"),
                   inits=inits,
                   parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                        "b.age","b.male","b.ses","b.ed2","b.ed3","b.satis",
                                        "sd.b","sd.y","mu.pious","mu.news",
                                        "mu.age","mu.male","mu.ses","mu.ed2","mu.ed3","mu.satis"),
                   model.file=model.file,
                   n.chains=3,
                   n.iter=500,
                   debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)

round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

round(bugs.model$mean$mu.age,2)
round(bugs.model$sd$mu.age,2)

round(bugs.model$mean$mu.male,2)
round(bugs.model$sd$mu.male,2)

round(bugs.model$mean$mu.ses,2)
round(bugs.model$sd$mu.ses,2)

round(bugs.model$mean$mu.ed2,2)
round(bugs.model$sd$mu.ed2,2)

round(bugs.model$mean$mu.ed3,2)
round(bugs.model$sd$mu.ed3,2)

round(bugs.model$mean$mu.satis,2)
round(bugs.model$sd$mu.satis,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)

################################################################################
## Models 7-11: Robustness checks with other z3's in the upper-level model.
################################################################################
# Model 7 estimates

inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.pious=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm.txt"

z1 <- cvars$struggle                # Country: percent seeing a reformer-Islamist struggle
z2 <- cvars$pctmus                  # Country: percent Muslim in country
#z3.list[[1]] <- cvars$log.gdppc     # Country: log GDP per capita
z3 <- cvars$log.usimppc   # Country: Imports from US per capita, log
#z3.list[[3]] <- cvars$usaid/1000    # Country: US foreign economic aid (billions)
#z3.list[[4]] <- cvars$m2j/1000      # Country: miles from country capital to Jerusalem (1000s)
#z3.list[[5]] <- cvars$milexp        # Country: Military expenditure
#z3.list[[6]] <- cvars$fh.pr         # Country: Political rights
z4 <- cvars$fivedisp             # Country: MID- Militarized disputes
z5 <- cvars$fiveexp            # Country: EXP- U.S. Military Aid to Disputing Neighbors
z6 <- cvars$fivevote            # Country: UN- UN Vote distance with U.S.

  bugs.model <- bugs(data=c("y","pious","news","z1","z2","z3","z4", "z5", "z6","cnum","N","ncountry"),
                     inits=inits,
                     parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                          "mu.country","mu.pious","mu.news","sd.b","sd.y"),
                     model.file=model.file,
                     n.chains=3,
                     n.iter=500,
                     debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)

round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)

# Model 8 estimates

inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.pious=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm.txt"

z1 <- cvars$struggle                # Country: percent seeing a reformer-Islamist struggle
z2 <- cvars$pctmus                  # Country: percent Muslim in country
#z3.list[[1]] <- cvars$log.gdppc     # Country: log GDP per capita
#z3.lis[[2]] <- cvars$log.usimppc   # Country: Imports from US per capita, log
z3 <- cvars$usaid/1000    # Country: US foreign economic aid (billions)
#z3.list[[4]] <- cvars$m2j/1000      # Country: miles from country capital to Jerusalem (1000s)
#z3.list[[5]] <- cvars$milexp        # Country: Military expenditure
#z3.list[[6]] <- cvars$fh.pr         # Country: Political rights
z4 <- cvars$fivedisp             # Country: MID- Militarized disputes
z5 <- cvars$fiveexp            # Country: EXP- U.S. Military Aid to Disputing Neighbors
z6 <- cvars$fivevote            # Country: UN- UN Vote distance with U.S.

bugs.model <- bugs(data=c("y","pious","news","z1","z2","z3","z4","z5","z6","cnum","N","ncountry"),
                     inits=inits,
                     parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                          "mu.country","mu.pious","mu.news","sd.b","sd.y"),
                     model.file=model.file,
                     n.chains=3,
                     n.iter=500,
                     debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)


round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)

# Model 9 estimates
inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.pious=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm.txt"

z1 <- cvars$struggle                # Country: percent seeing a reformer-Islamist struggle
z2 <- cvars$pctmus                  # Country: percent Muslim in country
#z3.list[[1]] <- cvars$log.gdppc     # Country: log GDP per capita
#z3.list[[2]] <- cvars$log.usimppc   # Country: Imports from US per capita, log
#z3.list[[3]] <- cvars$usaid/1000    # Country: US foreign economic aid (billions)
z3<- cvars$m2j/1000      # Country: miles from country capital to Jerusalem (1000s)
#z3.list[[5]] <- cvars$milexp        # Country: Military expenditure
#z3.list[[6]] <- cvars$fh.pr         # Country: Political rights
z4 <- cvars$fivedisp             # Country: MID- Militarized disputes
z5 <- cvars$fiveexp            # Country: EXP- U.S. Military Aid to Disputing Neighbors
z6 <- cvars$fivevote            # Country: UN- UN Vote distance with U.S.


  bugs.model <- bugs(data=c("y","pious","news","z1","z2","z3", "z4", "z5","z6","cnum","N","ncountry"),
                     inits=inits,
                     parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                          "mu.country","mu.pious","mu.news","sd.b","sd.y"),
                     model.file=model.file,
                     n.chains=3,
                     n.iter=500,
                     debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)
  

round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)

#Model 10 estimates
inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.pious=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm.txt"

z1 <- cvars$struggle                # Country: percent seeing a reformer-Islamist struggle
z2 <- cvars$pctmus                  # Country: percent Muslim in country
#z3.list[[1]] <- cvars$log.gdppc     # Country: log GDP per capita
#z3.list[[2]] <- cvars$log.usimppc   # Country: Imports from US per capita, log
#z3.list[[3]] <- cvars$usaid/1000    # Country: US foreign economic aid (billions)
#z3.list[[4]] <- cvars$m2j/1000      # Country: miles from country capital to Jerusalem (1000s)
z3<- cvars$milexp        # Country: Military expenditure
#z3.list[[6]] <- cvars$fh.pr         # Country: Political rights
z4 <- cvars$fivedisp             # Country: MID- Militarized disputes
z5 <- cvars$fiveexp            # Country: EXP- U.S. Military Aid to Disputing Neighbors
z6 <- cvars$fivevote            # Country: UN- UN Vote distance with U.S.


  bugs.model <- bugs(data=c("y","pious","news","z1","z2","z3","z4","z5","z6","cnum","N","ncountry"),
                     inits=inits,
                     parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                          "mu.country","mu.pious","mu.news","sd.b","sd.y"),
                     model.file=model.file,
                     n.chains=3,
                     n.iter=500,
                     debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)
  
round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)

#Model 11 estimates
inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(7,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.pious=0.6,
                           p.news=0.6)
}

model.file <- "AA-GAP-hlm.txt"

z1 <- cvars$struggle                # Country: percent seeing a reformer-Islamist struggle
z2 <- cvars$pctmus                  # Country: percent Muslim in country
#z3.list[[1]] <- cvars$log.gdppc     # Country: log GDP per capita
#z3.list[[2]] <- cvars$log.usimppc   # Country: Imports from US per capita, log
#z3.list[[3]] <- cvars$usaid/1000    # Country: US foreign economic aid (billions)
#z3.list[[4]] <- cvars$m2j/1000      # Country: miles from country capital to Jerusalem (1000s)
#z3.list[[5]] <- cvars$milexp        # Country: Military expenditure
z3<- cvars$fh.pr         # Country: Political rights
z4 <- cvars$fivedisp             # Country: MID- Militarized disputes
z5 <- cvars$fiveexp            # Country: EXP- U.S. Military Aid to Disputing Neighbors
z6 <- cvars$fivevote            # Country: UN- UN Vote distance with U.S.

  bugs.model <- bugs(data=c("y","pious","news","z1","z2","z3","z4","z5","z6","cnum","N","ncountry"),
                     inits=inits,
                     parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                          "mu.country","mu.pious","mu.news","sd.b","sd.y"),
                     model.file=model.file,
                     n.chains=3,
                     n.iter=500,
                     debug=FALSE, bugs.directory="D:/WinBUGS14", summary.only=FALSE)
  

round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

bugs.model$summary[,c('2.5%','97.5%')]

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)

# end of file.
############################################################################



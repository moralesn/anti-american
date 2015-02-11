# Author: Norberto R. Morales
# Project: Anti-American Public Opinion
# Purpose: Cleaning data and creating variables
# Last Updated: 06/25/2014
# Machine(Windows 7): S/No: 0039130478
############################################################################
## Clean data, identify variables, and explore data.
## Variables are to be employed in a Hierchierchical/Multi-Level Model 
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

## Bar charts of media content analysis
## number of articles positive, neutral, negative

sen.sol <- c(35,22,8)
sen.sud <- c(15,14,15)
sen <- sen.sol+sen.sud

mor.auj <- c(23,40,40)
mor.mat <- c(23,38,18)
mor <- mor.auj+mor.mat

turk.hurr <- c(7,24,49)
turk.zam <- c(16,18,37)
turk <- turk.hurr+turk.zam

windows(6,3.5)
par(mar=c(2.5,4,1,1),las=1)
barplot(100*cbind(sen/sum(sen),mor/sum(mor),turk/sum(turk)),beside=T,
        ylab="Percent of articles",axes=T,ylim=c(0,70),axis.lty=1,
        col=c("gray90","gray60","gray30"),cex.lab=1.2,cex.names=1.2,
        names.arg=c("Senegal","Morocco","Turkey"),cex.axis=1.2,
        legend.text=c("Positive","Neutral","Negative"),
        args.legend=list(x=10.5,y=72,bty="n",ncol=3))
text(seq(1.5,11.5,1),5+100*c(sen/sum(sen),NA,mor/sum(mor),NA,turk/sum(turk)),
     round(100*c(sen/sum(sen),0,mor/sum(mor),0,turk/sum(turk))))
#savePlot("mediabars",type="pdf")

## Pew GAP 2007 ##
## http://pewglobal.org/database/?indicator=1&survey=8&response=Favorable&mode=table
## http://pewglobal.org/datasets/

# open data in SPSS format
library(foreign)

gap07 <- read.spss("Data-Raw/Pew-GAP 2007/GAP_2007_Data.sav",to.data.frame=T) # read data file 

# # export data frame to Stata binary format 
# write.dta(gap07, "Data-Raw/gap07.dta",convert.factors = c("labels", "string", "numeric", "codes"))
# 
# # read Stata data file 
# gap07 <- read.dta("Data/gap07.dta",convert.factors = TRUE,
#                   missing.type = FALSE)

dat <- gap07
dat$cnum <- as.numeric(dat$country)

# Muslim respondents
levels(dat$country)[levels(dat$country)=="Palestinian Territories"] <- "Palestinian ter."
dat$muspray <- 0+(!is.na(dat$Q114))     # 11436 Muslims asked about prayer
dat$Q44GHA[is.na(dat$Q44GHA)] <- "Don't know"
dat$Q44KEN[is.na(dat$Q44KEN)] <- "Don't know"
levels(dat$Q44INDA) <- c(levels(dat$Q44INDA),"Don't know")
dat$Q44INDA[is.na(dat$Q44INDA)] <- "Don't know"
dat$Q44MOR[is.na(dat$Q44MOR)] <- "Don't know"
dat$Q44UGA[is.na(dat$Q44UGA)] <- "Other "
dat$muslim <- 0+(dat$muspray==1 | dat$Q44GHA=="Muslim" | dat$Q44KEN=="Islam" |
                 dat$Q44INDA=="Muslim" | dat$Q44MOR=="Islam" | dat$Q44UGA=="Islam")

## Data set of 12,831 Muslims in 21 countries
mus <- dat[dat$muslim==1,]
cnames <- names(table(mus$country)[table(mus$country)>0])
mus$cnum2 <- recode(mus$cnum,"2=1;10=2;11=3;14=4;16=5;17=6;20=7;22=8;
                              23=9;24=10;25=11;26=12;27=13;29=14;30=15;
                              31=16;32=17;36=18;42=19;43=20;45=21")

## Create variables for analysis

## Attitudes towards the United States
mus$us <- as.numeric(mus$Q16A)          # [1] opinions of US (4=worst)
mus$us[mus$us>4] <- NA

mus$amer <- as.numeric(mus$Q16B)        # [2] opinions of Americans (4=worst)
mus$amer[mus$amer>4] <- NA

mus$spread <- as.numeric(mus$Q27)       # [3] spread of American customs (2=bad)
mus$spread[mus$spread>2] <- NA

mus$democ <- as.numeric(mus$Q28)        # [4] American ideas re democracy (2=dislike)
mus$democ[mus$democ>2] <- NA

mus$bus <- as.numeric(mus$Q29)          # [5] American ideas re business (2=dislike)
mus$bus[mus$bus>2] <- NA

mus$movies <- as.numeric(mus$Q30)       # [6] American music, movies, TV (2=dislike)
mus$movies[mus$movies>2] <- NA

mus$admire <- as.numeric(mus$Q31)       # [7] Admire America's tech (2=do not admire)
mus$admire[mus$admire>2] <- NA

mus$us.scale <- rowMeans(cbind((mus$us-1)/3,(mus$amer-1)/3,mus$spread-1,
                         mus$democ-1,mus$bus-1,mus$movies-1,mus$admire-1),na.rm=T)
mus$us.scale[is.na(mus$us.scale)] <- NA

# Anti-Americanism scale has a Cronbach's alpha of 0.78
cronbach.alpha(cbind((mus$us-1)/3,(mus$amer-1)/3,mus$spread-1,
                         mus$democ-1,mus$bus-1,mus$movies-1,mus$admire-1),na.rm=T)

## Attitudes about religion and government
mus$struggle <- 2-as.numeric(mus$Q75)   # 1=think there is a struggle
mus$struggle[mus$struggle<0] <- NA      # between modernizers/Fundamentalists

mus$id.fund <- as.numeric(mus$Q75B)-1   # 1=identify with fundamentalists, if see struggle
mus$id.fund[mus$id.fund>1] <- NA

## Religiosity
mus$pray <- as.numeric(mus$Q114)
mus$pray[mus$pray>7] <- NA
mus$pray5 <- (mus$pray==7)+0            # 1=pray 5x/day

mus$fast <- as.numeric(mus$Q116)
mus$fast[mus$fast>4] <- NA

mus$relimpt <- as.numeric(mus$Q117)
mus$relimpt[mus$relimpt>4] <- NA
mus$relimpt <- 5-mus$relimpt

table(mus$fast,mus$relimpt,mus$pray5)
table(mus$relimpt)/sum(table(mus$relimpt))
table(mus$fast[mus$relimpt==4])/sum(table(mus$fast[mus$relimpt==4]))
table(mus$pray5[mus$relimpt==4])/sum(table(mus$pray5[mus$relimpt==4]))

mus$pious <- ((mus$pray5==1) & (mus$fast>=3) & (mus$relimpt==4)) + 0
mus$pious[is.na(mus$pray5) | is.na(mus$fast)] <- NA

# Validate dichotomous coding scheme with a latent class model
if (F) {
mus$pray5b <- mus$pray5+1
lc <- poLCA(cbind(pray5b,fast,relimpt)~1,mus,nclass=2,na.rm=F)
plot(lc)
# posterior classification finds expected clusters
table(lc$predclass,mus$pious)
windows()
hist(lc$posterior[,1])
}


## Attitudinal scale: policy and economic evaluations
mus$righttrack <- 2-as.numeric(mus$Q7)      # [1] satisfied with how things going in country
mus$righttrack[mus$righttrack<0] <- NA

mus$econsit <- (4-as.numeric(mus$Q11))/3    # [2] current economic situation in country
mus$econsit[mus$econsit<0] <- NA

mus$kids <- (2-as.numeric(mus$Q13))         # [3] children better off or worse off than people are now?
mus$kids[mus$kids<0] <- NA

mus$govt <- (4-as.numeric(mus$Q23A))/3      # [4] influence of national government
mus$govt[mus$govt<0] <- NA

mus$satisfied <- rowMeans(cbind(mus$righttrack,mus$econsit,mus$kids,mus$govt),na.rm=T)
mus$satisfied[is.na(mus$satisfied)] <- NA

## Media awareness
mus$news <- as.numeric(mus$Q37)-1       # follow int'l news (2=most of the time)
mus$news[mus$news>1] <- NA

# compare to percentages in other countries
table(dat$country,dat$Q37)
sort(table(dat$country,dat$Q37)[,2]/rowSums(table(dat$country,dat$Q37)[,1:2]))
sort(table(mus$country,mus$news)[,2]/rowSums(table(mus$country,mus$news)))


## Demographic variables: age, gender, economic status, education
mus$male <- 2-as.numeric(mus$Q107)  # 1=male, 0=female
mus$age <- as.numeric(mus$Q108)
mus$age[mus$age>97] <- NA


# Socioeconomic status
sesvars <- c("Q123BAN","Q123EGY","Q123ETH","Q123GHA","Q123INDA","Q123INDO","Q123IVO",
             "Q123JOR","Q123KEN","Q123KUW","Q123LEB","Q123MALA","Q123MALI","Q123MOR",
             "Q123NIG","Q123PAK","Q123PAL","Q123SEN","Q123TAN","Q123TUR","Q123UGA")
sesmat <- mus[,sesvars]
mus$ses <- NA
for (i in 1:ncol(sesmat)) {
    seltab <- table(sesmat[,i])
    for (j in 1:length(seltab)) {
        mus$ses[(mus$country==cnames[i]) & (sesmat[,i]==names(seltab)[j])] <- j-1
    }
    mus$ses[grep("Refused",sesmat[,i])] <- NA
    mus$ses[grep("know",sesmat[,i])] <- NA
    mus$ses[mus$country==cnames[i]] <- mus$ses[mus$country==cnames[i]]/max(mus$ses[mus$country==cnames[i]],na.rm=T)
}


# Education level
edvars <- c("Q118BAN","Q118EGY","Q118ETH","Q118GHA","Q118INDA","Q118INDO","Q118IVO",
            "Q118JOR","Q118KEN","Q118KUW","Q118LEB","Q118MALA","Q118MALI","Q118MOR",
            "Q118NIG","Q118PAK","Q118PAL","Q118SEN","Q118TAN","Q118TUR","Q118UGA")
edmat <- mus[,edvars]   # highest level of education
apply(edmat,2,table)
edlevels <- sort(names(table(as.matrix(edmat)))) # 71 outcome categories
# in Pakistan, "Matric" means secondary school

primary <- c(" Incomplete primary school",
                " No formal education",
                "5-9 classes",
                "Complete primary",
                "Complete primary school",
                "Completed elementary",
                "Completed primary school",
                "Did not complete intermediate",
                "Illiterate",
                "Incomplete primary",
                "Incomplete primary school",
                "Less than 5 classes",
                "Literate but no formal schooling",
                "No formal education",
                "No formal education but can read and write",
                "School up to 4 years",
                "School up to 5 to 9 years",
                "Some elementary or less")

secondary <- c("Complete JSS/Middle school",
                "Complete Polytechnic",
                "Complete secondary (preparatory)",
                "Complete secondary (vocational)",
                "Complete secondary school",
                "Complete secondary school technical/vocational type",
                "Complete secondary school: technical/vocational type",
                "Complete Secondary University-preparatory type",
                "Complete secondary: university-preparatory type",
                "Complete SSS/Vocational/Technical/ School",
                "Completed vocational/technical institute",
                "Completed complementary: not sec/vocational ",
                "Completed secondary",
                "Completed secondary school",
                "Did not complete secondary",
                "Incomplete JSS/Middle school",
                "Incomplete Polytechnic",
                "Incomplete secondary (preparatory)",
                "Incomplete secondary (vocational)",
                "Incomplete secondary school",
                "Incomplete secondary school technical/vocational type",
                "Incomplete secondary school: technical/vocational type",
                "Incomplete secondary University-preparatory type",
                "Incomplete secondary: university-preparatory type",
                "Incomplete SSS/Vocational/Technical/ School",
                "Intermediate",
                "Matric",
                "Some secondary",
                "SSC/HSC")

university <- c("Complete College of Education",
                "Completed college",
                "Completed college (Diploma/Certificate)",
                "Completed diploma/ vocational type",
                "Completed university in Middle East/Africa",
                "Completed university in Western Europe/America",
                "Completed university or more",
                "Entered university, did not complete",
                "Graduate",
                "Graduate/Post grad-Gen BA MSc BCom etc",
                "Graduate/Post grad-Prof BE MTech MBA MBBS etc",
                "Incomplete College of Education",
                "Post-graduate",
                "Some college (Diploma/Certifiate)",
                "Some college but not graduated",
                "Some post-secondary (university or technical)",
                "Some university-level education, without a degree",
                "Some university without degree",
                "University-level education, with a degree",
                "University-level education, with degree",
                "University-level education, without degree",
                "University with degree")

other <- c("Don't know","Refused")

length(c(primary,secondary,university,other)) # 71

mus$educ <- NA
for (i in 1:ncol(edmat)) {
    mus$educ[edmat[,i] %in% primary] <- 1
    mus$educ[edmat[,i] %in% secondary] <- 2
    mus$educ[edmat[,i] %in% university] <- 3
}

mus$ed2 <- 0+(mus$educ==2)
mus$ed3 <- 0+(mus$educ==3)


## Set up country-level variables:

# Percent of country that is Muslim. Source: Pew study, http://pewforum.org/docs/?DocID=450
# Distance in 1000 miles between country capital and Jerusalem. Source: Google maps
# Imports from the US, in thousands of dollars, 2007. Source: http://tse.export.gov
# Population, total, 2007. Source: World Bank WDI
# GDP per capita (current US$), 2005. Source: World Bank WDI
# Total Economic Assistance (no military), 2007, in millions, historical 2007 $US
#   Source: U.S. Overseas Loans and Grants [Greenbook] http://gbk.eads.usaidallnet.gov/
# Gallup world poll: http://www.gallup.com/poll/142631/Worldwide-Leadership-Approval.aspx

#cvars <- read.csv("Data-Raw/cvars.csv",row.names=1)
# read Stata data file 
cvars <- read.dta("Data/cvarsALL.dta",convert.factors = TRUE, missing.type = FALSE)

struggle <- (table(mus$cnum,mus$struggle)/rowSums(table(mus$cnum,mus$struggle)))[,2]
struggle[is.na(struggle)] <- NA
cvars$struggle <- struggle
pctrel <- (table(mus$cnum,mus$pious)/rowSums(table(mus$cnum,mus$pious)))[,2]
pctrel[is.na(pctrel)] <- NA
cvars$pctrel <- pctrel
cvars$decdisp  <- cvars$decdisp  #Militarized Disputes
cvars$decexp  <- cvars$decexp  #U.S. Military Aid to Disputing Neighbors
cvars$decvote <-cvars$decvote  #UN Vote Distance with U.S.
cvars$fivedisp  <- cvars$fivedisp  #Militarized Disputes (Five Years)
cvars$fiveexp  <- cvars$fiveexp  #U.S. Military Aid to Disputing Neighbors (Five Years)
cvars$fivevote <-cvars$fivevote  #UN Vote Distance with U.S.
cvars$pctAA <- as.vector((table(mus$cnum,mus$us)/rowSums(table(mus$cnum,mus$us)))[,4])
cvars$meanAA <- sapply(split(mus$us.scale,mus$cnum),mean,na.rm=T)
cvars$log.usimppc <- log(cvars$usimppc)
cvars$log.gdppc <- log(cvars$gdppc)

cvars$gallup <- (100-c(34,6,NA,69,31,46,94,9,82,43,25,23,87,33,73,11,13,76,76,16,84))/100
cor(cvars$gallup,cvars$meanAA,use="complete.obs") 
#plot(cvars$gallup,cvars$meanAA)

## Tabulate pro-/anti-American using a barplot
anti34 <- rowSums((table(mus$cnum,mus$us)/rowSums(table(mus$cnum,mus$us)))[,3:4])
o <- order(anti34)

windows()
par(mar=c(2.5,8,1,1),las=1)
barplot(t(table(mus$cnum,mus$us)/rowSums(table(mus$cnum,mus$us)))[2:1,o],
        horiz=T,names.arg=cnames[o],xlim=c(-1.1,1.1),xaxt="n",space=0.5,cex.names=1.2)
barplot(-t(table(mus$cnum,mus$us)/rowSums(table(mus$cnum,mus$us)))[3:4,o],
        horiz=T,add=T,xaxt="n",yaxt="n",space=0.5)
axis(1,at=seq(-1,1,0.2),labels=c(seq(100,0,-20),seq(20,100,20)))
abline(v=0)
text(rep(-0.95,21),1.5*c(1:21)-0.65,format(round(100*anti34[o])),pos=2)
text(rep(1.05,21),1.5*c(1:21)-0.65,format(100-round(100*anti34[o])),pos=2)
mtext("Percent anti-American Percent pro-American",side=3,font=2)
#savePlot("countryAA",type="pdf")

## Scatterplot average AA vs. pct seeing a struggle (asked in 17 countries)
windows(5.5,5.5)
par(mar=c(4.5,4,1,1),las=1)
plot(meanAA~struggle,cvars,xlim=c(0,0.8),ylim=c(0.1,1),col="white",
        cex.axis=1.2,cex.lab=1.2,
        xlab="Proportion seeing a reformer-fundamentalist struggle",
        ylab="Average unfavorability of United States")
text(cvars$struggle,cvars$meanAA,cnames)
#savePlot("AAstruggle",type="pdf")

## Scatterplot percent strongly AA vs. pct seeing a struggle (asked in 17 countries)
windows(5.5,5.5)
par(mar=c(4.5,4,1,1),las=1)
plot(pctAA~struggle,cvars,xlim=c(0,0.8),ylim=c(0,1),col="white",
        cex.axis=1.2,cex.lab=1.2,
        xlab="Proportion seeing a reformer-fundamentalist struggle",
        ylab="Proportion strongly unfavorable of the United States")
text(cvars$struggle,cvars$pctAA,cnames)
#savePlot("AAstruggle2",type="pdf")

## Plot mean AA vs. percent religious
windows(5.5,5.5)
par(mar=c(4.5,4,1,1),las=1)
plot(meanAA~pctrel,cvars,ylim=c(0.1,1),xlim=c(0.2,1),col="white",
        cex.axis=1.2,cex.lab=1.2,
        xlab="Proportion highly religious",
        ylab="Average unfavorability of United States")
text(cvars$pctrel,cvars$meanAA,cnames)
#savePlot("AAreligious",type="pdf")

#Save data frames in R format
save(cnames, file="Data/cnames.Rda")
save(cvars, file="Data/cvars.Rda")
save(dat, file="Data/dta.Rda")
save(edmat, file="Data/edmat.Rda")
save(gap07, file="Data/gap07.Rda")
save(mus, file="Data/mus.Rda")
save(sesmat, file="Data/sesmat.Rda")

# end of file
############################################################################


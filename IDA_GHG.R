# R script to perform IDA on RECS 1990-2015 #####
# outcome variable decomposed: GHG emissions

rm(list=ls()) # clear workspace
cat("\014") # clear console
graphics.off() # remove graphics windows
setwd("C:/Users/pb637/Documents/Yale Courses/Research/RECS research/")
# packages
library(dplyr)
library(ggplot2)
library(abind)
library(reshape2)
library(magrittr)
library(treemap)
# load prepared data of combined RECS surveys 1990-2015, with estimates of GHG emissions, including weather adjusted energy/GHG 
load("RECSGHG.Rdata")
r<-RECSGHG
# extract years
r90<-filter(r,r$RECSYEAR==1990)
r93<-filter(r,r$RECSYEAR==1993)
r97<-filter(r,r$RECSYEAR==1997)
r01<-filter(r,r$RECSYEAR==2001)
r15<-filter(r,r$RECSYEAR==2015)
r09<-filter(r,r$RECSYEAR==2009)
r05<-filter(r,r$RECSYEAR==2005)
## some tables for intro ######

# # barchart of final energyeach year ######

sph<-tapply(r$BTUSPH*r$NWEIGHT,r$RECSYEAR,sum)
spc<-tapply(r$BTUCOL*r$NWEIGHT,r$RECSYEAR,sum)
dhw<-tapply(r$BTUDHW*r$NWEIGHT,r$RECSYEAR,sum)
oth<-tapply(r$BTUOTH*r$NWEIGHT,r$RECSYEAR,sum)

eu<-as.data.frame(rbind(sph,spc,dhw,oth))

eub<-as.data.frame(as.numeric(unlist(eu)))
colnames(eub)<-"Energy"
eub$Energy<-1.055e-12*eub$Energy # convert to EJ
eub$EndUse<-rep(as.factor(c("Space Heating","Space Cooling","Hot Water","All Other")),7)
eub$Year<-rep(colnames(eu),each=4)
p <- ggplot(eub, aes(x = Year, y = Energy,group = EndUse))+ #ylim(0,12.7) +
  geom_col(aes(fill = EndUse), width = 0.7) +
  labs(title = "a) Final energy by end-use, 1990-2015", y = "EJ/yr") + theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) + scale_fill_brewer(palette="Dark2") 
windows(width = 6, height = 7)
p + theme(legend.position = "None")

# # barchart of weather adjusted final energy enduses each year ######

sph<-tapply(r$BTUSPHwa*r$NWEIGHT,r$RECSYEAR,sum)
spc<-tapply(r$BTUCOLwa*r$NWEIGHT,r$RECSYEAR,sum)
dhw<-tapply(r$BTUDHWwa*r$NWEIGHT,r$RECSYEAR,sum)
oth<-tapply(r$BTUOTH*r$NWEIGHT,r$RECSYEAR,sum)

euwa<-as.data.frame(rbind(sph,spc,dhw,oth))

eubwa<-as.data.frame(as.numeric(unlist(euwa)))
colnames(eubwa)<-"Energy"
eubwa$Energy<-1.055e-12*eubwa$Energy # convert to EJ
eubwa$EndUse<-rep(as.factor(c("Space Heating","Space Cooling","Hot Water","All Other")),7)
eubwa$Year<-rep(colnames(euwa),each=4)
p <- ggplot(eubwa, aes(x = Year, y = Energy,group = EndUse))+ ylim(0,12.6) +
  geom_col(aes(fill = EndUse), width = 0.7) +
  # theme_minimal() +
  labs(title = "Weather-adjusted FE by end-use, 1990-2015", y = "EJ") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2") 
windows()
p + scale_y_continuous(breaks = seq(0,12.5,2.5))
# 

# # barchart of primary energy enduses each year ######

sph<-tapply(r$PESPH*r$NWEIGHT,r$RECSYEAR,sum)
spc<-tapply(r$PECOL*r$NWEIGHT,r$RECSYEAR,sum)
dhw<-tapply(r$PEDHW*r$NWEIGHT,r$RECSYEAR,sum)
oth<-tapply(r$PEOTH*r$NWEIGHT,r$RECSYEAR,sum)

euwa<-as.data.frame(rbind(sph,spc,dhw,oth))

eubwa<-as.data.frame(as.numeric(unlist(euwa)))
colnames(eubwa)<-"Energy"
eubwa$Energy<-1.055e-12*eubwa$Energy # convert to EJ
eubwa$EndUse<-rep(as.factor(c("Space Heating","Space Cooling","Hot Water","All Other")),7)
eubwa$Year<-rep(colnames(euwa),each=4)
p <- ggplot(eubwa, aes(x = Year, y = Energy,group = EndUse))+ #ylim(0,12560) +
  geom_col(aes(fill = EndUse), width = 0.7) +
  labs(title = "b) Primary Energy by end-use, 1990-2015", y = "EJ/yr") + theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face = "bold"),
        plot.title = element_text(size = 17, face = "bold")) + scale_fill_brewer(palette="Dark2") 
windows(width = 6, height = 7)
p + theme(legend.position = "None")
# 

# # barchart of weather adjusted primary energy enduses each year ######

sph<-tapply(r$PESPHwa*r$NWEIGHT,r$RECSYEAR,sum)
spc<-tapply(r$PECOLwa*r$NWEIGHT,r$RECSYEAR,sum)
dhw<-tapply(r$PEDHWwa*r$NWEIGHT,r$RECSYEAR,sum)
oth<-tapply(r$PEOTH*r$NWEIGHT,r$RECSYEAR,sum)

euwa<-as.data.frame(rbind(sph,spc,dhw,oth))

eubwa<-as.data.frame(as.numeric(unlist(euwa)))
colnames(eubwa)<-"Energy"
eubwa$Energy<-1.055e-12*eubwa$Energy # convert to EJ
eubwa$EndUse<-rep(as.factor(c("Space Heating","Space Cooling","Hot Water","All Other")),7)
eubwa$Year<-rep(colnames(euwa),each=4)
p <- ggplot(eubwa, aes(x = Year, y = Energy,group = EndUse))+ #ylim(0,12560) +
  geom_col(aes(fill = EndUse), width = 0.7) +
  labs(title = "Weather-adjusted PE by end-use, 1990-2015", y = "EJ") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2") 
windows(width = 6, height = 7)
p
# 

# # barchart of  GHG by enduses each year ######

sph<-tapply(r$GHGSPH*r$NWEIGHT,r$RECSYEAR,sum)
spc<-tapply(r$GHGCOL*r$NWEIGHT,r$RECSYEAR,sum)
dhw<-tapply(r$GHGDHW*r$NWEIGHT,r$RECSYEAR,sum)
oth<-tapply(r$GHGOTH*r$NWEIGHT,r$RECSYEAR,sum)

euwa<-as.data.frame(rbind(sph,spc,dhw,oth))

eubwa<-as.data.frame(as.numeric(unlist(euwa)))
colnames(eubwa)<-"Energy"
eubwa$Energy<-1e-9*eubwa$Energy # convert to Mt
eubwa$EndUse<-rep(as.factor(c("Space Heating","Space Cooling","Hot Water","All Other")),7)
eubwa$Year<-rep(colnames(euwa),each=4)
p <- ggplot(eubwa, aes(x = Year, y = Energy,group = EndUse))+ #ylim(0,12560) +
  geom_col(aes(fill = EndUse), width = 0.7) +
  # theme_minimal() +
  labs(title = "c) GHG by end-use, 1990-2015", y = "Mt/yr") + theme_bw() +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) + scale_fill_brewer(palette="Dark2") 
windows(width = 6, height = 7)
p + theme(legend.position = "None")
# 

# # barchart of weather adjusted GHG by enduses each year ######

sph<-tapply(r$GHGSPHwa*r$NWEIGHT,r$RECSYEAR,sum)
spc<-tapply(r$GHGCOLwa*r$NWEIGHT,r$RECSYEAR,sum)
dhw<-tapply(r$GHGDHWwa*r$NWEIGHT,r$RECSYEAR,sum)
oth<-tapply(r$GHGOTH*r$NWEIGHT,r$RECSYEAR,sum)

euwa<-as.data.frame(rbind(sph,spc,dhw,oth))

eubwa<-as.data.frame(as.numeric(unlist(euwa)))
colnames(eubwa)<-"Energy"
eubwa$Energy<-1e-9*eubwa$Energy # convert to Mt
eubwa$EndUse<-rep(as.factor(c("Space Heating","Space Cooling","Hot Water","All Other")),7)
eubwa$Year<-rep(colnames(euwa),each=4)
p <- ggplot(eubwa, aes(x = Year, y = Energy,group = EndUse))+ #ylim(0,12560) +
  geom_col(aes(fill = EndUse), width = 0.7) +
  # theme_minimal() +
  labs(title = "Weather-adjusted GHG by end-use, 1990-2015", y = "Mt") +
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=12,face = "bold"),
        plot.title = element_text(size = 14, face = "bold")) + scale_fill_brewer(palette="Dark2") +
  windows()
p
# 

## some small data cleaning/organizing ######
# remove year 1997 because of poor data quality
r<-RECSGHG[!(RECSGHG$RECSYEAR==1997),]
r$AgeCohort<-as.character(r$AgeCohort) # added to debug changing Cohort levels
r[r$AgeCohort=="1990s" | r$AgeCohort == "2000s" | r$AgeCohort =="2010s","AgeCohort"]<-"1990+" # problem with invalid factor level here. Added the lines above and below to debug this.
r$AgeCohort<-as.factor(r$AgeCohort) # added to debug changing Cohort levels
r$count<-1
r$TOTBTUnew<-r$BTUSPH+r$BTUCOL+r$BTUDHW+r$BTUOTH

r$AIRCOND[r$BTUCOLwa==0]<-0
r$HeatFuel<-r$FUELHEAT
r$HeatFuel[r$HeatFuel==4]<-"3"
r$HeatFuel[r$HeatFuel==2 | r$HeatFuel == 6 | r$HeatFuel==7 | r$HeatFuel== 8 | r$HeatFuel ==9]<-"21" # not sure if #2 propane should be included in other
r$HeatFuel<-droplevels(r$HeatFuel)
r$DHWFuel<-r$FUELH2O
r$DHWFuel[r$DHWFuel==2 | r$DHWFuel == 6 | r$DHWFuel==7 | r$DHWFuel== 8 | r$DHWFuel ==9]<-"21"
r$DHWFuel<-droplevels(r$DHWFuel)

# extract years
r90<-filter(r,r$RECSYEAR==1990)
r93<-filter(r,r$RECSYEAR==1993)
r97<-filter(r,r$RECSYEAR==1997)
r01<-filter(r,r$RECSYEAR==2001)
r15<-filter(r,r$RECSYEAR==2015)
r09<-filter(r,r$RECSYEAR==2009)
r05<-filter(r,r$RECSYEAR==2005)

r15$TYPE<-"MF"
r15[r15$TYPEHUQ==2 | r15$TYPEHUQ==3,]$TYPE<-"SF"
r15[r15$TYPEHUQ==1,]$TYPE<-"MH"
r15$TYPE<-as.factor(r15$TYPE)

## IDA for space heating
# decomposition new sph, 1990 #####
E190<-tapply(r90$GHGstSPHwa*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$HeatFuel),sum)  # energy for space heat in 1990 by type, division, agecohort, main fuel type
E190[is.na(E190)]<-0
F90<-tapply(r90$TOTHSQFT*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$HeatFuel),sum) # floor area in 1990 by type and division
F90[is.na(F90)]<-0
I190<-E190/F90 # intensity of heating (1000btu/sqft)
I190[is.nan(I190)]<-0
E190_o<-tapply(r90$GHGSPH*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$HeatFuel),sum)  # energy for space heat in 1990 by type, division, agecohort
E190_o[is.na(E190_o)]<-0
E190_w<-tapply(r90$GHGSPHwa*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$HeatFuel),sum) 
E190_w[is.na(E190_w)]<-0
W190<-E190_o/E190_w
W190[is.na(W190)]<-0
X190<-E190_w/E190
X190[is.na(X190)]<-0
N90<-tapply(r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$HeatFuel),sum)
N90[is.na(N90)]<-0
L190<-F90/N90
L190[is.na(L190)]<-0
Pijkl90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$HeatFuel),sum)
Pijkl90[is.na(Pijkl90)]<-0
O90<-N90/Pijkl90
O90[is.na(O90)]<-0
Pijk90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum)
Pijk90[is.na(Pijk90)]<-0
# portion of population in each type-division-cohort combo with heating fuel x
H90<-array(rep(0,1350),dim(O90))
for (i in 1:length(Pijkl90[1,1,1,])) {
  H90[,,,i]<-Pijkl90[,,,i]/Pijk90
}
H90[is.na(H90)]<-0
Pij90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$TYPEHUQ,r90$DIVISION),sum)
C90<-array(rep(0,270),dim(O90)[1:3])
for (i in 1:length(Pijk90[1,1,])) {
  C90[,,i]<-Pijk90[,,i]/Pij90
}
Pi90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$DIVISION),sum)
T90<-sweep(Pij90,MARGIN = 2, FUN = "/",STATS = Pi90)
P90<-sum(r90$NWEIGHT*r90$NHSLDMEM)
R90<-Pi90/P90
# # identity check equation
e190<-array(rep(0,1350),dim(O90))
# e191<-array(rep(0,270),dim(O90))
CHOLIXW<-e190
CH<-CHOLIXW
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C90*H90[,,,i]
}
CHOLIXW<-CH*O90*L190*I190*X190*W190
PRT<-P90*sweep(T90,MARGIN=2,FUN='*',STATS = R90)

for (i in 1:length((e190[1,1,,1]))) {
  for (j in 1:length(e190[1,1,1,])) {
    e190[,,i,j]<-PRT*CHOLIXW[,,i,j]
  }
}
# check
max(abs(E190_o-e190))

# decomp new sph 1993 ######

E193<-tapply(r93$GHGstSPHwa*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$HeatFuel),sum)  # energy for space heat in 1993 by type, division, agecohort, main fuel type
E193[is.na(E193)]<-0
F93<-tapply(r93$TOTHSQFT*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$HeatFuel),sum) # floor area in 1993 by type and division
F93[is.na(F93)]<-0
I193<-E193/F93 # intensity of heating (1000PE/sqft)
I193[is.nan(I193)]<-0
E193_o<-tapply(r93$GHGSPH*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$HeatFuel),sum)  # energy for space heat in 1993 by type, division, agecohort
E193_o[is.na(E193_o)]<-0
E193_w<-tapply(r93$GHGSPHwa*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$HeatFuel),sum)  # energy for space heat in 1993 by type, division, agecohort
E193_w[is.na(E193_w)]<-0
W193<-E193_o/E193_w
W193[is.na(W193)]<-0
X193<-E193_w/E193
X193[is.na(X193)]<-0
N93<-tapply(r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$HeatFuel),sum)
N93[is.na(N93)]<-0
L193<-F93/N93
L193[is.na(L193)]<-0
Pijkl93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$HeatFuel),sum)
Pijkl93[is.na(Pijkl93)]<-0
O93<-N93/Pijkl93
O93[is.na(O93)]<-0
Pijk93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum)
Pijk93[is.na(Pijk93)]<-0
H93<-array(rep(0,1350),dim(O93))
for (i in 1:length(Pijkl93[1,1,1,])) {
  H93[,,,i]<-Pijkl93[,,,i]/Pijk93
}
H93[is.na(H93)]<-0
Pij93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$TYPEHUQ,r93$DIVISION),sum)
C93<-array(rep(0,270),dim(O93)[1:3])
for (i in 1:length(Pijk93[1,1,])) {
  C93[,,i]<-Pijk93[,,i]/Pij93
}
Pi93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$DIVISION),sum)
T93<-sweep(Pij93,MARGIN = 2, FUN = "/",STATS = Pi93)
P93<-sum(r93$NWEIGHT*r93$NHSLDMEM)
R93<-Pi93/P93
# # identity check equation
e193<-array(rep(0,1350),dim(O93))
# e191<-array(rep(0,270),dim(O93))
CHOLIXW<-e193
CH<-CHOLIXW
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C93*H93[,,,i]
}
CHOLIXW<-CH*O93*L193*I193*X193*W193
PRT<-P93*sweep(T93,MARGIN=2,FUN='*',STATS = R93)
for (i in 1:length((e193[1,1,,1]))) {
  for (j in 1:length(e193[1,1,1,])) {
    e193[,,i,j]<-PRT*CHOLIXW[,,i,j]
  }
}
# check
max(abs(E193_o-e193))

# decomp new sph 2001 ######

E101<-tapply(r01$GHGstSPHwa*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$HeatFuel),sum)  # energy for space heat in 1901 by type, division, agecohort, main fuel type
E101[is.na(E101)]<-0
F01<-tapply(r01$TOTHSQFT*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$HeatFuel),sum) # floor area in 1901 by type and division
F01[is.na(F01)]<-0
I101<-E101/F01 # intensity of heating (1000btu/sqft)
I101[is.nan(I101)]<-0

E101_o<-tapply(r01$GHGSPH*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$HeatFuel),sum)  # energy for space heat in 2001 by type, division, agecohort
E101_o[is.na(E101_o)]<-0
E101_w<-tapply(r01$GHGSPHwa*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$HeatFuel),sum) 
E101_w[is.na(E101_w)]<-0
W101<-E101_o/E101_w
W101[is.na(W101)]<-0
X101<-E101_w/E101 # effect of increase in energy efficiency in electricity generation
X101[is.na(X101)]<-0

N01<-tapply(r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$HeatFuel),sum)
N01[is.na(N01)]<-0
L101<-F01/N01
L101[is.na(L101)]<-0
Pijkl01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$HeatFuel),sum)
Pijkl01[is.na(Pijkl01)]<-0
O01<-N01/Pijkl01
O01[is.na(O01)]<-0
Pijk01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum)
Pijk01[is.na(Pijk01)]<-0
H01<-array(rep(0,1350),dim(O01))
for (i in 1:length(Pijkl01[1,1,1,])) {
  H01[,,,i]<-Pijkl01[,,,i]/Pijk01
}
H01[is.na(H01)]<-0
Pij01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$TYPEHUQ,r01$DIVISION),sum)
C01<-array(rep(0,270),dim(O01)[1:3])
for (i in 1:length(Pijk01[1,1,])) {
  C01[,,i]<-Pijk01[,,i]/Pij01
}
Pi01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$DIVISION),sum)
T01<-sweep(Pij01,MARGIN = 2, FUN = "/",STATS = Pi01)
P01<-sum(r01$NWEIGHT*r01$NHSLDMEM)
R01<-Pi01/P01
# # identity check equation
e101<-array(rep(0,1350),dim(O01))
CHOLIXW<-e101
CH<-CHOLIXW
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C01*H01[,,,i]
}
CHOLIXW<-CH*O01*L101*I101*X101*W101
PRT<-P01*sweep(T01,MARGIN=2,FUN='*',STATS = R01)
# for (a in 1:5) { for (b in 1:dim(O01)[3]){ COLIW[a,,b]<-COLI[a,,b]*W101}}
for (i in 1:length((e101[1,1,,1]))) {
  for (j in 1:length(e101[1,1,1,])) {
    e101[,,i,j]<-PRT*CHOLIXW[,,i,j]
  }
}
# check
max(abs(E101_o-e101))

# decomp new sph 2005 ######

E105<-tapply(r05$GHGstSPHwa*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$HeatFuel),sum)  # energy for space heat in 1905 by type, division, agecohort, main fuel type
E105[is.na(E105)]<-0
F05<-tapply(r05$TOTHSQFT*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$HeatFuel),sum) # floor area in 1905 by type and division
F05[is.na(F05)]<-0
I105<-E105/F05 # intensity of heating (1000btu/sqft)
I105[is.nan(I105)]<-0

E105_o<-tapply(r05$GHGSPH*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$HeatFuel),sum)  # energy for space heat in 2005 by type, division, agecohort
E105_o[is.na(E105_o)]<-0
E105_w<-tapply(r05$GHGSPHwa*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$HeatFuel),sum) 
E105_w[is.na(E105_w)]<-0
W105<-E105_o/E105_w
W105[is.na(W105)]<-0
X105<-E105_w/E105 # effect of increase in energy efficiency in electricity generation
X105[is.na(X105)]<-0

N05<-tapply(r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$HeatFuel),sum)
N05[is.na(N05)]<-0
L105<-F05/N05
L105[is.na(L105)]<-0
Pijkl05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$HeatFuel),sum)
Pijkl05[is.na(Pijkl05)]<-0
O05<-N05/Pijkl05
O05[is.na(O05)]<-0
Pijk05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum)
Pijk05[is.na(Pijk05)]<-0
H05<-array(rep(0,1350),dim(O05))
for (i in 1:length(Pijkl05[1,1,1,])) {
  H05[,,,i]<-Pijkl05[,,,i]/Pijk05
}
H05[is.na(H05)]<-0
Pij05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$TYPEHUQ,r05$DIVISION),sum)
C05<-array(rep(0,270),dim(O05)[1:3])
for (i in 1:length(Pijk05[1,1,])) {
  C05[,,i]<-Pijk05[,,i]/Pij05
}
Pi05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$DIVISION),sum)
T05<-sweep(Pij05,MARGIN = 2, FUN = "/",STATS = Pi05)
P05<-sum(r05$NWEIGHT*r05$NHSLDMEM)
R05<-Pi05/P05
# # identity check equation
e105<-array(rep(0,1350),dim(O05))
# e191<-array(rep(0,270),dim(O05))
CHOLIXW<-e105
CH<-CHOLIXW
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C05*H05[,,,i]
}
CHOLIXW<-CH*O05*L105*I105*X105*W105
PRT<-P05*sweep(T05,MARGIN=2,FUN='*',STATS = R05)
for (i in 1:length((e105[1,1,,1]))) {
  for (j in 1:length(e105[1,1,1,])) {
    e105[,,i,j]<-PRT*CHOLIXW[,,i,j]
  }
}
# check
max(abs(E105_o-e105))

# decomp new sph 2009 ######

E109<-tapply(r09$GHGstSPHwa*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$HeatFuel),sum)  # energy for space heat in 2009 by type, division, agecohort, main fuel type
E109[is.na(E109)]<-0
F09<-tapply(r09$TOTHSQFT*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$HeatFuel),sum) # floor area in 2009 by type and division
F09[is.na(F09)]<-0
I109<-E109/F09 # intensity of heating (1000btu/sqft)
I109[is.nan(I109)]<-0

E109_o<-tapply(r09$GHGSPH*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$HeatFuel),sum)  # energy for space heat in 2009 by type, division, agecohort
E109_o[is.na(E109_o)]<-0
E109_w<-tapply(r09$GHGSPHwa*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$HeatFuel),sum) 
E109_w[is.na(E109_w)]<-0
W109<-E109_o/E109_w
W109[is.na(W109)]<-0
X109<-E109_w/E109 # effect of increase in energy efficiency in electricity generation
X109[is.na(X109)]<-0

N09<-tapply(r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$HeatFuel),sum)
N09[is.na(N09)]<-0
L109<-F09/N09
L109[is.na(L109)]<-0
Pijkl09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$HeatFuel),sum)
Pijkl09[is.na(Pijkl09)]<-0
O09<-N09/Pijkl09
O09[is.na(O09)]<-0
Pijk09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum)
Pijk09[is.na(Pijk09)]<-0
H09<-array(rep(0,1350),dim(O09))
for (i in 1:length(Pijkl09[1,1,1,])) {
  H09[,,,i]<-Pijkl09[,,,i]/Pijk09
}
H09[is.na(H09)]<-0
Pij09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$TYPEHUQ,r09$DIVISION),sum)
C09<-array(rep(0,270),dim(O09)[1:3])
for (i in 1:length(Pijk09[1,1,])) {
  C09[,,i]<-Pijk09[,,i]/Pij09
}
Pi09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$DIVISION),sum)
T09<-sweep(Pij09,MARGIN = 2, FUN = "/",STATS = Pi09)
P09<-sum(r09$NWEIGHT*r09$NHSLDMEM)
R09<-Pi09/P09
# # identity check equation
e109<-array(rep(0,1350),dim(O09))
CHOLIXW<-e109
CH<-CHOLIXW
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C09*H09[,,,i]
}
CHOLIXW<-CH*O09*L109*I109*X109*W109
PRT<-P09*sweep(T09,MARGIN=2,FUN='*',STATS = R09)

for (i in 1:length((e109[1,1,,1]))) {
  for (j in 1:length(e109[1,1,1,])) {
    e109[,,i,j]<-PRT*CHOLIXW[,,i,j]
  }
}
# check
max(abs(E109_o-e109))

# decomp new sph 2015 ######

E115<-tapply(r15$GHGstSPHwa*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$HeatFuel),sum)  # energy for space heat in 1915 by type, division, agecohort, main fuel type
E115[is.na(E115)]<-0
F15<-tapply(r15$TOTHSQFT*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$HeatFuel),sum) # floor area in 1915 by type and division
F15[is.na(F15)]<-0
I115<-E115/F15 # intensity of heating (1000btu/sqft)
I115[is.nan(I115)]<-0

E115_o<-tapply(r15$GHGSPH*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$HeatFuel),sum)  # energy for space heat in 2015 by type, division, agecohort
E115_o[is.na(E115_o)]<-0
E115_w<-tapply(r15$GHGSPHwa*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$HeatFuel),sum) 
E115_w[is.na(E115_w)]<-0
W115<-E115_o/E115_w
W115[is.na(W115)]<-0
X115<-E115_w/E115 # effect of increase in energy efficiency in electricity generation
X115[is.na(X115)]<-0

N15<-tapply(r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$HeatFuel),sum)
N15[is.na(N15)]<-0
L115<-F15/N15
L115[is.na(L115)]<-0
Pijkl15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$HeatFuel),sum)
Pijkl15[is.na(Pijkl15)]<-0
O15<-N15/Pijkl15
O15[is.na(O15)]<-0
Pijk15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum)
Pijk15[is.na(Pijk15)]<-0
H15<-array(rep(0,1350),dim(O15))
for (i in 1:length(Pijkl15[1,1,1,])) {
  H15[,,,i]<-Pijkl15[,,,i]/Pijk15
}
H15[is.na(H15)]<-0
Pij15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$TYPEHUQ,r15$DIVISION),sum)
C15<-array(rep(0,270),dim(O15)[1:3])
for (i in 1:length(Pijk15[1,1,])) {
  C15[,,i]<-Pijk15[,,i]/Pij15
}
Pi15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$DIVISION),sum)
T15<-sweep(Pij15,MARGIN = 2, FUN = "/",STATS = Pi15)
P15<-sum(r15$NWEIGHT*r15$NHSLDMEM)
R15<-Pi15/P15
# # identity check equation
e115<-array(rep(0,1350),dim(O15))
CHOLIXW<-e115
CH<-CHOLIXW
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C15*H15[,,,i]
}
CHOLIXW<-CH*O15*L115*I115*X115*W115
PRT<-P15*sweep(T15,MARGIN=2,FUN='*',STATS = R15)
for (i in 1:length((e115[1,1,,1]))) {
  for (j in 1:length(e115[1,1,1,])) {
    e115[,,i,j]<-PRT*CHOLIXW[,,i,j]
  }
}
# check
max(abs(E115_o-e115))

# 90-93 new sph changes #######
# add small values to e193C and e190C to avoid problems of zeros with log means
ed<-E193_o/E190_o
ed[is.na(ed)|is.infinite(ed)]<-0
e193[e193==0]<-100
e190[e190==0]<-100

le1<-log(e193)-log(e190)
w9093<-(e193-e190)/le1
w9093[is.nan(w9093)]<-0
# population effects
lP1<-log(P93/P90)
delP1<-w9093*lP1
delPtot1<-sum(delP1)
delPpc1<-delPtot1/(sum(e193-e190))
dPpcbas1<-delPtot1/sum(e190)
# regional effects
lR1<-log(R93/R90)
delR1<-sweep(w9093,MARGIN = 2,FUN='*',STATS=lR1)
delRtot1<-sum(delR1)
delRpc1<-delRtot1/(sum(e193-e190))
dRpcbas1<-delRtot1/sum(e190)
# type effects
delT1<-array(rep(0,1350),dim(O93))
lT1<-log(T93/T90)
for (i in 1:length(w9093[1,1,,1])) {
  for (j in 1:length(w9093[1,1,1,])) {
    delT1[,,i,j]<-w9093[,,i,j]*lT1
  }
}
delTtot1<-sum(delT1)
delTpc1<-delTtot1/(sum(e193-e190))
dTpcbas1<-delTtot1/sum(e190)
# cohort effects
noW<-array(rep(1,270),dim(O93))
lC1<-log(C93/C90)
cd<-C93/C90

lC1long<-array(rep(0,1350),dim(O93))
for (i in 1:dim(O93)[4]) {
  lC1long[,,,i] <-lC1
}
for (a in 1:dim(O93)[1]) { for (b in 1:dim(O93)[2]) { for (c in 1:dim(O93)[3]) { for (d in 1:dim(O93)[4]) {
  if (is.nan(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-0}
  if (is.infinite(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b])}
} }}
}
delC1<-w9093*lC1long
delCtot1<-sum(delC1)
delCpc1<-delCtot1/(sum(e193-e190))
dCpcbas1<-delCtot1/sum(e190)
# heating fuel effect
lH1<-log(H93/H90)
for (a in 1:dim(O93)[1]) { for (b in 1:dim(O93)[2]) { for (c in 1:dim(O93)[3]) { for (d in 1:dim(O93)[4]) {
  if (is.nan(lH1[a,b,c,d])) {lH1[a,b,c,d]<-0}
  if (is.infinite(lH1[a,b,c,d])) {lH1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d])}
} }}
}
delH1<-w9093*lH1
delH1[is.na(delH1)|is.infinite(delH1)]<-0
delHtot1<-sum(delH1)
delHpc1<-delHtot1/(sum(e193-e190))
dHpcbas1<-delHtot1/sum(e190)
# occupancy effects
od<-O93/O90
od[is.na(od)|is.infinite(od)]<-0
lO1<-log(O93/O90)
delO1<-w9093*lO1
delO1[is.na(delO1)|is.infinite(delO1)]<-0
delOtot1<-sum(delO1)
delOpc1<-delOtot1/(sum(e193-e190))
dOpcbas1<-delOtot1/sum(e190)
# Lifestyle (house size) effects
ld<-L193/L190
ld[is.na(ld)|is.infinite(ld)]<-0
lL1<-log(L193/L190)
delL1<-w9093*lL1
delL1[is.na(delL1)|is.infinite(delL1)]<-0
delLtot1<-sum(delL1)
delLpc1<-delLtot1/(sum(e193-e190))
dLpcbas1<-delLtot1/sum(e190)
# Intensity effects
id<-I193/I190
id[is.na(id)|is.infinite(id)]<-0
lI1<-log(I193/I190)
for (a in 1:dim(O93)[1]) { for (b in 1:dim(O93)[2]) { for (c in 1:dim(O93)[3]) { for (d in 1:dim(O93)[4]) {
  if (is.nan(lI1[a,b,c,d])) {lI1[a,b,c,d]<-0}
  if (is.infinite(lI1[a,b,c,d])) {lI1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d]+lH1[a,b,c,d]+lO1[a,b,c,d]+lL1[a,b,c,d])}
} }}
}
delI1<-w9093*lI1
delI1[is.na(delI1)|is.infinite(delI1)]<-0
delItot1<-sum(delI1)
delIpc1<-delItot1/(sum(e193-e190))
dIpcbas1<-delItot1/sum(e190)
# electricity efficiency effects 
xd<-X193/X190
xd[is.na(xd)|is.infinite(xd)]<-0
lX1<-log(X193/X190)
delX1<-w9093*lX1
delX1[is.na(delX1)|is.infinite(delX1)]<-0
delXtot1<-sum(delX1)
delXpc1<-delXtot1/(sum(e193-e190))
dXpcbas1<-delXtot1/sum(e190)
# Weather effects
wd<-W193/W190
wd[is.na(wd)|is.infinite(wd)]<-0
lW1<-log(W193/W190)
delW1<-w9093*lW1
delW1[is.na(delW1)|is.infinite(delW1)]<-0
delWtot1<-sum(delW1)
delWpc1<-delWtot1/(sum(e193-e190))
dWpcbas1<-delWtot1/sum(e190)
# Total heating effects
deltot1<-sum(e193-e190)
deltotpc1<-deltot1/sum(e190)
diff1<-e193-e190
sum_eff1<-delP1+delR1+delT1+delC1+delH1+delO1+delL1+delI1+delX1+delW1
db<-sum_eff1-diff1 # for debugging
hpc9093<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity", "Weather"),
                    pc=100*c(deltotpc1,dPpcbas1,dRpcbas1,dTpcbas1,dCpcbas1,dHpcbas1,dOpcbas1,dLpcbas1,dIpcbas1,dXpcbas1,dWpcbas1))
#Turn your 'treatment' column into a character vector
hpc9093$Effect <- as.character(hpc9093$Effect)
#Then turn it back into a factor with the levels in the correct order
hpc9093$Effect <- factor(hpc9093$Effect, levels=unique(hpc9093$Effect))
windows() # heating
ggplot(data=hpc9093, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space heating 1990-1993", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
habs9093<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot1,delPtot1,delRtot1,delTtot1,delCtot1,delHtot1,delOtot1,delLtot1,delItot1,delXtot1,delWtot1))
# check
habs9093$abs[1]-sum(habs9093$abs[2:11])

# 93-01 new sph changes #######
# add small values to e101C and e193C to avoid problems of zeros with log means
ed<-E101_o/E193_o
ed[is.na(ed)|is.infinite(ed)]<-0
e101[e101==0]<-100
e193[e193==0]<-100

le1<-log(e101)-log(e193)
w9301<-(e101-e193)/le1
w9301[is.nan(w9301)]<-0
# population effects
lP1<-log(P01/P93)
delP1<-w9301*lP1
delPtot1<-sum(delP1)
delPpc1<-delPtot1/(sum(e101-e193))
dPpcbas1<-delPtot1/sum(e193)
# regional effects
lR1<-log(R01/R93)
delR1<-sweep(w9301,MARGIN = 2,FUN='*',STATS=lR1)
delRtot1<-sum(delR1)
delRpc1<-delRtot1/(sum(e101-e193))
dRpcbas1<-delRtot1/sum(e193)
# type effects
delT1<-array(rep(0,1350),dim(O01))
lT1<-log(T01/T93)
for (i in 1:length(w9301[1,1,,1])) {
  for (j in 1:length(w9301[1,1,1,])) {
    delT1[,,i,j]<-w9301[,,i,j]*lT1
  }
}
delTtot1<-sum(delT1)
delTpc1<-delTtot1/(sum(e101-e193))
dTpcbas1<-delTtot1/sum(e193)
# cohort effects
noW<-array(rep(1,270),dim(O01))
lC1<-log(C01/C93)
cd<-C01/C93
lC1long<-array(rep(0,1350),dim(O01))
for (i in 1:dim(O01)[4]) {
  lC1long[,,,i] <-lC1
}
for (a in 1:dim(O01)[1]) { for (b in 1:dim(O01)[2]) { for (c in 1:dim(O01)[3]) { for (d in 1:dim(O01)[4]) {
  if (is.nan(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-0}
  if (is.infinite(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b])}
} }}
}
delC1<-w9301*lC1long
delCtot1<-sum(delC1)
delCpc1<-delCtot1/(sum(e101-e193))
dCpcbas1<-delCtot1/sum(e193)
# heating fuel effect
lH1<-log(H01/H93)
for (a in 1:dim(O01)[1]) { for (b in 1:dim(O01)[2]) { for (c in 1:dim(O01)[3]) { for (d in 1:dim(O01)[4]) {
  if (is.nan(lH1[a,b,c,d])) {lH1[a,b,c,d]<-0}
  if (is.infinite(lH1[a,b,c,d])) {lH1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d])}
} }}
}
delH1<-w9301*lH1
delH1[is.na(delH1)|is.infinite(delH1)]<-0
delHtot1<-sum(delH1)
delHpc1<-delHtot1/(sum(e101-e193))
dHpcbas1<-delHtot1/sum(e193)
# occupancy effects
od<-O01/O93
od[is.na(od)|is.infinite(od)]<-0
lO1<-log(O01/O93)
delO1<-w9301*lO1
delO1[is.na(delO1)|is.infinite(delO1)]<-0
delOtot1<-sum(delO1)
delOpc1<-delOtot1/(sum(e101-e193))
dOpcbas1<-delOtot1/sum(e193)
# Lifestyle (house size) effects
ld<-L101/L193
ld[is.na(ld)|is.infinite(ld)]<-0
lL1<-log(L101/L193)
delL1<-w9301*lL1
delL1[is.na(delL1)|is.infinite(delL1)]<-0
delLtot1<-sum(delL1)
delLpc1<-delLtot1/(sum(e101-e193))
dLpcbas1<-delLtot1/sum(e193)
# Intensity effects
id<-I101/I193
id[is.na(id)|is.infinite(id)]<-0
lI1<-log(I101/I193)
# Intensity debugging only required when there are intensities of 0 due to energy from 'other' fuel types
for (a in 1:dim(O93)[1]) { for (b in 1:dim(O93)[2]) { for (c in 1:dim(O93)[3]) { for (d in 1:dim(O93)[4]) {
  if (is.nan(lI1[a,b,c,d])) {lI1[a,b,c,d]<-0}
  if (is.infinite(lI1[a,b,c,d])) {lI1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d]+lH1[a,b,c,d]+lO1[a,b,c,d]+lL1[a,b,c,d])}
} }}
}
delI1<-w9301*lI1
delI1[is.na(delI1)|is.infinite(delI1)]<-0
delItot1<-sum(delI1)
delIpc1<-delItot1/(sum(e101-e193))
dIpcbas1<-delItot1/sum(e193)
# electricity efficiency effects 
xd<-X101/X193
xd[is.na(xd)|is.infinite(xd)]<-0
lX1<-log(X101/X193)
delX1<-w9301*lX1
delX1[is.na(delX1)|is.infinite(delX1)]<-0
delXtot1<-sum(delX1)
delXpc1<-delXtot1/(sum(e101-e193))
dXpcbas1<-delXtot1/sum(e193)
# Weather effects
wd<-W101/W193
wd[is.na(wd)|is.infinite(wd)]<-0
lW1<-log(W101/W193)
delW1<-w9301*lW1
delW1[is.na(delW1)|is.infinite(delW1)]<-0
delWtot1<-sum(delW1)
delWpc1<-delWtot1/(sum(e101-e193))
dWpcbas1<-delWtot1/sum(e193)
# Total heating effects
deltot1<-sum(e101-e193)
deltotpc1<-deltot1/sum(e193)
diff1<-e101-e193
sum_eff1<-delP1+delR1+delT1+delC1+delH1+delO1+delL1+delI1+delX1+delW1
db<-sum_eff1-diff1 # for debugging
hpc9301<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc1,dPpcbas1,dRpcbas1,dTpcbas1,dCpcbas1,dHpcbas1,dOpcbas1,dLpcbas1,dIpcbas1,dXpcbas1,dWpcbas1))
#Turn your 'treatment' column into a character vector
hpc9301$Effect <- as.character(hpc9301$Effect)
#Then turn it back into a factor with the levels in the correct order
hpc9301$Effect <- factor(hpc9301$Effect, levels=unique(hpc9301$Effect))
windows() # heating
ggplot(data=hpc9301, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space heating 1993-2001", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
habs9301<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot1,delPtot1,delRtot1,delTtot1,delCtot1,delHtot1,delOtot1,delLtot1,delItot1,delXtot1,delWtot1))
# check
habs9301$abs[1]-sum(habs9301$abs[2:11])

# 01-05 new sph changes #######
# add small values to e105C and e101C to avoid problems of zeros with log means
ed<-E105_o/E101_o
ed[is.na(ed)|is.infinite(ed)]<-0
e105[e105==0]<-100
e101[e101==0]<-100

le1<-log(e105)-log(e101)
w0105<-(e105-e101)/le1
w0105[is.nan(w0105)]<-0
# population effects
lP1<-log(P05/P01)
delP1<-w0105*lP1
delPtot1<-sum(delP1)
delPpc1<-delPtot1/(sum(e105-e101))
dPpcbas1<-delPtot1/sum(e101)
# regional effects
lR1<-log(R05/R01)
delR1<-sweep(w0105,MARGIN = 2,FUN='*',STATS=lR1)
delRtot1<-sum(delR1)
delRpc1<-delRtot1/(sum(e105-e101))
dRpcbas1<-delRtot1/sum(e101)
# type effects
delT1<-array(rep(0,1350),dim(O05))
lT1<-log(T05/T01)
for (i in 1:length(w0105[1,1,,1])) {
  for (j in 1:length(w0105[1,1,1,])) {
    delT1[,,i,j]<-w0105[,,i,j]*lT1
  }
}
delTtot1<-sum(delT1)
delTpc1<-delTtot1/(sum(e105-e101))
dTpcbas1<-delTtot1/sum(e101)
# cohort effects
noW<-array(rep(1,270),dim(O05))
lC1<-log(C05/C01)
cd<-C05/C01

lC1long<-array(rep(0,1350),dim(O05))
for (i in 1:dim(O05)[4]) {
  lC1long[,,,i] <-lC1
}
for (a in 1:dim(O05)[1]) { for (b in 1:dim(O05)[2]) { for (c in 1:dim(O05)[3]) { for (d in 1:dim(O05)[4]) {
  if (is.nan(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-0}
  if (is.infinite(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b])}
} }}
}
delC1<-w0105*lC1long
delCtot1<-sum(delC1)
delCpc1<-delCtot1/(sum(e105-e101))
dCpcbas1<-delCtot1/sum(e101)
# heating fuel effect
lH1<-log(H05/H01)
for (a in 1:dim(O05)[1]) { for (b in 1:dim(O05)[2]) { for (c in 1:dim(O05)[3]) { for (d in 1:dim(O05)[4]) {
  if (is.nan(lH1[a,b,c,d])) {lH1[a,b,c,d]<-0}
  if (is.infinite(lH1[a,b,c,d])) {lH1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d])}
} }}
}
delH1<-w0105*lH1
delH1[is.na(delH1)|is.infinite(delH1)]<-0
delHtot1<-sum(delH1)
delHpc1<-delHtot1/(sum(e105-e101))
dHpcbas1<-delHtot1/sum(e101)
# occupancy effects
od<-O05/O01
od[is.na(od)|is.infinite(od)]<-0
lO1<-log(O05/O01)
delO1<-w0105*lO1
delO1[is.na(delO1)|is.infinite(delO1)]<-0
delOtot1<-sum(delO1)
delOpc1<-delOtot1/(sum(e105-e101))
dOpcbas1<-delOtot1/sum(e101)
# Lifestyle (house size) effects
ld<-L105/L101
ld[is.na(ld)|is.infinite(ld)]<-0
lL1<-log(L105/L101)
delL1<-w0105*lL1
delL1[is.na(delL1)|is.infinite(delL1)]<-0
delLtot1<-sum(delL1)
delLpc1<-delLtot1/(sum(e105-e101))
dLpcbas1<-delLtot1/sum(e101)
# Intensity effects
id<-I105/I101
id[is.na(id)|is.infinite(id)]<-0
lI1<-log(I105/I101)
delI1<-w0105*lI1
delI1[is.na(delI1)|is.infinite(delI1)]<-0
delItot1<-sum(delI1)
delIpc1<-delItot1/(sum(e105-e101))
dIpcbas1<-delItot1/sum(e101)
# electricity efficiency effects 
xd<-X105/X101
xd[is.na(xd)|is.infinite(xd)]<-0
lX1<-log(X105/X101)
delX1<-w0105*lX1
delX1[is.na(delX1)|is.infinite(delX1)]<-0
delXtot1<-sum(delX1)
delXpc1<-delXtot1/(sum(e105-e101))
dXpcbas1<-delXtot1/sum(e101)
# Weather effects
wd<-W105/W101
wd[is.na(wd)|is.infinite(wd)]<-0
lW1<-log(W105/W101)
delW1<-w0105*lW1
delW1[is.na(delW1)|is.infinite(delW1)]<-0
delWtot1<-sum(delW1)
delWpc1<-delWtot1/(sum(e105-e101))
dWpcbas1<-delWtot1/sum(e101)
# Total heating effects
deltot1<-sum(e105-e101)
deltotpc1<-deltot1/sum(e101)
diff1<-e105-e101
sum_eff1<-delP1+delR1+delT1+delC1+delH1+delO1+delL1+delI1+delX1+delW1
db<-sum_eff1-diff1 # for debugging
hpc0105<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc1,dPpcbas1,dRpcbas1,dTpcbas1,dCpcbas1,dHpcbas1,dOpcbas1,dLpcbas1,dIpcbas1,dXpcbas1,dWpcbas1))
#Turn your 'treatment' column into a character vector
hpc0105$Effect <- as.character(hpc0105$Effect)
#Then turn it back into a factor with the levels in the correct order
hpc0105$Effect <- factor(hpc0105$Effect, levels=unique(hpc0105$Effect))
windows() # heating
ggplot(data=hpc0105, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space heating 2001-2005", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
habs0105<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot1,delPtot1,delRtot1,delTtot1,delCtot1,delHtot1,delOtot1,delLtot1,delItot1,delXtot1,delWtot1))
# check
habs0105$abs[1]-sum(habs0105$abs[2:11])

# 05-09 new sph changes #######
# add small values to e109C and e105C to avoid problems of zeros with log means
ed<-E109_o/E105_o
ed[is.na(ed)|is.infinite(ed)]<-0
e109[e109==0]<-100
e105[e105==0]<-100

le1<-log(e109)-log(e105)
w0509<-(e109-e105)/le1
w0509[is.nan(w0509)]<-0
# population effects
lP1<-log(P09/P05)
delP1<-w0509*lP1
delPtot1<-sum(delP1)
delPpc1<-delPtot1/(sum(e109-e105))
dPpcbas1<-delPtot1/sum(e105)
# regional effects
lR1<-log(R09/R05)
delR1<-sweep(w0509,MARGIN = 2,FUN='*',STATS=lR1)
delRtot1<-sum(delR1)
delRpc1<-delRtot1/(sum(e109-e105))
dRpcbas1<-delRtot1/sum(e105)
# type effects
delT1<-array(rep(0,1350),dim(O09))
lT1<-log(T09/T05)
for (i in 1:length(w0509[1,1,,1])) {
  for (j in 1:length(w0509[1,1,1,])) {
    delT1[,,i,j]<-w0509[,,i,j]*lT1
  }
}
delTtot1<-sum(delT1)
delTpc1<-delTtot1/(sum(e109-e105))
dTpcbas1<-delTtot1/sum(e105)
# cohort effects
noW<-array(rep(1,270),dim(O09))
lC1<-log(C09/C05)
cd<-C09/C05

lC1long<-array(rep(0,1350),dim(O09))
for (i in 1:dim(O09)[4]) {
  lC1long[,,,i] <-lC1
}
for (a in 1:dim(O09)[1]) { for (b in 1:dim(O09)[2]) { for (c in 1:dim(O09)[3]) { for (d in 1:dim(O09)[4]) {
  if (is.nan(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-0}
  if (is.infinite(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b])}
} }}
}
delC1<-w0509*lC1long
delCtot1<-sum(delC1)
delCpc1<-delCtot1/(sum(e109-e105))
dCpcbas1<-delCtot1/sum(e105)
# heating fuel effect
lH1<-log(H09/H05)
for (a in 1:dim(O09)[1]) { for (b in 1:dim(O09)[2]) { for (c in 1:dim(O09)[3]) { for (d in 1:dim(O09)[4]) {
  if (is.nan(lH1[a,b,c,d])) {lH1[a,b,c,d]<-0}
  if (is.infinite(lH1[a,b,c,d])) {lH1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d])}
} }}
}
delH1<-w0509*lH1
delH1[is.na(delH1)|is.infinite(delH1)]<-0
delHtot1<-sum(delH1)
delHpc1<-delHtot1/(sum(e109-e105))
dHpcbas1<-delHtot1/sum(e105)
# occupancy effects
od<-O09/O05
od[is.na(od)|is.infinite(od)]<-0
lO1<-log(O09/O05)
delO1<-w0509*lO1
delO1[is.na(delO1)|is.infinite(delO1)]<-0
delOtot1<-sum(delO1)
delOpc1<-delOtot1/(sum(e109-e105))
dOpcbas1<-delOtot1/sum(e105)
# Lifestyle (house size) effects
ld<-L109/L105
ld[is.na(ld)|is.infinite(ld)]<-0
lL1<-log(L109/L105)
# long debugging for L only needed if there are non-zero houses but zero heated floor area and heating
lC1<-log(C09/C05)
for (a in 1:dim(lL1)[1]) { for (b in 1:dim(lL1)[2]) { for (c in 1:dim(lL1)[3]) { for (d in 1:dim(lL1)[4]) {
  if (is.infinite(lL1[a,b,c,d])&!is.infinite((lC1[a,b,c]))) {lL1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d]+lH1[a,b,c,d]) ;delO1[a,b,c,d]<-0}
  if (is.nan(lL1[a,b,c,d])|is.infinite(lL1[a,b,c,d])) {lL1[a,b,c,d]<-0}
}
}
}
}
delOtot1<-sum(delO1)
delL1<-w0509*lL1
delL1[is.na(delL1)|is.infinite(delL1)]<-0
delLtot1<-sum(delL1)
delLpc1<-delLtot1/(sum(e109-e105))
dLpcbas1<-delLtot1/sum(e105)
# Intensity effects
id<-I109/I105
id[is.na(id)|is.infinite(id)]<-0
lI1<-log(I109/I105)
# Intensity debugging only required when there are intensities of 0 due to energy from 'other' fuel types
for (a in 1:dim(O93)[1]) { for (b in 1:dim(O93)[2]) { for (c in 1:dim(O93)[3]) { for (d in 1:dim(O93)[4]) {
  if (is.nan(lI1[a,b,c,d])) {lI1[a,b,c,d]<-0}
  if (is.infinite(lI1[a,b,c,d])) {lI1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d]+lH1[a,b,c,d]+lO1[a,b,c,d]+lL1[a,b,c,d])}
} }}
}
delI1<-w0509*lI1
delI1[is.na(delI1)|is.infinite(delI1)]<-0
delItot1<-sum(delI1)
delIpc1<-delItot1/(sum(e109-e105))
dIpcbas1<-delItot1/sum(e105)
# electricity efficiency effects 
xd<-X109/X105
xd[is.na(xd)|is.infinite(xd)]<-0
lX1<-log(X109/X105)
delX1<-w0509*lX1
delX1[is.na(delX1)|is.infinite(delX1)]<-0
delXtot1<-sum(delX1)
delXpc1<-delXtot1/(sum(e109-e105))
dXpcbas1<-delXtot1/sum(e105)
# Weather effects
wd<-W109/W105
wd[is.na(wd)|is.infinite(wd)]<-0
lW1<-log(W109/W105)
delW1<-w0509*lW1
delW1[is.na(delW1)|is.infinite(delW1)]<-0
delWtot1<-sum(delW1)
delWpc1<-delWtot1/(sum(e109-e105))
dWpcbas1<-delWtot1/sum(e105)
# Total heating effects
deltot1<-sum(e109-e105)
deltotpc1<-deltot1/sum(e105)
diff1<-e109-e105
sum_eff1<-delP1+delR1+delT1+delC1+delH1+delO1+delL1+delI1+delX1+delW1
db<-sum_eff1-diff1 # for debugging
hpc0509<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc1,dPpcbas1,dRpcbas1,dTpcbas1,dCpcbas1,dHpcbas1,dOpcbas1,dLpcbas1,dIpcbas1,dXpcbas1,dWpcbas1))
#Turn your 'treatment' column into a character vector
hpc0509$Effect <- as.character(hpc0509$Effect)
#Then turn it back into a factor with the levels in the correct order
hpc0509$Effect <- factor(hpc0509$Effect, levels=unique(hpc0509$Effect))
windows() # heating
ggplot(data=hpc0509, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space heating 2005-2009", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
habs0509<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot1,delPtot1,delRtot1,delTtot1,delCtot1,delHtot1,delOtot1,delLtot1,delItot1,delXtot1,delWtot1))
# check
habs0509$abs[1]-sum(habs0509$abs[2:11]) # discrepancy of magnitude 0.01%

# 09-15 new sph changes ###########
# add small values to e115C and e109C to avoid problems of zeros with log means
ed<-E115_o/E109_o
ed[is.na(ed)|is.infinite(ed)]<-0
e115[e115==0]<-100
e109[e109==0]<-100

le1<-log(e115)-log(e109)
w0915<-(e115-e109)/le1
w0915[is.nan(w0915)]<-0
# population effects
lP1<-log(P15/P09)
delP1<-w0915*lP1
delPtot1<-sum(delP1)
delPpc1<-delPtot1/(sum(e115-e109))
dPpcbas1<-delPtot1/sum(e109)
# regional effects
lR1<-log(R15/R09)
delR1<-sweep(w0915,MARGIN = 2,FUN='*',STATS=lR1)
delRtot1<-sum(delR1)
delRpc1<-delRtot1/(sum(e115-e109))
dRpcbas1<-delRtot1/sum(e109)
# type effects
delT1<-array(rep(0,1350),dim(O15))
lT1<-log(T15/T09)
for (i in 1:length(w0915[1,1,,1])) {
  for (j in 1:length(w0915[1,1,1,])) {
    delT1[,,i,j]<-w0915[,,i,j]*lT1
  }
}
delTtot1<-sum(delT1)
delTpc1<-delTtot1/(sum(e115-e109))
dTpcbas1<-delTtot1/sum(e109)
# cohort effects
noW<-array(rep(1,270),dim(O15))
lC1<-log(C15/C09)
cd<-C15/C09

lC1long<-array(rep(0,1350),dim(O15))
for (i in 1:dim(O15)[4]) {
  lC1long[,,,i] <-lC1
}
for (a in 1:dim(O15)[1]) { for (b in 1:dim(O15)[2]) { for (c in 1:dim(O15)[3]) { for (d in 1:dim(O15)[4]) {
  if (is.nan(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-0}
  if (is.infinite(lC1long[a,b,c,d])) {lC1long[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b])}
} }}
}
delC1<-w0915*lC1long
delCtot1<-sum(delC1)
delCpc1<-delCtot1/(sum(e115-e109))
dCpcbas1<-delCtot1/sum(e109)
# heating fuel effect
lH1<-log(H15/H09)
for (a in 1:dim(O15)[1]) { for (b in 1:dim(O15)[2]) { for (c in 1:dim(O15)[3]) { for (d in 1:dim(O15)[4]) {
  if (is.nan(lH1[a,b,c,d])) {lH1[a,b,c,d]<-0}
  if (is.infinite(lH1[a,b,c,d])) {lH1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d])}
} }}
}
delH1<-w0915*lH1
delH1[is.na(delH1)|is.infinite(delH1)]<-0
delHtot1<-sum(delH1)
delHpc1<-delHtot1/(sum(e115-e109))
dHpcbas1<-delHtot1/sum(e109)
# occupancy effects
od<-O15/O09
od[is.na(od)|is.infinite(od)]<-0
lO1<-log(O15/O09)
delO1<-w0915*lO1
delO1[is.na(delO1)|is.infinite(delO1)]<-0
delOtot1<-sum(delO1)
delOpc1<-delOtot1/(sum(e115-e109))
dOpcbas1<-delOtot1/sum(e109)
# Lifestyle (house size) effects
ld<-L115/L109
ld[is.na(ld)|is.infinite(ld)]<-0
lL1<-log(L115/L109)
delL1<-w0915*lL1
delL1[is.na(delL1)|is.infinite(delL1)]<-0
delLtot1<-sum(delL1)
delLpc1<-delLtot1/(sum(e115-e109))
dLpcbas1<-delLtot1/sum(e109)
# Intensity effects
id<-I115/I109
id[is.na(id)|is.infinite(id)]<-0
lI1<-log(I115/I109)
# Intensity debugging only required when there are intensities of 0 due to energy from 'other' fuel types
for (a in 1:dim(O93)[1]) { for (b in 1:dim(O93)[2]) { for (c in 1:dim(O93)[3]) { for (d in 1:dim(O93)[4]) {
  if (is.nan(lI1[a,b,c,d])) {lI1[a,b,c,d]<-0}
  if (is.infinite(lI1[a,b,c,d])) {lI1[a,b,c,d]<-le1[a,b,c,d]-(lP1+lR1[b]+lT1[a,b]+lC1long[a,b,c,d]+lH1[a,b,c,d]+lO1[a,b,c,d]+lL1[a,b,c,d])}
} }}
}
delI1<-w0915*lI1
delI1[is.na(delI1)|is.infinite(delI1)]<-0
delItot1<-sum(delI1)
delIpc1<-delItot1/(sum(e115-e109))
dIpcbas1<-delItot1/sum(e109)
# electricity efficiency effects 
xd<-X115/X109
xd[is.na(xd)|is.infinite(xd)]<-0
lX1<-log(X115/X109)
delX1<-w0915*lX1
delX1[is.na(delX1)|is.infinite(delX1)]<-0
delXtot1<-sum(delX1)
delXpc1<-delXtot1/(sum(e115-e109))
dXpcbas1<-delXtot1/sum(e109)
# Weather effects
wd<-W115/W109
wd[is.na(wd)|is.infinite(wd)]<-0
lW1<-log(W115/W109)
delW1<-w0915*lW1
delW1[is.na(delW1)|is.infinite(delW1)]<-0
delWtot1<-sum(delW1)
delWpc1<-delWtot1/(sum(e115-e109))
dWpcbas1<-delWtot1/sum(e109)
# Total heating effects
deltot1<-sum(e115-e109)
deltotpc1<-deltot1/sum(e109)
diff1<-e115-e109
sum_eff1<-delP1+delR1+delT1+delC1+delH1+delO1+delL1+delI1+delX1+delW1
db<-sum_eff1-diff1 # for debugging
hpc0915<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc1,dPpcbas1,dRpcbas1,dTpcbas1,dCpcbas1,dHpcbas1,dOpcbas1,dLpcbas1,dIpcbas1,dXpcbas1,dWpcbas1))
#Turn your 'treatment' column into a character vector
hpc0915$Effect <- as.character(hpc0915$Effect)
#Then turn it back into a factor with the levels in the correct order
hpc0915$Effect <- factor(hpc0915$Effect, levels=unique(hpc0915$Effect))
windows() # heating
ggplot(data=hpc0915, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space heating 2009-2015", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
habs0915<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot1,delPtot1,delRtot1,delTtot1,delCtot1,delHtot1,delOtot1,delLtot1,delItot1,delXtot1,delWtot1))
# check
habs0915$abs[1]-sum(habs0915$abs[2:11])

## 1990-1993-2001 compound #####

habs909301<-habs9093
habs909301$abs<-(habs9093$abs + habs9301$abs)
# habs909301$comp<-habs9001$abs

### 2001-2005-2009 compound #####

habs010509<-habs0105
habs010509$abs<-(habs0105$abs + habs0509$abs)
# habs010509$comp<-habs0109$abs

### 1990-2015 compound all years #####
habs9015tot<-habs9093
habs9015tot$abs<-habs909301$abs+habs010509$abs+habs0915$abs

eff<-c("Population","Region","Type","Cohort","Fuel","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather")
habsall<-data.frame(Effect=c("1990",eff,"1993",eff,"2001",eff,"2005",eff,"2009",eff,"2015"),
                    kg=c(sum(e190),habs9093$abs[2:11],sum(e193),habs9301$abs[2:11],sum(e101),habs0105$abs[2:11],sum(e105),habs0509$abs[2:11],sum(e109),habs0915$abs[2:11],sum(e115)))
habsall$Mton<-1e-9*habsall$kg
habsall$Effect3<-c("1990",eff,"2001",eff,"2009",eff,"2015",rep("NA",22))
habsall$kg3<-c(sum(e190),habs909301$abs[2:11],sum(e101),habs010509$abs[2:11],sum(e109),habs0915$abs[2:11],sum(e115),rep(0,22)) 
habsall$Mton3<-1e-9*habsall$kg3
habsall$Effect1<-c("1990",eff,"2015",rep("NA",44))
habsall$kg1<-c(sum(e190),habs9015tot$abs[2:11],sum(e115),rep(0,44))
habsall$Mton1<-1e-9*habsall$kg1

res<-paste("Results", as.character(Sys.Date()))
dir.create(res)
write.csv(habsall,file = paste(res, "/sph_IDA_GHG.csv",sep = ""))

####### IDA for space cooling ######

# cooling decomposition, 1990 ####
E290<-tapply(r90$GHGstCOLwa*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum)  # adjusted energy for space cooling in 1990 by type, division, agecohort
E290[is.na(E290)]<-0
A290<-tapply(r90$AIRCOND*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum) 
A290[is.na(A290)]<-0
I290<-E290/A290 # intensity of cooling (1000btu/house)
I290[is.nan(I290)]<-0

E290_o<-tapply(r90$GHGCOL*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum)  # energy for space cooling in 1990 by type, division, agecohort
E290_o[is.na(E290_o)]<-0
E290_w<-tapply(r90$GHGCOLwa*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum) 
E290_w[is.na(E290_w)]<-0
W290<-E290_o/E290_w
W290[is.na(W290)]<-0
X290<-E290_w/E290
X290[is.na(X290)]<-0

N90<-tapply(r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum)
N90[is.na(N90)]<-0
L290<-A290/N90 # Lifestyle, percentage of houses with AC
L290[is.nan(L290)]<-0
O90<-N90/Pijk90
O90[is.na(O90)]<-0
# identity check equation
e290<-array(rep(0,270),dim(O90))
COLWIX<-e290
PRT<-P90*sweep(T90,MARGIN=2,FUN='*',STATS = R90)
COLWIX<-C90*O90*L290*W290*I290*X290
for (i in 1:length(e290[1,1,])) {
  e290[,,i]<-PRT*COLWIX[,,i]
}
# check
max(abs(E290_o-e290))

# cooling decomposition, 1993 ####
E293<-tapply(r93$GHGstCOLwa*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum)  # adjusted energy for space cooling in 1993 by type, division, agecohort
E293[is.na(E293)]<-0
A293<-tapply(r93$AIRCOND*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum) 
A293[is.na(A293)]<-0
I293<-E293/A293 # intensity of cooling (1000btu/house)
I293[is.nan(I293)]<-0
E293_o<-tapply(r93$GHGCOL*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum)  # energy for space heat in 1993 by type, division, agecohort
E293_o[is.na(E293_o)]<-0
E293_w<-tapply(r93$GHGCOLwa*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum) 
E293_w[is.na(E293_w)]<-0
W293<-E293_o/E293_w
W293[is.na(W293)]<-0
X293<-E293_w/E293
X293[is.na(X293)]<-0

N93<-tapply(r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum)
N93[is.na(N93)]<-0
L293<-A293/N93 # Lifestyle, percentage of houses with AC
L293[is.nan(L293)]<-0
O93<-N93/Pijk93
O93[is.na(O93)]<-0
# identity check equation
e293<-array(rep(0,270),dim(O93))
COLWIX<-e293
PRT<-P93*sweep(T93,MARGIN=2,FUN='*',STATS = R93)
COLWIX<-C93*O93*L293*W293*I293*X293
for (i in 1:length(e293[1,1,])) {
  e293[,,i]<-PRT*COLWIX[,,i]
}
# check
max(abs(E293_o-e293))

# cooling decomposition, 2001 ####
E201<-tapply(r01$GHGstCOLwa*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum)  # adjusted energy for space cooling in 2001 by type, division, agecohort
E201[is.na(E201)]<-0
A201<-tapply(r01$AIRCOND*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum) 
A201[is.na(A201)]<-0
I201<-E201/A201 # intensity of cooling (1000btu/house)
I201[is.nan(I201)]<-0
E201_o<-tapply(r01$GHGCOL*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum)  # energy for space cooling in 2001 by type, division, agecohort
E201_o[is.na(E201_o)]<-0
E201_w<-tapply(r01$GHGCOLwa*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum) 
E201_w[is.na(E201_w)]<-0
W201<-E201_o/E201_w
W201[is.na(W201)]<-0
X201<-E201_w/E201
X201[is.na(X201)]<-0

N01<-tapply(r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum)
N01[is.na(N01)]<-0
L201<-A201/N01 # Lifestyle, percentage of houses with AC
L201[is.nan(L201)]<-0
O01<-N01/Pijk01
O01[is.na(O01)]<-0
# identity check equation
e201<-array(rep(0,270),dim(O01))
COLWIX<-e201
PRT<-P01*sweep(T01,MARGIN=2,FUN='*',STATS = R01)
COLWIX<-C01*O01*L201*W201*I201*X201
for (i in 1:length(e201[1,1,])) {
  e201[,,i]<-PRT*COLWIX[,,i]
}
# check
max(abs(E201_o-e201))

# cooling decomposition, 2005 ####
E205<-tapply(r05$GHGstCOLwa*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum)  # adjusted energy for space cooling in 2005 by type, division, agecohort
E205[is.na(E205)]<-0
A205<-tapply(r05$AIRCOND*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum) 
A205[is.na(A205)]<-0
I205<-E205/A205 # intensity of cooling (1000btu/house)
I205[is.nan(I205)]<-0
E205_o<-tapply(r05$GHGCOL*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum)  # energy for space cooling in 2005 by type, division, agecohort
E205_o[is.na(E205_o)]<-0
E205_w<-tapply(r05$GHGCOLwa*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum) 
E205_w[is.na(E205_w)]<-0
W205<-E205_o/E205_w
W205[is.na(W205)]<-0
X205<-E205_w/E205
X205[is.na(X205)]<-0

N05<-tapply(r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum)
N05[is.na(N05)]<-0
L205<-A205/N05 # Lifestyle, percentage of houses with AC
L205[is.nan(L205)]<-0
O05<-N05/Pijk05
O05[is.na(O05)]<-0
# identity check equation
e205<-array(rep(0,270),dim(O05))
COLWIX<-e205
PRT<-P05*sweep(T05,MARGIN=2,FUN='*',STATS = R05)
COLWIX<-C05*O05*L205*W205*I205*X205
for (i in 1:length(e205[1,1,])) {
  e205[,,i]<-PRT*COLWIX[,,i]
}
# check
max(abs(E205_o-e205))

# cooling decomposition, 2009 ####
E209<-tapply(r09$GHGstCOLwa*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum)  # adjusted energy for space cooling in 2009 by type, division, agecohort
E209[is.na(E209)]<-0
A209<-tapply(r09$AIRCOND*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum) 
A209[is.na(A209)]<-0
I209<-E209/A209 # intensity of cooling (1000btu/house)
I209[is.nan(I209)]<-0

E209_o<-tapply(r09$GHGCOL*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum)  # energy for space cooling in 2009 by type, division, agecohort
E209_o[is.na(E209_o)]<-0
E209_w<-tapply(r09$GHGCOLwa*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum) 
E209_w[is.na(E209_w)]<-0
W209<-E209_o/E209_w
W209[is.na(W209)]<-0
X209<-E209_w/E209
X209[is.na(X209)]<-0

N09<-tapply(r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum)
N09[is.na(N09)]<-0
L209<-A209/N09 # Lifestyle, percentage of houses with AC
L209[is.nan(L209)]<-0
O09<-N09/Pijk09
O09[is.na(O09)]<-0
# identity check equation
e209<-array(rep(0,270),dim(O09))
COLWIX<-e209
PRT<-P09*sweep(T09,MARGIN=2,FUN='*',STATS = R09)
COLWIX<-C09*O09*L209*W209*I209*X209
for (i in 1:length(e209[1,1,])) {
  e209[,,i]<-PRT*COLWIX[,,i]
}
# check
max(abs(E209_o-e209))

# cooling decomposition, 2015 ####
E215<-tapply(r15$GHGstCOLwa*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum)  # adjusted energy for space cooling in 2015 by type, division, agecohort
E215[is.na(E215)]<-0
A215<-tapply(r15$AIRCOND*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum) 
A215[is.na(A215)]<-0
I215<-E215/A215 # intensity of cooling (1000btu/house)
I215[is.nan(I215)]<-0
E215_o<-tapply(r15$GHGCOL*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum)  # energy for space cooling in 2015 by type, division, agecohort
E215_o[is.na(E215_o)]<-0
E215_w<-tapply(r15$GHGCOLwa*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum) 
E215_w[is.na(E215_w)]<-0
W215<-E215_o/E215_w
W215[is.na(W215)]<-0
X215<-E215_w/E215
X215[is.na(X215)]<-0

N15<-tapply(r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum)
N15[is.na(N15)]<-0
L215<-A215/N15 # Lifestyle, percentage of houses with AC
L215[is.nan(L215)]<-0
O15<-N15/Pijk15
O15[is.na(O15)]<-0
# identity check equation
e215<-array(rep(0,270),dim(O15))
COLWIX<-e215
PRT<-P15*sweep(T15,MARGIN=2,FUN='*',STATS = R15)
COLWIX<-C15*O15*L215*W215*I215*X215
for (i in 1:length(e215[1,1,])) {
  e215[,,i]<-PRT*COLWIX[,,i]
}
# check
max(abs(E215_o-e215))

#### 1990-1993 spc changes ######
# add small values to e193C and e190C to avoid problems of zeros with log means
e293[e293==0]<-100
e290[e290==0]<-100
le2<-log(e293)-log(e290)
w9093<-(e293-e290)/le2
w9093[is.nan(w9093)]<-0
# population effects
lP2<-log(P93/P90)
delP2<-w9093*lP2
delPtot2<-sum(delP2)
delPpc2<-delPtot2/(sum(e293-e290))
dPpcbas2<-delPtot2/sum(e290)
# regional effects
lR2<-log(R93/R90)
delR2<-sweep(w9093,MARGIN = 2,FUN='*',STATS=lR2)
delRtot2<-sum(delR2)
delRpc2<-delRtot2/(sum(e293-e290))
dRpcbas2<-delRtot2/sum(e290)
# type effects
delT2<-array(rep(0,270),c(5,9,6)) # problem with dimension here, should be 5,9,8?
lT2<-log(T93/T90)
for (i in 1:length(w9093[1,1,])) {
  delT2[,,i]<-w9093[,,i]*lT2
}
delTtot2<-sum(delT2)
delTpc2<-delTtot2/(sum(e293-e290))
dTpcbas2<-delTtot2/sum(e290)
# cohort effects
noW<-array(rep(1,270),dim(O93))
lC2<-log(C93/C90)
for (a in 1:dim(lC2)[1]) { for (b in 1:dim(lC2)[2]) { for (c in 1:dim(lC2)[3]) {
  if (is.nan(lC2[a,b,c])) {lC2[a,b,c]<-0}
  if (is.infinite(lC2[a,b,c])) {lC2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]) ;noW[a,b,c]<-0}
}
}
}
delC2<-w9093*lC2
delCtot2<-sum(delC2)
delCpc2<-delCtot2/(sum(e293-e290))
dCpcbas2<-delCtot2/sum(e290)
# occupancy effects
lO2<-log(O93/O90)
delO2<-w9093*lO2
delO2[is.na(delO2)|is.infinite(delO2)]<-0
delOtot2<-sum(delO2)
delOpc2<-delOtot2/(sum(e293-e290))
dOpcbas2<-delOtot2/sum(e290)
# Lifestyle (AC penetration) effects
lL2<-log(L293/L290)
for (a in 1:dim(lL2)[1]) { for (b in 1:dim(lL2)[2]) { for (c in 1:dim(lL2)[3]) {
  if (is.nan(lL2[a,b,c])) {lL2[a,b,c]<-0}
  if (is.infinite(lL2[a,b,c])) {lL2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]+lC2[a,b,c]+lO2[a,b,c]) ;noW[a,b,c]<-0}
}
}
}
delL2<-w9093*lL2
delL2[is.na(delL2)|is.infinite(delL2)]<-0
delLtot2<-sum(delL2)
delLpc2<-delLtot2/(sum(e293-e290))
dLpcbas2<-delLtot2/sum(e290)
# Intensity effects
lI2<-log(I293/I290)
delI2<-w9093*lI2
delI2[is.na(delI2)|is.infinite(delI2)]<-0
delItot2<-sum(delI2)
delIpc2<-delItot2/(sum(e293-e290))
dIpcbas2<-delItot2/sum(e290)
# electricity efficiency effects 
lX2<-log(X293/X290)
delX2<-w9093*lX2
delX2[is.na(delX2)|is.infinite(delX2)]<-0
delXtot2<-sum(delX2)
delXpc2<-delXtot2/(sum(e293-e290))
dXpcbas2<-delXtot2/sum(e290)
# Weather effects
lW2<-log(W293/W290)
delW2<-w9093*lW2
delW2[is.na(delW2)|is.infinite(delW2)]<-0
delWtot2<-sum(delW2)
delWpc2<-delWtot2/(sum(e293-e290))
dWpcbas2<-delWtot2/sum(e290)
# Total cooling effects
deltot2<-sum(e293-e290)
deltotpc2<-deltot2/sum(e290)
diff2<-e293-e290
sum_eff2<-delP2+delR2+delT2+delC2+delO2+delL2+delI2+delW2+delX2
cpc9093<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc2,dPpcbas2,dRpcbas2,dTpcbas2,dCpcbas2,dOpcbas2,dLpcbas2,dIpcbas2,dXpcbas2,dWpcbas2))
#Turn your 'treatment' column into a character vector
cpc9093$Effect <- as.character(cpc9093$Effect)
#Then turn it back into a factor with the levels in the correct order
cpc9093$Effect <- factor(cpc9093$Effect, levels=unique(cpc9093$Effect))
windows() # cooling
ggplot(data=cpc9093, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space cooling 1990-1993", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
cabs9093<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot2,delPtot2,delRtot2,delTtot2,delCtot2,delOtot2,delLtot2,delItot2,delXtot2,delWtot2))
# check
cabs9093$abs[1]-sum(cabs9093$abs[2:10])

#### 1993-2001 spc changes  ######
# add small values to e193C and e190C to avoid problems of zeros with log means
e201[e201==0]<-100
le2<-log(e201)-log(e293)
w9301<-(e201-e293)/le2
w9301[is.nan(w9301)]<-0
# population effects
lP2<-log(P01/P93)
delP2<-w9301*lP2
delPtot2<-sum(delP2)
delPpc2<-delPtot2/(sum(e201-e293))
dPpcbas2<-delPtot2/sum(e293)
# regional effects
lR2<-log(R01/R93)
delR2<-sweep(w9301,MARGIN = 2,FUN='*',STATS=lR2)
delRtot2<-sum(delR2)
delRpc2<-delRtot2/(sum(e201-e293))
dRpcbas2<-delRtot2/sum(e293)
# type effects
delT2<-array(rep(0,270),c(5,9,6))
lT2<-log(T01/T93)
for (i in 1:length(w9301[1,1,])) {
  delT2[,,i]<-w9301[,,i]*lT2
}
delTtot2<-sum(delT2)
delTpc2<-delTtot2/(sum(e201-e293))
dTpcbas2<-delTtot2/sum(e293)
# cohort effects
noW<-array(rep(1,270),dim(O01))
lC2<-log(C01/C93)
for (a in 1:dim(lC2)[1]) { for (b in 1:dim(lC2)[2]) { for (c in 1:dim(lC2)[3]) {
  if (is.nan(lC2[a,b,c])) {lC2[a,b,c]<-0}
  if (is.infinite(lC2[a,b,c])) {lC2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]) ;noW[a,b,c]<-0}
}
}
}
delC2<-w9301*lC2
delCtot2<-sum(delC2)
delCpc2<-delCtot2/(sum(e201-e293))
dCpcbas2<-delCtot2/sum(e293)
# occupancy effects
lO2<-log(O01/O93)
delO2<-w9301*lO2
delO2[is.na(delO2)|is.infinite(delO2)]<-0
delOtot2<-sum(delO2)
delOpc2<-delOtot2/(sum(e201-e293))
dOpcbas2<-delOtot2/sum(e293)
# Lifestyle (AC penetration) effects
lL2<-log(L201/L293)
for (a in 1:dim(lL2)[1]) { for (b in 1:dim(lL2)[2]) { for (c in 1:dim(lL2)[3]) {
  if (is.nan(lL2[a,b,c])) {lL2[a,b,c]<-0}
  if (is.infinite(lL2[a,b,c])) {lL2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]+lC2[a,b,c]+lO2[a,b,c]) ;noW[a,b,c]<-0}
}}}
delL2<-w9301*lL2
delL2[is.na(delL2)|is.infinite(delL2)]<-0
delLtot2<-sum(delL2)
delLpc2<-delLtot2/(sum(e201-e293))
dLpcbas2<-delLtot2/sum(e293)
# Intensity effects
lI2<-log(I201/I293)
delI2<-w9301*lI2
delI2[is.na(delI2)|is.infinite(delI2)]<-0
delItot2<-sum(delI2)
delIpc2<-delItot2/(sum(e201-e293))
dIpcbas2<-delItot2/sum(e293)
# electricity efficiency effects 
lX2<-log(X201/X293)
delX2<-w9301*lX2
delX2[is.na(delX2)|is.infinite(delX2)]<-0
delXtot2<-sum(delX2)
delXpc2<-delXtot2/(sum(e201-e293))
dXpcbas2<-delXtot2/sum(e293)
# Weather effects
lW2<-log(W201/W293)
delW2<-w9301*lW2
delW2[is.na(delW2)|is.infinite(delW2)]<-0
delWtot2<-sum(delW2)
delWpc2<-delWtot2/(sum(e201-e293))
dWpcbas2<-delWtot2/sum(e293)
# Total cooling effects
deltot2<-sum(e201-e293)
deltotpc2<-deltot2/sum(e293)
diff2<-e201-e293
sum_eff2<-delP2+delR2+delT2+delC2+delO2+delL2+delI2+delW2+delX2
cpc9301<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc2,dPpcbas2,dRpcbas2,dTpcbas2,dCpcbas2,dOpcbas2,dLpcbas2,dIpcbas2,dXpcbas2,dWpcbas2))
#Turn your 'treatment' column into a character vector
cpc9301$Effect <- as.character(cpc9301$Effect)
#Then turn it back into a factor with the levels in the correct order
cpc9301$Effect <- factor(cpc9301$Effect, levels=unique(cpc9301$Effect))
windows() # cooling
ggplot(data=cpc9301, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space cooling 1993-2001", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
cabs9301<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot2,delPtot2,delRtot2,delTtot2,delCtot2,delOtot2,delLtot2,delItot2,delXtot2,delWtot2))
# check
cabs9301$abs[1]-sum(cabs9301$abs[2:10])

### 1990-1993-2001 spc compound #####

cabs909301<-cabs9093
cabs909301$abs<-(cabs9093$abs + cabs9301$abs)

#### 2001-2005 spc changes ######

# add small values to e205 and e201 to avoid problems of zeros with log means
e205[e205==0]<-100
le2<-log(e205)-log(e201)
w0105<-(e205-e201)/le2
w0105[is.nan(w0105)]<-0
# population effects
lP2<-log(P05/P01)
delP2<-w0105*lP2
delPtot2<-sum(delP2)
delPpc2<-delPtot2/(sum(e205-e201))
dPpcbas2<-delPtot2/sum(e201)
# regional effects
lR2<-log(R05/R01)
delR2<-sweep(w0105,MARGIN = 2,FUN='*',STATS=lR2)
delRtot2<-sum(delR2)
delRpc2<-delRtot2/(sum(e205-e201))
dRpcbas2<-delRtot2/sum(e201)
# type effects
delT2<-array(rep(0,270),c(5,9,6))
lT2<-log(T05/T01)
for (i in 1:length(w0105[1,1,])) {
  delT2[,,i]<-w0105[,,i]*lT2
}
delTtot2<-sum(delT2)
delTpc2<-delTtot2/(sum(e205-e201))
dTpcbas2<-delTtot2/sum(e201)
# cohort effects
noW<-array(rep(1,270),dim(O05))
lC2<-log(C05/C01)
for (a in 1:dim(lC2)[1]) { for (b in 1:dim(lC2)[2]) { for (c in 1:dim(lC2)[3]) {
  if (is.nan(lC2[a,b,c])) {lC2[a,b,c]<-0}
  if (is.infinite(lC2[a,b,c])) {lC2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]) ;noW[a,b,c]<-0}
}
}
}
delC2<-w0105*lC2
delCtot2<-sum(delC2)
delCpc2<-delCtot2/(sum(e205-e201))
dCpcbas2<-delCtot2/sum(e201)
# occupancy effects
lO2<-log(O05/O01)
delO2<-w0105*lO2
delO2[is.na(delO2)|is.infinite(delO2)]<-0
delOtot2<-sum(delO2)
delOpc2<-delOtot2/(sum(e205-e201))
dOpcbas2<-delOtot2/sum(e201)
# Lifestyle (AC penetration) effects
lL2<-log(L205/L201)
for (a in 1:dim(lL2)[1]) { for (b in 1:dim(lL2)[2]) { for (c in 1:dim(lL2)[3]) {
  if (is.nan(lL2[a,b,c])) {lL2[a,b,c]<-0}
  if (is.infinite(lL2[a,b,c])) {lL2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]+lC2[a,b,c]+lO2[a,b,c]) ;noW[a,b,c]<-0}
}}}
delL2<-w0105*lL2
delL2[is.na(delL2)|is.infinite(delL2)]<-0
delLtot2<-sum(delL2)
delLpc2<-delLtot2/(sum(e205-e201))
dLpcbas2<-delLtot2/sum(e201)
# Intensity effects
lI2<-log(I205/I201)
delI2<-w0105*lI2
delI2[is.na(delI2)|is.infinite(delI2)]<-0
delItot2<-sum(delI2)
delIpc2<-delItot2/(sum(e205-e201))
dIpcbas2<-delItot2/sum(e201)
# electricity efficiency effects 
lX2<-log(X205/X201)
delX2<-w0105*lX2
delX2[is.na(delX2)|is.infinite(delX2)]<-0
delXtot2<-sum(delX2)
delXpc2<-delXtot2/(sum(e205-e201))
dXpcbas2<-delXtot2/sum(e201)
# Weather effects
lW2<-log(W205/W201)
delW2<-w0105*lW2
delW2[is.na(delW2)|is.infinite(delW2)]<-0
delWtot2<-sum(delW2)
delWpc2<-delWtot2/(sum(e205-e201))
dWpcbas2<-delWtot2/sum(e201)
# Total cooling effects
deltot2<-sum(e205-e201)
deltotpc2<-deltot2/sum(e201)
diff2<-e205-e201
sum_eff2<-delP2+delR2+delT2+delC2+delO2+delL2+delI2+delW2+delX2
cpc0105<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc2,dPpcbas2,dRpcbas2,dTpcbas2,dCpcbas2,dOpcbas2,dLpcbas2,dIpcbas2,dXpcbas2,dWpcbas2))
#Turn your 'treatment' column into a character vector
cpc0105$Effect <- as.character(cpc0105$Effect)
#Then turn it back into a factor with the levels in the correct order
cpc0105$Effect <- factor(cpc0105$Effect, levels=unique(cpc0105$Effect))
windows() # cooling
ggplot(data=cpc0105, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space cooling 2001-2005", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
cabs0105<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot2,delPtot2,delRtot2,delTtot2,delCtot2,delOtot2,delLtot2,delItot2,delXtot2,delWtot2))
# check
cabs0105$abs[1]-sum(cabs0105$abs[2:10])

#### 2005-2009 spc changes ######

# add small values to e209 and e205 to avoid problems of zeros with log means
e209[e209==0]<-100
le2<-log(e209)-log(e205)
w0509<-(e209-e205)/le2
w0509[is.nan(w0509)]<-0
# population effects
lP2<-log(P09/P05)
delP2<-w0509*lP2
delPtot2<-sum(delP2)
delPpc2<-delPtot2/(sum(e209-e205))
dPpcbas2<-delPtot2/sum(e205)
# regional effects
lR2<-log(R09/R05)
delR2<-sweep(w0509,MARGIN = 2,FUN='*',STATS=lR2)
delRtot2<-sum(delR2)
delRpc2<-delRtot2/(sum(e209-e205))
dRpcbas2<-delRtot2/sum(e205)
# type effects
delT2<-array(rep(0,270),c(5,9,6))
lT2<-log(T09/T05)
for (i in 1:length(w0509[1,1,])) {
  delT2[,,i]<-w0509[,,i]*lT2
}
delTtot2<-sum(delT2)
delTpc2<-delTtot2/(sum(e209-e205))
dTpcbas2<-delTtot2/sum(e205)
# cohort effects
noW<-array(rep(1,270),dim(O09))
lC2<-log(C09/C05)
for (a in 1:dim(lC2)[1]) { for (b in 1:dim(lC2)[2]) { for (c in 1:dim(lC2)[3]) {
  if (is.nan(lC2[a,b,c])) {lC2[a,b,c]<-0}
  if (is.infinite(lC2[a,b,c])) {lC2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]) ;noW[a,b,c]<-0}
}
}
}
delC2<-w0509*lC2
delCtot2<-sum(delC2)
delCpc2<-delCtot2/(sum(e209-e205))
dCpcbas2<-delCtot2/sum(e205)
# occupancy effects
lO2<-log(O09/O05)
delO2<-w0509*lO2
delO2[is.na(delO2)|is.infinite(delO2)]<-0
delOtot2<-sum(delO2)
delOpc2<-delOtot2/(sum(e209-e205))
dOpcbas2<-delOtot2/sum(e205)
# Lifestyle (AC penetration) effects
lL2<-log(L209/L205)
for (a in 1:dim(lL2)[1]) { for (b in 1:dim(lL2)[2]) { for (c in 1:dim(lL2)[3]) {
  if (is.nan(lL2[a,b,c])) {lL2[a,b,c]<-0}
  if (is.infinite(lL2[a,b,c])) {lL2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]+lC2[a,b,c]+lO2[a,b,c]) ;noW[a,b,c]<-0}
}}}
delL2<-w0509*lL2
delL2[is.na(delL2)|is.infinite(delL2)]<-0
delLtot2<-sum(delL2)
delLpc2<-delLtot2/(sum(e209-e205))
dLpcbas2<-delLtot2/sum(e205)
# Intensity effects
lI2<-log(I209/I205)
delI2<-w0509*lI2
delI2[is.na(delI2)|is.infinite(delI2)]<-0
delItot2<-sum(delI2)
delIpc2<-delItot2/(sum(e209-e205))
dIpcbas2<-delItot2/sum(e205)
# electricity efficiency effects 
lX2<-log(X209/X205)
delX2<-w0509*lX2
delX2[is.na(delX2)|is.infinite(delX2)]<-0
delXtot2<-sum(delX2)
delXpc2<-delXtot2/(sum(e209-e205))
dXpcbas2<-delXtot2/sum(e205)
# Weather effects
lW2<-log(W209/W205)
delW2<-w0509*lW2
delW2[is.na(delW2)|is.infinite(delW2)]<-0
delWtot2<-sum(delW2)
delWpc2<-delWtot2/(sum(e209-e205))
dWpcbas2<-delWtot2/sum(e205)
# Total cooling effects
deltot2<-sum(e209-e205)
deltotpc2<-deltot2/sum(e205)
diff2<-e209-e205
sum_eff2<-delP2+delR2+delT2+delC2+delO2+delL2+delI2+delW2+delX2
cpc0509<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc2,dPpcbas2,dRpcbas2,dTpcbas2,dCpcbas2,dOpcbas2,dLpcbas2,dIpcbas2,dXpcbas2,dWpcbas2))
#Turn your 'treatment' column into a character vector
cpc0509$Effect <- as.character(cpc0509$Effect)
#Then turn it back into a factor with the levels in the correct order
cpc0509$Effect <- factor(cpc0509$Effect, levels=unique(cpc0509$Effect))
windows() # cooling
ggplot(data=cpc0509, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space cooling 2005-2009", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
cabs0509<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot2,delPtot2,delRtot2,delTtot2,delCtot2,delOtot2,delLtot2,delItot2,delXtot2,delWtot2))
# check
cabs0509$abs[1]-sum(cabs0509$abs[2:10])

### 2001-2005-2009 spc compound #####

cabs010509<-cabs0105
cabs010509$abs<-(cabs0105$abs + cabs0509$abs)
#### 2009-2015 spc changes ####
# add small values to e215 and e209 to avoid problems of zeros with log means
e215[e215==0]<-100
le2<-log(e215)-log(e209)
w0915<-(e215-e209)/le2
w0915[is.nan(w0915)]<-0
# population effects
lP2<-log(P15/P09)
delP2<-w0915*lP2
delPtot2<-sum(delP2)
delPpc2<-delPtot2/(sum(e215-e209))
dPpcbas2<-delPtot2/sum(e209)
# regional effects
lR2<-log(R15/R09)
delR2<-sweep(w0915,MARGIN = 2,FUN='*',STATS=lR2)
delRtot2<-sum(delR2)
delRpc2<-delRtot2/(sum(e215-e209))
dRpcbas2<-delRtot2/sum(e209)
# type effects
delT2<-array(rep(0,270),c(5,9,6))
lT2<-log(T15/T09)
for (i in 1:length(w0915[1,1,])) {
  delT2[,,i]<-w0915[,,i]*lT2
}
delTtot2<-sum(delT2)
delTpc2<-delTtot2/(sum(e215-e209))
dTpcbas2<-delTtot2/sum(e209)
# cohort effects
noW<-array(rep(1,270),dim(O15))
lC2<-log(C15/C09)
for (a in 1:dim(lC2)[1]) { for (b in 1:dim(lC2)[2]) { for (c in 1:dim(lC2)[3]) {
  if (is.nan(lC2[a,b,c])) {lC2[a,b,c]<-0}
  if (is.infinite(lC2[a,b,c])) {lC2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]) ;noW[a,b,c]<-0}
}
}
}
delC2<-w0915*lC2
delCtot2<-sum(delC2)
delCpc2<-delCtot2/(sum(e215-e209))
dCpcbas2<-delCtot2/sum(e209)
# occupancy effects
lO2<-log(O15/O09)
delO2<-w0915*lO2
delO2[is.na(delO2)|is.infinite(delO2)]<-0
delOtot2<-sum(delO2)
delOpc2<-delOtot2/(sum(e215-e209))
dOpcbas2<-delOtot2/sum(e209)
# Lifestyle (AC penetration) effects
lL2<-log(L215/L209)
for (a in 1:dim(lL2)[1]) { for (b in 1:dim(lL2)[2]) { for (c in 1:dim(lL2)[3]) {
  if (is.nan(lL2[a,b,c])) {lL2[a,b,c]<-0}
  if (is.infinite(lL2[a,b,c])) {lL2[a,b,c]<-le2[a,b,c]-(lP2+lR2[b]+lT2[a,b]+lC2[a,b,c]+lO2[a,b,c]) ;noW[a,b,c]<-0}
}}}
delL2<-w0915*lL2
delL2[is.na(delL2)|is.infinite(delL2)]<-0
delLtot2<-sum(delL2)
delLpc2<-delLtot2/(sum(e215-e209))
dLpcbas2<-delLtot2/sum(e209)
# Intensity effects
lI2<-log(I215/I209)
delI2<-w0915*lI2
delI2[is.na(delI2)|is.infinite(delI2)]<-0
delItot2<-sum(delI2)
delIpc2<-delItot2/(sum(e215-e209))
dIpcbas2<-delItot2/sum(e209)
# electricity efficiency effects 
lX2<-log(X215/X209)
delX2<-w0915*lX2
delX2[is.na(delX2)|is.infinite(delX2)]<-0
delXtot2<-sum(delX2)
delXpc2<-delXtot2/(sum(e215-e209))
dXpcbas2<-delXtot2/sum(e209)
# Weather effects
lW2<-log(W215/W209)
delW2<-w0915*lW2
delW2[is.na(delW2)|is.infinite(delW2)]<-0
delWtot2<-sum(delW2)
delWpc2<-delWtot2/(sum(e215-e209))
dWpcbas2<-delWtot2/sum(e209)
# Total cooling effects
deltot2<-sum(e215-e209)
deltotpc2<-deltot2/sum(e209)
diff2<-e215-e209
sum_eff2<-delP2+delR2+delT2+delC2+delO2+delL2+delI2+delW2+delX2
cpc0915<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc2,dPpcbas2,dRpcbas2,dTpcbas2,dCpcbas2,dOpcbas2,dLpcbas2,dIpcbas2,dXpcbas2,dWpcbas2))
#Turn your 'treatment' column into a character vector
cpc0915$Effect <- as.character(cpc0915$Effect)
#Then turn it back into a factor with the levels in the correct order
cpc0915$Effect <- factor(cpc0915$Effect, levels=unique(cpc0915$Effect))
windows() # cooling
ggplot(data=cpc0915, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in space cooling 2009-2015", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
cabs0915<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot2,delPtot2,delRtot2,delTtot2,delCtot2,delOtot2,delLtot2,delItot2,delXtot2,delWtot2))
# check
cabs0915$abs[1]-sum(cabs0915$abs[2:10])

### 1990-2015 compound spc all years #####
cabs9015tot<-cabs9093
cabs9015tot$abs<-cabs909301$abs+cabs010509$abs+cabs0915$abs
eff<-c("Population","Region","Type","Cohort","Occupancy","Conditioned Space","Intensity","GHG Intensity","Weather")
cabsall<-data.frame(Effect=c("1990",eff,"1993",eff,"2001",eff,"2005",eff,"2009",eff,"2015"),
                    kg=c(sum(e290),cabs9093$abs[2:10],sum(e293),cabs9301$abs[2:10],sum(201),cabs0105$abs[2:10],sum(e205),cabs0509$abs[2:10],sum(e209),cabs0915$abs[2:10],sum(e215)))
cabsall$Mton<-1e-9*cabsall$kg
cabsall$Effect3<-c("1990",eff,"2001",eff,"2009",eff,"2015",rep("NA",20))
cabsall$kg3<-c(sum(e290),cabs909301$abs[2:10],sum(e201),cabs010509$abs[2:10],sum(e209),cabs0915$abs[2:10],sum(e215),rep(0,20)) 
cabsall$Mton3<-1e-9*cabsall$kg3
cabsall$Effect1<-c("1990",eff,"2015",rep("NA",40))
cabsall$kg1<-c(sum(e290),cabs9015tot$abs[2:10],sum(e215),rep(0,40))
cabsall$Mton1<-1e-9*cabsall$kg1
write.csv(cabsall,file = paste(res, "/spc_IDA_GHG.csv",sep = ""))

# decomposition new dhw, incl type and age cohort, 1990 #####
E390<-tapply(r90$GHGstDHWwa*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$DHWFuel),sum)  # energy for hot water in 3990 by type, division, agecohort, main fuel type
E390[is.na(E390)]<-0
Pijkl90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$DHWFuel),sum)
Pijkl90[is.na(Pijkl90)]<-0
I390<-E390/Pijkl90 # intensity of hot water (energy/cap)
I390[is.nan(I390)]<-0

E390_o<-tapply(r90$GHGDHW*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$DHWFuel),sum)  # energy for water heating in 1990 by type, division, agecohort
E390_o[is.na(E390_o)]<-0
E390_w<-tapply(r90$GHGDHWwa*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort,r90$DHWFuel),sum) 
E390_w[is.na(E390_w)]<-0
W390<-E390_o/E390_w
W390[is.na(W390)]<-0
X390<-E390_w/E390
X390[is.na(X390)]<-0

Pijk90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum)
Pijk90[is.na(Pijk90)]<-0
# portion of population in each type-division-cohort combo with heating fuel x
H390<-array(rep(0,1350),dim(I390))
for (i in 1:length(Pijkl90[1,1,1,])) {
  H390[,,,i]<-Pijkl90[,,,i]/Pijk90
}
H390[is.na(H390)]<-0
Pij90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$TYPEHUQ,r90$DIVISION),sum)
C90<-array(rep(0,270),dim(I390)[1:3])
for (i in 1:length(Pijk90[1,1,])) {
  C90[,,i]<-Pijk90[,,i]/Pij90
}
Pi90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$DIVISION),sum)
T90<-sweep(Pij90,MARGIN = 2, FUN = "/",STATS = Pi90)
P90<-sum(r90$NWEIGHT*r90$NHSLDMEM)
R90<-Pi90/P90
# # identity check equation
e390<-array(rep(0,1350),dim(I390))

CHIXW<-e390
CH<-e390
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C90*H390[,,,i]
}
CHIXW<-CH*I390*X390*W390
PRT<-P90*sweep(T90,MARGIN=2,FUN='*',STATS = R90)

for (i in 1:length((e390[1,1,,1]))) {
  for (j in 1:length(e390[1,1,1,])) {
    e390[,,i,j]<-PRT*CHIXW[,,i,j]
  }
}
# check
max(abs(E390_o-e390))

# decomposition new dhw, incl type and age cohort, 1993 #####
E393<-tapply(r93$GHGstDHWwa*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$DHWFuel),sum)  # energy for hot water in 3993 by type, division, agecohort, main fuel type
E393[is.na(E393)]<-0
Pijkl93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$DHWFuel),sum)
Pijkl93[is.na(Pijkl93)]<-0
I393<-E393/Pijkl93 # intensity of hot water (energy/cap)
I393[is.nan(I393)]<-0

E393_o<-tapply(r93$GHGDHW*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$DHWFuel),sum)  # energy for water heating in 1993 by type, division, agecohort
E393_o[is.na(E393_o)]<-0
E393_w<-tapply(r93$GHGDHWwa*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort,r93$DHWFuel),sum) 
E393_w[is.na(E393_w)]<-0
W393<-E393_o/E393_w
W393[is.na(W393)]<-0
X393<-E393_w/E393
X393[is.na(X393)]<-0

Pijk93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum)
Pijk93[is.na(Pijk93)]<-0
# portion of population in each type-division-cohort combo with heating fuel x
H393<-array(rep(0,1350),dim(I393))
for (i in 1:length(Pijkl93[1,1,1,])) {
  H393[,,,i]<-Pijkl93[,,,i]/Pijk93
}
H393[is.na(H393)]<-0
Pij93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$TYPEHUQ,r93$DIVISION),sum)
C93<-array(rep(0,270),dim(I393)[1:3])
for (i in 1:length(Pijk93[1,1,])) {
  C93[,,i]<-Pijk93[,,i]/Pij93
}
Pi93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$DIVISION),sum)
T93<-sweep(Pij93,MARGIN = 2, FUN = "/",STATS = Pi93)
P93<-sum(r93$NWEIGHT*r93$NHSLDMEM)
R93<-Pi93/P93
# # identity check equation
e393<-array(rep(0,1350),dim(I393))

CHIXW<-e393
CH<-e393
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C93*H393[,,,i]
}
CHIXW<-CH*I393*X393*W393
PRT<-P93*sweep(T93,MARGIN=2,FUN='*',STATS = R93)

for (i in 1:length((e393[1,1,,1]))) {
  for (j in 1:length(e393[1,1,1,])) {
    e393[,,i,j]<-PRT*CHIXW[,,i,j]
  }
}
# check
max(abs(E393_o-e393))

# decomposition new dhw, incl type and age cohort, 2001 #####
E301<-tapply(r01$GHGstDHWwa*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$DHWFuel),sum)  # energy for hot water in 3901 by type, division, agecohort, main fuel type
E301[is.na(E301)]<-0
Pijkl01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$DHWFuel),sum)
Pijkl01[is.na(Pijkl01)]<-0
I301<-E301/Pijkl01 # intensity of hot water (energy/cap)
I301[is.nan(I301)]<-0

E301_o<-tapply(r01$GHGDHW*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$DHWFuel),sum)  # energy for water heating in 2001 by type, division, agecohort
E301_o[is.na(E301_o)]<-0
E301_w<-tapply(r01$GHGDHWwa*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort,r01$DHWFuel),sum) 
E301_w[is.na(E301_w)]<-0
W301<-E301_o/E301_w
W301[is.na(W301)]<-0
X301<-E301_w/E301
X301[is.na(X301)]<-0

Pijk01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum)
Pijk01[is.na(Pijk01)]<-0
# portion of population in each type-division-cohort combo with heating fuel x
H301<-array(rep(0,1350),dim(I301))
for (i in 1:length(Pijkl01[1,1,1,])) {
  H301[,,,i]<-Pijkl01[,,,i]/Pijk01
}
H301[is.na(H301)]<-0
Pij01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$TYPEHUQ,r01$DIVISION),sum)
C01<-array(rep(0,270),dim(I301)[1:3])
for (i in 1:length(Pijk01[1,1,])) {
  C01[,,i]<-Pijk01[,,i]/Pij01
}
Pi01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$DIVISION),sum)
T01<-sweep(Pij01,MARGIN = 2, FUN = "/",STATS = Pi01)
P01<-sum(r01$NWEIGHT*r01$NHSLDMEM)
R01<-Pi01/P01
# # identity check equation
e301<-array(rep(0,1350),dim(I301))

CHIXW<-e301
CH<-e301
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C01*H301[,,,i]
}
CHIXW<-CH*I301*X301*W301
PRT<-P01*sweep(T01,MARGIN=2,FUN='*',STATS = R01)

for (i in 1:length((e301[1,1,,1]))) {
  for (j in 1:length(e301[1,1,1,])) {
    e301[,,i,j]<-PRT*CHIXW[,,i,j]
  }
}
# check
max(abs(E301_o-e301))

# decomposition new dhw, incl type and age cohort, 2005 #####
E305<-tapply(r05$GHGstDHWwa*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$DHWFuel),sum)  # energy for hot water in 3905 by type, division, agecohort, main fuel type
E305[is.na(E305)]<-0
Pijkl05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$DHWFuel),sum)
Pijkl05[is.na(Pijkl05)]<-0
I305<-E305/Pijkl05 # intensity of hot water (energy/cap)
I305[is.nan(I305)]<-0

E305_o<-tapply(r05$GHGDHW*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$DHWFuel),sum)  # energy for water heating in 2005 by type, division, agecohort
E305_o[is.na(E305_o)]<-0
E305_w<-tapply(r05$GHGDHWwa*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort,r05$DHWFuel),sum) 
E305_w[is.na(E305_w)]<-0
W305<-E305_o/E305_w
W305[is.na(W305)]<-0
X305<-E305_w/E305
X305[is.na(X305)]<-0

Pijk05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum)
Pijk05[is.na(Pijk05)]<-0
# portion of population in each type-division-cohort combo with heating fuel x
H305<-array(rep(0,1350),dim(I305))
for (i in 1:length(Pijkl05[1,1,1,])) {
  H305[,,,i]<-Pijkl05[,,,i]/Pijk05
}
H305[is.na(H305)]<-0
Pij05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$TYPEHUQ,r05$DIVISION),sum)
C05<-array(rep(0,270),dim(I305)[1:3])
for (i in 1:length(Pijk05[1,1,])) {
  C05[,,i]<-Pijk05[,,i]/Pij05
}
Pi05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$DIVISION),sum)
T05<-sweep(Pij05,MARGIN = 2, FUN = "/",STATS = Pi05)
P05<-sum(r05$NWEIGHT*r05$NHSLDMEM)
R05<-Pi05/P05
# # identity check equation
e305<-array(rep(0,1350),dim(I305))

CHIXW<-e305
CH<-e305
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C05*H305[,,,i]
}
CHIXW<-CH*I305*X305*W305
PRT<-P05*sweep(T05,MARGIN=2,FUN='*',STATS = R05)

for (i in 1:length((e305[1,1,,1]))) {
  for (j in 1:length(e305[1,1,1,])) {
    e305[,,i,j]<-PRT*CHIXW[,,i,j]
  }
}
# check
max(abs(E305_o-e305))

# decomposition new dhw, incl type and age cohort, 2009 #####
E309<-tapply(r09$GHGstDHWwa*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$DHWFuel),sum)  # energy for hot water in 3909 by type, division, agecohort, main fuel type
E309[is.na(E309)]<-0
Pijkl09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$DHWFuel),sum)
Pijkl09[is.na(Pijkl09)]<-0
I309<-E309/Pijkl09 # intensity of hot water (energy/cap)
I309[is.nan(I309)]<-0

E309_o<-tapply(r09$GHGDHW*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$DHWFuel),sum)  # energy for water heating in 2009 by type, division, agecohort
E309_o[is.na(E309_o)]<-0
E309_w<-tapply(r09$GHGDHWwa*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort,r09$DHWFuel),sum) 
E309_w[is.na(E309_w)]<-0
W309<-E309_o/E309_w
W309[is.na(W309)]<-0
X309<-E309_w/E309
X309[is.na(X309)]<-0

Pijk09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum)
Pijk09[is.na(Pijk09)]<-0
# portion of population in each type-division-cohort combo with heating fuel x
H309<-array(rep(0,1350),dim(I309))
for (i in 1:length(Pijkl09[1,1,1,])) {
  H309[,,,i]<-Pijkl09[,,,i]/Pijk09
}
H309[is.na(H309)]<-0
Pij09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$TYPEHUQ,r09$DIVISION),sum)
C09<-array(rep(0,270),dim(I309)[1:3])
for (i in 1:length(Pijk09[1,1,])) {
  C09[,,i]<-Pijk09[,,i]/Pij09
}
Pi09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$DIVISION),sum)
T09<-sweep(Pij09,MARGIN = 2, FUN = "/",STATS = Pi09)
P09<-sum(r09$NWEIGHT*r09$NHSLDMEM)
R09<-Pi09/P09
# # identity check equation
e309<-array(rep(0,1350),dim(I309))

CHIXW<-e309
CH<-e309
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C09*H309[,,,i]
}
CHIXW<-CH*I309*X309*W309
PRT<-P09*sweep(T09,MARGIN=2,FUN='*',STATS = R09)

for (i in 1:length((e309[1,1,,1]))) {
  for (j in 1:length(e309[1,1,1,])) {
    e309[,,i,j]<-PRT*CHIXW[,,i,j]
  }
}
# check
max(abs(E309_o-e309))

# decomposition new dhw, incl type and age cohort, 2015 #####
E315<-tapply(r15$GHGstDHWwa*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$DHWFuel),sum)  # energy for hot water in 3915 by type, division, agecohort, main fuel type
E315[is.na(E315)]<-0
Pijkl15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$DHWFuel),sum)
Pijkl15[is.na(Pijkl15)]<-0
I315<-E315/Pijkl15 # intensity of hot water (energy/cap)
I315[is.nan(I315)]<-0

E315_o<-tapply(r15$GHGDHW*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$DHWFuel),sum)  # energy for water heating in 2015 by type, division, agecohort
E315_o[is.na(E315_o)]<-0
E315_w<-tapply(r15$GHGDHWwa*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort,r15$DHWFuel),sum) 
E315_w[is.na(E315_w)]<-0
W315<-E315_o/E315_w
W315[is.na(W315)]<-0
X315<-E315_w/E315
X315[is.na(X315)]<-0

Pijk15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum)
Pijk15[is.na(Pijk15)]<-0
# portion of population in each type-division-cohort combo with heating fuel x
H315<-array(rep(0,1350),dim(I315))
for (i in 1:length(Pijkl15[1,1,1,])) {
  H315[,,,i]<-Pijkl15[,,,i]/Pijk15
}
H315[is.na(H315)]<-0
Pij15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$TYPEHUQ,r15$DIVISION),sum)
C15<-array(rep(0,270),dim(I315)[1:3])
for (i in 1:length(Pijk15[1,1,])) {
  C15[,,i]<-Pijk15[,,i]/Pij15
}
Pi15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$DIVISION),sum)
T15<-sweep(Pij15,MARGIN = 2, FUN = "/",STATS = Pi15)
P15<-sum(r15$NWEIGHT*r15$NHSLDMEM)
R15<-Pi15/P15
# # identity check equation
e315<-array(rep(0,1350),dim(I315))

CHIXW<-e315
CH<-e315
for (i in 1:length(CH[1,1,1,])) {
  CH[,,,i]<-C15*H315[,,,i]
}
CHIXW<-CH*I315*X315*W315
PRT<-P15*sweep(T15,MARGIN=2,FUN='*',STATS = R15)

for (i in 1:length((e315[1,1,,1]))) {
  for (j in 1:length(e315[1,1,1,])) {
    e315[,,i,j]<-PRT*CHIXW[,,i,j]
  }
}
# check
max(abs(E315_o-e315))


# 1990-1993 new dhw changes, including type effects #######
# add small values to e193C and e190C to avoid problems of zeros with log means
e393[e393==0]<-100
e390[e390==0]<-100

le3<-log(e393)-log(e390)
w9093<-(e393-e390)/le3
w9093[is.nan(w9093)]<-0
# population effects
lP3<-log(P93/P90)
delP3<-w9093*lP3
delPtot3<-sum(delP3)
delPpc3<-delPtot3/(sum(e393-e390))
dPpcbas3<-delPtot3/sum(e390)
# regional effects
lR3<-log(R93/R90)
delR3<-sweep(w9093,MARGIN = 2,FUN='*',STATS=lR3)
delRtot3<-sum(delR3)
delRpc3<-delRtot3/(sum(e393-e390))
dRpcbas3<-delRtot3/sum(e390)

# type effects
delT3<-array(rep(0,1350),dim(I393))
lT3<-log(T93/T90)
for (i in 1:length(w9093[1,1,,1])) {
  for (j in 1:length(w9093[1,1,1,])) {
    delT3[,,i,j]<-w9093[,,i,j]*lT3
  }
}
delTtot3<-sum(delT3)
delTpc3<-delTtot3/(sum(e393-e390))
dTpcbas3<-delTtot3/sum(e390)
# cohort effects

lC3<-log(C93/C90)
lC3long<-array(rep(0,1350),dim(I393))
for (i in 1:dim(I393)[4]) {
  lC3long[,,,i] <-lC3
}
for (a in 1:dim(I393)[1]) { for (b in 1:dim(I393)[2]) { for (c in 1:dim(I393)[3]) { for (d in 1:dim(I393)[4]) {
  if (is.nan(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-0}
  if (is.infinite(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b])}
} }}
}
delC3<-w9093*lC3long
delCtot3<-sum(delC3)
delCpc3<-delCtot3/(sum(e393-e390))
dCpcbas3<-delCtot3/sum(e390)
# heating fuel effect
lH3<-log(H393/H390)
for (a in 1:dim(I393)[1]) { for (b in 1:dim(I393)[2]) { for (c in 1:dim(I393)[3]) { for (d in 1:dim(I393)[4]) {
  if (is.nan(lH3[a,b,c,d])) {lH3[a,b,c,d]<-0}
  if (is.infinite(lH3[a,b,c,d])) {lH3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d])}
} }}
}
delH3<-w9093*lH3
delH3[is.na(delH3)|is.infinite(delH3)]<-0
delHtot3<-sum(delH3)
delHpc3<-delHtot3/(sum(e393-e390))
dHpcbas3<-delHtot3/sum(e390)

# Intensity effects
lI3<-log(I393/I390)
# long debugging for I only needed if there are non-zero houses but zero hot water
lH3<-log(H393/H390)
for (a in 1:dim(lI3)[1]) { for (b in 1:dim(lI3)[2]) { for (c in 1:dim(lI3)[3]) { for (d in 1:dim(lI3)[4]) {
  if (is.infinite(lI3[a,b,c,d])&!is.infinite((lH3[a,b,c,d]))) {lI3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d]+lH3[a,b,c,d])}
  if (is.nan(lI3[a,b,c,d])|is.infinite(lI3[a,b,c,d])) {lI3[a,b,c,d]<-0}
}
}
}
}
delI3<-w9093*lI3
delI3[is.na(delI3)|is.infinite(delI3)]<-0
delItot3<-sum(delI3)
delIpc3<-delItot3/(sum(e393-e390))
dIpcbas3<-delItot3/sum(e390)
# Elec efficiency effects
lX3<-log(X393/X390)
delX3<-w9093*lX3
delX3[is.na(delX3)|is.infinite(delX3)]<-0
delXtot3<-sum(delX3)
delXpc3<-delXtot3/(sum(e393-e390))
dXpcbas3<-delXtot3/sum(e390)
# Weather effects
lW3<-log(W393/W390)
delW3<-w9093*lW3
delW3[is.na(delW3)|is.infinite(delW3)]<-0
delWtot3<-sum(delW3)
delWpc3<-delWtot3/(sum(e393-e390))
dWpcbas3<-delWtot3/sum(e390)
# Total water heating effects
deltot3<-sum(e393-e390)
deltotpc3<-deltot3/sum(e390)
diff3<-e393-e390
sum_eff3<-delP3+delR3+delT3+delC3+delH3+delI3+delX3+delW3
db<-sum_eff3-diff3 # for debugging
wpc9093<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc3,dPpcbas3,dRpcbas3,dTpcbas3,dCpcbas3,dHpcbas3,dIpcbas3,dXpcbas3,dWpcbas3))
#Turn your 'treatment' column into a character vector
wpc9093$Effect <- as.character(wpc9093$Effect)
#Then turn it back into a factor with the levels in the correct order
wpc9093$Effect <- factor(wpc9093$Effect, levels=unique(wpc9093$Effect))
windows() # dhw
ggplot(data=wpc9093, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in domestic hot water 1990-1993", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
wabs9093<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot3,delPtot3,delRtot3,delTtot3,delCtot3,delHtot3,delItot3,delXtot3,delWtot3))
# check
wabs9093$abs[1]-sum(wabs9093$abs[2:9])

# 1993-2001 new dhw changes, including type effects #######
# add small values to e301C and e393C to avoid problems of zeros with log means
e301[e301==0]<-100
e393[e393==0]<-100

le3<-log(e301)-log(e393)
w9301<-(e301-e393)/le3
w9301[is.nan(w9301)]<-0
# population effects
lP3<-log(P01/P93)
delP3<-w9301*lP3
delPtot3<-sum(delP3)
delPpc3<-delPtot3/(sum(e301-e393))
dPpcbas3<-delPtot3/sum(e393)
# regional effects
lR3<-log(R01/R93)
delR3<-sweep(w9301,MARGIN = 2,FUN='*',STATS=lR3)
delRtot3<-sum(delR3)
delRpc3<-delRtot3/(sum(e301-e393))
dRpcbas3<-delRtot3/sum(e393)

# type effects
delT3<-array(rep(0,1350),dim(I301))
lT3<-log(T01/T93)
for (i in 1:length(w9301[1,1,,1])) {
  for (j in 1:length(w9301[1,1,1,])) {
    delT3[,,i,j]<-w9301[,,i,j]*lT3
  }
}
delTtot3<-sum(delT3)
delTpc3<-delTtot3/(sum(e301-e393))
dTpcbas3<-delTtot3/sum(e393)
# cohort effects
lC3<-log(C01/C93)
lC3long<-array(rep(0,1350),dim(I301))
for (i in 1:dim(I301)[4]) {
  lC3long[,,,i] <-lC3
}
for (a in 1:dim(I301)[1]) { for (b in 1:dim(I301)[2]) { for (c in 1:dim(I301)[3]) { for (d in 1:dim(I301)[4]) {
  if (is.nan(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-0}
  if (is.infinite(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b])}
} }}
}
delC3<-w9301*lC3long
delCtot3<-sum(delC3)
delCpc3<-delCtot3/(sum(e301-e393))
dCpcbas3<-delCtot3/sum(e393)
# heating fuel effect
lH3<-log(H301/H393)
for (a in 1:dim(I301)[1]) { for (b in 1:dim(I301)[2]) { for (c in 1:dim(I301)[3]) { for (d in 1:dim(I301)[4]) {
  if (is.nan(lH3[a,b,c,d])) {lH3[a,b,c,d]<-0}
  if (is.infinite(lH3[a,b,c,d])) {lH3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d])}
} }}
}
delH3<-w9301*lH3
delH3[is.na(delH3)|is.infinite(delH3)]<-0
delHtot3<-sum(delH3)
delHpc3<-delHtot3/(sum(e301-e393))
dHpcbas3<-delHtot3/sum(e393)

# Intensity effects
lI3<-log(I301/I393)
# long debugging for I only needed if there are non-zero houses but zero hot water
lH3<-log(H301/H393)
for (a in 1:dim(lI3)[1]) { for (b in 1:dim(lI3)[2]) { for (c in 1:dim(lI3)[3]) { for (d in 1:dim(lI3)[4]) {
  if (is.infinite(lI3[a,b,c,d])&!is.infinite((lH3[a,b,c,d]))) {lI3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d]+lH3[a,b,c,d])}
  if (is.nan(lI3[a,b,c,d])|is.infinite(lI3[a,b,c,d])) {lI3[a,b,c,d]<-0}
}
}
}
}
delI3<-w9301*lI3
delI3[is.na(delI3)|is.infinite(delI3)]<-0
delItot3<-sum(delI3)
delIpc3<-delItot3/(sum(e301-e393))
dIpcbas3<-delItot3/sum(e393)
# Elec efficiency effects
lX3<-log(X301/X393)
delX3<-w9301*lX3
delX3[is.na(delX3)|is.infinite(delX3)]<-0
delXtot3<-sum(delX3)
delXpc3<-delXtot3/(sum(e301-e393))
dXpcbas3<-delXtot3/sum(e393)
# Weather effects
lW3<-log(W301/W393)
delW3<-w9301*lW3
delW3[is.na(delW3)|is.infinite(delW3)]<-0
delWtot3<-sum(delW3)
delWpc3<-delWtot3/(sum(e301-e393))
dWpcbas3<-delWtot3/sum(e393)
# Total water heating effects
deltot3<-sum(e301-e393)
deltotpc3<-deltot3/sum(e393)
diff3<-e301-e393
sum_eff3<-delP3+delR3+delT3+delC3+delH3+delI3+delX3+delW3
db<-sum_eff3-diff3 # for debugging
wpc9301<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc3,dPpcbas3,dRpcbas3,dTpcbas3,dCpcbas3,dHpcbas3,dIpcbas3,dXpcbas3,dWpcbas3))
#Turn your 'treatment' column into a character vector
wpc9301$Effect <- as.character(wpc9301$Effect)
#Then turn it back into a factor with the levels in the correct order
wpc9301$Effect <- factor(wpc9301$Effect, levels=unique(wpc9301$Effect))
windows() # dhw
ggplot(data=wpc9301, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in domestic hot water 1993-2001", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
wabs9301<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot3,delPtot3,delRtot3,delTtot3,delCtot3,delHtot3,delItot3,delXtot3,delWtot3))
# check
wabs9301$abs[1]-sum(wabs9301$abs[2:9])

# 2001-2005 new dhw changes, including type effects #######
# add small values to e305C and e301C to avoid problems of zeros with log means
e305[e305==0]<-100
e301[e301==0]<-100

le3<-log(e305)-log(e301)
w0105<-(e305-e301)/le3
w0105[is.nan(w0105)]<-0
# population effects
lP3<-log(P05/P01)
delP3<-w0105*lP3
delPtot3<-sum(delP3)
delPpc3<-delPtot3/(sum(e305-e301))
dPpcbas3<-delPtot3/sum(e301)
# regional effects
lR3<-log(R05/R01)
delR3<-sweep(w0105,MARGIN = 2,FUN='*',STATS=lR3)
delRtot3<-sum(delR3)
delRpc3<-delRtot3/(sum(e305-e301))
dRpcbas3<-delRtot3/sum(e301)

# type effects
delT3<-array(rep(0,1350),dim(I305))
lT3<-log(T05/T01)
for (i in 1:length(w0105[1,1,,1])) {
  for (j in 1:length(w0105[1,1,1,])) {
    delT3[,,i,j]<-w0105[,,i,j]*lT3
  }
}
delTtot3<-sum(delT3)
delTpc3<-delTtot3/(sum(e305-e301))
dTpcbas3<-delTtot3/sum(e301)
# cohort effects
# noW<-array(rep(1,270),dim(O05))
lC3<-log(C05/C01)
lC3long<-array(rep(0,1350),dim(I305))
for (i in 1:dim(I305)[4]) {
  lC3long[,,,i] <-lC3
}
for (a in 1:dim(I305)[1]) { for (b in 1:dim(I305)[2]) { for (c in 1:dim(I305)[3]) { for (d in 1:dim(I305)[4]) {
  if (is.nan(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-0}
  if (is.infinite(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b])}
} }}
}
delC3<-w0105*lC3long
delCtot3<-sum(delC3)
delCpc3<-delCtot3/(sum(e305-e301))
dCpcbas3<-delCtot3/sum(e301)
# heating fuel effect
lH3<-log(H305/H301)
for (a in 1:dim(I305)[1]) { for (b in 1:dim(I305)[2]) { for (c in 1:dim(I305)[3]) { for (d in 1:dim(I305)[4]) {
  if (is.nan(lH3[a,b,c,d])) {lH3[a,b,c,d]<-0}
  if (is.infinite(lH3[a,b,c,d])) {lH3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d])}
} }}
}
delH3<-w0105*lH3
delH3[is.na(delH3)|is.infinite(delH3)]<-0
delHtot3<-sum(delH3)
delHpc3<-delHtot3/(sum(e305-e301))
dHpcbas3<-delHtot3/sum(e301)

# Intensity effects
lI3<-log(I305/I301)
# long debugging for I only needed if there are non-zero houses but zero hot water
lH3<-log(H305/H301)
for (a in 1:dim(lI3)[1]) { for (b in 1:dim(lI3)[2]) { for (c in 1:dim(lI3)[3]) { for (d in 1:dim(lI3)[4]) {
  if (is.infinite(lI3[a,b,c,d])&!is.infinite((lH3[a,b,c,d]))) {lI3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d]+lH3[a,b,c,d])}
  if (is.nan(lI3[a,b,c,d])|is.infinite(lI3[a,b,c,d])) {lI3[a,b,c,d]<-0}
}
}
}
}
delI3<-w0105*lI3
delI3[is.na(delI3)|is.infinite(delI3)]<-0
delItot3<-sum(delI3)
delIpc3<-delItot3/(sum(e305-e301))
dIpcbas3<-delItot3/sum(e301)
# Elec efficiency effects
lX3<-log(X305/X301)
delX3<-w0105*lX3
delX3[is.na(delX3)|is.infinite(delX3)]<-0
delXtot3<-sum(delX3)
delXpc3<-delXtot3/(sum(e305-e301))
dXpcbas3<-delXtot3/sum(e301)
# Weather effects
lW3<-log(W305/W301)
delW3<-w0105*lW3
delW3[is.na(delW3)|is.infinite(delW3)]<-0
delWtot3<-sum(delW3)
delWpc3<-delWtot3/(sum(e305-e301))
dWpcbas3<-delWtot3/sum(e301)
# Total water heating effects
deltot3<-sum(e305-e301)
deltotpc3<-deltot3/sum(e301)
diff3<-e305-e301
sum_eff3<-delP3+delR3+delT3+delC3+delH3+delI3+delX3+delW3
db<-sum_eff3-diff3 # for debugging
wpc0105<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc3,dPpcbas3,dRpcbas3,dTpcbas3,dCpcbas3,dHpcbas3,dIpcbas3,dXpcbas3,dWpcbas3))
#Turn your 'treatment' column into a character vector
wpc0105$Effect <- as.character(wpc0105$Effect)
#Then turn it back into a factor with the levels in the correct order
wpc0105$Effect <- factor(wpc0105$Effect, levels=unique(wpc0105$Effect))
windows() # dhw
ggplot(data=wpc0105, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in domestic hot water 2001-2005", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
wabs0105<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot3,delPtot3,delRtot3,delTtot3,delCtot3,delHtot3,delItot3,delXtot3,delWtot3))
# check
wabs0105$abs[1]-sum(wabs0105$abs[2:9])

# 2005-2009 new dhw changes, including type effects #######
# add small values to e309C and e305C to avoid problems of zeros with log means
e309[e309==0]<-100
e305[e305==0]<-100

le3<-log(e309)-log(e305)
w0509<-(e309-e305)/le3
w0509[is.nan(w0509)]<-0
# population effects
lP3<-log(P09/P05)
delP3<-w0509*lP3
delPtot3<-sum(delP3)
delPpc3<-delPtot3/(sum(e309-e305))
dPpcbas3<-delPtot3/sum(e305)
# regional effects
lR3<-log(R09/R05)
delR3<-sweep(w0509,MARGIN = 2,FUN='*',STATS=lR3)
delRtot3<-sum(delR3)
delRpc3<-delRtot3/(sum(e309-e305))
dRpcbas3<-delRtot3/sum(e305)

# type effects
delT3<-array(rep(0,1350),dim(I309))
lT3<-log(T09/T05)
for (i in 1:length(w0509[1,1,,1])) {
  for (j in 1:length(w0509[1,1,1,])) {
    delT3[,,i,j]<-w0509[,,i,j]*lT3
  }
}
delTtot3<-sum(delT3)
delTpc3<-delTtot3/(sum(e309-e305))
dTpcbas3<-delTtot3/sum(e305)
# cohort effects
lC3<-log(C09/C05)
lC3long<-array(rep(0,1350),dim(I309))
for (i in 1:dim(I309)[4]) {
  lC3long[,,,i] <-lC3
}
for (a in 1:dim(I309)[1]) { for (b in 1:dim(I309)[2]) { for (c in 1:dim(I309)[3]) { for (d in 1:dim(I309)[4]) {
  if (is.nan(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-0}
  if (is.infinite(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b])}
} }}
}
delC3<-w0509*lC3long
delCtot3<-sum(delC3)
delCpc3<-delCtot3/(sum(e309-e305))
dCpcbas3<-delCtot3/sum(e305)
# heating fuel effect
lH3<-log(H309/H305)
for (a in 1:dim(I309)[1]) { for (b in 1:dim(I309)[2]) { for (c in 1:dim(I309)[3]) { for (d in 1:dim(I309)[4]) {
  if (is.nan(lH3[a,b,c,d])) {lH3[a,b,c,d]<-0}
  if (is.infinite(lH3[a,b,c,d])) {lH3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d])}
} }}
}
delH3<-w0509*lH3
delH3[is.na(delH3)|is.infinite(delH3)]<-0
delHtot3<-sum(delH3)
delHpc3<-delHtot3/(sum(e309-e305))
dHpcbas3<-delHtot3/sum(e305)

# Intensity effects
lI3<-log(I309/I305)
# long debugging for I only needed if there are non-zero houses but zero hot water
lH3<-log(H309/H305)
for (a in 1:dim(lI3)[1]) { for (b in 1:dim(lI3)[2]) { for (c in 1:dim(lI3)[3]) { for (d in 1:dim(lI3)[4]) {
  if (is.infinite(lI3[a,b,c,d])&!is.infinite((lH3[a,b,c,d]))) {lI3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d]+lH3[a,b,c,d])}
  if (is.nan(lI3[a,b,c,d])|is.infinite(lI3[a,b,c,d])) {lI3[a,b,c,d]<-0}
}
}
}
}
delI3<-w0509*lI3
delI3[is.na(delI3)|is.infinite(delI3)]<-0
delItot3<-sum(delI3)
delIpc3<-delItot3/(sum(e309-e305))
dIpcbas3<-delItot3/sum(e305)
# Elec efficiency effects
lX3<-log(X309/X305)
delX3<-w0509*lX3
delX3[is.na(delX3)|is.infinite(delX3)]<-0
delXtot3<-sum(delX3)
delXpc3<-delXtot3/(sum(e309-e305))
dXpcbas3<-delXtot3/sum(e305)
# Weather effects
lW3<-log(W309/W305)
delW3<-w0509*lW3
delW3[is.na(delW3)|is.infinite(delW3)]<-0
delWtot3<-sum(delW3)
delWpc3<-delWtot3/(sum(e309-e305))
dWpcbas3<-delWtot3/sum(e305)
# Total water heating effects
deltot3<-sum(e309-e305)
deltotpc3<-deltot3/sum(e305)
diff3<-e309-e305
sum_eff3<-delP3+delR3+delT3+delC3+delH3+delI3+delX3+delW3
db<-sum_eff3-diff3 # for debugging
wpc0509<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc3,dPpcbas3,dRpcbas3,dTpcbas3,dCpcbas3,dHpcbas3,dIpcbas3,dXpcbas3,dWpcbas3))
#Turn your 'treatment' column into a character vector
wpc0509$Effect <- as.character(wpc0509$Effect)
#Then turn it back into a factor with the levels in the correct order
wpc0509$Effect <- factor(wpc0509$Effect, levels=unique(wpc0509$Effect))
windows() # dhw
ggplot(data=wpc0509, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in domestic hot water 2005-2009", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
wabs0509<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot3,delPtot3,delRtot3,delTtot3,delCtot3,delHtot3,delItot3,delXtot3,delWtot3))
# check
wabs0509$abs[1]-sum(wabs0509$abs[2:9])

# 2009-2015 new dhw changes, including type effects #######
# add small values to e315C and e309C to avoid problems of zeros with log means
e315[e315==0]<-100
e309[e309==0]<-100

le3<-log(e315)-log(e309)
w0915<-(e315-e309)/le3
w0915[is.nan(w0915)]<-0
# population effects
lP3<-log(P15/P09)
delP3<-w0915*lP3
delPtot3<-sum(delP3)
delPpc3<-delPtot3/(sum(e315-e309))
dPpcbas3<-delPtot3/sum(e309)
# regional effects
lR3<-log(R15/R09)
delR3<-sweep(w0915,MARGIN = 2,FUN='*',STATS=lR3)
delRtot3<-sum(delR3)
delRpc3<-delRtot3/(sum(e315-e309))
dRpcbas3<-delRtot3/sum(e309)

# type effects
delT3<-array(rep(0,1350),dim(I315))
lT3<-log(T15/T09)
for (i in 1:length(w0915[1,1,,1])) {
  for (j in 1:length(w0915[1,1,1,])) {
    delT3[,,i,j]<-w0915[,,i,j]*lT3
  }
}
delTtot3<-sum(delT3)
delTpc3<-delTtot3/(sum(e315-e309))
dTpcbas3<-delTtot3/sum(e309)
# cohort effects
# noW<-array(rep(1,270),dim(O15))
lC3<-log(C15/C09)
lC3long<-array(rep(0,1350),dim(I315))
for (i in 1:dim(I315)[4]) {
  lC3long[,,,i] <-lC3
}
for (a in 1:dim(I315)[1]) { for (b in 1:dim(I315)[2]) { for (c in 1:dim(I315)[3]) { for (d in 1:dim(I315)[4]) {
  if (is.nan(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-0}
  if (is.infinite(lC3long[a,b,c,d])) {lC3long[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b])}
} }}
}
delC3<-w0915*lC3long
delCtot3<-sum(delC3)
delCpc3<-delCtot3/(sum(e315-e309))
dCpcbas3<-delCtot3/sum(e309)
# heating fuel effect
lH3<-log(H315/H309)
for (a in 1:dim(I315)[1]) { for (b in 1:dim(I315)[2]) { for (c in 1:dim(I315)[3]) { for (d in 1:dim(I315)[4]) {
  if (is.nan(lH3[a,b,c,d])) {lH3[a,b,c,d]<-0}
  if (is.infinite(lH3[a,b,c,d])) {lH3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d])}
} }}
}
delH3<-w0915*lH3
delH3[is.na(delH3)|is.infinite(delH3)]<-0
delHtot3<-sum(delH3)
delHpc3<-delHtot3/(sum(e315-e309))
dHpcbas3<-delHtot3/sum(e309)

# Intensity effects
lI3<-log(I315/I309)
# long debugging for I only needed if there are non-zero houses but zero hot water
lH3<-log(H315/H309)
for (a in 1:dim(lI3)[1]) { for (b in 1:dim(lI3)[2]) { for (c in 1:dim(lI3)[3]) { for (d in 1:dim(lI3)[4]) {
  if (is.infinite(lI3[a,b,c,d])&!is.infinite((lH3[a,b,c,d]))) {lI3[a,b,c,d]<-le3[a,b,c,d]-(lP3+lR3[b]+lT3[a,b]+lC3long[a,b,c,d]+lH3[a,b,c,d])}
  if (is.nan(lI3[a,b,c,d])|is.infinite(lI3[a,b,c,d])) {lI3[a,b,c,d]<-0}
}
}
}
}
delI3<-w0915*lI3
delI3[is.na(delI3)|is.infinite(delI3)]<-0
delItot3<-sum(delI3)
delIpc3<-delItot3/(sum(e315-e309))
dIpcbas3<-delItot3/sum(e309)
# Elec efficiency effects
lX3<-log(X315/X309)
delX3<-w0915*lX3
delX3[is.na(delX3)|is.infinite(delX3)]<-0
delXtot3<-sum(delX3)
delXpc3<-delXtot3/(sum(e315-e309))
dXpcbas3<-delXtot3/sum(e309)
# Weather effects
lW3<-log(W315/W309)
delW3<-w0915*lW3
delW3[is.na(delW3)|is.infinite(delW3)]<-0
delWtot3<-sum(delW3)
delWpc3<-delWtot3/(sum(e315-e309))
dWpcbas3<-delWtot3/sum(e309)
# Total water heating effects
deltot3<-sum(e315-e309)
deltotpc3<-deltot3/sum(e309)
diff3<-e315-e309
sum_eff3<-delP3+delR3+delT3+delC3+delH3+delI3+delX3+delW3
db<-sum_eff3-diff3 # for debugging
wpc0915<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                    pc=100*c(deltotpc3,dPpcbas3,dRpcbas3,dTpcbas3,dCpcbas3,dHpcbas3,dIpcbas3,dXpcbas3,dWpcbas3))
#Turn your 'treatment' column into a character vector
wpc0915$Effect <- as.character(wpc0915$Effect)
#Then turn it back into a factor with the levels in the correct order
wpc0915$Effect <- factor(wpc0915$Effect, levels=unique(wpc0915$Effect))
windows() # dhw
ggplot(data=wpc0915, aes(x=Effect, y=pc)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  labs(title = "Percent changes in domestic hot water 2009-2015", x= "Effect", 
       y= "%",caption = "Model 3") + theme(axis.text=element_text(size=10.5),
                                           axis.title=element_text(size=12,face = "bold"),
                                           plot.title = element_text(size = 14, face = "bold"))
wabs0915<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather"),
                     abs=c(deltot3,delPtot3,delRtot3,delTtot3,delCtot3,delHtot3,delItot3,delXtot3,delWtot3))
# check
wabs0915$abs[1]-sum(wabs0915$abs[2:9])


### 1990-1993-2001 dhw compound #####
wabs909301<-wabs9093
wabs909301$abs<-(wabs9093$abs + wabs9301$abs)

### 2001-2005-2009 dhw compound #####
wabs010509<-wabs0105
wabs010509$abs<-(wabs0105$abs + wabs0509$abs)

### 1990-2015 compound dhw all years #####
wabs9015tot<-wabs9093
wabs9015tot$abs<-wabs909301$abs+wabs010509$abs+wabs0915$abs
eff<-c("Population","Region","Type","Cohort","Fuel","Intensity","GHG Intensity","Weather")
wabsall<-data.frame(Effect=c("1990",eff,"1993",eff,"2001",eff,"2005",eff,"2009",eff,"2015"),
                    kg=c(sum(e390),wabs9093$abs[2:9],sum(e393),wabs9301$abs[2:9],sum(e301),wabs0105$abs[2:9],sum(e305),wabs0509$abs[2:9],sum(e309),wabs0915$abs[2:9],sum(e315)))
wabsall$Mton<-1e-9*wabsall$kg
wabsall$Effect3<-c("1990",eff,"2001",eff,"2009",eff,"2015",rep("NA",18))
wabsall$kg3<-c(sum(e390),wabs909301$abs[2:9],sum(e301),wabs010509$abs[2:9],sum(e309),wabs0915$abs[2:9],sum(e315),rep(0,18)) 
wabsall$Mton3<-1e-9*wabsall$kg3
wabsall$Effect1<-c("1990",eff,"2015",rep("NA",36))
wabsall$kg1<-c(sum(e390),wabs9015tot$abs[2:9],sum(e315),rep(0,36))
wabsall$Mton1<-1e-9*wabsall$kg1

write.csv(wabsall,file = paste(res, "/dhw_IDA_GHG.csv",sep = ""))

# other enduse decomposition incl cohort, 1990 ######
E490<-tapply(r90$GHGstOTH*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum) # energy for other uses in 1990 by type and division, agecohort
E490[is.na(E490)]<-0
N90<-tapply(r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum)
N90[is.na(N90)]<-0
I490<-E490/N90
I490[is.nan(I490)]<-0

E490_o<-tapply(r90$GHGOTH*r90$NWEIGHT,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum) # energy for other uses in 1990 by type and division, age cohort
E490_o[is.na(E490_o)]<-0

X490<-E490_o/E490
X490[is.na(X490)]<-0

Pijk90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$TYPEHUQ,r90$DIVISION,r90$AgeCohort),sum)
Pijk90[is.na(Pijk90)]<-0
O90<-N90/Pijk90
O90[is.na(O90)]<-0
Pij90<-tapply(r90$NWEIGHT*r90$NHSLDMEM,list(r90$TYPEHUQ,r90$DIVISION),sum)
T90<-sweep(Pij90,MARGIN = 2, FUN = "/",STATS = Pi90)
C90<-array(rep(0,270),dim(O90)[1:3])
for (i in 1:length(Pijk90[1,1,])) {
  C90[,,i]<-Pijk90[,,i]/Pij90
}
# identity check equation
e490<-array(rep(0,270),dim(O90))
PRT<-P90*sweep(T90,MARGIN=2,FUN='*',STATS = R90)
COIX<-C90*O90*I490*X490
for (i in 1:length(e490[1,1,])) {
  e490[,,i]<-PRT*COIX[,,i]
}
max(abs(E490_o-e490))

# other enduse decomposition incl cohort, 1993 ######
E493<-tapply(r93$GHGstOTH*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum) # energy for other uses in 1993 by type and division, agecohort
E493[is.na(E493)]<-0
N93<-tapply(r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum)
N93[is.na(N93)]<-0
I493<-E493/N93
I493[is.nan(I493)]<-0

E493_o<-tapply(r93$GHGOTH*r93$NWEIGHT,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum) # energy for other uses in 1993 by type and division, age cohort
E493_o[is.na(E493_o)]<-0

X493<-E493_o/E493
X493[is.na(X493)]<-0

Pijk93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$TYPEHUQ,r93$DIVISION,r93$AgeCohort),sum)
Pijk93[is.na(Pijk93)]<-0
O93<-N93/Pijk93
O93[is.na(O93)]<-0
Pij93<-tapply(r93$NWEIGHT*r93$NHSLDMEM,list(r93$TYPEHUQ,r93$DIVISION),sum)
T93<-sweep(Pij93,MARGIN = 2, FUN = "/",STATS = Pi93)
C93<-array(rep(0,270),dim(O93)[1:3])
for (i in 1:length(Pijk93[1,1,])) {
  C93[,,i]<-Pijk93[,,i]/Pij93
}
# identity check equation
e493<-array(rep(0,270),dim(O93))
PRT<-P93*sweep(T93,MARGIN=2,FUN='*',STATS = R93)
COIX<-C93*O93*I493*X493
for (i in 1:length(e493[1,1,])) {
  e493[,,i]<-PRT*COIX[,,i]
}
max(abs(E493_o-e493))

# other enduse decomposition incl cohort, 2001 ######
E401<-tapply(r01$GHGstOTH*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum) # energy for other uses in 2001 by type and division, agecohort
E401[is.na(E401)]<-0
N01<-tapply(r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum)
N01[is.na(N01)]<-0
I401<-E401/N01
I401[is.nan(I401)]<-0

E401_o<-tapply(r01$GHGOTH*r01$NWEIGHT,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum) # energy for other uses in 2001 by type and division, age cohort
E401_o[is.na(E401_o)]<-0

X401<-E401_o/E401
X401[is.na(X401)]<-0

Pijk01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$TYPEHUQ,r01$DIVISION,r01$AgeCohort),sum)
Pijk01[is.na(Pijk01)]<-0
O01<-N01/Pijk01
O01[is.na(O01)]<-0
Pij01<-tapply(r01$NWEIGHT*r01$NHSLDMEM,list(r01$TYPEHUQ,r01$DIVISION),sum)
T01<-sweep(Pij01,MARGIN = 2, FUN = "/",STATS = Pi01)
C01<-array(rep(0,270),dim(O01)[1:3])
for (i in 1:length(Pijk01[1,1,])) {
  C01[,,i]<-Pijk01[,,i]/Pij01
}
# identity check equation
e401<-array(rep(0,270),dim(O01))
PRT<-P01*sweep(T01,MARGIN=2,FUN='*',STATS = R01)
COIX<-C01*O01*I401*X401
for (i in 1:length(e401[1,1,])) {
  e401[,,i]<-PRT*COIX[,,i]
}
max(abs(E401_o-e401))

# other enduse decomposition incl cohort, 2005 ######
E405<-tapply(r05$GHGstOTH*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum) # energy for other uses in 2005 by type and division, agecohort
E405[is.na(E405)]<-0
N05<-tapply(r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum)
N05[is.na(N05)]<-0
I405<-E405/N05
I405[is.nan(I405)]<-0

E405_o<-tapply(r05$GHGOTH*r05$NWEIGHT,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum) # energy for other uses in 2005 by type and division, age cohort
E405_o[is.na(E405_o)]<-0

X405<-E405_o/E405
X405[is.na(X405)]<-0

Pijk05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$TYPEHUQ,r05$DIVISION,r05$AgeCohort),sum)
Pijk05[is.na(Pijk05)]<-0
O05<-N05/Pijk05
O05[is.na(O05)]<-0
Pij05<-tapply(r05$NWEIGHT*r05$NHSLDMEM,list(r05$TYPEHUQ,r05$DIVISION),sum)
T05<-sweep(Pij05,MARGIN = 2, FUN = "/",STATS = Pi05)
C05<-array(rep(0,270),dim(O05)[1:3])
for (i in 1:length(Pijk05[1,1,])) {
  C05[,,i]<-Pijk05[,,i]/Pij05
}
# identity check equation
e405<-array(rep(0,270),dim(O05))
PRT<-P05*sweep(T05,MARGIN=2,FUN='*',STATS = R05)
COIX<-C05*O05*I405*X405
for (i in 1:length(e405[1,1,])) {
  e405[,,i]<-PRT*COIX[,,i]
}
max(abs(E405_o-e405))

# other enduse decomposition incl cohort, 2009 ######
E409<-tapply(r09$GHGstOTH*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum) # energy for other uses in 2009 by type and division, agecohort
E409[is.na(E409)]<-0
N09<-tapply(r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum)
N09[is.na(N09)]<-0
I409<-E409/N09
I409[is.nan(I409)]<-0

E409_o<-tapply(r09$GHGOTH*r09$NWEIGHT,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum) # energy for other uses in 2009 by type and division, age cohort
E409_o[is.na(E409_o)]<-0

X409<-E409_o/E409
X409[is.na(X409)]<-0

Pijk09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$TYPEHUQ,r09$DIVISION,r09$AgeCohort),sum)
Pijk09[is.na(Pijk09)]<-0
O09<-N09/Pijk09
O09[is.na(O09)]<-0
Pij09<-tapply(r09$NWEIGHT*r09$NHSLDMEM,list(r09$TYPEHUQ,r09$DIVISION),sum)
T09<-sweep(Pij09,MARGIN = 2, FUN = "/",STATS = Pi09)
C09<-array(rep(0,270),dim(O09)[1:3])
for (i in 1:length(Pijk09[1,1,])) {
  C09[,,i]<-Pijk09[,,i]/Pij09
}
# identity check equation
e409<-array(rep(0,270),dim(O09))
PRT<-P09*sweep(T09,MARGIN=2,FUN='*',STATS = R09)
COIX<-C09*O09*I409*X409
for (i in 1:length(e409[1,1,])) {
  e409[,,i]<-PRT*COIX[,,i]
}
max(abs(E409_o-e409))

# other enduse decomposition incl cohort, 2015 ######
E415<-tapply(r15$GHGstOTH*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum) # energy for other uses in 2015 by type and division, agecohort
E415[is.na(E415)]<-0
N15<-tapply(r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum)
N15[is.na(N15)]<-0
I415<-E415/N15
I415[is.nan(I415)]<-0

E415_o<-tapply(r15$GHGOTH*r15$NWEIGHT,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum) # energy for other uses in 2015 by type and division, age cohort
E415_o[is.na(E415_o)]<-0

X415<-E415_o/E415
X415[is.na(X415)]<-0

Pijk15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$TYPEHUQ,r15$DIVISION,r15$AgeCohort),sum)
Pijk15[is.na(Pijk15)]<-0
O15<-N15/Pijk15
O15[is.na(O15)]<-0
Pij15<-tapply(r15$NWEIGHT*r15$NHSLDMEM,list(r15$TYPEHUQ,r15$DIVISION),sum)
T15<-sweep(Pij15,MARGIN = 2, FUN = "/",STATS = Pi15)
C15<-array(rep(0,270),dim(O15)[1:3])
for (i in 1:length(Pijk15[1,1,])) {
  C15[,,i]<-Pijk15[,,i]/Pij15
}
# identity check equation
e415<-array(rep(0,270),dim(O15))
PRT<-P15*sweep(T15,MARGIN=2,FUN='*',STATS = R15)
COIX<-C15*O15*I415*X415
for (i in 1:length(e415[1,1,])) {
  e415[,,i]<-PRT*COIX[,,i]
}
max(abs(E415_o-e415))

# 1990-1993  other changes, incl cohort effects #####
e493[e493==0]<-100
e490[e490==0]<-100

le4<-log(e493)-log(e490)
w9093<-(e493-e490)/le4
w9093[is.nan(w9093)]<-0
# population effects
lP4<-log(P93/P90)
delPtot4<-sum(w9093*log(P93/P90))
delPpc<-delPtot4/(sum(e493-e490))
dPpcbas4<-delPtot4/sum(e490)
# regional effects
lR4<-log(R93/R90)
delRtot4<-sum(sweep(w9093,MARGIN = 2,FUN='*',STATS=log(R93/R90)))
delRpc<-delRtot4/(sum(e493-e490))
dRpcbas4<-delRtot4/sum(e490)
# type effects
delT4<-array(rep(0,270),c(5,9,6))
lT4<-log(T93/T90)
for (i in 1:length(w9093[1,1,])) {
  delT4[,,i]<-w9093[,,i]*lT4
}
delTtot4<-sum(delT4)
delTpc4<-delTtot4/(sum(e493-e490))
dTpcbas4<-delTtot4/sum(e490)
# cohort effects
lC4<-log(C93/C90)
for (a in 1:dim(lC4)[1]) { for (b in 1:dim(lC4)[2]) { for (c in 1:dim(lC4)[3]) {
  if (is.nan(lC4[a,b,c])) {lC4[a,b,c]<-0}
  if (is.infinite(lC4[a,b,c])) {lC4[a,b,c]<-le4[a,b,c]-(lP4+lR4[b]+lT4[a,b])}
}
}
}
delC4<-w9093*lC4
delCtot4<-sum(delC4)
delCpc4<-delCtot4/(sum(e493-e490))
dCpcbas4<-delCtot4/sum(e490)

# occupancy effects
lO4<-log(O93/O90)
delO4<-w9093*lO4
delO4[is.na(delO4)|is.infinite(delO4)]<-0
delOtot4<-sum(delO4)
delOpc4<-delOtot4/(sum(e493-e490))
dOpcbas4<-delOtot4/sum(e490)

# Intensity (per house) effects
lI4<-log(I493/I490)
delI4<-w9093*lI4
delI4[is.na(delI4)|is.infinite(delI4)]<-0
delItot4<-sum(delI4)
delIpc4<-delItot4/(sum(e493-e490))
dIpcbas4<-delItot4/sum(e490)
# Electricity efficiency effects
lX4<-log(X493/X490)
delX4<-w9093*lX4
delX4[is.na(delX4)|is.infinite(delX4)]<-0
delXtot4<-sum(delX4)
delXpc4<-delXtot4/(sum(e493-e490))
dXpcbas4<-delXtot4/sum(e490)

# Total effects
deltot4<-sum(e493-e490)
deltotpc<-deltot4/sum(e490)
opc9093<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                pc=100*c(deltotpc,dPpcbas4,dRpcbas4,dTpcbas4,dCpcbas4,dOpcbas4,dIpcbas4,dXpcbas4))
#Turn your 'treatment' column into a character vector
opc9093$Effect <- as.character(opc9093$Effect)
#Then turn it back into a factor with the levels in the correct order
opc9093$Effect <- factor(opc9093$Effect, levels=unique(opc9093$Effect))
windows() # other
print(ggplot(data=opc9093, aes(x=Effect, y=pc)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        labs(title = "Percent changes in other end uses 1990-1993", x= "Effect", 
             y= "%",caption = "Model 1-3") + theme(axis.text=element_text(size=10.5),
                                                   axis.title=element_text(size=12,face = "bold"),
                                                   plot.title = element_text(size = 14, face = "bold")))
oabs9093<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                 abs=c(deltot4,delPtot4,delRtot4,delTtot4,delCtot4,delOtot4,delItot4,delXtot4))
oabs9093$abs[1]-sum(oabs9093$abs[2:8])

# 1993-2001  other changes, incl cohort effects #####
e401[e401==0]<-100
e493[e493==0]<-100

le4<-log(e401)-log(e493)
w9301<-(e401-e493)/le4
w9301[is.nan(w9301)]<-0
# population effects
lP4<-log(P01/P93)
delPtot4<-sum(w9301*log(P01/P93))
delPpc<-delPtot4/(sum(e401-e493))
dPpcbas4<-delPtot4/sum(e493)
# regional effects
lR4<-log(R01/R93)
delRtot4<-sum(sweep(w9301,MARGIN = 2,FUN='*',STATS=log(R01/R93)))
delRpc<-delRtot4/(sum(e401-e493))
dRpcbas4<-delRtot4/sum(e493)
# type effects
delT4<-array(rep(0,270),c(5,9,6))
lT4<-log(T01/T93)
for (i in 1:length(w9301[1,1,])) {
  delT4[,,i]<-w9301[,,i]*lT4
}
delTtot4<-sum(delT4)
delTpc4<-delTtot4/(sum(e401-e493))
dTpcbas4<-delTtot4/sum(e493)
# cohort effects
lC4<-log(C01/C93)
for (a in 1:dim(lC4)[1]) { for (b in 1:dim(lC4)[2]) { for (c in 1:dim(lC4)[3]) {
  if (is.nan(lC4[a,b,c])) {lC4[a,b,c]<-0}
  if (is.infinite(lC4[a,b,c])) {lC4[a,b,c]<-le4[a,b,c]-(lP4+lR4[b]+lT4[a,b])}
}
}
}
delC4<-w9301*lC4
delCtot4<-sum(delC4)
delCpc4<-delCtot4/(sum(e401-e493))
dCpcbas4<-delCtot4/sum(e493)

# occupancy effects
lO4<-log(O01/O93)
delO4<-w9301*lO4
delO4[is.na(delO4)|is.infinite(delO4)]<-0
delOtot4<-sum(delO4)
delOpc4<-delOtot4/(sum(e401-e493))
dOpcbas4<-delOtot4/sum(e493)

# Intensity (per house) effects
lI4<-log(I401/I493)
delI4<-w9301*lI4
delI4[is.na(delI4)|is.infinite(delI4)]<-0
delItot4<-sum(delI4)
delIpc4<-delItot4/(sum(e401-e493))
dIpcbas4<-delItot4/sum(e493)
# Electricity efficiency effects
lX4<-log(X401/X493)
delX4<-w9301*lX4
delX4[is.na(delX4)|is.infinite(delX4)]<-0
delXtot4<-sum(delX4)
delXpc4<-delXtot4/(sum(e401-e493))
dXpcbas4<-delXtot4/sum(e493)

# Total effects
deltot4<-sum(e401-e493)
deltotpc<-deltot4/sum(e493)
opc9301<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                    pc=100*c(deltotpc,dPpcbas4,dRpcbas4,dTpcbas4,dCpcbas4,dOpcbas4,dIpcbas4,dXpcbas4))
#Turn your 'treatment' column into a character vector
opc9301$Effect <- as.character(opc9301$Effect)
#Then turn it back into a factor with the levels in the correct order
opc9301$Effect <- factor(opc9301$Effect, levels=unique(opc9301$Effect))
windows() # other
print(ggplot(data=opc9301, aes(x=Effect, y=pc)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        labs(title = "Percent changes in other end uses 1993-2001", x= "Effect", 
             y= "%",caption = "Model 1-3") + theme(axis.text=element_text(size=10.5),
                                                   axis.title=element_text(size=12,face = "bold"),
                                                   plot.title = element_text(size = 14, face = "bold")))
oabs9301<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                     abs=c(deltot4,delPtot4,delRtot4,delTtot4,delCtot4,delOtot4,delItot4,delXtot4))
oabs9301$abs[1]-sum(oabs9301$abs[2:8])

# 2001-2005  other changes, incl cohort effects #####
e405[e405==0]<-100
e401[e401==0]<-100

le4<-log(e405)-log(e401)
w0105<-(e405-e401)/le4
w0105[is.nan(w0105)]<-0
# population effects
lP4<-log(P05/P01)
delPtot4<-sum(w0105*log(P05/P01))
delPpc<-delPtot4/(sum(e405-e401))
dPpcbas4<-delPtot4/sum(e401)
# regional effects
lR4<-log(R05/R01)
delRtot4<-sum(sweep(w0105,MARGIN = 2,FUN='*',STATS=log(R05/R01)))
delRpc<-delRtot4/(sum(e405-e401))
dRpcbas4<-delRtot4/sum(e401)
# type effects
delT4<-array(rep(0,270),c(5,9,6))
lT4<-log(T05/T01)
for (i in 1:length(w0105[1,1,])) {
  delT4[,,i]<-w0105[,,i]*lT4
}
delTtot4<-sum(delT4)
delTpc4<-delTtot4/(sum(e405-e401))
dTpcbas4<-delTtot4/sum(e401)
# cohort effects
lC4<-log(C05/C01)
for (a in 1:dim(lC4)[1]) { for (b in 1:dim(lC4)[2]) { for (c in 1:dim(lC4)[3]) {
  if (is.nan(lC4[a,b,c])) {lC4[a,b,c]<-0}
  if (is.infinite(lC4[a,b,c])) {lC4[a,b,c]<-le4[a,b,c]-(lP4+lR4[b]+lT4[a,b])}
}
}
}
delC4<-w0105*lC4
delCtot4<-sum(delC4)
delCpc4<-delCtot4/(sum(e405-e401))
dCpcbas4<-delCtot4/sum(e401)

# occupancy effects
lO4<-log(O05/O01)
delO4<-w0105*lO4
delO4[is.na(delO4)|is.infinite(delO4)]<-0
delOtot4<-sum(delO4)
delOpc4<-delOtot4/(sum(e405-e401))
dOpcbas4<-delOtot4/sum(e401)

# Intensity (per house) effects
lI4<-log(I405/I401)
delI4<-w0105*lI4
delI4[is.na(delI4)|is.infinite(delI4)]<-0
delItot4<-sum(delI4)
delIpc4<-delItot4/(sum(e405-e401))
dIpcbas4<-delItot4/sum(e401)
# Electricity efficiency effects
lX4<-log(X405/X401)
delX4<-w0105*lX4
delX4[is.na(delX4)|is.infinite(delX4)]<-0
delXtot4<-sum(delX4)
delXpc4<-delXtot4/(sum(e405-e401))
dXpcbas4<-delXtot4/sum(e401)

# Total effects
deltot4<-sum(e405-e401)
deltotpc<-deltot4/sum(e401)
opc0105<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                    pc=100*c(deltotpc,dPpcbas4,dRpcbas4,dTpcbas4,dCpcbas4,dOpcbas4,dIpcbas4,dXpcbas4))
#Turn your 'treatment' column into a character vector
opc0105$Effect <- as.character(opc0105$Effect)
#Then turn it back into a factor with the levels in the correct order
opc0105$Effect <- factor(opc0105$Effect, levels=unique(opc0105$Effect))
windows() # other
print(ggplot(data=opc0105, aes(x=Effect, y=pc)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        labs(title = "Percent changes in other end uses 2001-2005", x= "Effect", 
             y= "%",caption = "Model 1-3") + theme(axis.text=element_text(size=10.5),
                                                   axis.title=element_text(size=12,face = "bold"),
                                                   plot.title = element_text(size = 14, face = "bold")))
oabs0105<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                     abs=c(deltot4,delPtot4,delRtot4,delTtot4,delCtot4,delOtot4,delItot4,delXtot4))
oabs0105$abs[1]-sum(oabs0105$abs[2:8])

# 2005-2009  other changes, incl cohort effects #####
e409[e409==0]<-100
e405[e405==0]<-100

le4<-log(e409)-log(e405)
w0509<-(e409-e405)/le4
w0509[is.nan(w0509)]<-0
# population effects
lP4<-log(P09/P05)
delPtot4<-sum(w0509*log(P09/P05))
delPpc<-delPtot4/(sum(e409-e405))
dPpcbas4<-delPtot4/sum(e405)
# regional effects
lR4<-log(R09/R05)
delRtot4<-sum(sweep(w0509,MARGIN = 2,FUN='*',STATS=log(R09/R05)))
delRpc<-delRtot4/(sum(e409-e405))
dRpcbas4<-delRtot4/sum(e405)
# type effects
delT4<-array(rep(0,270),c(5,9,6))
lT4<-log(T09/T05)
for (i in 1:length(w0509[1,1,])) {
  delT4[,,i]<-w0509[,,i]*lT4
}
delTtot4<-sum(delT4)
delTpc4<-delTtot4/(sum(e409-e405))
dTpcbas4<-delTtot4/sum(e405)
# cohort effects
lC4<-log(C09/C05)
for (a in 1:dim(lC4)[1]) { for (b in 1:dim(lC4)[2]) { for (c in 1:dim(lC4)[3]) {
  if (is.nan(lC4[a,b,c])) {lC4[a,b,c]<-0}
  if (is.infinite(lC4[a,b,c])) {lC4[a,b,c]<-le4[a,b,c]-(lP4+lR4[b]+lT4[a,b])}
}
}
}
delC4<-w0509*lC4
delCtot4<-sum(delC4)
delCpc4<-delCtot4/(sum(e409-e405))
dCpcbas4<-delCtot4/sum(e405)

# occupancy effects
lO4<-log(O09/O05)
delO4<-w0509*lO4
delO4[is.na(delO4)|is.infinite(delO4)]<-0
delOtot4<-sum(delO4)
delOpc4<-delOtot4/(sum(e409-e405))
dOpcbas4<-delOtot4/sum(e405)

# Intensity (per house) effects
lI4<-log(I409/I405)
delI4<-w0509*lI4
delI4[is.na(delI4)|is.infinite(delI4)]<-0
delItot4<-sum(delI4)
delIpc4<-delItot4/(sum(e409-e405))
dIpcbas4<-delItot4/sum(e405)
# Electricity efficiency effects
lX4<-log(X409/X405)
delX4<-w0509*lX4
delX4[is.na(delX4)|is.infinite(delX4)]<-0
delXtot4<-sum(delX4)
delXpc4<-delXtot4/(sum(e409-e405))
dXpcbas4<-delXtot4/sum(e405)

# Total effects
deltot4<-sum(e409-e405)
deltotpc<-deltot4/sum(e405)
opc0509<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                    pc=100*c(deltotpc,dPpcbas4,dRpcbas4,dTpcbas4,dCpcbas4,dOpcbas4,dIpcbas4,dXpcbas4))
#Turn your 'treatment' column into a character vector
opc0509$Effect <- as.character(opc0509$Effect)
#Then turn it back into a factor with the levels in the correct order
opc0509$Effect <- factor(opc0509$Effect, levels=unique(opc0509$Effect))
windows() # other
print(ggplot(data=opc0509, aes(x=Effect, y=pc)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        labs(title = "Percent changes in other end uses 2005-2009", x= "Effect", 
             y= "%",caption = "Model 1-3") + theme(axis.text=element_text(size=10.5),
                                                   axis.title=element_text(size=12,face = "bold"),
                                                   plot.title = element_text(size = 14, face = "bold")))
oabs0509<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                     abs=c(deltot4,delPtot4,delRtot4,delTtot4,delCtot4,delOtot4,delItot4,delXtot4))
oabs0509$abs[1]-sum(oabs0509$abs[2:8])

# 2009-2015  other changes, incl cohort effects #####
e415[e415==0]<-100
e409[e409==0]<-100

le4<-log(e415)-log(e409)
w0915<-(e415-e409)/le4
w0915[is.nan(w0915)]<-0
# population effects
lP4<-log(P15/P09)
delPtot4<-sum(w0915*log(P15/P09))
delPpc<-delPtot4/(sum(e415-e409))
dPpcbas4<-delPtot4/sum(e409)
# regional effects
lR4<-log(R15/R09)
delRtot4<-sum(sweep(w0915,MARGIN = 2,FUN='*',STATS=log(R15/R09)))
delRpc<-delRtot4/(sum(e415-e409))
dRpcbas4<-delRtot4/sum(e409)
# type effects
delT4<-array(rep(0,270),c(5,9,6))
lT4<-log(T15/T09)
for (i in 1:length(w0915[1,1,])) {
  delT4[,,i]<-w0915[,,i]*lT4
}
delTtot4<-sum(delT4)
delTpc4<-delTtot4/(sum(e415-e409))
dTpcbas4<-delTtot4/sum(e409)
# cohort effects
lC4<-log(C15/C09)
for (a in 1:dim(lC4)[1]) { for (b in 1:dim(lC4)[2]) { for (c in 1:dim(lC4)[3]) {
  if (is.nan(lC4[a,b,c])) {lC4[a,b,c]<-0}
  if (is.infinite(lC4[a,b,c])) {lC4[a,b,c]<-le4[a,b,c]-(lP4+lR4[b]+lT4[a,b])}
}
}
}
delC4<-w0915*lC4
delCtot4<-sum(delC4)
delCpc4<-delCtot4/(sum(e415-e409))
dCpcbas4<-delCtot4/sum(e409)

# occupancy effects
lO4<-log(O15/O09)
delO4<-w0915*lO4
delO4[is.na(delO4)|is.infinite(delO4)]<-0
delOtot4<-sum(delO4)
delOpc4<-delOtot4/(sum(e415-e409))
dOpcbas4<-delOtot4/sum(e409)

# Intensity (per house) effects
lI4<-log(I415/I409)
delI4<-w0915*lI4
delI4[is.na(delI4)|is.infinite(delI4)]<-0
delItot4<-sum(delI4)
delIpc4<-delItot4/(sum(e415-e409))
dIpcbas4<-delItot4/sum(e409)
# Electricity efficiency effects
lX4<-log(X415/X409)
delX4<-w0915*lX4
delX4[is.na(delX4)|is.infinite(delX4)]<-0
delXtot4<-sum(delX4)
delXpc4<-delXtot4/(sum(e415-e409))
dXpcbas4<-delXtot4/sum(e409)

# Total effects
deltot4<-sum(e415-e409)
deltotpc<-deltot4/sum(e409)
opc0915<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                    pc=100*c(deltotpc,dPpcbas4,dRpcbas4,dTpcbas4,dCpcbas4,dOpcbas4,dIpcbas4,dXpcbas4))
#Turn your 'treatment' column into a character vector
opc0915$Effect <- as.character(opc0915$Effect)
#Then turn it back into a factor with the levels in the correct order
opc0915$Effect <- factor(opc0915$Effect, levels=unique(opc0915$Effect))
windows() # other
print(ggplot(data=opc0915, aes(x=Effect, y=pc)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        labs(title = "Percent changes in other end uses 2009-2015", x= "Effect", 
             y= "%",caption = "Model 1-3") + theme(axis.text=element_text(size=10.5),
                                                   axis.title=element_text(size=12,face = "bold"),
                                                   plot.title = element_text(size = 14, face = "bold")))
oabs0915<-data.frame(Effect=c("Total","Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity"),
                     abs=c(deltot4,delPtot4,delRtot4,delTtot4,delCtot4,delOtot4,delItot4,delXtot4))
oabs0915$abs[1]-sum(oabs0915$abs[2:8])

### 1990-1993-2001 other compound #####
oabs909301<-oabs9093
oabs909301$abs<-(oabs9093$abs + oabs9301$abs)

### 2001-2005-2009 other compound #####
oabs010509<-oabs0105
oabs010509$abs<-(oabs0105$abs + oabs0509$abs)

### 1990-2015 compound other all years #####
oabs9015tot<-oabs9093
oabs9015tot$abs<-oabs909301$abs+oabs010509$abs+oabs0915$abs
eff<-c("Population","Region","Type","Cohort","Occupancy","Intensity","GHG Intensity")
oabsall<-data.frame(Effect=c("1990",eff,"1993",eff,"2001",eff,"2005",eff,"2009",eff,"2015"),
                    kg=c(sum(e490),oabs9093$abs[2:8],sum(e493),oabs9301$abs[2:8],sum(201),oabs0105$abs[2:8],sum(e405),oabs0509$abs[2:8],sum(e409),oabs0915$abs[2:8],sum(e415)))
oabsall$Mton<-1e-9*oabsall$kg
oabsall$Effect3<-c("1990",eff,"2001",eff,"2009",eff,"2015",rep("NA",16))
oabsall$kg3<-c(sum(e490),oabs909301$abs[2:8],sum(e401),oabs010509$abs[2:8],sum(e409),oabs0915$abs[2:8],sum(e415),rep(0,16)) 
oabsall$Mton3<-1e-9*oabsall$kg3
oabsall$Effect1<-c("1990",eff,"2015",rep("NA",32))
oabsall$kg1<-c(sum(e490),oabs9015tot$abs[2:8],sum(e415),rep(0,32))
oabsall$Mton1<-1e-9*oabsall$kg1
write.csv(oabsall,file = paste(res, "/oth_IDA_GHG.csv",sep = ""))

## all drivers for all enduses #####
all<-list(subset(habsall[1:12,7:9],select = c(Effect1,Mton1)),subset(cabsall[1:11,7:9],select = c(Effect1,Mton1)),subset(wabsall[1:10,7:9],select = c(Effect1,Mton1)),subset(oabsall[1:9,7:9],select = c(Effect1,Mton1))) %>%
  Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="Effect1"), .) # in Mton

all[is.na(all)]<-0
colnames(all)<-c("Effect","Space_heating","Space_cooling","DHW","Other")
all$Total_Energy<-rowSums(all[,2:5])
all[13,2:6]<-colSums(all[2:11,2:6])
all[13,1]<-"Total_Change"
write.csv(all,file = paste(res, "/all_GHG.csv",sep = ""))
## Drivers ######################
# Show regional population share over time
cols12<-c("darkviolet","blue", "tomato3","tan4","red2","orange","chartreuse4","magenta1","steelblue3","green3","grey52","grey26")

RDri<-matrix(c(Pi90, Pi93, Pi01,Pi05,Pi09,Pi15),9,6)
rd<-as.data.frame(matrix(c(Pi90, Pi93, Pi01,Pi05,Pi09,Pi15),54,1))
colnames(rd)[1]<-"Pop"
rd$Div<-rep(1:9,6)
rd$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=9)
rd$Pop_pc<-100*rd$Pop/rep(tapply(rd$Pop,rd$Year,sum),each=9)
rd$Division<-as.factor(rd$Div)
rd$Division<-recode(rd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
p<-ggplot(aes(x=Year,y=Pop_pc,group=Division,color=Division),data=rd) + geom_line(size=1) + geom_point() +
  ylab('Population (%)') + labs(title='Population share by Division, 1990-2015')
windows()
p +xlim(1990,2020) + ylim(0,20) + geom_text(data=rd[rd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=0.5) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

# type mix
TDri<-matrix(c(rowSums(Pij90),rowSums(Pij93),rowSums(Pij01),rowSums(Pij05),rowSums(Pij09),rowSums(Pij15)),5,6)
td<-as.data.frame(matrix(c(rowSums(Pij90),rowSums(Pij93),rowSums(Pij01),rowSums(Pij05),rowSums(Pij09),rowSums(Pij15)),30,1))
colnames(td)[1]<-"Pop"
td$type<-rep(1:5,6)
td$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=5)
td$Pop_pc<-100*td$Pop/rep(tapply(td$Pop,td$Year,sum),each=5)
td$Type<-as.factor(td$type)
td$Type<-recode(td$Type, "1"="Manuf. Housing", "2"="SF Detached","3"="SF Attached","4"="MF 2-4","5"="MF 5+")
p<-ggplot(aes(x=Year,y=Pop_pc,group=Type,color=Type),data=td) + geom_line(size=1) + geom_point() +
  ylab('Population (%)') + labs(title='Population share by House Type, 1990-2015') + theme_bw() +
  theme(text = element_text(size=13),axis.text=element_text(size=12),legend.text=element_text(size=11))
windows()
p

# occupancy/household size
ODri<-1/(tapply(r$NHSLDMEM*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum))
od<-as.data.frame(matrix(ODri))
colnames(od)[1]<-"Hpp" # house per person
od$Div<-rep(1:9,6)
od$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=9)
od$Division<-as.factor(od$Div)
od$Division<-recode(od$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")

p<-ggplot(aes(x=Year,y=Hpp,group=Division,color=Division),data=od) + geom_line(size=1) + geom_point() +
  ylab('House/person') + labs(title='Occupied homes per person, 1990-2015')
vj<-c(rep(0.5,6),1,0,0.5)
windows()
p +xlim(1990,2020) + ylim(0.35,0.435) + geom_text(data=od[od$Year==2015,],aes(label=Division), hjust=-0.25,vjust=vj) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

CDri<-tapply(r$NHSLDMEM*r$NWEIGHT,list(r$AgeCohort,r$RECSYEAR),sum)
cd<-as.data.frame(matrix(CDri))
colnames(cd)[1]<-"Pop"
cd$Cohort<-rep(c("<1950","1950s","1960s","1970s","1980s","1990+"),6)
cd$Year<-as.factor(rep(c(1990,1993,2001,2005,2009,2015),each=6))
cd$Pop_pc<-100*cd$Pop/rep(tapply(cd$Pop,cd$Year,sum),each=6)
p<-ggplot(aes(x=Year,y=Pop,group=Cohort),data=cd,class) + geom_col(aes(fill=Cohort),position = position_stack(reverse = TRUE)) + 
   ylab('Population') + labs(title='Population by housing age cohort, 1990-2015')+ 
windows()
p  + guides(fill = guide_legend(reverse=TRUE))

p<-ggplot(aes(x=Year,y=Pop_pc,group=Cohort,color=Cohort),data=cd) + geom_line(size=1) + geom_point()+ theme_bw() +
  ylab('Population (%)') + labs(title='Population share by housing age cohort, 1990-2015') +
  theme(text = element_text(size=13),axis.text=element_text(size=12),legend.text=element_text(size=11)) 
windows()
p + guides(colour = guide_legend(reverse=TRUE))


p <- ggplot(eub, aes(x = Year, y = Energy,group = EndUse))+ #ylim(0,12.7) +
  geom_col(aes(fill = EndUse), width = 0.7)

HDri<-tapply(r$NHSLDMEM*r$NWEIGHT,list(r$HeatFuel,r$RECSYEAR),sum)
hd<-as.data.frame(matrix(HDri))
colnames(hd)[1]<-"Pop"
hd$Fuel<-rep(c("None","Gas","Oil","Electricity","Other"),6)
hd$Year<-as.factor(rep(c(1990,1993,2001,2005,2009,2015),each=5))
hd$Pop_pc<-100*hd$Pop/rep(tapply(hd$Pop,hd$Year,sum),each=5)

p<-ggplot(aes(x=Year,y=Pop_pc,group=Fuel,color=Fuel),data=hd) + geom_line(size=1) + geom_point()+ theme_bw() +
  ylab('Population (%)') + labs(title='Population by main heating fuel, 1990-2015') + ylim(0,60) +
  theme(text = element_text(size=13),axis.text=element_text(size=12),legend.text=element_text(size=11))
windows()
p

H2Dri<-tapply(r$NHSLDMEM*r$NWEIGHT,list(r$DHWFuel,r$RECSYEAR),sum) # see below for division level stats
H2Dri[is.na(H2Dri)]<-0

h2d<-as.data.frame(matrix(H2Dri))
colnames(h2d)[1]<-"Pop"
h2d$Fuel<-rep(c("None","Gas","Oil","Electricity","Other"),6)
h2d$Year<-as.factor(rep(c(1990,1993,2001,2005,2009,2015),each=5))
h2d$Pop_pc<-100*h2d$Pop/rep(tapply(h2d$Pop,h2d$Year,sum),each=5)

p<-ggplot(aes(x=Year,y=Pop_pc,group=Fuel,color=Fuel),data=h2d) + geom_line(size=1) + geom_point()+ theme_bw() +
  ylab('Population (%)') + labs(title='Population by main water heating fuel, 1990-2015') + ylim(0,60) +
  theme(text = element_text(size=13),axis.text=element_text(size=12),legend.text=element_text(size=11))
windows()
p

LHDriT<-(1/10.765)*tapply(r$TOTHSQFT*r$NWEIGHT,list(r$TYPEHUQ,r$RECSYEAR),sum)/tapply(r$NWEIGHT,list(r$TYPEHUQ,r$RECSYEAR),sum)
lhdt<-as.data.frame(matrix(LHDriT))
colnames(lhdt)[1]<-"HFA" # heat floor area per house
lhdt$type<-rep(1:5,6)
lhdt$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=5)
lhdt$Type<-as.factor(lhdt$type)
lhdt$Type<-recode(lhdt$Type, "1"="Manuf. Housing", "2"="SF Detached","3"="SF Attached","4"="MF 2-4","5"="MF 5+")
p<-ggplot(aes(x=Year,y=HFA,group=Type,color=Type),data=lhdt) + geom_line(size=1) + geom_point() +
  ylab('Heated floor area (m2) per house') + labs(title='a) Average heated floor area by house type, 1990-2015') + theme_bw() +
  theme(text = element_text(size=13),axis.text=element_text(size=12),legend.text=element_text(size=11)) + ylim(50,210)
windows()
p


LHDri<-(1/10.765)*tapply(r$TOTHSQFT*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)
lhd<-as.data.frame(matrix(LHDri))
colnames(lhd)[1]<-"HFA" # house per person
lhd$Div<-rep(1:9,6)
lhd$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=9)
lhd$Division<-as.factor(lhd$Div)
lhd$Division<-recode(lhd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")

p<-ggplot(aes(x=Year,y=HFA,group=Division,color=Division),data=lhd) + geom_line(size=1) + geom_point() +
  ylab('Heated floor area (m2) per house') + labs(title='b) Average heated floor area by Division, 1990-2015')
windows()
p +xlim(1990,2020) + geom_text(data=lhd[lhd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=0.5) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

LHpc<-tapply(r$TOTHSQFT*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$NWEIGHT*r$TOTSQFT,list(r$DIVISION,r$RECSYEAR),sum)

LCDri<-tapply(r$AIRCOND*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)
lcd<-as.data.frame(matrix(LCDri))
colnames(lcd)[1]<-"AC" # house per person
lcd$AC<-100*lcd$AC
lcd$Div<-rep(1:9,6)
lcd$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=9)
lcd$Division<-as.factor(lcd$Div)
lcd$Division<-recode(lcd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
vj<-c(rep(0.5,2),1.2,0.5,0.2,0.5,-0.1,0.5,0.5)
p<-ggplot(aes(x=Year,y=AC,group=Division,color=Division),data=lcd) + geom_line(size=1) + geom_point() +
  ylab('Percentage of homes owning AC equipment (%)') + labs(title='Access to space cooling, by Division, 1990-2015')
windows()
p +xlim(1990,2020) + geom_text(data=lcd[lcd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=vj) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

#
p<-ggplot(aes(x=Year,y=AC,group=Division,color=Division),data=lcd) + geom_line(size=1) + geom_point() +
  ylab('Percentage of homes owning AC equipment (%)') + labs(title='a) Access to space cooling, by Division, 1990-2015')
windows()
p +xlim(1990,2020) + geom_text(data=lcd[lcd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=vj) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=11),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

LCDriSF<-tapply(r$TOTCSQFT*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$TOTSQFT*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)
LCDri_pcrooms<-tapply(r$ACROOMS*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$NWEIGHT*r$TOTROOMS,list(r$DIVISION,r$RECSYEAR),sum)

# intensites ,heat
IHeatDri<-tapply(r$BTUSPHwa*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$TOTHSQFT*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)*1.055056*10.765 # to mj/m2
ihd<-as.data.frame(matrix(IHeatDri))
colnames(ihd)[1]<-"SHI" # space heating intensity
ihd$Div<-rep(1:9,6)
ihd$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=9)
ihd$Division<-as.factor(ihd$Div)
ihd$Division<-recode(ihd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")

p<-ggplot(aes(x=Year,y=SHI,group=Division,color=Division),data=ihd) + geom_line(size=1) + geom_point() +
  ylab('Final energy per heated floor area (MJ/m2)') + labs(title='Space heating final energy intensity, weather-adjusted, 1990-2015')
windows()
p +xlim(1990,2020) + geom_text(data=ihd[ihd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=0.5) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])
# Cooling
ICoolDri<-tapply(r$BTUCOLwa*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$AIRCOND*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)*1.055056*0.001 # to GJ/house
icd<-as.data.frame(matrix(ICoolDri))
colnames(icd)[1]<-"SCI" # space cooling intensity
icd$Div<-rep(1:9,6)
icd$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=9)
icd$Division<-as.factor(icd$Div)
icd$Division<-recode(icd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
vj<-c(0.5,1,rep(0.5,7))
p<-ggplot(aes(x=Year,y=SCI,group=Division,color=Division),data=icd) + geom_line(size=1) + geom_point() +
  ylab('Final energy per house with cooling (GJ/house)') + labs(title='Space cooling final energy intensity, weather-adjusted, 1990-2015')
windows()
p +xlim(1990,2020) + ylim(0,20) + geom_text(data=icd[icd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=vj) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])
# DHW
IDHWDri<-tapply(r$BTUDHWwa*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$NHSLDMEM*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)*1.055056*0.001 # to GJ/cap
ihwd<-as.data.frame(matrix(IDHWDri))
colnames(ihwd)[1]<-"WHI" # water heating intensity
ihwd$Div<-rep(1:9,6)
ihwd$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=9)
ihwd$Division<-as.factor(ihwd$Div)
ihwd$Division<-recode(ihwd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
vj<-c(-0.2,rep(0.5,7),-0.2)
p<-ggplot(aes(x=Year,y=WHI,group=Division,color=Division),data=ihwd) + geom_line(size=1) + geom_point() +
  ylab('Final energy per person (GJ/cap)') + labs(title='Water heating final energy intensity, weather-adjusted, 1990-2015')
windows()
p +xlim(1990,2020) + ylim(5,12) + geom_text(data=ihwd[ihwd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=vj) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])
# other
IOthDri<-tapply(r$BTUOTH*r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)/tapply(r$NWEIGHT,list(r$DIVISION,r$RECSYEAR),sum)*1.055056*0.001 # to GJ/house
iod<-as.data.frame(matrix(IOthDri))
colnames(iod)[1]<-"OI" # water heating intensity
iod$Div<-rep(1:9,6)
iod$Year<-rep(c(1990,1993,2001,2005,2009,2015),each=9)
iod$Division<-as.factor(iod$Div)
iod$Division<-recode(iod$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
vj<-c(rep(0.5,5),-0.2,rep(0.5,3))
p<-ggplot(aes(x=Year,y=OI,group=Division,color=Division),data=iod) + geom_line(size=1) + geom_point() +
  ylab('Final energy per house (GJ/house)') + labs(title='Other energy final energy intensity, 1990-2015')
windows()
p +xlim(1990,2020) + ylim(20,40) + geom_text(data=iod[iod$Year==2015,],aes(label=Division), hjust=-0.25,vjust=vj) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

# natural gas becomes less prominent in space heating overall, but it increases in the East coast, and reduces everywhere else, most notably south-central and pacific
g<-r[r$HeatFuel==1,]
e<-r[r$HeatFuel==5,]
o<-r[r$HeatFuel==3,]

gh<-r[r$DHWFuel==1,]
eh<-r[r$DHWFuel==5,]
oh<-r[r$DHWFuel==3,]

gas<-tapply(g$NWEIGHT*g$NHSLDMEM,list(g$RECSYEAR,g$DIVISION),sum)/tapply(r$NWEIGHT*r$NHSLDMEM,list(r$RECSYEAR,r$DIVISION),sum)
ele<-tapply(e$NWEIGHT*e$NHSLDMEM,list(e$RECSYEAR,e$DIVISION),sum)/tapply(r$NWEIGHT*r$NHSLDMEM,list(r$RECSYEAR,r$DIVISION),sum)
fo<-tapply(o$NWEIGHT*o$NHSLDMEM,list(o$RECSYEAR,o$DIVISION),sum)/tapply(r$NWEIGHT*r$NHSLDMEM,list(r$RECSYEAR,r$DIVISION),sum)
fo[is.na(fo)]<-0

hwgas<-tapply(gh$NWEIGHT*gh$NHSLDMEM,list(gh$RECSYEAR,gh$DIVISION),sum)/tapply(r$NWEIGHT*r$NHSLDMEM,list(r$RECSYEAR,r$DIVISION),sum)
hwele<-tapply(eh$NWEIGHT*eh$NHSLDMEM,list(eh$RECSYEAR,eh$DIVISION),sum)/tapply(r$NWEIGHT*r$NHSLDMEM,list(r$RECSYEAR,r$DIVISION),sum)
hwfo<-tapply(oh$NWEIGHT*oh$NHSLDMEM,list(oh$RECSYEAR,oh$DIVISION),sum)/tapply(r$NWEIGHT*r$NHSLDMEM,list(r$RECSYEAR,r$DIVISION),sum)
hwfo[is.na(hwfo)]<-0

ghd<-as.data.frame(matrix(gas))
colnames(ghd)[1]<-"Pop"
ghd$Div<-rep(1:9,each=6)
ghd$Year<-rep(c(1990,1993,2001,2005,2009,2015),9)
ghd$Pop_pc<-100*ghd$Pop
ghd$Division<-as.factor(ghd$Div)
ghd$Division<-recode(ghd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
p<-ggplot(aes(x=Year,y=Pop_pc,group=Division,color=Division),data=ghd) + geom_line(size=1) + geom_point() +
  ylab('Population (%)') + labs(title='a) Population using mainly gas for space heat by Division, 1990-2015')
windows()
p +xlim(1990,2020) + ylim(20,90) + geom_text(data=ghd[ghd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=0.5) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

ehd<-as.data.frame(matrix(ele))
colnames(ehd)[1]<-"Pop"
ehd$Div<-rep(1:9,each=6)
ehd$Year<-rep(c(1990,1993,2001,2005,2009,2015),9)
ehd$Pop_pc<-100*ehd$Pop
ehd$Division<-as.factor(ehd$Div)
ehd$Division<-recode(ehd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
p<-ggplot(aes(x=Year,y=Pop_pc,group=Division,color=Division),data=ehd) + geom_line(size=1) + geom_point() +
  ylab('Population (%)') + labs(title='b) Population using mainly electricity for space heat by Division, 1990-2015')
windows()
p +xlim(1990,2020) + ylim(0,70) + geom_text(data=ehd[ehd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=0.5) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

ohd<-as.data.frame(matrix(fo))
colnames(ohd)[1]<-"Pop"
ohd$Div<-rep(1:9,each=6)
ohd$Year<-rep(c(1990,1993,2001,2005,2009,2015),9)
ohd$Pop_pc<-100*ohd$Pop
ohd$Division<-as.factor(ohd$Div)
ohd$Division<-recode(ohd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
p<-ggplot(aes(x=Year,y=Pop_pc,group=Division,color=Division),data=ohd) + geom_line(size=1) + geom_point() +
  ylab('Population (%)') + labs(title='c) Population using mainly fuel oil for space heat by Division, 1990-2015')
windows()
p +xlim(1990,2020) + ylim(0,60) + geom_text(data=ohd[ohd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=0.5) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])


ghwd<-as.data.frame(matrix(hwgas))
colnames(ghwd)[1]<-"Pop"
ghwd$Div<-rep(1:9,each=6)
ghwd$Year<-rep(c(1990,1993,2001,2005,2009,2015),9)
ghwd$Pop_pc<-100*ghwd$Pop
ghwd$Division<-as.factor(ghwd$Div)
ghwd$Division<-recode(ghwd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
p<-ggplot(aes(x=Year,y=Pop_pc,group=Division,color=Division),data=ghwd) + geom_line(size=1) + geom_point() +
  ylab('Population (%)') + labs(title='a) Population using mainly gas for hot water by Division, 1990-2015')
windows()
p +xlim(1990,2020) + ylim(20,85) + geom_text(data=ghwd[ghwd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=0.5) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

ehwd<-as.data.frame(matrix(hwele))
colnames(ehwd)[1]<-"Pop"
ehwd$Div<-rep(1:9,each=6)
ehwd$Year<-rep(c(1990,1993,2001,2005,2009,2015),9)
ehwd$Pop_pc<-100*ehwd$Pop
ehwd$Division<-as.factor(ehwd$Div)
ehwd$Division<-recode(ehwd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
vj<-c(0,rep(0.5,7),1)
p<-ggplot(aes(x=Year,y=Pop_pc,group=Division,color=Division),data=ehwd) + geom_line(size=1) + geom_point() +
  ylab('Population (%)') + labs(title='b) Population using mainly electricity for hot water by Division, 1990-2015')
windows()
p +xlim(1990,2020) + geom_text(data=ehwd[ehwd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=vj) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

ohwd<-as.data.frame(matrix(hwfo))
colnames(ohwd)[1]<-"Pop"
ohwd$Div<-rep(1:9,each=6)
ohwd$Year<-rep(c(1990,1993,2001,2005,2009,2015),9)
ohwd$Pop_pc<-100*ohwd$Pop
ohwd$Division<-as.factor(ohwd$Div)
ohwd$Division<-recode(ohwd$Division, "1"="New England", "2"="Mid Atlantic","3"="E-N Central","4"="W-N Central","5"="South Atlantic","6"="E-S Central","7"="W-S Central","8"="Mountain","9"="Pacific")
p<-ggplot(aes(x=Year,y=Pop_pc,group=Division,color=Division),data=ohwd) + geom_line(size=1) + geom_point() +
  ylab('Population (%)') + labs(title='c) Population using mainly fuel oil for hot water by Division, 1990-2015')
windows()
p +xlim(1990,2020) + ylim(0,40) + geom_text(data=ohwd[ohwd$Year==2015,],aes(label=Division), hjust=-0.25,vjust=0.5) + theme_bw() +
  theme(legend.position = "none",text = element_text(size=13),axis.text=element_text(size=12)) +  
  scale_colour_manual(values = cols12[1:9])

# # the following stats shw that natural gas heating grew as a portion of total heating until 2001, then declined, and in 2015 was slightly lower (by 5pp) than in 1990
tapply(r$BTUNGSPH*r$NWEIGHT,list(r$RECSYEAR,r$DIVISION),sum)/tapply(r$BTUSPH*r$NWEIGHT,list(r$RECSYEAR,r$DIVISION),sum)
tapply(r$BTUNGSPH*r$NWEIGHT,list(r$RECSYEAR),sum)/tapply(r$BTUSPH*r$NWEIGHT,list(r$RECSYEAR),sum)
# 
# trends in pe, fe, ghg
trend<-read.csv('E_GHG_trend.csv')
colnames(trend)[2:4]<-c("Final Energy","Primary Energy","GHG Emissions")
tr<-melt(trend,id="Year")
colnames(tr)[2:3]<-c("Indicator","Index")
p<-ggplot(aes(x=Year,y=Index,group=Indicator,color=Indicator),data=tr) + geom_line(size=1) + geom_point() +
  ylab('Index (Base=1990)') + labs(title='US Residential Energy and GHG growth, 1990-2020')  + ylim(0.8,1.4) + xlim(1990,2020) + theme_bw() 
  
windows()
p + theme(text = element_text(size=14),axis.text=element_text(size=14),legend.text=element_text(size=12)) + scale_color_brewer(palette="Dark2")
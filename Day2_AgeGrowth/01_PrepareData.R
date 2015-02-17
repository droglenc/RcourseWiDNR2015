# 01_PrepareData.Rmd
# clears objects in R workspace
rm(list = ls())
# load needed packages
library(fishWiDNR)   # for setDBClasses()
library(FSA)         # for lencat(), filterD()
library(dplyr)       # for select(), mutate(), arrange(), %>%
library(lubridate)   # for month()
# Load and prepare the data
setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("SAWYER_fish_raw_data_012915.csv",stringsAsFactors=FALSE,na.strings=c("-","NA","")) %>%
  setDBClasses(type="RDNR") %>%
  select(County,Waterbody.Name,Survey.Year,Sample.Date,Gear,Fish.Data.Seq.No,Species,
         Length.or.Lower.Length.IN,Gender,Age..observed.annuli.,Edge.Counted.Desc,Age.Structure) %>%
  mutate(mon=month(Sample.Date,label=TRUE)) %>%
  mutate(lcat=lencat(Length.or.Lower.Length.IN,w=0.5)) %>%
  arrange(Species,Length.or.Lower.Length.IN)

wae <- filterD(d,Waterbody.Name=="NELSON LAKE",Survey.Year==2014,mon=="May",Species=="WALLEYE")
wae.aged <- filterD(wae,!is.na(Age..observed.annuli.))
xtabs(~Gender+lcat,data=wae)
xtabs(~Gender+lcat,data=wae.aged)
clrs <- c("black","gray40","gray70")
plot(Length.or.Lower.Length.IN~jitter(Age..observed.annuli.),data=wae.aged,pch=16,
      col=clrs[Gender],xlab="Age (yrs)",ylab="Total Length (in)")
legend("bottomright",levels(wae$Gender),col=clrs,pch=16,cex=0.75,bty="n")

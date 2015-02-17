# 02_AgeLengthKey.Rmd
# Some of the output was suppressed when making the final document.
# Thus, running this script will give somewhat different results.
# clears objects in R workspace
rm(list = ls())
# load needed packages
library(fishWiDNR)   # for setDBClasses()
library(FSA)         # for lencat(), filterD()
library(dplyr)       # for select(), mutate(), arrange(),%>%
library(magrittr)    # for %<>%
library(lubridate)   # for month()
library(plotrix)     # for plotH(), histStack()
# Load and prepare data ... copied from previous handout
setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("SAWYER_fish_raw_data_012915.csv",stringsAsFactors=FALSE,na.strings=c("-","NA","")) %>%
  setDBClasses(type="RDNR") %>%
  select(County,Waterbody.Name,Survey.Year,Sample.Date,Gear,Fish.Data.Seq.No,Species,
         Length.or.Lower.Length.IN,Gender,Age..observed.annuli.,Edge.Counted.Desc,Age.Structure) %>%
  mutate(mon=month(Sample.Date,label=TRUE)) %>%
  mutate(lcat=lencat(Length.or.Lower.Length.IN,w=0.5)) %>%
  arrange(Species,Length.or.Lower.Length.IN)
wae <- filterD(d,Waterbody.Name=="NELSON LAKE",Survey.Year==2014,mon=="May",Species=="WALLEYE",
               Gender!="U",Length.or.Lower.Length.IN>11.5,Length.or.Lower.Length.IN<21)

waeF <- filterD(wae,Gender=="F")
waeM <- filterD(wae,Gender=="M")
waeM.aged <- filterD(waeM,!is.na(Age..observed.annuli.))
waeM.aged$Age..observed.annuli.
( rawM <- xtabs(~lcat+Age..observed.annuli.,data=waeM) )
alkM1 <- prop.table(rawM,margin=1)
print(alkM1,digits=2,zero.print="-")                         # for display only
alkPlot(alkM1,pal="gray",xlab="Total Length (in)")
waeM.unaged <- filter(waeM,is.na(Age..observed.annuli.))
waeM.unaged <- alkIndivAge(alkM1,Age..observed.annuli.~Length.or.Lower.Length.IN,data=waeM.unaged)
waeM.fnl <- rbind(waeM.aged,waeM.unaged)
waeM.fnl$Age..observed.annuli.
waeF.aged <- filter(waeF,!is.na(Age..observed.annuli.))

rawF <- xtabs(~lcat+Age..observed.annuli.,data=waeF)
alkF1 <- prop.table(rawF,margin=1)
print(alkF1,digits=2,zero.print="-")                         # for display only
alkPlot(alkF1,pal="gray",xlab="Total Length (in)")

waeF.unaged <- filter(waeF,is.na(Age..observed.annuli.))
waeF.unaged <- alkIndivAge(alkF1,Age..observed.annuli.~Length.or.Lower.Length.IN,data=waeF.unaged)
waeF.fnl <- rbind(waeF.aged,waeF.unaged)
headtail(waeF.fnl)
# # remove some objects because this will be sourced for 03_LinearModelIntro
rm(alkF1,alkM1,d,rawF,rawM,wae,waeF,waeF.aged,waeF.unaged,waeM,waeM.aged,waeM.unaged)

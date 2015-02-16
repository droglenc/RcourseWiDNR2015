setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day2_AgeGrowth")

library(rmarkdown)
library(FSA)

fn <- "01_PrepareData.Rmd"
render(fn,"all")
swvCode(fn,moreItems=c("knitr"),topnotes=c(fn),blanks="none")

fn <- "02_AgeLengthKey.Rmd"
render(fn,"all")
swvCode(fn,moreItems=c("knitr"),topnotes=c(fn,"Some of the output was suppressed when making the final document.","Thus, running this script will give somewhat different results."),blanks="none")

fn <- "03_SummarizeAgeData.Rmd"
render(fn,"all")
swvCode(fn,moreItems=c("knitr"),topnotes=c(fn),blanks="none")

fn <- "04_LinearModelIntro.Rmd"
render(fn,"all")
swvCode(fn,moreItems=c("knitr"),topnotes=c(fn),blanks="none")

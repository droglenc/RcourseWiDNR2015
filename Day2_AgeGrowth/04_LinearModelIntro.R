# 04_LinearModelIntro.Rmd
# clears objects in R workspace
rm(list = ls())
# load needed packages
library(FSA)         # for headtail(), filterD(), fitPlot()
library(dplyr)       # for mutate(),%>%
library(multcomp)    # for glht(), mcp()
ruf <- read.csv("RuffeSLRH.csv") %>%
  mutate(logW=log10(wt),logL=log10(tl),fYear=factor(year))
headtail(ruf)
ruf90 <- filterD(ruf,year==1990)
fit1 <- lm(logW~logL,data=ruf90)
coef(fit1)
confint(fit1)
anova(fit1)
( tmp <- range(ruf90$logL) )
xs <- seq(tmp[1],tmp[2],length.out=99)
xs[1:10]
ys <- predict(fit1,data.frame(logL=xs))
ys[1:10]
plot(logW~logL,data=ruf90,pch=19,col=rgb(0,0,0,1/6),ylab="log Weight (g)",xlab="log Total Length (mm)")
lines(ys~xs,lwd=2)

plot(wt~tl,data=ruf90,pch=19,col=rgb(0,0,0,1/6),ylab="Weight (g)",xlab="Total Length (mm)")
btxs <- 10^xs
btys <- 10^ys
lines(btys~btxs,lwd=2)
ruf9000 <- filterD(ruf,year %in% c(1990,2000))
fit2 <- lm(logW~logL*fYear,data=ruf9000)
anova(fit2)
coef(fit2)
confint(fit2)

tmp <- ruf9000 %>% group_by(fYear) %>% summarize(min=min(tl,na.rm=TRUE),max=max(tl,na.rm=TRUE))
tmp

# base plot
clrs <- c(rgb(0,0,0,1/6),rgb(0,0,1,1/6))
plot(wt~tl,data=ruf9000,pch=19,col=clrs[ruf9000$fYear],ylab="Weight (g)",xlab="Total Length (mm)")

# plot line for 1990
tmpx <- seq(tmp$min[1],tmp$max[1],length.out=99)
tmpy <- 10^(predict(fit2,data.frame(logL=log10(tmpx),fYear=factor(1990))))
lines(tmpy~tmpx,lwd=2)

# plot line for 2000
tmpx <- seq(tmp$min[2],tmp$max[2],length.out=99)
tmpy <- 10^(predict(fit2,data.frame(logL=log10(tmpx),fYear=factor(2000))))
lines(tmpy~tmpx,col="blue",lwd=2)

# add a legend
legend("topleft",c("1990","2000"),pch=19,col=c("black","blue"),bty="n")
ruf2 <- filterD(ruf,year %in% c(1990,1995,2000,2006))
fit3 <- lm(tl~fYear,data=ruf2)
anova(fit3)

mc1 <- glht(fit3,mcp(fYear="Tukey"))
summary(mc1)
cld(mc1)
fitPlot(fit3,ylab="Total Length (mm)",xlab="Capture Year")

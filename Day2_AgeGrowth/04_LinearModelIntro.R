# 04_LinearModelIntro.Rmd
source("03_SummarizeAgeData.R")
ls()
plot(Length.or.Lower.Length.IN~Age..observed.annuli.,data=waeM.fnl,pch=16,col=rgb(0,0,0,1/6),
     xlab="Age",ylab="Total Length (mm)",ylim=c(10,21))

lmM <- lm(Length.or.Lower.Length.IN~Age..observed.annuli.,data=waeM.fnl)
coef(lmM)
confint(lmM)
anova(lmM)

ages <- waeM.sumlen$Age..observed.annuli.
( waeM.sumlen %<>% mutate(predL=predict(lmM,data.frame(Age..observed.annuli.=ages))) )
plot(Length.or.Lower.Length.IN~Age..observed.annuli.,data=waeM.fnl,pch=16,col=rgb(0,0,0,1/6),
     xlab="Age",ylab="Total Length (mm)",ylim=c(10,21))
lines(mean~Age..observed.annuli.,data=waeM.sumlen,lwd=2,lty=2)
lines(predL~Age..observed.annuli.,data=waeM.sumlen,lwd=2,lty=1)
wae <- rbind(waeF.fnl,waeM.fnl)
levels(wae$Gender)

lmMF <- lm(Length.or.Lower.Length.IN~Age..observed.annuli.*Gender,data=wae)
coef(lmMF)
confint(lmMF)
anova(lmMF)

ages <- waeF.sumlen$Age..observed.annuli.
( waeF.sumlen %<>% mutate(predL2=predict(lmMF,data.frame(Age..observed.annuli.=ages,Gender="F"))) )
ages <- waeM.sumlen$Age..observed.annuli.
( waeM.sumlen %<>% mutate(predL2=predict(lmMF,data.frame(Age..observed.annuli.=ages,Gender="M"))) )
clr <- c(rgb(0,0,0,1/6),rgb(1,0,0,1/6))
plot(Length.or.Lower.Length.IN~Age..observed.annuli.,data=wae,pch=16,col=clr[Gender],
     xlab="Age",ylab="Total Length (mm)",ylim=c(10,21))
lines(predL2~Age..observed.annuli.,data=waeF.sumlen,lwd=2,lty=2,col="black")
lines(predL2~Age..observed.annuli.,data=waeM.sumlen,lwd=2,lty=1,col="red")
lm3 <- lm(Age..observed.annuli.~Gender,data=wae)
anova(lm3)

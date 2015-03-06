# 04_SizeStructure_I.Rmd

# load needed packages
library(fishWiDNR)   # for setDBClasses()
library(dplyr)       # for filter(), select(), mutate(), group_by(), summarize()
#  options(dplyr.print_max=1e9)
library(FSA)         # for expandCounts(), filterD(), Summarize(), hist(),
library(lubridate)   # for month()

# load FM data, expand lengths, select pertintent variables ... mostly copied code from previous
setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("SAWYER_fish_raw_data_012915.csv",stringsAsFactors=FALSE,na.strings=c("-","NA",""))
d <- setDBClasses(d,type="RDNR")
d <- expandCounts(d,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len")
d <- mutate(d,Mon=month(Survey.Begin.Date,label=TRUE))
d <- select(d,Species,Waterbody.Name,Survey.Year,Gear,Survey.Begin.Date,Mon,Len)

Spr <- filterD(d,Survey.Year==2013,Mon %in% c("Apr","May","Jun"))
BGSpr <- filterD(Spr,Species=="BLUEGILL")
BGSprLC <- filterD(BGSpr,Waterbody.Name=="LAKE CHETAC",Gear=="BOOM SHOCKER")
headtail(BGSprLC)

Summarize(~Len,data=BGSprLC,digits=2)

hist(~Len,data=BGSprLC)
hist(~Len,data=BGSprLC,xlab="Total Length (In.)",ylab="Number of Bluegill",
     xlim=c(3,9),ylim=c(0,80),col="salmon")
hist(~Len,data=BGSprLC,xlab="Total Length (In.)",ylab="Number of Bluegill",
     xlim=c(3,9),ylim=c(0,40),breaks=seq(3,9,0.2),col="#FA8072")

BGSpr <- group_by(BGSpr,Waterbody.Name)
summarize(BGSpr,n=n(),meanLen=mean(Len))                    # see use of na.rm=TRUE below
summarize(BGSpr,n=n(),valid_n=sum(!is.na(Len)),
           meanLen=mean(Len,na.rm=TRUE),sdLen=sd(Len,na.rm=TRUE),
           minLen=min(Len,na.rm=TRUE),maxLen=max(Len,na.rm=TRUE)  )

BGSpr <- filterD(BGSpr,Len>=3)
summarize(BGSpr,n=n(),valid_n=sum(!is.na(Len)),
           meanLen=round(mean(Len,na.rm=TRUE),2),sdLen=round(sd(Len,na.rm=TRUE),2),
           minLen=min(Len,na.rm=TRUE),maxLen=max(Len,na.rm=TRUE),
           PSDQ=perc(Len,6,digits=0),PSD7=perc(Len,7,digits=0),PSDP=perc(Len,8,digits=0)  )

Spr <- group_by(Spr,Waterbody.Name,Species)
tmp <- summarize(Spr,n=n(),valid_n=sum(!is.na(Len)),
                 meanLen=round(mean(Len,na.rm=TRUE),2),sdLen=round(sd(Len,na.rm=TRUE),2) )
tmp                                                           # only partial results shown

write.csv(tmp,"LenSum_Sawyer_Spr13.csv",row.names=FALSE)


# ############################################################
# ############################################################
# # Create a formatted table as a picture that allows results
# # to be cirectly input into presentations or reports
# #    largely from Gretchen Hansen
# ############################################################

# # Load some extra packages
library(ggplot2)
library(gridExtra)

# # Get a summarized data.frame (2nd to last example from above)
tmp <- summarize(BGSpr,n=n(),valid_n=sum(!is.na(Len)),
                 meanLen=round(mean(Len,na.rm=TRUE),2),sdLen=round(sd(Len,na.rm=TRUE),2),
                 minLen=min(Len,na.rm=TRUE),maxLen=max(Len,na.rm=TRUE),
                 PSDQ=perc(Len,6,digits=0),PSD7=perc(Len,7,digits=0),PSDP=perc(Len,8,digits=0)  )

# # Create a title for the table and the outputted file
ttl <- "Summary Statistics Sawyer Co. Bluegill Spring 2013"

# # This creates the table
p <- ggplot(tmp,aes(x=1,y=1)) +
  annotation_custom(tableGrob(tmp,show.rownames=FALSE)) +
  ggtitle(ttl) +
  theme_bw() +
  theme(panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title=element_text(size=rel(1.2),face="bold",vjust=2),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank())
p

ggsave(paste0(ttl,".png"),p,width=9,height=4)
# ############################################################
# ############################################################

# 05_SizeStructure_II.Rmd

# load needed packages
library(fishWiDNR)   # for setDBClasses()
library(FSA)         # for expandCounts(), Summarize(), hist()
library(magrittr)    # for %<>%
library(dplyr)       # for %>%, filter(), select(), mutate(), group_by(), summarize()
#  options(dplyr.print_max=1e9)
library(lubridate)   # for month()

# load FM data, expand lengths, add variables, select pertintent variables
setwd("C:/aaaWork/Web/fishR/Courses/WiDNR_Statewide_2015/Day1_IntroR_FMData")
d <- read.csv("SAWYER_fish_raw_data_012915.csv",stringsAsFactors=FALSE,na.strings=c("-","NA","")) %>%
      setDBClasses(type="RDNR") %>%
      expandCounts(~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN,new.name="Len") %>%
      mutate(Mon=month(Survey.Begin.Date,label=TRUE),Species1=capFirst(Species)) %>%
      select(Species,Species1,Waterbody.Name,Survey.Year,Mon,Gear,Len)

# create some subsets for use below.
Spr <- filterD(d,Survey.Year==2013,Mon %in% c("Apr","May","Jun"))
BGSpr <- filterD(Spr,Species=="BLUEGILL")
BGSprLC <- filterD(BGSpr,Waterbody.Name=="LAKE CHETAC",Gear=="BOOM SHOCKER")
SprLC <- filterD(Spr,Waterbody.Name=="LAKE CHETAC")

( brks <- psdVal("Bluegill",units="in",addLens=7) )

BGSprLC %<>% mutate(lcat=lencat(Len,breaks=brks),
                     lcat1=lencat(Len,breaks=brks,use.names=TRUE),
                     lcat2=lencat(Len,breaks=brks,use.names=TRUE,drop.levels=TRUE))
headtail(BGSprLC)

xtabs(~lcat,data=BGSprLC)
xtabs(~lcat1,data=BGSprLC)
( freq <- xtabs(~lcat2,data=BGSprLC) )

( rcum <- rcumsum(freq) )
rcum["stock"]                                                 # demo number of stock fish
rcum/rcum["stock"]*100

BGSpr %<>% mutate(lcat2=lencat(Len,breaks=brks,use.names=TRUE,drop.levels=TRUE))
( freq <- xtabs(~Waterbody.Name+lcat2,data=BGSpr) )
apply(freq,MARGIN=1,FUN=rcumsum)     # apply result has wrong orientation, only partial results shown
apply(freq,MARGIN=1,FUN=rcumsum)[1:5,1:6]
( rcum <- t(apply(freq,MARGIN=1,FUN=rcumsum)) )
rcum <- rcum[,-1]                                             # remove "substock" column
rcum/rcum[,"stock"]*100

SprLC %<>% mutate(lcat2=psdAdd(Len,Species1,units="in"))
headtail(SprLC)

( freq <- xtabs(~Species+lcat2,data=SprLC) )
( rcum <- t(apply(freq,MARGIN=1,FUN=rcumsum)) )
rcum <- rcum[,-1]
rcum/rcum[,"stock"]*100


# ############################################################
# ############################################################
# # Create a formatted table as a picture that allows results
# # to be cirectly input into presentations or reports
# #    largely from Gretchen Hansen
# ############################################################

# # Load some extra packages
library(ggplot2)
library(gridExtra)

# # Save last result from above, must be coerced to a data.frame
tmp <- data.frame(round(rcum/rcum[,"stock"]*100,0))

# # Create a title for the table and the outputted file
ttl <- "PSD Analysis, Lake Chetac, Spring 2013"

# # This creates the table (note the use of show.rownames=TRUE)
p <- ggplot(tmp,aes(x=1,y=1)) +
  annotation_custom(tableGrob(tmp,show.rownames=TRUE)) +
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

ggsave(paste0(ttl,".png"),p,width=6,height=4)
# ############################################################
# ############################################################

#This code was used on 2020-2-28 to perform the analyses reported in:
#Savage, P. E., Yamauchi, M., Hamaguchi, M., Tarr, B., Kitayama, Y., & Fujii, S. Rhythm, synchrony and cooperation.

#Set working directory
setwd("/Users/pesavage/Documents/Research/Papers/Unpublished/Social harmony")

#Install and load packages
install.packages("TOSTER")
install.packages("pwr")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("plotrix")
install.packages("compute.es")

library(TOSTER)
library(pwr)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(plotrix)
library(compute.es)
library(psych)

#power analysis
#pwr.t.test(n = , d = 0.45 , sig.level = 0.025, power = 0.95, type = c("two.sample"), alternative=c("greater")) # for d =.45, power =.95
pwr.t.test(n = , d = 0.4 , sig.level = 0.025, power = 0.8, type = c("two.sample"), alternative=c("greater")) # for d =.4, power =.8

#Specify data download location
file.data <- "https://raw.githubusercontent.com/compmusiclab/rhythm-coop/master/RhythmPilotData(Fig3).csv"

#Load pilot data for Fig. 3 and Fig. S1
data<-read.csv(file=file.data)
data$attitudinal<-rowMeans(data[,4:7])
attitudinal <- data$attitudinal

#Define function for mean and 95% confidence interval
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-std.error(x)*1.96
  ymax <- m+std.error(x)*1.96
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#Plot data for Fig. 3
ggplot(data, aes(x=group, y=behavior, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(0,1000) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=attitudinal, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))

#Plot data for Fig. S1
ggplot(data, aes(x=group, y=entitativity, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=similarity, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=trust, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=IOS, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=difficulty, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=fun, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=embarrassment, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=familiarity, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,4) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=InstructionFollowing, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=synchronization, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + ylim(1,7) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=sex, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=age, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=NativeLanguage, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))
ggplot(data, aes(x=group, y=musicianship, color=group)) + geom_violin() +stat_summary(fun.data=data_summary, geom="pointrange") + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6) + theme(axis.text=element_text(size=21),axis.title=element_text(size=23,face="bold"))

#Calculate effect sizes

#Effect size behavior
mes(mean(subset(data,group=="beat")$behavior), mean(subset(data,group=="no-beat")$behavior), sd(subset(data,group=="beat")$behavior), sd(subset(data,group=="no-beat")$behavior), length(subset(data,group=="beat")$behavior),length(subset(data,group=="no-beat")$behavior))

#Effect size attitude
mes(mean(subset(data,group=="beat")$attitudinal), mean(subset(data,group=="no-beat")$attitudinal), sd(subset(data,group=="beat")$attitudinal), sd(subset(data,group=="no-beat")$attitudinal), length(subset(data,group=="beat")$attitudinal),length(subset(data,group=="no-beat")$attitudinal))

#Effect size each item
mes(mean(subset(data,group=="beat")$entitativity), mean(subset(data,group=="no-beat")$entitativity), sd(subset(data,group=="beat")$entitativity), sd(subset(data,group=="no-beat")$entitativity), length(subset(data,group=="beat")$entitativity),length(subset(data,group=="no-beat")$entitativity))
mes(mean(subset(data,group=="beat")$similarity), mean(subset(data,group=="no-beat")$similarity), sd(subset(data,group=="beat")$similarity), sd(subset(data,group=="no-beat")$similarity), length(subset(data,group=="beat")$similarity),length(subset(data,group=="no-beat")$similarity))
mes(mean(subset(data,group=="beat")$trust), mean(subset(data,group=="no-beat")$trust), sd(subset(data,group=="beat")$trust), sd(subset(data,group=="no-beat")$trust), length(subset(data,group=="beat")$trust),length(subset(data,group=="no-beat")$trust))
mes(mean(subset(data,group=="beat")$IOS), mean(subset(data,group=="no-beat")$IOS), sd(subset(data,group=="beat")$IOS), sd(subset(data,group=="no-beat")$IOS), length(subset(data,group=="beat")$IOS),length(subset(data,group=="no-beat")$IOS))
mes(mean(subset(data,group=="beat")$difficulty), mean(subset(data,group=="no-beat")$difficulty), sd(subset(data,group=="beat")$difficulty), sd(subset(data,group=="no-beat")$difficulty), length(subset(data,group=="beat")$difficulty),length(subset(data,group=="no-beat")$difficulty))
mes(mean(subset(data,group=="beat")$fun), mean(subset(data,group=="no-beat")$fun), sd(subset(data,group=="beat")$fun), sd(subset(data,group=="no-beat")$fun), length(subset(data,group=="beat")$fun),length(subset(data,group=="no-beat")$fun))
mes(mean(subset(data,group=="beat")$embarrassment), mean(subset(data,group=="no-beat")$embarrassment), sd(subset(data,group=="beat")$embarrassment), sd(subset(data,group=="no-beat")$embarrassment), length(subset(data,group=="beat")$embarrassment),length(subset(data,group=="no-beat")$embarrassment))
mes(mean(subset(data,group=="beat")$familiarity), mean(subset(data,group=="no-beat")$familiarity), sd(subset(data,group=="beat")$familiarity), sd(subset(data,group=="no-beat")$familiarity), length(subset(data,group=="beat")$familiarity),length(subset(data,group=="no-beat")$familiarity))
mes(mean(subset(data,group=="beat")$InstructionFollowing), mean(subset(data,group=="no-beat")$InstructionFollowing), sd(subset(data,group=="beat")$InstructionFollowing), sd(subset(data,group=="no-beat")$InstructionFollowing), length(subset(data,group=="beat")$InstructionFollowing),length(subset(data,group=="no-beat")$InstructionFollowing))
mes(mean(subset(data,group=="beat")$synchronization), mean(subset(data,group=="no-beat")$synchronization), sd(subset(data,group=="beat")$synchronization), sd(subset(data,group=="no-beat")$synchronization), length(subset(data,group=="beat")$synchronization),length(subset(data,group=="no-beat")$synchronization))

#mean behavior
mean(subset(data,group=="no-beat")$behavior)
mean(subset(data,group=="beat")$behavior)

#mean attitudinal
mean(subset(data,group=="no-beat")$attitudinal)
mean(subset(data,group=="beat")$attitudinal)

#Internal consistency analysis (Cronbach's alpha)
psych::alpha(data[,4:7])

#significance tests (to be performed for confirmatory analyses on full data only):

#t-tests:
#1) Behavioral
t.test(behavior ~ group, data, alternative="greater")

#2) Attitudinal
t.test(attitudinal ~ group, data, alternative="greater")

#equivalence tests:

#1) Behavioral
#TOSTtwo(m1= mean(subset(data,group=="beat")$behavior), m2= mean(subset(data,group=="no-beat")$behavior), sd1= sd(subset(data,group=="beat")$behavior), sd2= sd(subset(data,group=="no-beat")$behavior), n1= length(subset(data,group=="beat")$behavior), n2= length(subset(data,group=="no-beat")$behavior), low_eqbound_d=-0.45, high_eqbound_d=0.45, alpha = 0.025) #assuming SESOI .45
TOSTtwo(m1= mean(subset(data,group=="beat")$behavior), m2= mean(subset(data,group=="no-beat")$behavior), sd1= sd(subset(data,group=="beat")$behavior), sd2= sd(subset(data,group=="no-beat")$behavior), n1= length(subset(data,group=="beat")$behavior), n2= length(subset(data,group=="no-beat")$behavior), low_eqbound_d=-0.4, high_eqbound_d=0.4, alpha = 0.025)#assuming SESOI .4

#2) Attitudinal
#TOSTtwo(m1= mean(subset(data,group=="beat")$attitudinal), m2= mean(subset(data,group=="no-beat")$attitudinal), sd1= sd(subset(data,group=="beat")$attitudinal), sd2= sd(subset(data,group=="no-beat")$attitudinal), n1= length(subset(data,group=="beat")$attitudinal), n2= length(subset(data,group=="no-beat")$attitudinal), low_eqbound_d=-0.45, high_eqbound_d=0.45, alpha = 0.025) #assuming SESOI .45
TOSTtwo(m1= mean(subset(data,group=="beat")$attitudinal), m2= mean(subset(data,group=="no-beat")$attitudinal), sd1= sd(subset(data,group=="beat")$attitudinal), sd2= sd(subset(data,group=="no-beat")$attitudinal), n1= length(subset(data,group=="beat")$attitudinal), n2= length(subset(data,group=="no-beat")$attitudinal), low_eqbound_d=-0.4, high_eqbound_d=0.4, alpha = 0.025) #assuming SESOI .4

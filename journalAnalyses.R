####   Authors: Kominksy, J. & Phillips, J.                       ###
####                                                              ###
####   Title:   Immoral professors and malfunctioning tools:      ###
####              Counterfactual relevance accounts explain the   ###
####              effect of norm violations on causal selection   ###
####                                                              ###
####   Contact: phillips01@g.harvard.edu                          ###

 

#### directory and packages #####
setwd("C:/Users/jonat/Dropbox/stillRelevant/Cognitive Science Draft/repo/KominskyPhillips/")
setwd("C:/Users/jonathan/Dropbox/stillRelevant/Cognitive Science Draft/repo/KominskyPhillips/")
setwd("/Users/jonk/Dropbox/stillRelevant/Cognitive Science Draft/repo/KominskyPhillips")

## clear workspace
rm(list=ls())

## load packages
library(tidyverse)
library(lme4)
#library(mediation)
library(lsr)
library(MASS)
library(pwr)
se <- function(x) {sd(x)/sqrt(length(x))}
jkomPalette <- c("#C93312","#046C9A","#EBCC2A","grey")

#### Experiment 1 ####
d.1 <- read.csv("data/study1.csv")

##demongraphics
d1.age <- matrix(c("mean",mean(d.1$Age,na.rm=T),"sd",sd(d.1$Age,na.rm=T),"n",length(d.1$Age)),ncol = 3)
print(d1.age)

d.1$Sex <- factor(c("Male","Female")[d.1$Sex])
d1.gender <- table(d.1$Sex, exclude=NULL)
print(d1.gender)

d.1$study <- "Study 1"

# Study 1 
d.1$condition <- factor(c("Deceived","Ignorant","Standard","Unintended")[d.1$DO.BR.FL_15])
d.1$question <- factor(c("Action","Agent","Object")[d.1$DO.BR.FL_22])

d.1 <- d.1[,-c(2:33,79:104)]

d.1$Control1_1_1 <- rowSums(d.1[,c(6,21,36)],na.rm=T)
d.1$Control1_1_2 <- rowSums(d.1[,c(7,22,37)],na.rm=T)
d.1$Control1_2_1 <- rowSums(d.1[,c(8,23,38)],na.rm=T)
d.1$Control1_2_2 <- rowSums(d.1[,c(9,24,39)],na.rm=T)

d.1$Control1_1 <- 0
d.1$Control1_1[d.1$Control1_1_1==1 & d.1$Control1_1_2==0] <- 1
d.1$Control1_2 <- 0
d.1$Control1_2[d.1$Control1_2_1==0 & d.1$Control1_2_2==1] <- 1

d.1$Control1 <- 0
d.1$Control1[d.1$Control1_1==1 & d.1$Control1_2==1] <- 1

d.1$Control2_1_1 <- rowSums(d.1[,c(10,25,40)],na.rm=T)
d.1$Control2_1_2 <- rowSums(d.1[,c(11,26,41)],na.rm=T)
d.1$Control2_2_1 <- rowSums(d.1[,c(12,27,42)],na.rm=T)
d.1$Control2_2_2 <- rowSums(d.1[,c(13,28,43)],na.rm=T)

d.1$Control2_1 <- 0
d.1$Control2_1[d.1$Control2_1_1==1 & d.1$Control2_1_2==0] <- 1
d.1$Control2_2 <- 0
d.1$Control2_2[d.1$Control2_2_1==1 & d.1$Control2_2_2==0] <- 1

d.1$Control2 <- 0
d.1$Control2[d.1$Control2_1==1 & d.1$Control2_2==1] <- 1

d.1$control <- 0
d.1$control[d.1$Control1==1 & d.1$Control2==1] <- 1

d.1$estimate1 <- rowSums(d.1[,c(14,29,44)],na.rm = T)
d.1$estimate2 <- rowSums(d.1[,c(15,30,45)],na.rm = T)
d.1$estimate3 <- rowSums(d.1[,c(16,31,46)],na.rm = T)

d.1$relevanceA <- rowSums(d.1[,c(2,17,32)],na.rm = T)
d.1$relevanceB <- rowSums(d.1[,c(3,18,33)],na.rm = T)

d.1$causeA <- rowSums(d.1[,c(4,19,34)],na.rm = T)
d.1$causeB <- rowSums(d.1[,c(5,20,35)],na.rm = T)

#Compute exclusions, verify same across conditions
d.1excl <- d.1[d.1$control!=1,-c(2:46,50:63)] # all excludes
Exp1Excl <- chisq.test(d.1excl$condition, y=d.1excl$question)
Exp1Excl #Same X-squared = 9.7437, df = 6, p-value = 0.1359. 


# For analyses that include all participants simply delete the d.1$control==1 fromt he line below
d.1 <- d.1[d.1$control==1,-c(2:46,50:63)]

length(unique(d.1$ResponseID)) # number of participants after exclusion

## bringing in the Samland & Waldmann results
d.sw <- read.csv("data/SandWdata.csv",stringsAsFactors = F)

d.sw$condition <- factor(c("Standard","Unintended","Ignorant","Deceived")[d.sw$IV1_Norm.Transgression+1])
d.sw$quesiton <- factor(c("Object","Action","Agent")[d.sw$IV2_Question.Type])
d.sw$study <- "S&W"
d.sw <- d.sw[d.sw$Correct.Answer.to.all.Checks.1..2...3...1.yes.0.no.==1,] 
d.sw1 <- d.sw[,c(1,6:7,22:24)] 
colnames(d.sw1) <- c("ResponseID","causeA","causeB","condition","question","study")

## correlation plots

d1.corrPlot1 <- gather(d.1[,-c(1:2,5:10)],causeQ, cause,-c(question,condition))
dsw.corrPlot1 <- gather(d.sw1[,-c(1,6)], causeQ, cause, -c(question,condition))
d1.corrPlotR <- gather(d.1[,-c(1:2,5:8,11:12)],relevanceQ, relevance,-c(question,condition))

d1.sum1 <- d1.corrPlot1 %>%
  group_by(condition,causeQ,question) %>%
  summarise(N = length(cause),
            mean = mean(cause, na.rm=TRUE), 
            sd   = sd(cause,na.rm=TRUE),
            se   = sd / sqrt(N) )

d1.sum2 <- dsw.corrPlot1 %>%
  group_by(condition,causeQ,question) %>%
  summarise(N = length(cause),
            mean = mean(cause, na.rm=TRUE), 
            sd   = sd(cause,na.rm=TRUE),
            se   = sd / sqrt(N) )

d1.sum3 <- d1.corrPlotR %>%
  group_by(condition,relevanceQ,question) %>%
  summarise(N = length(relevance),
            Rmean = mean(relevance, na.rm=TRUE)*-1+2,
            Rsd   = sd(relevance,na.rm=TRUE),
            Rse   = Rsd / sqrt(N))

d1.corrPlot <- bind_cols(d1.sum1[,c(1:3,5,7)],d1.sum2[,c(5,7)],d1.sum3[,c(5,7)])

colnames(d1.corrPlot) <- c("Condition","Agent","Question","d1Cause","d1SE","swCause","swSE","d1Relevance","d1rSE")

d1.corrPlot$Agent <- factor(d1.corrPlot$Agent) 
d1.corrPlot$Agent <- factor(c("C","V")[d1.corrPlot$Agent]) 

## Correlation between causation results:
cor.test(d1.corrPlot$swCause,d1.corrPlot$d1Cause)
pwr.r.test(n=24,r=.9504613)

## Figure 1a-b ----

fig1a <- ggplot(d1.corrPlot, aes(x=swCause, y=d1Cause)) +
  geom_errorbarh(aes(xmin=swCause-swSE, xmax=swCause+swSE), linetype=3) +
  geom_errorbar(aes(ymin=d1Cause-d1SE, ymax=d1Cause+d1SE), linetype=3) +
  stat_smooth(method=lm, formula= y ~ x) +
  #coord_cartesian(ylim=c(.15,1)) +
  geom_point(aes(color=Condition,shape=Question),stat = "identity", position = "jitter",size=rel((6))) +
  geom_text(aes(label=Agent),stat = "identity",size=rel((4))) +
  theme_bw() +
  ylab("Average Causal Judgment") +
  xlab("S&W (2016) Causal Judgment") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #,legend.position=c(.85,.15)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.25))
    ,axis.text=element_text(size=rel(1.5))
    ,strip.text=element_text(size=rel(1.25))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
    ,plot.title = element_text(face="bold",vjust=.75)
  )

#fig1a ## If you want to see the image

# ggsave(fig1a, file="figures/fig1a.jpg",dpi=800)

## Relevance plots

fig1b <- ggplot(d1.corrPlot, aes(x= d1Relevance, y=d1Cause)) +
  geom_errorbarh(aes(xmin=d1Relevance-d1rSE, xmax=d1Relevance+d1rSE), linetype=3) +
  geom_errorbar(aes(ymin=d1Cause-d1SE, ymax=d1Cause+d1SE), linetype=3) +
  geom_point(aes(color=Condition,shape=Question),stat = "identity", position = "jitter",size=rel((6))) +
  geom_text(aes(label=Agent),stat = "identity",size=rel((4))) +
  stat_smooth(method=lm, formula= y ~ x) +
  #coord_cartesian(ylim=c(.2,1)) +
  theme_bw() +
  ylab("Average Causal Judgment") +
  xlab("Average Relevance Judgment") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #,legend.position=c(.85,.15)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.25))
    ,axis.text=element_text(size=rel(1.5))
    ,strip.text=element_text(size=rel(1.25))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
    ,plot.title = element_text(face="bold",vjust=.75)
  )

#fig1b ##if you want to see the image

# ggsave(fig1b, file="figures/fig1b.jpg",dpi=800)


## Exp. 1 Primary analyses ####
d.1$cause <- NA
d.1$cause[d.1$causeA==1 & d.1$causeB==1] <- "Both"
d.1$cause[d.1$causeA==0 & d.1$causeB==1] <- "Norm violating"
d.1$cause[d.1$causeA==1 & d.1$causeB==0] <- "Norm conforming"
d.1$cause <- factor(d.1$cause)

d.1$relevance <- NA
d.1$relevance[d.1$relevanceA==1 & d.1$relevanceB==1] <- "Both"
d.1$relevance[d.1$relevanceA==2 & d.1$relevanceB==1] <- "Norm violating"
d.1$relevance[d.1$relevanceA==1 & d.1$relevanceB==2] <- "Norm conforming"
d.1$relevance <- factor(d.1$relevance)

## Causal judgments

#main effects
olr1.1 <- polr(cause ~ condition + question,data=d.1)
olr1.1.results <- dropterm(olr1.1,test="Chisq")
olr1.1.results
# Power analysis for revision
pwr.chisq.test(w=olr1.1.results$LRT[2],df=olr1.1.results$Df[2],N=length(unique(d.1$ResponseID)))
pwr.chisq.test(w=olr1.1.results$LRT[3],df=olr1.1.results$Df[3],N=length(unique(d.1$ResponseID)))

#interaction effect
olr1.0 <- polr(cause ~ condition * question,data=d.1)
olr1.0.results <- dropterm(olr1.0, test = "Chisq") 
olr1.0.results
pwr.chisq.test(w=olr1.0.results$LRT[2],df=olr1.0.results$Df[2],N=length(unique(d.1$ResponseID)))


## relevance judgments

#main effects
olr1.5 <- polr(relevance ~ condition + question,data=d.1)
olr1.5.results <- dropterm(olr1.5,test="Chisq")
olr1.5.results
pwr.chisq.test(w=olr1.5.results$LRT[2],df=olr1.5.results$Df[2],N=length(unique(d.1$ResponseID)))
pwr.chisq.test(w=olr1.5.results$LRT[3],df=olr1.5.results$Df[3],N=length(unique(d.1$ResponseID)))


#interaction effect
olr1.4 <- polr(relevance ~ condition * question,data=d.1)
olr1.4.results <- dropterm(olr1.4, test = "Chisq")
olr1.4.results
pwr.chisq.test(w=olr1.4.results$LRT[2],df=olr1.4.results$Df[2],N=length(unique(d.1$ResponseID)))


## correlations 

# At the level of conditions:
cor.test(d1.corrPlot$swCause,d1.corrPlot$d1Relevance)
pwr.r.test(n=24,r=.8477429)

# At the level of individual responses

#Norm-violating agent/action/artifact
cor.test(d.1$causeB,d.1$relevanceB)

#Norm-conforming agent/action/artifact
cor.test(d.1$causeA,d.1$relevanceA)

# Splitting by agent/action/artifact
names <- c("relevance", "cause")

d1.temp <- d.1[,c(3:4,9:12)]
colnames(d1.temp) <- c("condition","question",rep(names,each=2))

d1.temp <- rbind(d1.temp[,c(1:3,5)],d1.temp[,c(1:2,4,6)])

# Agents
cor.test(d1.temp$cause[d.1$question=="Agent"],d1.temp$relevance[d.1$question=="Agent"])
# Actions
cor.test(d1.temp$cause[d.1$question=="Action"],d1.temp$relevance[d.1$question=="Action"])
# Artifacts
cor.test(d1.temp$cause[d.1$question=="Object"],d1.temp$relevance[d.1$question=="Object"])


## Replications reported in Supplementary Materials (Table 1)

#Standard norm violation
table1a <- aggregate(cause~question, FUN='table', data=d.1[d.1$condition=="Standard",])
chisq.test(as.table(table1a[[2]])[,-2]) #NB: removed norm confirming bc of low values to follow S&W (2016)
cramersV(as.table(table1a[[2]])[,-2])

chisq.test(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Object"])[-2])
cramersV(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Object"])[-2])

chisq.test(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Action"])[-2])
cramersV(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Action"])[-2])

chisq.test(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Agent"])[-2])
cramersV(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Agent"])[-2])

#Unintentional norm violation
table1b <- aggregate(cause~question, FUN='table', data=d.1[d.1$condition=="Unintended",])
chisq.test(table1b[[2]]) 
cramersV(table1b[[2]])

#Ignorant norm violation
table1c <- aggregate(cause~question, FUN='table', data=d.1[d.1$condition=="Ignorant",])
chisq.test(table1c[[2]]) 
cramersV(table1c[[2]])

# Deception-based  
table1d <- aggregate(cause~question, FUN='table', data=d.1[d.1$condition=="Deceived",])
chisq.test(table1d[[2]])
cramersV(table1d[[2]])

# Chemical question 
table1e <- aggregate(cause~condition, FUN='table', data=d.1[d.1$question=="Object",])
chisq.test(table1e[[2]])
cramersV(table1e[[2]])

# Action question 
table1f <- aggregate(cause~condition, FUN='table', data=d.1[d.1$question=="Action",])
chisq.test(table1f[[2]])
cramersV(table1f[[2]])

# Agent question 
table1g <- aggregate(cause~condition, FUN='table', data=d.1[d.1$question=="Agent",])
chisq.test(table1g[[2]])
cramersV(table1g[[2]])

# Difference from standard violation 
table1h <- aggregate(cause~condition, FUN='table', data=d.1[d.1$question=="Agent",])
# unintended violation
chisq.test(as.table(table1h[[2]])[3:4,])
cramersV(as.table(table1h[[2]])[3:4,])
# ignorant violation
chisq.test(as.table(table1h[[2]])[2:3,])
cramersV(as.table(table1h[[2]])[2:3,])
# deception violation
chisq.test(as.table(table1h[[2]])[c(1,3),])
cramersV(as.table(table1h[[2]])[c(1,3),])


## relevance judgments

#Standard norm violation
table1i <- aggregate(relevance~question, FUN='table', data=d.1[d.1$condition=="Standard",])
chisq.test(as.table(table1i[[2]])[,-2]) #NB: removed norm confirming bc of low values to follow S&W (2016)
cramersV(as.table(table1i[[2]])[,-2])

#Unintentional norm violation
table1j <- aggregate(relevance~question, FUN='table', data=d.1[d.1$condition=="Unintended",])
chisq.test(table1j[[2]])
cramersV(table1j[[2]])

#Ignorant norm violation
table1k <- aggregate(relevance~question, FUN='table', data=d.1[d.1$condition=="Ignorant",])
chisq.test(table1k[[2]])
cramersV(table1k[[2]])

# Deception-based
table1l <- aggregate(relevance~question, FUN='table', data=d.1[d.1$condition=="Deceived",])
chisq.test(table1l[[2]])
cramersV(table1l[[2]])

# Chemical question
table1m <- aggregate(relevance~condition, FUN='table', data=d.1[d.1$question=="Object",])
chisq.test(table1m[[2]])
cramersV(table1m[[2]])

# Action question
table1n <- aggregate(relevance~condition, FUN='table', data=d.1[d.1$question=="Action",])
chisq.test(table1n[[2]])
cramersV(table1n[[2]])

# Agent question
table1o <- aggregate(relevance~condition, FUN='table', data=d.1[d.1$question=="Agent",])
chisq.test(table1o[[2]])
cramersV(table1o[[2]])

# Difference from standard violation
table1p <- aggregate(relevance~condition, FUN='table', data=d.1[d.1$question=="Agent",])
# unintended violation
chisq.test(as.table(table1p[[2]])[3:4,])
cramersV(as.table(table1p[[2]])[3:4,])
# ignorant violation
chisq.test(as.table(table1p[[2]])[2:3,])
cramersV(as.table(table1p[[2]])[2:3,])
# deception violation
chisq.test(as.table(table1p[[2]])[c(1,3),])
cramersV(as.table(table1p[[2]])[c(1,3),])


## Replication of probability estimates from S&W 
## - reported in the Supplemenatary Materials. Experiment 1: Probability Estimates
summary(aov(lm(estimate1~condition, data=d.1)))
summary(aov(lm(estimate2~condition, data=d.1)))
summary(aov(lm(estimate3~condition, data=d.1)))

## Table reporting violator, conforming, both, neither.
d.1$cause<-as.character(d.1$cause)
d.1$cause[d.1$causeA==0 & d.1$causeB==0] <- "Neither"
d.1$cause <- factor(d.1$cause)
d.1$relevance <- as.character(d.1$relevance)
d.1$relevance[d.1$relevanceA==2 & d.1$relevanceB==2] <- "Neither"
d.1$relevance <- factor(d.1$relevance)
  
d1.causeSum <- d.1 %>%
  group_by(condition, question,cause) %>%
  summarise(Count=n())

d1.relSum <- d.1 %>%
  group_by(condition, question, relevance) %>%
  summarise(Count = n())



### Experiment 2 ####

d2 <- read.csv("data/study2.csv")

## Participant info
table(d2$Gender,exclude=NULL)
mean(as.numeric(d2$Age),na.rm=T)
sd(d2$Age,na.rm=T)
length(unique(d2$ResponseId)) ## total number of participants 

## this builds a tibble for the condition info for each participant
d2.c <- d2 %>% dplyr::select(c(9,52,54:59)) %>% 
  gather(context,condition,-c(1:5)) %>%
  filter(condition!="") %>%
  mutate(condition = factor(condition),
         structure = factor(c(rep(c(rep("Conjunctive",4),rep("Disjunctive",4)),3))[condition]),
         agentOrder = factor(rep(c("Billy-Suzy","Suzy-Billy"),12)[condition]),
         norm = factor(rep(c("Conforming","Conforming","Violating","Violating"),6)[condition]),
         questionOrder = case_when(
           as.numeric(FL_28_DO) == 2 ~ "Cause-Relevance",
           as.numeric(FL_28_DO) == 3 ~ "Relevance-Cause",
           as.numeric(FL_36_DO) == 2 ~ "Cause-Relevance",
           as.numeric(FL_36_DO) == 3 ~ "Relevance-Cause",
           as.numeric(FL_40_DO) == 2 ~ "Cause-Relevance",
           as.numeric(FL_40_DO) == 3 ~ "Relevance-Cause"
         )
  ) %>%
  dplyr::select(c(1,2,8:11))

## this builds a tibble for the resposnes for each participant
d2.r <- d2 %>% dplyr::select(c(9,23:24,33:34,42:43)) %>%
  gather(questionCode,response,-1, na.rm = T) %>%
  mutate(questionCode = factor(questionCode),
         question = factor(rep(c("Cause","Relevance"),3)[questionCode])
  ) %>%
  dplyr::select(-2)

## this builds a tibble for the control questions
d2.ex <- d2 %>% dplyr::select(c(9,25,27:28,35:37,44:46)) %>%
  gather(questionCode,response,-1,na.rm=T) %>%
  mutate(questionCode = factor(questionCode),
         question = factor(c("Check1","Check2","Feedback","Check1","Check2","Feedback","Feedback","Check1","Check2"))[questionCode]
  ) %>%
  filter(!is.na(response)) %>%
  filter(response!="")

d2.ex2 <- left_join(d2.ex[d2.ex$question=="Check1",c(1,3)],d2.ex[d2.ex$question=="Check2",c(1,3)],by="ResponseId") 
d2.ex3 <- left_join(d2.ex2,d2.ex[d2.ex$question=="Feedback",c(1,3)],by="ResponseId")

colnames(d2.ex3) <- c("ResponseId","Check1","Check2","Feedback")

d2l <- left_join(d2.r,d2.c,by="ResponseId")
d2l <- left_join(d2l,d2.ex3,by="ResponseId")

d2l <- d2l %>% mutate(
  firstQ = case_when(
    question=="Cause" & questionOrder=="Cause-Relevance" ~ TRUE,
    question=="Relevance" & questionOrder=="Relevance-Cause" ~ TRUE
  )
)

#control questions:
d2l$control <- 0 

# Norm conforming:

# Motion
d2l$control[d2l$norm=="Conforming" & d2l$Check1=="Neither Billy nor Suzy" & d2l$structure=="Conjunctive" & d2l$Story=="Motion" & d2l$Check2=="2 or more"] <- 1
d2l$control[d2l$norm=="Conforming" & d2l$Check1=="Neither Billy nor Suzy" & d2l$structure=="Disjunctive" & d2l$Story=="Motion" & d2l$Check2=="1 or more"] <- 1

# Battery
d2l$control[d2l$norm=="Conforming" & d2l$Check1=="Neither Billy nor Suzy" & d2l$structure=="Conjunctive" & d2l$Story=="Battery" & d2l$Check2=="Two"] <- 1
d2l$control[d2l$norm=="Conforming" & d2l$Check1=="Neither Billy nor Suzy" & d2l$structure=="Disjunctive" & d2l$Story=="Battery" & d2l$Check2=="One"] <- 1

# Train
d2l$control[d2l$norm=="Conforming" & d2l$Check1=="Neither Billy nor Suzy" & d2l$structure=="Conjunctive" & d2l$Story=="Train" & as.numeric(factor(d2l$Check2))==4] <- 1
d2l$control[d2l$norm=="Conforming" & d2l$Check1=="Neither Billy nor Suzy" & d2l$structure=="Disjunctive" & d2l$Story=="Train" & as.numeric(factor(d2l$Check2))==3] <- 1

# Norm violating:

# Motion
d2l$control[d2l$norm=="Violating" & d2l$Check1=="Billy" & d2l$structure=="Conjunctive" & d2l$Story=="Motion" & d2l$Check2=="2 or more"] <- 1
d2l$control[d2l$norm=="Violating" & d2l$Check1=="Billy" & d2l$structure=="Disjunctive" & d2l$Story=="Motion" & d2l$Check2=="1 or more"] <- 1

# Battery
d2l$control[d2l$norm=="Violating" & d2l$Check1=="Billy" & d2l$structure=="Conjunctive" & d2l$Story=="Battery" & d2l$Check2=="Two"] <- 1
d2l$control[d2l$norm=="Violating" & d2l$Check1=="Billy" & d2l$structure=="Disjunctive" & d2l$Story=="Battery" & d2l$Check2=="One"] <- 1

# Train
d2l$control[d2l$norm=="Violating" & d2l$Check1=="Billy" & d2l$structure=="Conjunctive" & d2l$Story=="Train" & as.numeric(factor(d2l$Check2))==4] <- 1
d2l$control[d2l$norm=="Violating" & d2l$Check1=="Billy" & d2l$structure=="Disjunctive" & d2l$Story=="Train" & as.numeric(factor(d2l$Check2))==3] <- 1

d2l.Excl <- d2l %>% filter(control!=1 & question!='Cause') # Getting 123 instead of 127...? Missing 4
Exp2Excl <- chisq.test(d2l.Excl$norm, y=d2l.Excl$structure)
Exp2Excl # Perfectly balanced. p=1.

d2l <- d2l %>% filter(control==1) 
length(unique(d2l$ResponseId))

d2.sum <- d2l %>% #filter(firstQ) %>%
  mutate(norm = factor(norm, levels=c("Violating","Conforming"))) %>%
  group_by(structure,norm,question) %>%
  summarize(N = length(response),
            mean = mean(response, na.rm=TRUE),
            sd   = sd(response,na.rm=TRUE),
            se   = sd / sqrt(N) 
  )

## Figure 2 a-b ----

fig2a <- d2.sum %>% filter(question=="Cause") %>%
  ggplot(aes(x=norm, y=mean, fill=norm)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=jkomPalette) + 
  facet_wrap(~structure) +
  ylab("Agreement with Causal Statement") +
  xlab("") +
  #scale_y_continuous(limits=c(-0.3,1.1),breaks = c(-0.25,0.00,0.25,0.50,1.00), labels = c(-0.25,0.00,0.25,0.50,1.00)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position="null"
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )
#fig2a ## If you want to see the causation graph

# ggsave(fig2a,file="figures/fig2a.jpg", dpi=800)

## Counterfactual Relevance plot
fig2b <- d2.sum %>% filter(question=="Relevance") %>%
  ggplot(aes(x=norm, y=mean, fill=norm)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=jkomPalette) + 
  facet_wrap(~structure) +
  ylab("Agreement with Relevance Statement") +
  xlab("") +
  #scale_y_continuous(limits=c(-0.3,1.1),breaks = c(-0.25,0.00,0.25,0.50,1.00), labels = c(-0.25,0.00,0.25,0.50,1.00)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position="null"
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )
# fig2b ##If you want to see the relevance plot
 
# ggsave(fig2b,file="figures/fig2b.jpg", dpi=800)

## Replication of Icard et al. 

d2i.lm0 <- lmer(response ~ norm * structure + (1|Story), data = d2l[d2l$question=="Cause",], REML = F)
d2i.lm1 <- lmer(response ~ norm + structure + (1|Story), data = d2l[d2l$question=="Cause",], REML = F)
anova(d2i.lm0,d2i.lm1)
# Power analysis
pwr.chisq.test(w=66.599,N=length(unique(d2$ResponseId)),df=1)

## Effect of norm
d2i.lm2 <- lmer(response ~ structure + (1|Story), data = d2l[d2l$question=="Cause",], REML = F)
anova(d2i.lm1,d2i.lm2)
pwr.chisq.test(w=11.729,N=length(unique(d2$ResponseId)),df=1)

# Effect of structure
d2i.lm3 <- lmer(response ~ norm + (1|Story), data = d2l[d2l$question=="Cause",], REML = F)
anova(d2i.lm1,d2i.lm3)
pwr.chisq.test(w=36.013,N=length(unique(d2$ResponseId)),df=1)

# Effect of norm in conjuctive structure
d2iC.0 <- lmer(response ~ norm + (1|Story), data=d2l[d2l$question=="Cause" & d2l$structure=="Conjunctive",], REML = F)
d2iC.1 <- lmer(response ~ (1|Story), data=d2l[d2l$question=="Cause" & d2l$structure=="Conjunctive",], REML = F)
anova(d2iC.0,d2iC.1)
pwr.chisq.test(w=70.557,N=length(unique(d2$ResponseId)),df=1)

# Efect of norm in disjunctive structure
d2iD.0 <- lmer(response ~ norm + (1|Story), data=d2l[d2l$question=="Cause" & d2l$structure=="Disjunctive",], REML = F)
d2iD.1 <- lmer(response ~ (1|Story), data=d2l[d2l$question=="Cause" & d2l$structure=="Disjunctive",], REML = F)
anova(d2iD.0,d2iD.1)
pwr.chisq.test(w=10.564,N=length(unique(d2$ResponseId)),df=1)


## Exp. 2 Primary analyses ####

d2.lm0 <- lmer(response ~ norm * structure * question + (1|ResponseId) + (1|Story), data = d2l, REML = F)
d2.lm1 <- lmer(response ~ (norm * structure) + (norm * question) + (question * structure) + (1|ResponseId) + (1|Story), data = d2l, REML = F)

anova(d2.lm0,d2.lm1)
pwr.chisq.test(w=42.951,N=length(unique(d2$ResponseId)),df=1)

## Causal judgments
d2.lm0c <- lmer(response ~ norm * structure + (1|Story), data = d2l[d2l$question=="Cause",], REML = F)
d2.lm1c <- lmer(response ~ norm + structure + (1|Story), data = d2l[d2l$question=="Cause",], REML = F)

anova(d2.lm0c,d2.lm1c)
pwr.chisq.test(w=66.599,N=length(unique(d2$ResponseId)),df=1)


## CF Relevance judgments
d2.lm0r <- lmer(response ~ norm * structure + (1|Story), data = d2l[d2l$question=="Relevance",], REML = F)
d2.lm1r <- lmer(response ~ norm + structure + (1|Story), data = d2l[d2l$question=="Relevance",], REML = F)
anova(d2.lm0r,d2.lm1r) 
pwr.chisq.test(w=1.0807,N=length(unique(d2$ResponseId)),df=1)

## CF Relevance judgments in Conjunctive Scenarios
d2C.lm0r <- lmer(response ~ norm  + (1|Story), data = d2l[d2l$question=="Relevance" & d2l$structure=="Conjunctive",], REML = F)
d2C.lm1r <- lmer(response ~ (1|Story), data = d2l[d2l$question=="Relevance" & d2l$structure=="Conjunctive",], REML = F)
anova(d2C.lm0r,d2C.lm1r) 
pwr.chisq.test(w=75.176,N=length(unique(d2$ResponseId)),df=1)

## CF Relevance judgments in Disjunctive Scenarios
d2D.lm0r <- lmer(response ~ norm + (1|Story), data = d2l[d2l$question=="Relevance" & d2l$structure=="Disjunctive",], REML = F)
d2D.lm1r <- lmer(response ~ (1|Story), data = d2l[d2l$question=="Relevance" & d2l$structure=="Disjunctive",], REML = F)
anova(d2D.lm0r,d2D.lm1r) 
pwr.chisq.test(w=43.652,N=length(unique(d2$ResponseId)),df=1)

d2.cond <- left_join(d2.cond[d2.cond$question=="Cause",c(1:2,4,6,8)],
                     d2.cond[d2.cond$question=="Relevance",c(1:2,4,6,8)],
                     by=c("norm","structure","Story"))
colnames(d2.cond) <- c("norm","structure","Story","cause.mean","cause.se","rel.mean","rel.se")

d2.subj$norm = factor(d2.subj$norm, levels=c("Violating","Conforming"))

fig3 <- d2.cond %>% ggplot(aes(x=rel.mean, y=cause.mean)) +
  geom_point(data=d2.subj, (aes(x=rel.mean,y=cause.mean,color=norm,shape=Story)),alpha=.35,stat="identity",position = "jitter") +
  stat_smooth(data=d2.subj,method=lm, formula= y ~ x, linetype=1, alpha=.35) +
  stat_smooth(method=lm, formula= y ~ x, linetype=2, alpha=.5) +
  geom_errorbar(aes(ymin=cause.mean-cause.se, ymax=cause.mean+cause.se), linetype=3) +
  geom_errorbarh(aes(xmin=rel.mean-rel.se, xmax=rel.mean+rel.se), linetype=3) +
  #coord_cartesian(ylim=c(.15,1)) +
  geom_point(aes(color=norm,shape=Story),stat = "identity", position="jitter",size=rel((8))) +
  #geom_text(aes(label=event),stat = "identity",size=rel((4))) +
  facet_grid(~structure) +
  scale_color_manual(values=jkomPalette) + 
  theme_bw() +
  ylab("Causal Judgments") +
  xlab("Counterfactual Relevance Judgments") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #,legend.position=c(.85,.15)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.25))
    ,axis.text=element_text(size=rel(1.5))
    ,strip.text=element_text(size=rel(1.25))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
    ,plot.title = element_text(face="bold",vjust=.75)
  )

#fig3 ## If you want to see the fig


## Correlations at the condition level:
### Conjunctive:
cor.test(d2.cond$cause.mean[d2.cond$structure=="Conjunctive"],d2.cond$rel.mean[d2.cond$structure=="Conjunctive"])
pwr.r.test(n=12,r=.9586433) # What's the n here...
### Disjunctive:
cor.test(d2.cond$cause.mean[d2.cond$structure=="Disjunctive"],d2.cond$rel.mean[d2.cond$structure=="Disjunctive"])
pwr.r.test(n=12,r=-.2205547)
pwr.r.test(n=12, power=.8) # We could detect a correlation of .72 or above.

## Correlations at the subject level:
### Conjunctive:
cor.test(d2.subj$cause.mean[d2.subj$structure=="Conjunctive"],d2.subj$rel.mean[d2.subj$structure=="Conjunctive"])
pwr.r.test(n=length(unique(d2.subj$ResponseId)),r=.6116181)
### Disjunctive:
cor.test(d2.subj$cause.mean[d2.subj$structure=="Disjunctive"],d2.subj$rel.mean[d2.subj$structure=="Disjunctive"])
pwr.r.test(n=length(unique(d2.subj$ResponseId)),r=.165471)


### Experiment 3a #####

d3a <- read.csv("data/study3a.csv")

## Participant info
table(d3a$Sex,exclude=NULL)
mean(as.numeric(d3a$Age),na.rm=T)
sd(d3a$Age,na.rm=T)
length(unique(d3a$ResponseId)) ## total number of participants 

d3a$time <- rowSums(d3a[grep("Page.Submit",names(d3a))],na.rm=T)

d3al <- d3a %>% dplyr::select(c(9,30:33,38:42)) %>%
  mutate(condition = factor(Manipulation_DO),
         condition = factor(c("No Counterfactual","Lever Counterfactual","Professor Counterfactual")[condition]),
         condition =  factor(condition, levels=c("Professor Counterfactual","No Counterfactual","Lever Counterfactual")),
         questionCond = case_when(
           FL_36_DO=="Agentcause" ~ "Agents",
           FL_36_DO=="Objectcause" ~ "Artifacts",
           FL_38_DO=="Agentcause" ~ "Agents",
           FL_38_DO=="Objectcause" ~ "Artifacts",
           FL_42_DO=="Agentcause" ~ "Agents",
           FL_42_DO=="Objectcause" ~ "Artifacts"
         )) %>%
  dplyr::select(-c(6:9)) %>%
  gather(question,response,-c(1,4:8)) %>%
  mutate(question = factor(question),
         question = factor(c("Agents","Artifacts")[question]),
         response = as.numeric(factor(response)),
         cause = case_when(
           response==1 ~ 0,
           response==2 ~ 1,
           response==3 ~ -1,
           response==4 ~ 0,
           response==5 ~ -1,
           response==6 ~ 0,
           response==7 ~ 1
         ),
         control = case_when(
           as.numeric(factor(control1))==4  & as.numeric(factor(control2))==3 ~ 1,
           TRUE ~ 0
         ),
         causeLabel = case_when(
           response==1 ~ "Neither",
           response==2 ~ "Focused",
           response==3 ~  "Not-focused",
           response==4 ~ "Both",
           response==5 ~ "Not-focused",
           response==6 ~ "Both",
           response==7 ~ "Focused"
         )
      ) %>%
      filter(questionCond == question) #%>% ## The reason you do this b/c NAs dont' distinguish between not seen and not selected

d3al.Excl <- d3al %>% 
  group_by(condition) %>%
  filter(control != 1) %>%
  summarise(n = n())
Exp3aExcl <- chisq.test(d3al.Excl$n)
Exp3aExcl # Nothing X-squared = 0.50888, df = 2, p-value = 0.7754

d3al <- d3al %>%  
  filter(control==1) %>%
  dplyr::select(c(1,5:7,9,11))

## number of subjs after exclusion
length(unique(d3al$ResponseId))

d3a.sum <- d3al %>% group_by(condition,question) %>%
  summarize(N    = length(cause),
            mean = mean(cause, na.rm=TRUE),
            sd   = sd(cause,na.rm=TRUE),
            se   = sd / sqrt(N) 
  )

## Figure 4a ----

fig4a <- ggplot(d3a.sum, aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=jkomPalette) + 
  facet_wrap(~question) +
  ylab("Causal Selection Preference for Professor/Red Lever") +
  xlab("") +
  scale_y_continuous(limits=c(0,.8),breaks = c(0.00,0.25,0.50,0.75), labels = c(0.00,0.25,0.50,0.75)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.165,.92)
    ,legend.title=element_blank()
    #,legend.title=element_text(size=rel(1.75))
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_blank()
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )

#fig4a ##if you want to see the fig

#ggsave(fig4a,file="figures/fig4a.jpg", dpi=800)

table3a <- d3al %>% group_by(condition, question,causeLabel) %>% tally() %>% spread(causeLabel,n)

d3al$cause <- factor(d3al$cause)

lm3a.1 <- polr(cause ~condition+question,data=d3al)
dropterm(lm3a.1, test="Chisq")
#Power for condition
pwr.chisq.test(w=22.2040,df=2,N=433)
#Question
pwr.chisq.test(w=.0089,df=1,N=433)
pwr.chisq.test(df=1,N=433,power=.8) # we could detect LRT of .135

# interaction effect
lm3a.0 <- polr(cause ~condition*question,data=d3al)
dropterm(lm3a.0, test = "Chisq")
pwr.chisq.test(w=17.631,df=2,N=433)


#Comparisons against no-violation condition.
## Agents
### Professor CF
aggregate(cause ~ condition, FUN = 'table', data=d3al[d3al$condition!="Lever Counterfactual" & d3al$question=="Agents",])
lm3a.1a <- polr(cause ~ condition, data=d3al[d3al$condition!="Lever Counterfactual" & d3al$question=="Agents",])
dropterm(lm3a.1a, test="Chisq") #Prof preference goes up in prof cf rel to no cf 
pwr.chisq.test(w=11.619, N=163, df=1)
### Lever CF
aggregate(cause ~ condition, FUN = 'table', data=d3al[d3al$condition!="No Counterfactual" & d3al$question=="Agents",])
lm3a.2a <- polr(cause ~ condition, data=d3al[d3al$condition!="No Counterfactual" & d3al$question=="Agents",])
dropterm(lm3a.2a, test="Chisq") #marginal
pwr.chisq.test(w=3.6466, N=155, df=1) # But fully powered.

## Objects
### Lever CF
aggregate(cause ~ condition, FUN = 'table', data=d3al[d3al$condition!="Professor Counterfactual" & d3al$question=="Artifacts",])
lm3a.1o <- polr(cause ~ condition, data=d3al[d3al$condition!="Professor Counterfactual" & d3al$question=="Artifacts",])
dropterm(lm3a.1o, test="Chisq") # Red lever preference goes up in lver cf rel to no cf 
pwr.chisq.test(w=27.406,df=1, N=132)
### Professor CF
aggregate(cause ~ condition, FUN = 'table', data=d3al[d3al$condition!="No Counterfactual" & d3al$question=="Artifacts",])
lm3a.2o <- polr(cause ~ condition, data=d3al[d3al$condition!="No Counterfactual" & d3al$question=="Artifacts",])
dropterm(lm3a.2o, test="Chisq") #sig
pwr.chisq.test(w=12.977,df=1, N=129)


#Comparisons to other CFs
## Agents
aggregate(cause ~ condition, FUN = 'table', data=d3al[d3al$condition!="No Counterfactual" & d3al$question=="Agents",])
lm3a.3a <- polr(cause ~ condition, data=d3al[d3al$condition!="No Counterfactual" & d3al$question=="Agents",])
dropterm(lm3a.3a, test="Chisq") #marginal 
pwr.chisq.test(w=3.6466,df=1, N=155)
### Artifacts
aggregate(cause ~ condition, FUN = 'table', data=d3al[d3al$condition!="No Counterfactual" & d3al$question=="Artifacts",])
lm3a.3o <- polr(cause ~ condition, data=d3al[d3al$condition!="No Counterfactual" & d3al$question=="Artifacts",])
dropterm(lm3a.3o, test="Chisq") #Doesn't quite go up... p = .1636 
#?? I get a significant effect here. p < .001.
pwr.chisq.test(w=12.977,df=1, N=129)



#### Experiment 3b: ####

## Question order 1 
d3ba <- read.csv("data/study3b_a.csv",stringsAsFactors = F)

d3ba$time <- rowSums(d3ba[,c(17,23)],na.rm = T)
d3ba$condition[d3ba$DO.BL.Manipulation=="LeverManip|altTime|outcomeChange"] <- "Artifact Counterfactual"
d3ba$condition[d3ba$DO.BL.Manipulation=="ProfManip|altTime|outcomeChange"] <- "Agent Counterfactual"
d3ba$order[d3ba$DO.BL.CausationQuestions=="Object_cause|Agent_cause"] <- "Object first"

d3ba$controlQ <- 0
d3ba$controlQ[d3ba$control1_1==1 & d3ba$control1_2==1 & is.na(d3ba$control1_3)  
             & d3ba$control2_1==1 & d3ba$control2_2==1 & is.na(d3ba$control2_3)] <- 1

d3bal <- gather(d3ba[,c(1,25:27,34:35,50:53)],judgment,cause,-c(1:2,5:10))

### Question order 2
d3bb <- read.csv("data/study3b_b.csv",stringsAsFactors = F)

d3bb$time <- rowSums(d3bb[,c(17,24)],na.rm = T)
d3bb$condition[d3bb$DO.BL.Manipulation=="control|altTime|outcomeChange"] <- "No Counterfactual"
d3bb$condition[d3bb$DO.BL.Manipulation=="LeverManip|altTime|outcomeChange"] <- "Artifact Counterfactual"
d3bb$condition[d3bb$DO.BL.Manipulation=="ProfManip|altTime|outcomeChange"] <- "Agent Counterfactual"
d3bb$order <- "Agent first"

d3bb$controlQ <- 0
d3bb$controlQ[d3bb$control1_1==1 & d3bb$control1_2==1 & is.na(d3bb$control1_3) &  
                d3bb$control2_1==1 & d3bb$control2_2==1 & is.na(d3bb$control2_3)] <- 1

d3bbl <- gather(d3bb[,c(1,26:28,35:36,51:54)],judgment,cause,-c(1:2,5:10))


### control version with No counterfactual manipulation
d3bc <- read.csv("data/study3b_c.csv",stringsAsFactors = F)

d3bc$time <- rowSums(d3bc[,c(17,22)],na.rm = T)
d3bc$condition <- "No Counterfactual"
d3bc$order[d3bc$DO.BL.CausationQuestions=="Object_cause|Agent_cause"] <- "Object first"
d3bc$outcomeChange <- NA
d3bc$controlQ <- 0
d3bc$controlQ[d3bc$control1_1==1 & d3bc$control1_2==1 & is.na(d3bc$control1_3) &  
                d3bc$control2_1==1 & d3bc$control2_2==1 & is.na(d3bc$control2_3)] <- 1

d3bcl <- gather(d3bc[,c(1,24:25,32:33,48:52)],judgment,cause,-c(1,4:10))

## Combing data from three versions

d3bl <- rbind(d3bal,d3bbl,d3bcl)


#Demographics
d3bl$Age[d3bl$Age==199] <- NA #somone seemed to be answering incorrectly...
d3b.age <- matrix(c("mean",mean(d3bl$Age[!duplicated(d3bl$ResponseID)],na.rm=T),
                   "sd",sd(d3bl$Age[!duplicated(d3bl$ResponseID)],na.rm=T),
                   "n",length(d3bl$Age[!duplicated(d3bl$ResponseID)])),ncol = 3)
print(d3b.age)

d3bl$Sex <- factor(c("Male","Female")[d3bl$Sex])
d3b.gender <- table(d3bl$Sex[!duplicated(d3bl$ResponseID)], exclude=NULL)
print(d3b.gender)

#Exclusions balanced?
d3blExcl <- d3bl[d3bl$controlQ!=1,]
d3blExcl <- d3blExcl %>% 
  group_by(condition) %>%
  summarise(n=n())
Exp3bExcl <- chisq.test(d3blExcl$n)
Exp3bExcl # Yep. X-squared = 2.0449, df = 2, p-value = 0.3597

#Control question exclusion
d3bl <- d3bl[d3bl$controlQ==1,]

#number of remaining participants is 423
length(unique(d3bl$ResponseID))

## Coding order information
d3bl$responseN <- "Second answer"
d3bl$responseN[d3bl$order=="Agent first" & d3bl$judgment=="Agent_cause_8"] <-  "First answer"
d3bl$responseN[d3bl$order=="Object first" & d3bl$judgment=="Object_cause_8"] <-  "First answer"


## Figure 4b ----

d3bl$judgment <- factor(d3bl$judgment)
d3bl$judgment <- factor(c("Agent","Artifact")[d3bl$judgment])

d3b.sum <- d3bl %>% group_by(condition,judgment) %>% 
            summarise(
                N    = length(cause),
                mean = mean(cause, na.rm=TRUE),
                sd   = sd(cause,na.rm=TRUE),
                se   = sd / sqrt(N) )

d3b.sum$condition <- factor(d3b.sum$condition)
d3b.sum$condition <- factor(c("Professor Counterfactual","Lever Counterfactual","No Counterfactual")[d3b.sum$condition])
d3b.sum$condition <- factor(d3b.sum$condition,levels=c("Professor Counterfactual","No Counterfactual","Lever Counterfactual"))

fig4b <- ggplot(d3b.sum, aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=jkomPalette) + 
  facet_wrap(~judgment) +
  ylab("Agreement with Causal Statement") +
  xlab("") +
  scale_y_continuous(limits=c(0,50)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.165,.92)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_blank()
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )

## fig4b # if you want to see figure 4b

# ggsave(fig4b,file="figures/fig4b.jpg",dpi=800)


## Overall analyses
lm3b.0 <- lmer(cause~condition * judgment + (1|ResponseID), data=d3bl)
## Interaction effect
lm3b.1 <- lmer(cause~condition + judgment + (1|ResponseID), data=d3bl)
anova(lm3b.0,lm3b.1)
pwr.chisq.test(w=23.04,df=2,N=423)
## main effect of question
lm3b.2 <- lmer(cause~condition + (1|ResponseID), data=d3bl)
anova(lm3b.1,lm3b.2)
pwr.chisq.test(w=53.135,df=1,N=423)
## main effect of condition
lm3b.3 <- lmer(cause~judgment + (1|ResponseID), data=d3bl)
anova(lm3b.1,lm3b.3)
pwr.chisq.test(w=13.492,df=2,N=423)

## Pairwise comparisons
aggregate(cause~condition*judgment, FUN=function(x) c(M = mean(x), SD = sd(x),n=length(x)),data=d3bl)
#AGENT Judgments: Agent Counterfactual vs Artifact Counterfactual
var.test(d3bl$cause[d3bl$condition=="Agent Counterfactual" & d3bl$judgment=="Agent"],
         d3bl$cause[d3bl$condition=="Artifact Counterfactual" & d3bl$judgment=="Agent"])
t.test(d3bl$cause[d3bl$condition=="Agent Counterfactual" & d3bl$judgment=="Agent"],
       d3bl$cause[d3bl$condition=="Artifact Counterfactual" & d3bl$judgment=="Agent"],var.equal =T)
cohensD(d3bl$cause[d3bl$condition=="Agent Counterfactual" & d3bl$judgment=="Agent"],
        d3bl$cause[d3bl$condition=="Artifact Counterfactual" & d3bl$judgment=="Agent"])
pwr.t2n.test(n1=151,n2=130, d=.272044) #62% power
pwr.t2n.test(n1=151,n2=130,power=.8) # Power to detect d>=.336

#AGENT Judgments: agent Counterfactual vs. no Counterfactual
var.test(d3bl$cause[d3bl$condition=="Agent Counterfactual" & d3bl$judgment=="Agent"],
         d3bl$cause[d3bl$condition=="No Counterfactual" & d3bl$judgment=="Agent"])
t.test(d3bl$cause[d3bl$condition=="Agent Counterfactual" & d3bl$judgment=="Agent"],
       d3bl$cause[d3bl$condition=="No Counterfactual" & d3bl$judgment=="Agent"])
cohensD(d3bl$cause[d3bl$condition=="Agent Counterfactual" & d3bl$judgment=="Agent"],
        d3bl$cause[d3bl$condition=="No Counterfactual" & d3bl$judgment=="Agent"])
pwr.t2n.test(n1=151,n2=136,d=.4824146) #98% power

#ARTIFACT Judgments: Artifact Counterfactual vs. Agent Counterfactual
var.test(d3bl$cause[d3bl$condition=="Artifact Counterfactual" & d3bl$judgment=="Artifact"],
         d3bl$cause[d3bl$condition=="Agent Counterfactual" & d3bl$judgment=="Artifact"])
t.test(d3bl$cause[d3bl$condition=="Artifact Counterfactual" & d3bl$judgment=="Artifact"],
       d3bl$cause[d3bl$condition=="Agent Counterfactual" & d3bl$judgment=="Artifact"])
cohensD(d3bl$cause[d3bl$condition=="Artifact Counterfactual" & d3bl$judgment=="Artifact"],
        d3bl$cause[d3bl$condition=="Agent Counterfactual" & d3bl$judgment=="Artifact"])
pwr.t2n.test(n1=147,n2=132,d=0.367483) #86% power

#ARTIFACT Judgments: Artifact Counterfactual vs.No Counterfactual
var.test(d3bl$cause[d3bl$condition=="Artifact Counterfactual" & d3bl$judgment=="Artifact"],
         d3bl$cause[d3bl$condition=="No Counterfactual" & d3bl$judgment=="Artifact"])
t.test(d3bl$cause[d3bl$condition=="Artifact Counterfactual" & d3bl$judgment=="Artifact"],
       d3bl$cause[d3bl$condition=="No Counterfactual" & d3bl$judgment=="Artifact"])
cohensD(d3bl$cause[d3bl$condition=="Artifact Counterfactual" & d3bl$judgment=="Artifact"],
        d3bl$cause[d3bl$condition=="No Counterfactual" & d3bl$judgment=="Artifact"])
pwr.t2n.test(n1=147,n2=135,d=0.4208768) #94% power


#### Experiment 4: #####

d4 <- read.csv("data/study4.csv")

##demographics
d4.age <- matrix(c("mean",mean(d4$Age,na.rm=T),"sd",sd(d4$Age,na.rm=T),"n",length(d4$Age)),ncol = 3)
print(d4.age)

d4$Sex <- factor(c("Male","Female")[d4$Sex])
d4.gender <- table(d4$Sex, exclude=NULL)
print(d4.gender)

d4$condition <- factor(c("Malfunction","Immoral","Normal")[d4$DO.BR.FL_15])
d4$question <- factor(c("Agent","Object")[d4$DO.BR.FL_22])
d4$time <- rowSums(d4[,c(17,22,27)],na.rm = T)
d4$order <- "Cause First"
d4$order[d4$DO.BL.AgentQuestions=="Agent_rel|Agent_cause"] <- "Relevance First"
d4$order[d4$DO.BL.ObjectQuestions=="Object_Rel|Object_cause"] <- "Relevance First"

d4 <- d4[,-c(2:28,51:71)]

d4$Control1_1 <- 0
d4$Control1_1[d4$condition=="Malfunction" & is.na(d4$Control1_1_1) & d4$Control1_1_2==1] <- 1
d4$Control1_1[d4$condition!="Malfunction" & d4$Control1_1_1==1 & is.na(d4$Control1_1_2)] <- 1
d4$Control1_2 <- 0
d4$Control1_2[d4$Control1_2_1==1 & is.na(d4$Control1_2_2)] <- 1

d4$Control1 <- 0
d4$Control1[d4$Control1_1==1 & d4$Control1_2==1] <- 1

d4$Control2_1 <- 0
d4$Control2_1[d4$condition=="Malfunction" & d4$Control2_1_1==1 & d4$Control2_1_2==1] <- 1
d4$Control2_1[d4$condition!="Malfunction" & d4$Control2_1_1==1 & is.na(d4$Control2_1_2)] <- 1
d4$Control2_2 <- 0
d4$Control2_2[d4$Control2_2_1==1 & is.na(d4$Control2_2_2)] <- 1

d4$Control2 <- 0
d4$Control2[d4$Control2_1==1 & d4$Control2_2==1] <- 1

d4$Control3_1 <- 0
d4$Control3_1[d4$condition=="Immoral" & is.na(d4$Control3_1_1)] <- 1
d4$Control3_1[d4$condition!="Immoral" & d4$Control3_1_1==1] <- 1
d4$Control3_2 <- 0
d4$Control3_2[d4$Control3_2_1==1] <- 1

d4$Control3 <- 0
d4$Control3[d4$Control3_1==1 & d4$Control3_2==1] <- 1

d4$control <- 0
d4$control[d4$Control1==1 & d4$Control2==1 & d4$Control3==1] <- 1

#Exclusions balanced?
d4Excl <- d4[d4$control!=1,-c(2:21,29:37)]
Exp4Excl <- chisq.test(d4Excl$condition, d4Excl$question)
Exp4Excl #X-squared = 2.0488, df = 2, p-value = 0.359

## Causal and Relevance Preference Scores
d4$relevanceA <- rowSums(d4[,c(2,6)],na.rm = T)
d4$relevanceB <- rowSums(d4[,c(3,7)],na.rm = T)
d4$causeA <- rowSums(d4[,c(4,8)],na.rm = T)
d4$causeB <- rowSums(d4[,c(5,9)],na.rm = T)

d4$causeStr <- NA
d4$causeStr[d4$causeA==0 & d4$causeB==0] <- 0 #Neither
d4$causeStr[d4$causeA==1 & d4$causeB==1] <- 0 # Both
d4$causeStr[d4$causeA==1 & d4$causeB==0] <- -1 # Admin only
d4$causeStr[d4$causeA==0 & d4$causeB==1] <- 1 # Prof only
table(d4$causeStr,exclude = NULL)

d4$relevanceStr <- NA
d4$relevanceStr[(d4$relevanceA==2 | d4$relevanceA==0) & (d4$relevanceB==2 | d4$relevanceB==0)] <- 0 #"Neither"
d4$relevanceStr[d4$relevanceA==1 & d4$relevanceB==1] <- 0 #"Both"
d4$relevanceStr[d4$relevanceA==1 & (d4$relevanceB==2 | d4$relevanceB==0)] <- -1 #"Admin Only"
d4$relevanceStr[(d4$relevanceA==2 | d4$relevanceA==0) & d4$relevanceB==1] <- 1 #"Prof Only"
table(d4$relevanceStr, exclude=NULL)

d4 <- d4[d4$control==1,-c(2:21,29:37)]
length(d4$ResponseID)

# Manipulation checks
##Moral check
d4$morality <- d4$morality-3 ## this was recoded incorrectly on Qualtrics and started at 4
aggregate(morality~condition, FUN=function(x) c(M = mean(x), SD = sd(x),n=length(x)), data=d4)
## Immoral vs normal
var.test(d4$morality[d4$condition=="Immoral"],d4$morality[d4$condition=="Normal"])
t.test(d4$morality[d4$condition=="Immoral"],d4$morality[d4$condition=="Normal"])
cohensD(d4$morality[d4$condition=="Immoral"],d4$morality[d4$condition=="Normal"])
pwr.t2n.test(n1=89,n2=83,d=2.094612)
## Immoral vs. malfunction
var.test(d4$morality[d4$condition=="Immoral"],d4$morality[d4$condition=="Malfunction"])
t.test(d4$morality[d4$condition=="Immoral"],d4$morality[d4$condition=="Malfunction"],var.equal=T)
cohensD(d4$morality[d4$condition=="Immoral"],d4$morality[d4$condition=="Malfunction"])
pwr.t2n.test(n1=89,n2=85,d=1.840503)
## Normal vs. Malfunction
var.test(d4$morality[d4$condition=="Normal"],d4$morality[d4$condition=="Malfunction"])
t.test(d4$morality[d4$condition=="Normal"],d4$morality[d4$condition=="Malfunction"],var.equal=T)
cohensD(d4$morality[d4$condition=="Normal"],d4$morality[d4$condition=="Malfunction"])
pwr.t2n.test(n1=83,n2=85,d=0.130144) # 13% power
pwr.t2n.test(n1=83,n2=85, power=.8) # 80% to detect d > .43

##Malfunciton check
aggregate(probability_4~condition, FUN=function(x) c(M = mean(x), SD = sd(x),n=length(x)), data=d4)
## Malfunction
t.test(d4$probability_4[d4$condition=="Malfunction"],mu=50, alternative="greater")
cohensD(d4$probability_4[d4$condition=="Malfunction"],mu=50)
pwr.t.test(n=79,d=2.826521,alternative='greater')
## Normal
t.test(d4$probability_4[d4$condition=="Normal"],mu=50, alternative="greater")
cohensD(d4$probability_4[d4$condition=="Normal"],mu=50)
pwr.t.test(n=77,d=1.685431,alternative='greater')
## Immoral
t.test(d4$probability_4[d4$condition=="Immoral"],mu=50, alternative="greater")
cohensD(d4$probability_4[d4$condition=="Immoral"],mu=50)
pwr.t.test(n=86,d=1.218988,alternative='greater')


d4l <- gather(d4[,c(1,4:5,13:14)],judgment,strength,-c(1:3))  

library(plyr)
d4.sum <- ddply(d4l, c("condition","question","judgment"), summarise,
                N    = length(strength),
                mean = mean(strength, na.rm=TRUE),
                sd   = sd(strength,na.rm=TRUE),
                se   = sd / sqrt(N) )
detach(package:plyr)
d4.sum$judgment <- factor(d4.sum$judgment)
d4.sum$judgment <- factor(c("Cause","Relevance")[d4.sum$judgment])

d4.sum$condition <- factor(c("Agent Violation","Artifact Violation","No Violation")[d4.sum$condition])
d4.sum$condition <- factor(d4.sum$condition, levels=c("Agent Violation","No Violation","Artifact Violation"))

d4.sum$question <- factor(c("Intentional Agent", "Functional Artifact")[d4.sum$question])
d4.sum$question <- factor(d4.sum$question, levels=c("Intentional Agent", "Functional Artifact"))

## Figure 5a-b ----

fig5a <- ggplot(d4.sum[d4.sum$judgment=="Cause",], aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=jkomPalette) + 
  facet_wrap(~question) +
  ylab("Causal Preference for Norm-violating Event") +
  xlab("") +
  scale_y_continuous(limits=c(-0.3,1.1),breaks = c(-0.25,0.00,0.25,0.50,1.00), labels = c(-0.25,0.00,0.25,0.50,1.00)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.125,.94)
    ,legend.title=element_blank()
    #,legend.title=element_text(size=rel(1.75))
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_blank()
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )

#fig5a ##if you want to view figure 5


# ggsave(fig5a, file="figures/fig5a.jpg",dpi=800)

fig5b <- ggplot(d4.sum[d4.sum$judgment=="Relevance",], aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  facet_wrap(~question) +
  scale_y_continuous(limits=c(-0.3,1.1),breaks = c(-0.25,0.00,0.25,0.50,1.00), labels = c(-0.25,0.00,0.25,0.50,1.00)) +
  scale_fill_manual(values=jkomPalette) + 
  ylab("Relevance Preference for Norm-violating Event") +
  xlab("") +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.125,.94)
    ,legend.title=element_blank()
    #,legend.title=element_text(size=rel(1.75))
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_blank()
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )

#fig5b ## if you want to view figure 5b

# ggsave(fig5b, file="figures/fig5b.jpg",dpi=800)


### Figure 6 ----

d4.sum2 <- cbind(d4.sum[d4.sum$judgment=="Cause",-3],d4.sum[d4.sum$judgment=="Relevance",4:7])

colnames(d4.sum2) <- c("condition","question","cause.n","cause.mean","cause.sd","cause.se","rel.n","rel.mean","rel.sd","rel.se")

d4.subjs <- d4

colnames(d4.subjs) <- c(names(d4)[1:12],"cause.mean","rel.mean")

d4.subjs$condition <- factor(c("Agent Violation","Artifact Violation","No Violation")[d4.subjs$condition])
d4.subjs$condition <- factor(d4.subjs$condition,levels=c("Agent Violation","No Violation", "Artifact Violation"))

d4.subjs$question <- factor(c("Intentional Agent","Functional Artifact")[d4.subjs$question])
d4.subjs$question <- factor(d4.subjs$question,levels=c("Functional Artifact","Intentional Agent"))

fig6 <- ggplot(d4.sum2, aes(x=rel.mean, y=cause.mean)) +
  geom_point(data=d4.subjs, (aes(x=rel.mean,y=cause.mean,color=condition,shape=question)),alpha=.35,stat="identity",position = "jitter") +
  stat_smooth(data=d4.subjs,method=lm, formula= y ~ x, linetype=1, alpha=.35) +
  stat_smooth(method=lm, formula= y ~ x, linetype=2, alpha=.5) +
  geom_errorbar(aes(ymin=cause.mean-cause.se, ymax=cause.mean+cause.se), linetype=3) +
  geom_errorbarh(aes(xmin=rel.mean-rel.se, xmax=rel.mean+rel.se), linetype=3) +
  #coord_cartesian(ylim=c(.15,1)) +
  geom_point(aes(color=condition,shape=question),stat = "identity",size=rel((8))) +
  #geom_text(aes(label=event),stat = "identity",size=rel((4))) +
  scale_color_manual(values=jkomPalette) + 
  theme_bw() +
  ylab("Causal Judgments") +
  xlab("Counterfactual Relevance Judgments") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #,legend.position=c(.85,.15)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.25))
    ,axis.text=element_text(size=rel(1.5))
    ,strip.text=element_text(size=rel(1.25))
    ,axis.title.y=element_text(vjust=.9)
    ,axis.ticks = element_blank()
    ,axis.title=element_text(size=rel(1.5))
    ,plot.title = element_text(face="bold",vjust=.75)
  )
#fig6 ## if you want to view figure 6

# ggsave(fig6,file="figures/fig6.jpg",dpi=800)

cor.test(d4.sum2$cause.mean,d4.sum2$rel.mean)
pwr.r.test(n=12,.9781127)
cor.test(d4$causeStr,d4$relevanceStr)
pwr.r.test(n=506,r=.5331274) # Participant-level
#proportional odds logistic regression
## Causal judgments
d4$causeStr <- factor(d4$causeStr)

lm2.1 <- polr(causeStr ~condition+question,data=d4)
dropterm(lm2.1, test="Chisq")
pwr.chisq.test(w=71.149,N=258,df=2)
pwr.chisq.test(w=0.045,N=258,df=1) # 11% power to detect tiny effect of question
pwr.chisq.test(N=258,df=1,power=.8) # 80% power to detect LRT of .17
# interaction effect
lm2.0 <- polr(causeStr ~condition*question,data=d4)
dropterm(lm2.0, test = "Chisq")
pwr.chisq.test(N=258,df=2,w=31.418)

# Moral Norm Condition
aggregate(causeStr ~ question, FUN='table', data=d4[d4$condition=="Immoral",])
lm2.1i <- polr(causeStr ~ question, data=d4[d4$condition=="Immoral",])
dropterm(lm2.1i, test="Chisq")
pwr.chisq.test(N=90,df=1,w=15.331)
# Functional Norm Condition 
aggregate(causeStr ~ question, FUN='table', data=d4[d4$condition=="Malfunction",])
lm2.1m <- polr(causeStr ~ question, data=d4[d4$condition=="Malfunction",])
dropterm(lm2.1m, test="Chisq")
pwr.chisq.test(N=85,w=12.356,df=1)
# No Norm Condition
aggregate(causeStr ~ question, FUN='table', data=d4[d4$condition=="Normal",])
lm2.1n <- polr(causeStr ~ question, data=d4[d4$condition=="Normal",])
dropterm(lm2.1n, test="Chisq")
pwr.chisq.test(N=83,w=1.1305,df=1) # Fully powered.

#Comparisons against no-violation condition.
aggregate(causeStr ~ condition, FUN = 'table', data=d4[d4$condition!="Immoral" & d4$question=="Object",])
lm2.1o <- polr(causeStr ~ condition, data=d4[d4$condition!="Immoral" & d4$question=="Object",])
dropterm(lm2.1o, test="Chisq") #Object preference goes up rel to normal for malfunction.
pwr.chisq.test(N=85,w=63.308,df=1)

aggregate(causeStr ~ condition, FUN = 'table', data=d4[d4$condition!="Malfunction" & d4$question=="Object",])
lm2.1p <- polr(causeStr ~ condition, data=d4[d4$condition!="Malfunction" & d4$question=="Object",])
dropterm(lm2.1p, test="Chisq") #Feed-forward effect, significant.
pwr.chisq.test(N=91,w=4.4781,df=1) # Fully powered.

aggregate(causeStr ~ condition, FUN = 'table', data=d4[d4$condition!="Malfunction" & d4$question=="Agent",])
lm2.1q <- polr(causeStr ~ condition, data=d4[d4$condition!="Malfunction" & d4$question=="Agent",])
dropterm(lm2.1q, test="Chisq") #Agent preference goes up rel to normal for immoral
pwr.chisq.test(N=82,w=45.957,df=1) # Fully powered.

aggregate(causeStr ~ condition, FUN = 'table', data=d4[d4$condition!="Immoral" & d4$question=="Agent",])
lm2.1r <- polr(causeStr ~ condition, data=d4[d4$condition!="Immoral" & d4$question=="Agent",])
dropterm(lm2.1r, test="Chisq") #Backtracking, significant
pwr.chisq.test(N=81,w=20.282,df=1) # Fully powered.

## Relevance judgments
d4$relevanceStr <- factor(d4$relevanceStr)
#main effects
lm2.3 <- polr(relevanceStr ~condition+question,data=d4)
dropterm(lm2.3, test="Chisq")
pwr.chisq.test(N=258,df=2,w=40.53)
pwr.chisq.test(N=258,df=1,w=0.104) # 38% power. As above fully powered to detect .17
# interaction effect
lm2.2 <- polr(relevanceStr ~condition*question,data=d4)
dropterm(lm2.2, test = "Chisq")
pwr.chisq.test(N=258,df=2,w=33.704)

# Moral Norm Condition
aggregate(relevanceStr ~ question, FUN='table', data=d4[d4$condition=="Immoral",])
lm2.3i <- polr(relevanceStr ~ question, data=d4[d4$condition=="Immoral",])
dropterm(lm2.3i, test="Chisq")
pwr.chisq.test(N=90,df=1,w=16.628)
# Functional Norm Condition 
aggregate(relevanceStr ~ question, FUN='table', data=d4[d4$condition=="Malfunction",])
lm2.3m <- polr(relevanceStr ~ question, data=d4[d4$condition=="Malfunction",])
dropterm(lm2.3m, test="Chisq")
pwr.chisq.test(N=85,w=11.199,df=1)
# No Norm Condition
aggregate(relevanceStr ~ question, FUN='table', data=d4[d4$condition=="Normal",])
lm2.3n <- polr(relevanceStr ~ question, data=d4[d4$condition=="Normal",])
dropterm(lm2.3n, test="Chisq")
pwr.chisq.test(N=83,w=4.4797,df=1) 

#comparisons against no-violation condition
aggregate(relevanceStr ~ condition, FUN = 'table', data=d4[d4$condition!="Immoral" & d4$question=="Object",])
lm2.3o <- polr(relevanceStr ~ condition, data=d4[d4$condition!="Immoral" & d4$question=="Object",])
dropterm(lm2.3o, test="Chisq") #Object preference goes up rel to normal for malfunction.
pwr.chisq.test(N=85,w=40.945,df=1)

aggregate(relevanceStr ~ condition, FUN = 'table', data=d4[d4$condition!="Malfunction" & d4$question=="Object",])
lm2.3p <- polr(relevanceStr ~ condition, data=d4[d4$condition!="Malfunction" & d4$question=="Object",])
dropterm(lm2.3p, test="Chisq") #Feed-forward effect, significant.
pwr.chisq.test(N=91,w=1.9064,df=1) # Fully powered.

aggregate(relevanceStr ~ condition, FUN = 'table', data=d4[d4$condition!="Malfunction" & d4$question=="Agent",])
lm2.3q <- polr(relevanceStr ~ condition, data=d4[d4$condition!="Malfunction" & d4$question=="Agent",])
dropterm(lm2.3q, test="Chisq") #Agent preference goes up rel to normal for immoral
pwr.chisq.test(N=82,w=34.82,df=1) # Fully powered.

aggregate(relevanceStr ~ condition, FUN = 'table', data=d4[d4$condition!="Immoral" & d4$question=="Agent",])
lm2.3r <- polr(relevanceStr ~ condition, data=d4[d4$condition!="Immoral" & d4$question=="Agent",])
dropterm(lm2.3r, test="Chisq") #Backtracking, significant
pwr.chisq.test(N=81,w=8.1972,df=1) # Fully powered.

## Descriptive choice frequency for tables in revision.
d4 <- d4 %>% mutate(cause = NA, relevance = NA)
d4$cause[d4$causeA==0 & d4$causeB==0] <- "Neither"
d4$cause[d4$causeA==1 & d4$causeB==1] <- "Both"
d4$cause[d4$causeA==1 & d4$causeB==0] <- "Norm conforming"
d4$cause[d4$causeA==0 & d4$causeB==1] <- "Norm violating"
d4$cause <- factor(d4$cause)

d4$relevance[(d4$relevanceA==2 | d4$relevanceA==0) & (d4$relevanceB==2 | d4$relevanceB==0)] <- "Neither"
d4$relevance[d4$relevanceA==1 & d4$relevanceB==1] <- "Both"
d4$relevance[d4$relevanceA==1 & (d4$relevanceB==2 | d4$relevanceB==0)] <- "Norm conforming"
d4$relevance[(d4$relevanceA==2 | d4$relevanceA==0) & d4$relevanceB==1] <- "Norm violating"
d4$relevance <- factor(d4$relevance)

d4.causeSum <- d4 %>%
  group_by(condition, question, cause) %>%
  summarise(count=n())

d4.relSum <- d4 %>%
  group_by(condition, question, relevance) %>%
  summarise(count=n())




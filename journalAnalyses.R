####   Authors: Kominksy, J. & Phillips, J.                       ###
####                                                              ###
####   Title:   Immoral professors and malfunctioning tools:      ###
####              Counterfactual relevance accounts explain the   ###
####              effect of norm violations on causal selection   ###
####                                                              ###
####   Contact: phillips01@g.harvard.edu                          ###

 

#### directory and packages #####
setwd("C:/Users/jonat/Dropbox/stillRelevant/Cognitive Science Draft/repo/KominskyPhillips/")
setwd("/Users/jonk/Dropbox/stillRelevant/Cognitive Science Draft")

## clear workspace
rm(list=ls())

## load packages
library(tidyverse)
library(lme4)
#library(mediation)
library(lsr)
library(MASS)
library(ggplot2)
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


## Figure 1:

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

# fig1a ## If you want to see the image

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

#fig1b #if you want to see the image

# ggsave(fig1b, file="figures/fig1b.jpg",dpi=800)


## Primary analyses ####
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
dropterm(olr1.1,test="Chisq")

#interaction effect
olr1.0 <- polr(cause ~ condition * question,data=d.1)
dropterm(olr1.0, test = "Chisq") 

## relevance judgments

#main effects
olr1.5 <- polr(relevance ~ condition + question,data=d.1)
dropterm(olr1.5,test="Chisq")

#interaction effect
olr1.4 <- polr(relevance ~ condition * question,data=d.1)
dropterm(olr1.4, test = "Chisq")


## correlations 

# At the level of conditions:
cor.test(d1.corrPlot$swCause,d1.corrPlot$d1Relevance)

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
table1a <- aggregate(cause~question, FUN=table, data=d.1[d.1$condition=="Standard",])
chisq.test(as.table(table1a[[2]])[,-2]) #NB: removed norm confirming bc of low values to follow S&W (2016)
cramersV(as.table(table1a[[2]])[,-2])

chisq.test(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Object"])[-2])
cramersV(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Object"])[-2])

chisq.test(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Action"])[-2])
cramersV(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Action"])[-2])

chisq.test(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Agent"])[-2])
cramersV(table(d.1$cause[d.1$condition=="Standard" & d.1$question=="Agent"])[-2])

#Unintentional norm violation
table1b <- aggregate(cause~question, FUN=table, data=d.1[d.1$condition=="Unintended",])
chisq.test(table1b[[2]]) 
cramersV(table1b[[2]])

#Ignorant norm violation
table1c <- aggregate(cause~question, FUN=table, data=d.1[d.1$condition=="Ignorant",])
chisq.test(table1c[[2]]) 
cramersV(table1c[[2]])

# Deception-based  
table1d <- aggregate(cause~question, FUN=table, data=d.1[d.1$condition=="Deceived",])
chisq.test(table1d[[2]])
cramersV(table1d[[2]])

# Chemical question 
table1e <- aggregate(cause~condition, FUN=table, data=d.1[d.1$question=="Object",])
chisq.test(table1e[[2]])
cramersV(table1e[[2]])

# Action question 
table1f <- aggregate(cause~condition, FUN=table, data=d.1[d.1$question=="Action",])
chisq.test(table1f[[2]])
cramersV(table1f[[2]])

# Agent question 
table1g <- aggregate(cause~condition, FUN=table, data=d.1[d.1$question=="Agent",])
chisq.test(table1g[[2]])
cramersV(table1g[[2]])

# Difference from standard violation 
table1h <- aggregate(cause~condition, FUN=table, data=d.1[d.1$question=="Agent",])
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
table1i <- aggregate(relevance~question, FUN=table, data=d.1[d.1$condition=="Standard",])
chisq.test(as.table(table1i[[2]])[,-2]) #NB: removed norm confirming bc of low values to follow S&W (2016)
cramersV(as.table(table1i[[2]])[,-2])

#Unintentional norm violation
table1j <- aggregate(relevance~question, FUN=table, data=d.1[d.1$condition=="Unintended",])
chisq.test(table1j[[2]])
cramersV(table1j[[2]])

#Ignorant norm violation
table1k <- aggregate(relevance~question, FUN=table, data=d.1[d.1$condition=="Ignorant",])
chisq.test(table1k[[2]])
cramersV(table1k[[2]])

# Deception-based
table1l <- aggregate(relevance~question, FUN=table, data=d.1[d.1$condition=="Deceived",])
chisq.test(table1l[[2]])
cramersV(table1l[[2]])

# Chemical question
table1m <- aggregate(relevance~condition, FUN=table, data=d.1[d.1$question=="Object",])
chisq.test(table1m[[2]])
cramersV(table1m[[2]])

# Action question
table1n <- aggregate(relevance~condition, FUN=table, data=d.1[d.1$question=="Action",])
chisq.test(table1n[[2]])
cramersV(table1n[[2]])

# Agent question
table1o <- aggregate(relevance~condition, FUN=table, data=d.1[d.1$question=="Agent",])
chisq.test(table1o[[2]])
cramersV(table1o[[2]])

# Difference from standard violation
table1p <- aggregate(relevance~condition, FUN=table, data=d.1[d.1$question=="Agent",])
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



  
#### Experiment 2: ####

## Question order 1 
d.2a <- read.csv("data/study2a.csv",stringsAsFactors = F)

d.2a$time <- rowSums(d.2a[,c(17,23)],na.rm = T)
d.2a$condition[d.2a$DO.BL.Manipulation=="LeverManip|altTime|outcomeChange"] <- "Artifact Counterfactual"
d.2a$condition[d.2a$DO.BL.Manipulation=="ProfManip|altTime|outcomeChange"] <- "Agent Counterfactual"
d.2a$order[d.2a$DO.BL.CausationQuestions=="Object_cause|Agent_cause"] <- "Object first"

d.2a$controlQ <- 0
d.2a$controlQ[d.2a$control1_1==1 & d.2a$control1_2==1 & is.na(d.2a$control1_3)  
             & d.2a$control2_1==1 & d.2a$control2_2==1 & is.na(d.2a$control2_3)] <- 1

d.2al <- gather(d.2a[,c(1,25:27,34:35,50:53)],judgment,cause,-c(1:2,5:10))

### Question order 2
d.2b <- read.csv("data/study2b.csv",stringsAsFactors = F)

d.2b$time <- rowSums(d.2b[,c(17,24)],na.rm = T)
d.2b$condition[d.2b$DO.BL.Manipulation=="control|altTime|outcomeChange"] <- "No Counterfactual"
d.2b$condition[d.2b$DO.BL.Manipulation=="LeverManip|altTime|outcomeChange"] <- "Artifact Counterfactual"
d.2b$condition[d.2b$DO.BL.Manipulation=="ProfManip|altTime|outcomeChange"] <- "Agent Counterfactual"
d.2b$order <- "Agent first"

d.2b$controlQ <- 0
d.2b$controlQ[d.2b$control1_1==1 & d.2b$control1_2==1 & is.na(d.2b$control1_3) &  
                d.2b$control2_1==1 & d.2b$control2_2==1 & is.na(d.2b$control2_3)] <- 1

d.2bl <- gather(d.2b[,c(1,26:28,35:36,51:54)],judgment,cause,-c(1:2,5:10))


### control version with No counterfactual manipulation
d.2c <- read.csv("data/study2c.csv",stringsAsFactors = F)

d.2c$time <- rowSums(d.2c[,c(17,22)],na.rm = T)
d.2c$condition <- "No Counterfactual"
d.2c$order[d.2c$DO.BL.CausationQuestions=="Object_cause|Agent_cause"] <- "Object first"
d.2c$outcomeChange <- NA
d.2c$controlQ <- 0
d.2c$controlQ[d.2c$control1_1==1 & d.2c$control1_2==1 & is.na(d.2c$control1_3) &  
                d.2c$control2_1==1 & d.2c$control2_2==1 & is.na(d.2c$control2_3)] <- 1

d.2cl <- gather(d.2c[,c(1,24:25,32:33,48:52)],judgment,cause,-c(1,4:10))

## Combing data from three versions

d.2l <- rbind(d.2al,d.2bl,d.2cl)


#Demographics
d.2l$Age[d.2l$Age==199] <- NA #somone seemed to be answering incorrectly...
d2.age <- matrix(c("mean",mean(d.2l$Age[!duplicated(d.2l$ResponseID)],na.rm=T),
                   "sd",sd(d.2$Age[!duplicated(d.2l$ResponseID)],na.rm=T),
                   "n",length(d.2$Age[!duplicated(d.2l$ResponseID)])),ncol = 3)
print(d2.age)

d.2l$Sex <- factor(c("Male","Female")[d.2l$Sex])
d2.gender <- table(d.2l$Sex[!duplicated(d.2l$ResponseID)], exclude=NULL)
print(d2.gender)

#Control question exclusion
d.2l <- d.2l[d.2l$controlQ==1,]

#number of remaining participants is 423
length(unique(d.2l$ResponseID))

## Coding order information
d.2l$responseN <- "Second answer"
d.2l$responseN[d.2l$order=="Agent first" & d.2l$judgment=="Agent_cause_8"] <-  "First answer"
d.2l$responseN[d.2l$order=="Object first" & d.2l$judgment=="Object_cause_8"] <-  "First answer"

## Figure 2 (plotting the results)

d.2l$judgment <- factor(d.2l$judgment)
d.2l$judgment <- factor(c("Agent","Artifact")[d.2l$judgment])

d2.sum <- d.2l %>% group_by(condition,judgment) %>% 
            summarise(
                N    = length(cause),
                mean = mean(cause, na.rm=TRUE),
                sd   = sd(cause,na.rm=TRUE),
                se   = sd / sqrt(N) )

d2.sum$condition <- factor(d2.sum$condition)
d2.sum$condition <- factor(c("Agent Counterfactual","Artifact Counterfactual","No Counterfactual")[d2.sum$condition])
d2.sum$condition <- factor(d2.sum$condition,levels=c("Agent Counterfactual","No Counterfactual","Artifact Counterfactual"))

d2.plot <- ggplot(d2.sum, aes(x=judgment, y=mean, fill=condition)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=jkomPalette) + 
  #facet_wrap(~responseN) +
  ylab("Agreement with Causal Statement") +
  xlab("") +
  scale_y_continuous(limits=c(0,50)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.8,.9)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.75))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )

d2.plot

#ggsave(d2.plot,file="figures/fig2.jpg",dpi=800)


## Overall analyses
lm4.0 <- lmer(cause~condition * judgment + (1|ResponseID), data=d.2l)
## Interaction effect
lm4.1 <- lmer(cause~condition + judgment + (1|ResponseID), data=d.2l)
anova(lm4.0,lm4.1)
## main effect of question
lm4.2 <- lmer(cause~condition + (1|ResponseID), data=d.2l)
anova(lm4.1,lm4.2)
## main effect of condition
lm4.3 <- lmer(cause~judgment + (1|ResponseID), data=d.2l)
anova(lm4.1,lm4.3)

## Pairwise comparisons
aggregate(cause~condition*judgment, FUN=function(x) c(M = mean(x), SD = sd(x)),data=d.2l)
#AGENT Judgments: Agent Counterfactual vs Artifact Counterfactual
var.test(d.2l$cause[d.2l$condition=="Agent Counterfactual" & d.2l$judgment=="Agent"],
         d.2l$cause[d.2l$condition=="Artifact Counterfactual" & d.2l$judgment=="Agent"])
t.test(d.2l$cause[d.2l$condition=="Agent Counterfactual" & d.2l$judgment=="Agent"],
       d.2l$cause[d.2l$condition=="Artifact Counterfactual" & d.2l$judgment=="Agent"],var.equal =T)
cohensD(d.2l$cause[d.2l$condition=="Agent Counterfactual" & d.2l$judgment=="Agent"],
        d.2l$cause[d.2l$condition=="Artifact Counterfactual" & d.2l$judgment=="Agent"])

#AGENT Judgments: agent Counterfactual vs. no Counterfactual
var.test(d.2l$cause[d.2l$condition=="Agent Counterfactual" & d.2l$judgment=="Agent"],
         d.2l$cause[d.2l$condition=="No Counterfactual" & d.2l$judgment=="Agent"])
t.test(d.2l$cause[d.2l$condition=="Agent Counterfactual" & d.2l$judgment=="Agent"],
       d.2l$cause[d.2l$condition=="No Counterfactual" & d.2l$judgment=="Agent"])
cohensD(d.2l$cause[d.2l$condition=="Agent Counterfactual" & d.2l$judgment=="Agent"],
        d.2l$cause[d.2l$condition=="No Counterfactual" & d.2l$judgment=="Agent"])

#ARTIFACT Judgments: Artifact Counterfactual vs. Agent Counterfactual
var.test(d.2l$cause[d.2l$condition=="Artifact Counterfactual" & d.2l$judgment=="Artifact"],
         d.2l$cause[d.2l$condition=="Agent Counterfactual" & d.2l$judgment=="Artifact"])
t.test(d.2l$cause[d.2l$condition=="Artifact Counterfactual" & d.2l$judgment=="Artifact"],
       d.2l$cause[d.2l$condition=="Agent Counterfactual" & d.2l$judgment=="Artifact"])
cohensD(d.2l$cause[d.2l$condition=="Artifact Counterfactual" & d.2l$judgment=="Artifact"],
        d.2l$cause[d.2l$condition=="Agent Counterfactual" & d.2l$judgment=="Artifact"])

#ARTIFACT Judgments: Artifact Counterfactual vs.No Counterfactual
var.test(d.2l$cause[d.2l$condition=="Artifact Counterfactual" & d.2l$judgment=="Artifact"],
         d.2l$cause[d.2l$condition=="No Counterfactual" & d.2l$judgment=="Artifact"])
t.test(d.2l$cause[d.2l$condition=="Artifact Counterfactual" & d.2l$judgment=="Artifact"],
       d.2l$cause[d.2l$condition=="No Counterfactual" & d.2l$judgment=="Artifact"])
cohensD(d.2l$cause[d.2l$condition=="Artifact Counterfactual" & d.2l$judgment=="Artifact"],
        d.2l$cause[d.2l$condition=="No Counterfactual" & d.2l$judgment=="Artifact"])

#### Experiment 3 #####

d.3 <- read.csv("data/study3.csv")

##demographics
d3.age <- matrix(c("mean",mean(d.3$Age,na.rm=T),"sd",sd(d.3$Age,na.rm=T),"n",length(d.3$Age)),ncol = 3)
print(d3.age)

d.3$Sex <- factor(c("Male","Female")[d.3$Sex])
d3.gender <- table(d.3$Sex, exclude=NULL)
print(d3.gender)

d.3$condition <- factor(c("Malfunction","Immoral","Normal")[d.3$DO.BR.FL_15])
d.3$question <- factor(c("Agent","Object")[d.3$DO.BR.FL_22])
d.3$time <- rowSums(d.3[,c(17,22,27)],na.rm = T)
d.3$order <- "Cause First"
d.3$order[d.3$DO.BL.AgentQuestions=="Agent_rel|Agent_cause"] <- "Relevance First"
d.3$order[d.3$DO.BL.ObjectQuestions=="Object_Rel|Object_cause"] <- "Relevance First"

d.3 <- d.3[,-c(2:28,51:71)]

d.3$Control1_1 <- 0
d.3$Control1_1[d.3$condition=="Malfunction" & is.na(d.3$Control1_1_1) & d.3$Control1_1_2==1] <- 1
d.3$Control1_1[d.3$condition!="Malfunction" & d.3$Control1_1_1==1 & is.na(d.3$Control1_1_2)] <- 1
d.3$Control1_2 <- 0
d.3$Control1_2[d.3$Control1_2_1==1 & is.na(d.3$Control1_2_2)] <- 1

d.3$Control1 <- 0
d.3$Control1[d.3$Control1_1==1 & d.3$Control1_2==1] <- 1

d.3$Control2_1 <- 0
d.3$Control2_1[d.3$condition=="Malfunction" & d.3$Control2_1_1==1 & d.3$Control2_1_2==1] <- 1
d.3$Control2_1[d.3$condition!="Malfunction" & d.3$Control2_1_1==1 & is.na(d.3$Control2_1_2)] <- 1
d.3$Control2_2 <- 0
d.3$Control2_2[d.3$Control2_2_1==1 & is.na(d.3$Control2_2_2)] <- 1

d.3$Control2 <- 0
d.3$Control2[d.3$Control2_1==1 & d.3$Control2_2==1] <- 1

d.3$Control3_1 <- 0
d.3$Control3_1[d.3$condition=="Immoral" & is.na(d.3$Control3_1_1)] <- 1
d.3$Control3_1[d.3$condition!="Immoral" & d.3$Control3_1_1==1] <- 1
d.3$Control3_2 <- 0
d.3$Control3_2[d.3$Control3_2_1==1] <- 1

d.3$Control3 <- 0
d.3$Control3[d.3$Control3_1==1 & d.3$Control3_2==1] <- 1

d.3$control <- 0
d.3$control[d.3$Control1==1 & d.3$Control2==1 & d.3$Control3==1] <- 1

# Manipulation checks
##Moral check
d.3$morality <- d.3$morality-3 ## this was recoded incorrectly on Qualtrics and started at 4
aggregate(morality~condition, FUN=function(x) c(M = mean(x), SD = sd(x)), data=d.3)
## Immoral vs normal
var.test(d.3$morality[d.3$condition=="Immoral"],d.3$morality[d.3$condition=="Normal"])
t.test(d.3$morality[d.3$condition=="Immoral"],d.3$morality[d.3$condition=="Normal"])
cohensD(d.3$morality[d.3$condition=="Immoral"],d.3$morality[d.3$condition=="Normal"])
## Immoral vs. malfunction
var.test(d.3$morality[d.3$condition=="Immoral"],d.3$morality[d.3$condition=="Malfunction"])
t.test(d.3$morality[d.3$condition=="Immoral"],d.3$morality[d.3$condition=="Malfunction"],var.equal=T)
cohensD(d.3$morality[d.3$condition=="Immoral"],d.3$morality[d.3$condition=="Malfunction"])
## Normal vs. Malfunction
var.test(d.3$morality[d.3$condition=="Normal"],d.3$morality[d.3$condition=="Malfunction"])
t.test(d.3$morality[d.3$condition=="Normal"],d.3$morality[d.3$condition=="Malfunction"],var.equal=T)
cohensD(d.3$morality[d.3$condition=="Normal"],d.3$morality[d.3$condition=="Malfunction"])

##Malfunciton check
aggregate(probability_4~condition, FUN=function(x) c(M = mean(x), SD = sd(x)), data=d.3)
## Malfunction
t.test(d.3$probability_4[d.3$condition=="Malfunction"],mu=50, alternative="greater")
## Normal
t.test(d.3$probability_4[d.3$condition=="Normal"],mu=50, alternative="greater")
## Immoral
t.test(d.3$probability_4[d.3$condition=="Immoral"],mu=50, alternative="greater")

## Causal and Relevance Preference Scores
d.3$relevanceA <- rowSums(d.3[,c(2,6)],na.rm = T)
d.3$relevanceB <- rowSums(d.3[,c(3,7)],na.rm = T)
d.3$causeA <- rowSums(d.3[,c(4,8)],na.rm = T)
d.3$causeB <- rowSums(d.3[,c(5,9)],na.rm = T)

d.3$causeStr <- NA
d.3$causeStr[d.3$causeA==0 & d.3$causeB==0] <- 0 #Neither
d.3$causeStr[d.3$causeA==1 & d.3$causeB==1] <- 0 # Both
d.3$causeStr[d.3$causeA==1 & d.3$causeB==0] <- -1 # Admin only
d.3$causeStr[d.3$causeA==0 & d.3$causeB==1] <- 1 # Prof only
table(d.3$causeStr,exclude = NULL)

d.3$relevanceStr <- NA
d.3$relevanceStr[(d.3$relevanceA==2 | d.3$relevanceA==0) & (d.3$relevanceB==2 | d.3$relevanceB==0)] <- 0 #"Neither"
d.3$relevanceStr[d.3$relevanceA==1 & d.3$relevanceB==1] <- 0 #"Both"
d.3$relevanceStr[d.3$relevanceA==1 & (d.3$relevanceB==2 | d.3$relevanceB==0)] <- -1 #"Admin Only"
d.3$relevanceStr[(d.3$relevanceA==2 | d.3$relevanceA==0) & d.3$relevanceB==1] <- 1 #"Prof Only"
table(d.3$relevanceStr, exclude=NULL)

d.3 <- d.3[d.3$control==1,-c(2:21,29:37)]
length(d.3$ResponseID)


d.3l <- gather(d.3[,c(1,4:5,13:14)],judgment,strength,-c(1:3))  

library(plyr)
d3.sum <- ddply(d.3l, c("condition","question","judgment"), summarise,
                N    = length(strength),
                mean = mean(strength, na.rm=TRUE),
                sd   = sd(strength,na.rm=TRUE),
                se   = sd / sqrt(N) )
detach(package:plyr)
d3.sum$judgment <- factor(d3.sum$judgment)
d3.sum$judgment <- factor(c("Cause","Relevance")[d3.sum$judgment])

d3.sum$condition <- factor(c("Agent Violation","Artifact Violation","No Violation")[d3.sum$condition])
d3.sum$condition <- factor(d3.sum$condition, levels=c("Agent Violation","No Violation","Artifact Violation"))

d3.sum$question <- factor(c("Intentional Agent", "Functional Artifact")[d3.sum$question])
d3.sum$question <- factor(d3.sum$question, levels=c("Intentional Agent", "Functional Artifact"))

d3.plot1 <- ggplot(d3.sum[d3.sum$judgment=="Cause",], aes(x=condition, y=mean, fill=condition)) +
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

d3.plot1


# ggsave(d3.plot1, file="figures/fig2a.jpg",dpi=600)

d3.plot2 <- ggplot(d3.sum[d3.sum$judgment=="Relevance",], aes(x=condition, y=mean, fill=condition)) +
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

d3.plot2

# ggsave(d3.plot2, file="fig2b.jpg",dpi=600)

### Corrleation plot

d3.sum2 <- cbind(d3.sum[d3.sum$judgment=="Cause",-3],d3.sum[d3.sum$judgment=="Relevance",4:7])

colnames(d3.sum2) <- c("condition","question","cause.n","cause.mean","cause.sd","cause.se","rel.n","rel.mean","rel.sd","rel.se")

d3.subjs <- d.3

colnames(d3.subjs) <- c(names(d.3)[1:12],"cause.mean","rel.mean")

d3.subjs$condition <- factor(c("Agent Violation","Artifact Violation","No Violation")[d3.subjs$condition])
d3.subjs$condition <- factor(d3.subjs$condition,levels=c("Agent Violation","No Violation", "Artifact Violation"))

d3.subjs$question <- factor(c("Intentional Agent","Functional Artifact")[d3.subjs$question])
d3.subjs$question <- factor(d3.subjs$question,levels=c("Functional Artifact","Intentional Agent"))

fig3c <- ggplot(d3.sum2, aes(x=rel.mean, y=cause.mean)) +
  geom_point(data=d3.subjs, (aes(x=rel.mean,y=cause.mean,color=condition,shape=question)),alpha=.35,stat="identity",position = "jitter") +
  stat_smooth(data=d3.subjs,method=lm, formula= y ~ x, linetype=1, alpha=.35) +
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
fig3c

cor.test(d3.sum2$cause.mean,d3.sum2$rel.mean)
cor.test(d.3$causeStr,d.3$relevanceStr)

#proportional odds logistic regression
## Causal judgments
d.3$causeStr <- factor(d.3$causeStr)

lm2.1 <- polr(causeStr ~condition+question,data=d.3)
dropterm(lm2.1, test="Chisq")
# interaction effect
lm2.0 <- polr(causeStr ~condition*question,data=d.3)
dropterm(lm2.0, test = "Chisq")

# Moral Norm Condition
aggregate(causeStr ~ question, FUN=table, data=d.3[d.3$condition=="Immoral",])
lm2.1i <- polr(causeStr ~ question, data=d.3[d.3$condition=="Immoral",])
dropterm(lm2.1i, test="Chisq")
# Functional Norm Condition 
aggregate(causeStr ~ question, FUN=table, data=d.3[d.3$condition=="Malfunction",])
lm2.1m <- polr(causeStr ~ question, data=d.3[d.3$condition=="Malfunction",])
dropterm(lm2.1m, test="Chisq")
# No Norm Condition
aggregate(causeStr ~ question, FUN=table, data=d.3[d.3$condition=="Normal",])
lm2.1n <- polr(causeStr ~ question, data=d.3[d.3$condition=="Normal",])
dropterm(lm2.1n, test="Chisq")

#Comparisons against no-violation condition.
aggregate(causeStr ~ condition, FUN = table, data=d.3[d.3$condition!="Immoral" & d.3$question=="Object",])
lm2.1o <- polr(causeStr ~ condition, data=d.3[d.3$condition!="Immoral" & d.3$question=="Object",])
dropterm(lm2.1o, test="Chisq") #Object preference goes up rel to normal for malfunction.

aggregate(causeStr ~ condition, FUN = table, data=d.3[d.3$condition!="Malfunction" & d.3$question=="Object",])
lm2.1p <- polr(causeStr ~ condition, data=d.3[d.3$condition!="Malfunction" & d.3$question=="Object",])
dropterm(lm2.1p, test="Chisq") #Feed-forward effect, significant.

aggregate(causeStr ~ condition, FUN = table, data=d.3[d.3$condition!="Malfunction" & d.3$question=="Agent",])
lm2.1q <- polr(causeStr ~ condition, data=d.3[d.3$condition!="Malfunction" & d.3$question=="Agent",])
dropterm(lm2.1q, test="Chisq") #Agent preference goes up rel to normal for immoral

aggregate(causeStr ~ condition, FUN = table, data=d.3[d.3$condition!="Immoral" & d.3$question=="Agent",])
lm2.1r <- polr(causeStr ~ condition, data=d.3[d.3$condition!="Immoral" & d.3$question=="Agent",])
dropterm(lm2.1r, test="Chisq") #Backtracking, significant

## Relevance judgments
d.3$relevanceStr <- factor(d.3$relevanceStr)
#main effects
lm2.3 <- polr(relevanceStr ~condition+question,data=d.3)
dropterm(lm2.3, test="Chisq")
# interaction effect
lm2.2 <- polr(relevanceStr ~condition*question,data=d.3)
dropterm(lm2.2, test = "Chisq")

# Moral Norm Condition
aggregate(relevanceStr ~ question, FUN=table, data=d.3[d.3$condition=="Immoral",])
lm2.3i <- polr(relevanceStr ~ question, data=d.3[d.3$condition=="Immoral",])
dropterm(lm2.3i, test="Chisq")
# Functional Norm Condition 
aggregate(relevanceStr ~ question, FUN=table, data=d.3[d.3$condition=="Malfunction",])
lm2.3m <- polr(relevanceStr ~ question, data=d.3[d.3$condition=="Malfunction",])
dropterm(lm2.3m, test="Chisq")
# No Norm Condition
aggregate(relevanceStr ~ question, FUN=table, data=d.3[d.3$condition=="Normal",])
lm2.3n <- polr(relevanceStr ~ question, data=d.3[d.3$condition=="Normal",])
dropterm(lm2.3n, test="Chisq")

#comparisons against no-violation condition
aggregate(relevanceStr ~ condition, FUN = table, data=d.3[d.3$condition!="Immoral" & d.3$question=="Object",])
lm2.3o <- polr(relevanceStr ~ condition, data=d.3[d.3$condition!="Immoral" & d.3$question=="Object",])
dropterm(lm2.3o, test="Chisq") #Object preference goes up rel to normal for malfunction.

aggregate(relevanceStr ~ condition, FUN = table, data=d.3[d.3$condition!="Malfunction" & d.3$question=="Object",])
lm2.3p <- polr(relevanceStr ~ condition, data=d.3[d.3$condition!="Malfunction" & d.3$question=="Object",])
dropterm(lm2.3p, test="Chisq") #Feed-forward effect, significant.

aggregate(relevanceStr ~ condition, FUN = table, data=d.3[d.3$condition!="Malfunction" & d.3$question=="Agent",])
lm2.3q <- polr(relevanceStr ~ condition, data=d.3[d.3$condition!="Malfunction" & d.3$question=="Agent",])
dropterm(lm2.3q, test="Chisq") #Agent preference goes up rel to normal for immoral

aggregate(relevanceStr ~ condition, FUN = table, data=d.3[d.3$condition!="Immoral" & d.3$question=="Agent",])
lm2.3r <- polr(relevanceStr ~ condition, data=d.3[d.3$condition!="Immoral" & d.3$question=="Agent",])
dropterm(lm2.3r, test="Chisq") #Backtracking, significant



### Experiment 4 ####

d.4 <- read.csv("data/study4.csv",stringsAsFactors = F)

d.4 <- d.4[-c(1:2),]

table(d.4$Sex,exclude=NULL)
mean(as.numeric(d.4$Age),na.rm=T)
sd(d.4$Age,na.rm=T)
length(unique(d.4$ResponseId)) ## total number of participants

d.4$Sviolation <- substr(d.4$Malfunctionvignettes_DO,1,6)
d.4$Dviolation <- substr(d.4$Moralvignettes_DO,1,6)
d.4$violation <- paste(d.4$Dviolation,d.4$Sviolation)
d.4$violation <- factor(d.4$violation)
d.4$violation <- factor(rep(c("No violation","Distal violation","Proximal violation"),2)[d.4$violation])

d.4$condition <- factor(d.4$Condition)
d.4$condition <- factor(c("Functional Artifacts","Intentional Agents")[d.4$condition])

d.4$control1 <- 0
d.4$control1[d.4$violation=="No violation" & d.4$Mal_control1_1=="Close" & d.4$Mal_control1_2=="Open"] <-1
d.4$control1[d.4$violation!="No violation" & d.4$Mal_control1_1=="Open" & d.4$Mal_control1_2=="Open"] <-1
d.4$control1[d.4$violation=="No violation" & d.4$Moral_control1_1=="Close" & d.4$Moral_control1_2=="Open"] <-1
d.4$control1[d.4$violation!="No violation" & d.4$Moral_control1_1=="Open" & d.4$Moral_control1_2=="Open"] <-1

d.4$control2 <- 0
d.4$control2[d.4$Mal_control2=="River valve"] <- 1
d.4$control2[d.4$Moral_control2=="River valve"] <- 1

d.4$Malfunction_time_Page.Submit <- as.numeric(d.4$Malfunction_time_Page.Submit)
d.4$Moral_timing_Page.Submit <- as.numeric(d.4$Moral_timing_Page.Submit)

d.4$time <- rowSums(d.4[,c(21,32)],na.rm=T)
#hist(d.4$time[d.4$time<200],breaks=75) # looks like < 25 would be a reasonable cutoff if you want to exclude people who went too quickly.

d.4$questionOrder <- "Causation First"
d.4$questionOrder[as.numeric(factor(d.4$FL_38_DO))==3 | as.numeric(factor(d.4$FL_40_DO))==3] <- "Relevance First"

## Order within the causal and relevance question blocks
d.4$causationOrder <- "Distal First"
d.4$causationOrder[as.numeric(factor(d.4$Artifactcausalquestions_DO))==3|as.numeric(factor(d.4$Moralcausalquestions_DO))==3] <- "Proximal First"

d.4$relevanceOrder <- "Distal First"
d.4$relevanceOrder[as.numeric(factor(d.4$Malfunctionrelevancequestions_DO))==3|as.numeric(factor(d.4$Moralrelevancequestions_DO))==3] <- "Proximal First"

d.4l <- gather(d.4[,c(9,23:26,34:37,41:42,59:66)],judgment,response,-c(1,10:19),na.rm=T)

d.4l$event <- "Proximal Event"
d.4l$event[d.4l$judgment=="Higher_cause_Mal_8" | d.4l$judgment=="Higher_cause_Moral_8" |
             d.4l$judgment=="Higher_rel_Mal_1" | d.4l$judgment=="Higher_rel_Moral_1"] <- "Distal Event"

d.4l$question <- "Relevance"
d.4l$question[d.4l$judgment=="Higher_cause_Mal_8"|d.4l$judgment=="Higher_cause_Moral_8"|
                d.4l$judgment=="Lower_cause_Mal_8"|d.4l$judgment=="Lower_cause_Moral_8"] <- "Causation"

d.4l$response <- as.numeric(d.4l$response)

d.4l$firstResponse <- 0
d.4l$firstResponse[d.4l$questionOrder=="Causation First" & d.4l$causationOrder=="Proximal First" &
                     d.4l$question=="Causation" & d.4l$event=="Distal Event"] <- 1
d.4l$firstResponse[d.4l$questionOrder=="Causation First" & d.4l$causationOrder=="Distal First" &
                     d.4l$question=="Causation" & d.4l$event=="Distal Event"] <- 1
d.4l$firstResponse[d.4l$questionOrder=="Relevance First" & d.4l$relevanceOrder=="Proximal First" &
                     d.4l$question=="Relevance" & d.4l$event=="Proximal Event"] <- 1
d.4l$firstResponse[d.4l$questionOrder=="Relevance First" & d.4l$relevanceOrder=="Distal First" &
                     d.4l$question=="Relevance" & d.4l$event=="Proximal Event"] <- 1

length(unique(d.4l$ResponseId[d.4l$control1==1 & d.4l$control2==1])) ## Number of people who passed all controls


d4.sum <- d.4l %>% filter(control1==1 & control2==1) %>% ## Filter out those who failed control questions
  group_by(violation,condition,event,question) %>%
  summarise(N = length(response),
            mean = mean(response, na.rm=TRUE), ## it may be that some NAs here are meant to be 0... 
            sd   = sd(response,na.rm=TRUE),
            se   = sd / sqrt(N) )

d4.sum$event <- factor(d4.sum$event)
d4.sum$event <- factor(c("Distal Event","Proximal Event")[d4.sum$event])

d4.sum$condition <- factor(d4.sum$condition)
d4.sum$condition <- factor(c("Functional Artifacts","Intentional Agents")[d4.sum$condition])
d4.sum$condition <- factor(d4.sum$condition,levels=c("Intentional Agents","Functional Artifacts"))


d4.plotC <- d4.sum %>% filter(question=="Causation") %>%
  ggplot(aes(x=event, y=mean, fill=violation)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=jkomPalette) + 
  facet_wrap(~condition) +
  ylab("Agreement with Causal Statement") +
  xlab("") +
  #scale_y_continuous(limits=c(-0.3,1.1),breaks = c(-0.25,0.00,0.25,0.50,1.00), labels = c(-0.25,0.00,0.25,0.50,1.00)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position=c(.89,.93)
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )
d4.plotC

# Here is the same plot of relevance judgments (not included in the paper)
# 
# d4.plotR <- d4.sum %>% filter(question=="Relevance") %>%
#   ggplot(aes(x=event, y=mean, fill=violation)) +
#   geom_bar(stat="identity", position="dodge") +
#   scale_fill_manual(values=jkomPalette) +
#   facet_wrap(~condition) +
#   ylab("Agreement with Relevance Statement") +
#   xlab("") +
#   #scale_y_continuous(limits=c(-0.3,1.1),breaks = c(-0.25,0.00,0.25,0.50,1.00), labels = c(-0.25,0.00,0.25,0.50,1.00)) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
#   theme_bw() +
#   theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,legend.position=c(.89,.93)
#     ,legend.title=element_blank()
#     ,legend.text=element_text(size=rel(1.5))
#     ,axis.text.x=element_text(size=rel(1.5))
#     ,axis.text.y=element_text(size=rel(1.25))
#     ,axis.title=element_text(size=rel(1.5))
#     ,strip.text = element_text(size = rel(1.7))
#     ,axis.title.y = element_text(vjust = 0.75)
#   )
# d4.plotR


d4.R <- d.4l %>% filter(question=="Relevance") %>% dplyr::select(ResponseId,violation,condition,control1,control2,response,event,question)

d4.C <- d.4l %>% filter(question=="Causation") %>% dplyr::select(ResponseId,violation,condition,control1,control2,response,event,question)

d4.subjs <- full_join(d4.R,d4.C, by=c("ResponseId","violation","condition","control1","control2","event")) %>% 
  filter(!is.na(response.y) & !is.na(response.x)) %>% ## ugly but functional line...
  filter(control1==1 & control2==1) %>%
  rename(rel.mean = response.x,
         cause.mean = response.y,
         norm = condition)

d4.sum2 <- cbind(d4.sum[d4.sum$question=="Causation",-3], d4.sum[d4.sum$question=="Relevance",5:8])

colnames(d4.sum2) <- c("violation","condition","event","cause.n","cause.mean","cause.sd","cause.se","rel.n","rel.mean","rel.sd","rel.se")

d4.sum2$event <- factor(d4.sum2$event)
d4.sum2$event <- factor(c("D","P")[d4.sum2$event])

d4.sum2 <- bind_cols(d4.sum[d4.sum$question=="Causation",-4],d4.sum[d4.sum$question=="Relevance",5:8])

colnames(d4.sum2) <- c("violation","norm","event","cause.n","cause.mean","cause.sd","cause.se","rel.n","rel.mean","rel.sd","rel.se")

d4.sum2$event <- factor(d4.sum2$event)
d4.sum2$event <- factor(c("D","P")[d4.sum2$event])


fig4c <- ggplot(d4.sum2, aes(x=rel.mean, y=cause.mean)) +
  geom_point(data=d4.subjs, (aes(x=rel.mean,y=cause.mean,color=violation,shape=norm)),alpha=.35,stat="identity",position = "jitter") +
  stat_smooth(data=d4.subjs,method=lm, formula= y ~ x, linetype=1, alpha=.35) +
  stat_smooth(method=lm, formula= y ~ x, linetype=2, alpha=.5) +
  geom_errorbar(aes(ymin=cause.mean-cause.se, ymax=cause.mean+cause.se), linetype=3) +
  geom_errorbarh(aes(xmin=rel.mean-rel.se, xmax=rel.mean+rel.se), linetype=3) +
  #coord_cartesian(ylim=c(.15,1)) +
  geom_point(aes(color=violation,shape=norm),stat = "identity", position="jitter",size=rel((8))) +
  geom_text(aes(label=event),stat = "identity",size=rel((4))) +
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
fig4c


## Main analyses:
d.4l <- d.4l %>% filter(control1==1 & control2==1) %>% filter(!is.na(response))

## four way interaction?
lm4.0 <- lmer(response~ event * violation * condition * question + (event+question|ResponseId), data=d.4l)
lm4.1 <- lmer(response~ (event * violation * condition) + (event * violation * question) + 
                (event * condition * question) + (violation * condition * question) + (event+question|ResponseId), data=d.4l)
anova(lm4.0,lm4.1)
## Nope.


##Cause ratings

lm4.0c <- lmer(response~ event * violation * condition + (1|ResponseId), data=d.4l[d.4l$question=="Causation",])
## Interaction effect
lm4.1c <- lmer(response~ (event * violation) + (event * condition) + (violation * condition) + (1|ResponseId), data=d.4l[d.4l$question=="Causation",])
anova(lm4.0c,lm4.1c) #sig three-way interaction mainly bc bigger effects for agents

##Causal ratings, Artifacts

lm4.0ac <- lmer(response~ event * violation + (1|ResponseId), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Functional Artifacts",])

## Interaction effect
lm4.1ac <- lmer(response~ event + violation + (1|ResponseId), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Functional Artifacts",])
anova(lm4.0ac,lm4.1ac)

##Main effect of violation
lm4.2ac <- lmer(response ~ event + (1|ResponseId), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Functional Artifacts",])
anova(lm4.2ac,lm4.1ac) #yup

##Main effect of event
lm4.3ac <- lmer(response ~ violation + (1|ResponseId), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Functional Artifacts",])
anova(lm4.3ac,lm4.1ac) #small but sig.

## Proximal event
aggregate(response ~ violation * event, FUN= function(x) c( m = mean(x), sd = sd(x)), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Functional Artifacts",])
### prox violation vs. no violation
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"])
## prox violation vs. distal violation
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"])
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"])
## pxoximal violation vs. no violation
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"])


## Distal event
### Distal violation vs.no violation
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"])
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"])
### distal vs. proximal violation
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"])
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"])
### proximal vs. no violation
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"])


##Causal ratings, Agents

lm4.0gc <- lmer(response~ event * violation + (1|ResponseId), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Intentional Agents",])
## Interaction effect
lm4.1gc <- lmer(response~ event + violation + (1|ResponseId), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Intentional Agents",])
anova(lm4.0gc,lm4.1gc)

#Main effect of violation
lm4.2gc <- lmer(response ~ event + (1|ResponseId), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Intentional Agents",])
anova(lm4.2gc,lm4.1gc) #yup

#Main effect of event
lm4.3gc <- lmer(response ~ violation + (1|ResponseId), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Intentional Agents",])
anova(lm4.3gc,lm4.1gc) #nope

## Proximal event
aggregate(response ~ violation * event, FUN= function(x) c( m = mean(x), sd = sd(x)), data=d.4l[d.4l$question=="Causation" & d.4l$condition=="Intentional Agents",])
### proximal vs.no violation
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"])
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"])
## proximal vs. distal violation
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Proximal Event"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"])
## Distal vs. no violation
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Proximal Event"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Proximal Event"])


## Distal event
### Disal vs. no violation
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"])
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"])
### distal vs. proximal event
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Distal violation" & d.4l$event=="Distal Event"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"])
### no violation vs.proximal violation
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"])
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="No violation" & d.4l$event=="Distal Event"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Causation" & d.4l$violation=="Proximal violation" & d.4l$event=="Distal Event"])


## Correlations at the condition and participant level
# condition level
cor.test(d4.sum2$cause.mean,d4.sum2$rel.mean)
# participant level
cor.test(d4.subjs$cause.mean,d4.subjs$rel.mean)

## JSP: I stopped here but should go back through this to clean it up before submitting for publication:

# The following analyses are not included in the main text, but here for those who are interested:
## Relevance resposnes 
lm4.0r <- lmer(response~ event * violation * condition + (1|ResponseId), data=d.4l[d.4l$question=="Relevance",])
## Interaction effect
lm4.1r <- lmer(response~ (event * violation) + (event * condition) + (violation * condition) + (1|ResponseId), data=d.4l[d.4l$question=="Relevance",])
anova(lm4.0r,lm4.1r)

## Functional condition violation
lm4.2 <- lmer(response~ event * violation + (1|ResponseId), data=d.4l[d.4l$question=="Relevance" & d.4l$condition=="Functional Artifacts",])
## Interaction effect
lm4.3 <- lmer(response~ event + violation + (1|ResponseId), data=d.4l[d.4l$question=="Relevance" & d.4l$condition=="Functional Artifacts",])
anova(lm4.2,lm4.3)

## Lower event
aggregate(response ~ violation * event, FUN= function(x) c( m = mean(x), sd = sd(x)), data=d.4l[d.4l$question=="Relevance" & d.4l$condition=="Functional Artifacts",])
### low vs.no
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
### low vs. high
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
## high vs. no
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"])
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"])

## Higher event
### no vs.high
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
### low vs.high
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
### no vs.low
var.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
         d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"])
t.test(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
       d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
        d.4l$response[d.4l$condition=="Functional Artifacts" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"])


## Moral condition violation
lm4.4 <- lmer(response~ event * violation + (1|ResponseId), data=d.4l[d.4l$question=="Relevance" & d.4l$condition=="Intentional Agents",])
## Interaction effect
lm4.5 <- lmer(response~ event + violation + (1|ResponseId), data=d.4l[d.4l$question=="Relevance" & d.4l$condition=="Intentional Agents",])
anova(lm4.4,lm4.5)

## Lower event
aggregate(response ~ violation * event, FUN= function(x) c( m = mean(x), sd = sd(x)), data=d.4l[d.4l$question=="Relevance" & d.4l$condition=="Intentional Agents",])
### low vs.no
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"], var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
### low vs. high
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"], var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Proximal Cause"])
## high vs. no
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"], var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Proximal Cause"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Proximal Cause"])

## Higher event
### no vs.high
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
### low vs.high
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="High violation" & d.4l$event=="Distal Cause"])
### no vs.low
var.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
         d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"])
t.test(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
       d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"],var.equal = T)
cohensD(d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="No violation" & d.4l$event=="Distal Cause"],
        d.4l$response[d.4l$condition=="Intentional Agents" & d.4l$question=="Relevance" & d.4l$violation=="Low violation" & d.4l$event=="Distal Cause"])




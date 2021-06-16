library(ggplot2)
library(tidyr)
library(dplyr)
library(naniar)
library(tidyverse)
Deer <- read.csv("~/Downloads/Deer2021.csv")
summary(Deer$Age.category)
summary(Deer$Sex)
summary(Deer$D.Category)
Deer$Month <-factor(Deer$Month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
Deer$Age.category <-na_if(Deer$Age.category, "Unknown")
Deer$Age <-na_if(Deer$Age, 0)
Deer$Age <-na_if(Deer$Age, 1)
Deer$Sex <-na_if(Deer$Sex, "U")
Deer.sub<-subset(Deer, Deer$Sex %in% c("F", "M") & Deer$Age.category%in% c("Adult", "Juvenile", "Neonate"))
Deer.sub1<-subset(Deer, Deer$Sex %in% c("F", "M"))
Deer.sub2<-subset(Deer, Deer$Age.category%in% c("Adult", "Juvenile", "Neonate"))

F1<- ggplot(data=Deer) +
  geom_bar(mapping = aes(x=Deer$Month), na.rm= TRUE) +xlab("Month")+ ylab("Frequency")+ theme(axis.text.x = element_text(angle = 90))
Deer$Month1<-as.numeric(Deer$Month) 
F1 +  geom_vline(data=Deer$Month1, xintercept = 3.5, color = "blue", linetype = "dashed") +
   geom_vline(data=Deer$Month1, xintercept = 6.5, color = "blue", linetype = "dashed") +
   geom_vline(data=Deer$Month1, xintercept = 9.5, color = "blue", linetype = "dashed")
 
#F2<-ggplot(data=Deer) +
  #geom_bar(mapping = aes(x=Deer$Month), na.rm= TRUE) + facet_wrap( ~ Deer$Sex) + scale_color_hue(h = c(180, 300))
#F2<-ggplot(data=Deer.sub)+geom_bar(mapping=aes(x=Deer.sub$Month, fill=Deer.sub$Sex), position="stack", stat="count") +xlab("Month")+ ylab("Frequency")+ theme(axis.text.x = element_text(angle = 90)) +labs(fill="Sex")#+ggtitle("Frequency of mortality by sex")
#F2

F2a<-ggplot(data=Deer.sub1)+geom_bar(mapping=aes(x=Deer.sub1$Month, fill=Deer.sub1$Sex), position="stack", stat="count") +xlab("Month")+ ylab("Frequency")+theme_gray()+theme(axis.text.x = element_text(angle = 90)) +labs(fill="Sex")+scale_fill_grey()#+ggtitle("Frequency of mortality by sex")
F2a
#F3<-ggplot(data=Deer) +
  #geom_bar(mapping = aes(x=Deer$Month), na.rm= TRUE) + facet_wrap( ~ Deer$Age.category) + theme_minimal() +xlab("Month")+ ylab("Frequency")+ theme(axis.text.x = element_text(angle = 90))
#F3<-ggplot(data=Deer.sub, na.rm=TRUE)+geom_bar(mapping=aes(x=Deer.sub$Month, fill=Deer.sub$Age.category), na.rm=TRUE, position="stack", stat="count") +xlab("Month")+ ylab("Frequency")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Age")
#F3
#F3a<-ggplot(data=Deer.sub2, na.rm=TRUE)+geom_bar(mapping=aes(x=Deer.sub2$Month, fill=Deer.sub2$Age.category), na.rm=TRUE, position="stack", stat="count") +xlab("Month")+ ylab("Frequency")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Age") +scale_fill_grey()
#F3a
#F4<-ggplot(data=Deer) +
  #geom_bar(mapping = aes(x=Deer$D.Category), na.rm= TRUE) + facet_wrap( ~ Deer$Age.category) + theme_minimal() +xlab("Month")+ ylab("Frequency")+ theme(axis.text.x = element_text(angle = 90))
F3<-ggplot(data=Deer.sub, na.rm=TRUE)+geom_bar(mapping=aes(x=Deer.sub$D.Category, fill=Deer.sub$Age.category), na.rm=TRUE, position="stack", stat="count") +xlab("Cause of mortality")+ ylab("Frequency")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Age") + scale_fill_grey()
F3

F5<-ggplot(data=Deer.sub, na.rm=TRUE)+geom_bar(mapping=aes(x=Deer.sub$Month, fill=Deer.sub$D.Category), na.rm=TRUE, position="stack", stat="count") +xlab("Month")+ ylab("Frequency")+ theme(axis.text.x = element_text(angle = 90)) + labs(fill="Cause of Mortality")
F5
Deer$Disease<- ifelse(Deer$D.Category=="Bacterial", 1, 
                        ifelse(Deer$D.Category=="Fungal", 2, 
                               ifelse(Deer$D.Category=="Nutritional", 3, 
                                      ifelse(Deer$D.Category=="Parasitic", 4, 
                                             ifelse(Deer$D.Category=="Trauma", 5, 
                                                    ifelse(Deer$D.Category=="Viral", 6,
                                                           ifelse(Deer$D.Category=="Other", 7, 8)))))))
summary(Deer$Disease)
Deer$Sexy<- ifelse(Deer$Sex=="F", 1, 2)

AgeD<-table(Deer$Disease, Deer$Age)
AgeD
chisq.test(AgeD)
#fisher.test(AgeD, simulate.p.value = TRUE)

SexD<-table(Deer$Disease, Deer$Sexy)
SexD
chisq.test(SexD)
Season<-table(Deer$D.Category, Deer$Season)
Season
chisq.test(Season)
#test for differences in weight by sex and age
summary(Deer$Weight)
shapiro.test(Deer$Weight)
#p=0.000123, not normal, have to use non parametric test
kruskal.test(Deer$Weight ~ Deer$Sexy)
pairwise.wilcox.test(Deer$Weight, Deer$Sexy, p.adjust.method = "BH")
kruskal.test(Deer$Weight ~ Deer$Age)
pairwise.wilcox.test(Deer$Weight, Deer$Age, p.adjust.method = "BH")
kruskal.test(Deer$Weight ~ Deer$Season)
pairwise.wilcox.test(Deer$Weight, Deer$Season, p.adjust.method = "BH")

#multinomial logistic regression
require(foreign)
require(nnet)
require(reshape2)
with(Deer, table(D.Category, Season))
test<-multinom(D.Category ~ Season + Age + Sexy, data=Deer)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
z
p<-(1-pnorm(abs(z),0,1))*2
p
exp(coef(test))

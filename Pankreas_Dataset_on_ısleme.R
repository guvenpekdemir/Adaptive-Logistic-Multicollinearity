library(mice)
library(tidyverse)
library(ggplot2)
library(VIM)
library(lattice)
library(pan)
library(ISLR)

set.seed(1000)
df<-read.csv("C:/Users/HP/pankreas0normal.csv", header = TRUE,sep=";")


#Gereksiz Deg?sken C?kar?m?

df$?..sample_id=NULL
df$patient_cohort=NULL
df$sample_origin=NULL

#Veri kalitesi inceleme
sum(is.na(df))

colSums(is.na(df))

which(is.na(df))

md.pattern(df)
data=df[,c("plasma_CA19_9","creatinine","LYVE1","REG1B","TFF1","REG1A")]
md.pattern(data)
nrow(df[complete.cases(df),])


aggr_plot <- aggr(data, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels= names(data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Eksik De?erlerin Oransal G?sterimi",
                         "Eksikli?in veri i?indeki yap?s?"))

#Eksik veri rassall?k testi

# H0: Veriler rassal da??lm??t?r.
# H1: Veriler rassal da??lmam??t?r.

#install.packages("remotes")
#remotes::install_github("njtierney/naniar")

library(naniar)

h?potest=mcar_test(data)
names(h?potest)

h?potest$p.value

# H0 Red. Veriler rassal olarak da??lmam??t?r.


a=which(is.na(df$REG1A))

c=which(is.na(df$plasma_CA19_9))

ortak=intersect(a,c)

kontrol=df[ortak,]



genel<-na.omit(data)
summary(genel)
cor(genel)



##Tahmine dayal? atama

summary(df)


df$TFF1<-as.double(df$TFF1)
df$age<-as.integer(df$age)
df$benign_sample_diagnosis<-as.factor(df$benign_sample_diagnosis)
df$stage<-as.factor(df$stage)
df$sex<-as.factor(df$sex)
df$diagnosis<-as.factor(df$diagnosis)

str(df)

#Eksik VER? doldurmas?

ini <- mice(df, maxit = 0)

pred <- ini$pred
pred

set.seed(1000)
imp1 <- mice(df, method = 'cart', pred = pred, print = FALSE)

summary(complete(imp1))

summary(df)
 
plot(imp1, c("plasma_CA19_9", "REG1A"))
densityplot(imp1)     
densityplot(df$TFF1)

yendolduruldu<-complete(imp1)

hist(yendolduruldu$plasma_CA19_9)
hist(yendolduruldu$creatinine)
hist(yendolduruldu$age)
hist(yendolduruldu$creatinine)
hist(yendolduruldu$REG1A)
str(yendolduruldu)

datanum<-yendolduruldu[,c("plasma_CA19_9","age","creatinine","LYVE1","REG1B","TFF1","REG1A")]

cor(datanum,method = "spearman")

#Log d?n???m?

yendolduruldu$plasma_CA19_9<-yendolduruldu$plasma_CA19_9 + 1
yendolduruldu$REG1A<-yendolduruldu$REG1A + 1

yendolduruldu$plasma_CA19_9<-log10(yendolduruldu$plasma_CA19_9)
yendolduruldu$creatinine<-log10(yendolduruldu$creatinine)
yendolduruldu$LYVE1<-log10(yendolduruldu$LYVE1)
yendolduruldu$REG1B<-log10(yendolduruldu$REG1B)
yendolduruldu$TFF1<-log10(yendolduruldu$TFF1)
yendolduruldu$REG1A<-log10(yendolduruldu$REG1A)

#Log d?n???m sonras?
set.seed(1000)
#H0: Verilerin geldi?i da??l?m ile normal da??l?m aras?nda farkl?l?k yoktur.(Veriler normal da??lm??t?r.)
#H1: Verilerin geldi?i da??l?m ile normal da??l?m aras?nda farkl?l?k vard?r.(Veriler normal da??lmam??t?r.)
datanum1<-yendolduruldu[,c("plasma_CA19_9","age","creatinine","LYVE1","REG1B","TFF1","REG1A")]

set.seed(1000)
cor(datanum1,method = "spearman")

hist(yendolduruldu$plasma_CA19_9) # Ayk?rlar var
hist(yendolduruldu$creatinine) # yok
hist(yendolduruldu$age) #yok
hist(yendolduruldu$REG1A) #Ayk?r?lar var
hist(yendolduruldu$REG1B) #yok
hist(yendolduruldu$TFF1) # incele
hist(yendolduruldu$LYVE1) # yok
str(yendolduruldu)


shapiro.test(yendolduruldu$plasma_CA19_9) 
shapiro.test(yendolduruldu$creatinine)
shapiro.test(yendolduruldu$REG1A) 
shapiro.test(yendolduruldu$REG1B)
shapiro.test(yendolduruldu$TFF1)
shapiro.test(yendolduruldu$LYVE1)


cor.test(yendolduruldu$plasma_CA19_9, yendolduruldu$creatinine, method ="spearman")
cor.test(yendolduruldu$plasma_CA19_9, yendolduruldu$LYVE1, method ="spearman")
cor.test(yendolduruldu$plasma_CA19_9, yendolduruldu$REG1B, method ="spearman")
cor.test(yendolduruldu$plasma_CA19_9, yendolduruldu$REG1A, method ="spearman")
cor.test(yendolduruldu$plasma_CA19_9, yendolduruldu$TFF1, method ="spearman")
cor.test(yendolduruldu$LYVE1, yendolduruldu$REG1A, method ="spearman")
cor.test(yendolduruldu$LYVE1, yendolduruldu$REG1B, method ="spearman")

#H0 Red. Veriler normal da??lmam??t?r.


# Ayk?r? De?er Analizleri


boxplot(yendolduruldu$plasma_CA19_9)
hist(yendolduruldu$plasma_CA19_9)
boxplot.stats(yendolduruldu$plasma_CA19_9)$out
a=which(yendolduruldu$plasma_CA19_9 %in% boxplot.stats(yendolduruldu$plasma_CA19_9)$out)
summary(yendolduruldu$plasma_CA19_9)
yendolduruldu[a, ]$plasma_CA19_9 <- 3.282

hist(yendolduruldu$creatinine)
boxplot(yendolduruldu$creatinine)
z<-boxplot.stats(yendolduruldu$creatinine)$out
b=which(yendolduruldu$creatinine %in% boxplot.stats(yendolduruldu$creatinine)$out)
summary(yendolduruldu$creatinine)
yen<- z[z > 4]
summary(yendolduruldu$creatinine)
yendolduruldu[yen,]$creatinine <- 5.070
yenalt<- z[z < 4]
yendolduruldu[yenalt,]$creatinine <- 4.501


hist(yendolduruldu$LYVE1)
boxplot(yendolduruldu$LYVE1)
boxplot.stats(yendolduruldu$LYVE1)$out
c<-which(yendolduruldu$LYVE1 %in% boxplot.stats(yendolduruldu$LYVE1)$out)
summary(yendolduruldu$LYVE1)
yendolduruldu[e, ]$LYVE1 <- 5.878

hist(yendolduruldu$REG1B)
boxplot(yendolduruldu$REG1B)
boxplot.stats(yendolduruldu$REG1B)$out
d<-which(yendolduruldu$REG1B %in% boxplot.stats(yendolduruldu$REG1B)$out)

hist(yendolduruldu$REG1A)
boxplot(yendolduruldu$REG1A)
boxplot.stats(yendolduruldu$REG1A)$out
e<-which(yendolduruldu$REG1A %in% boxplot.stats(yendolduruldu$REG1A)$out)
summary(yendolduruldu$REG1A)
yendolduruldu[e, ]$REG1A <- 4.353

boxplot(yendolduruldu$TFF1)
hist(yendolduruldu$TFF1) # incele
boxplot.stats(yendolduruldu$TFF1)$out
f<-which(yendolduruldu$TFF1 %in% boxplot.stats(yendolduruldu$TFF1)$out)
summary(yendolduruldu$TFF1)
yendolduruldu[f, ]$TFF<-5.480



#ab<-union(a,b)
#abc <- union(ab,c)
#abcd<-union(abc,d)
#abcde<-union(abcd,e)
#abcdef<-union(abcde,f)


#deneme<-yendolduruldu[c(abcdef),]
#temiz<-yendolduruldu[-c(abcdef),]

#summary(temiz)
#summary(yendolduruldu)


#write.csv(yendolduruldu, file="pankreasimputedbask?lanm?s.csv")
#write.csv(temiz, file="pankreasimputedayk?r?s?z.csv")

















































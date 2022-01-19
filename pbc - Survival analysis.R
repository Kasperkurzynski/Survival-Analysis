install.packages("survival")
library(survival)
library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(extrafont)
library(mice)

#testujemy Gita 

#pbcseq
#https://cran.r-project.org/web/packages/survival/survival.pdf

data(pbc)

#Liczba "1" w zmiennej status wynosi 147. Należy uznać te obserwacje jako cenzurowane. 
table(pbcseq$status)

dane <- pbc
dane <- dane[1:312,]
summary(dane)

NA_count <- colSums(is.na(dane))
NA_count
barplot(NA_count)
NA_obs <- dane[!complete.cases(dane),]
which(!complete.cases(dane))

# KNN_dia <- kNN(KNN_dane, variable = c("price","depth", "x"), k = 5)
?kNN()
daneKNN <- kNN(dane, variable = c("chol", "copper", "trig", "platelet"), k = 5)
daneKNN

daneKNN$chol_imp <- NULL
daneKNN$trig_imp <- NULL
daneKNN$copper_imp <- NULL
daneKNN$platelet_imp <- NULL

summary(dane)
summary(daneKNN)
cor.test(dane$age, dane$chol, use="complete.obs")

regresja <- lm(chol ~ sex + age + sex * age, data = dane)
summary(regresja)

 
# I rozwiązanie
#Imputacja metodą mediany / średniej zmiennej "chol" i "trig", a reszty zmiennych metodą KNN.

# II rozwiązanie
#Imputacja zostanie przeprowadzona metodą KNN w przypadku wzięcia pod uwagę tylko 312 obserwacji.

# III rozwiązanie
#Imputacja metodą regresji przy wykorzystaniu interakcji (sex*age).

dane$chol[is.na(dane$chol)] <- mean(dane$chol, na.rm = TRUE)
table(dane$status)

levels(daneKNN$sex) <- c(1,0)
daneKNN$status <- as.factor(daneKNN$status)
daneKNN$trt <- as.factor(daneKNN$trt)
daneKNN$ascites <- as.factor(daneKNN$ascites)
daneKNN$spiders <- as.factor(daneKNN$spiders)
daneKNN$hepato <- as.factor(daneKNN$hepato)
daneKNN$stage <- as.factor(daneKNN$stage)
daneKNN$edema <- as.factor(daneKNN$edema)
str(daneKNN)
summary(daneKNN)


daneKNN$cens <- ifelse(daneKNN$status=="0" | daneKNN$status=="1",0,1)

pbcChol <- ggplot(pbcseq, aes(x=chol)) + geom_histogram(binwidth = 50, fill="firebrick2") + labs(title = "Przed imputacją")
impChol <- ggplot(dane, aes(x=chol)) + geom_histogram(binwidth = 50, fill="gold2") + labs(title = "Po imputacji")
grid.arrange(pbcChol, impChol)

str(dane)
#1 = male, 0 - female
levels(dane$sex) <- c(1,0)
dane$status <- as.factor(dane$status)
dane$trt <- as.factor(dane$trt)
dane$ascites <- as.factor(dane$ascites)
dane$spiders <- as.factor(dane$spiders)
dane$hepato <- as.factor(dane$hepato)
dane$stage <- as.factor(dane$stage)
dane$edema <- as.factor(dane$edema)
str(dane)
summary(dane)

daneKNN$age <- round(daneKNN$age,0)


dane$cens <- ifelse(dane$status=="0" | dane$status=="1",0,1)
dane$cens <- as.factor(dane$cens)

daneKNN %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(age), Min = min(age), Max = max(age), Sd = sd(age))

dane %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(albumin), Min = min(albumin), Max = max(albumin), Sd = sd(trig))

dane %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(chol, na.rm = T), Median = median(chol, na.rm = T), Min = min(chol, na.rm = T), Max = max(chol, na.rm = T), Sd = sd(chol, na.rm = T))

dane %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(alk.phos), Min = min(alk.phos), Max = max(alk.phos), Sd = sd(alk.phos))

dane %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(protime), Min = min(protime), Max = max(protime), Sd = sd(protime))

dane %>%
  group_by(stage) %>%
  summarise(N = n(), Mean = mean(chol), Min = min(chol), Max = max(chol), Sd = sd(chol))

ggplot(daneKNN, aes(x=stage, fill=stage)) + geom_bar()
ggplot(dane, aes(x=sex, fill=sex)) + geom_bar()
ggplot(dane, aes(x=stage, y = age, fill=stage)) + geom_boxplot()
ggplot(dane, aes(x=stage, y=bili, fill=stage)) + geom_boxplot()
ggplot(dane, aes(x=sex, y=chol, fill=sex)) + geom_boxplot()

ggplot(dane, aes(x=age)) + geom_histogram(binwidth = 5, aes(fill=sex)) + facet_grid(dane$sex)

ggplot(dane, aes(x=chol, y=albumin, color=sex)) + geom_point()
ggplot(dane, aes(x=bili, y=ast, color=sex)) + geom_point()
ggplot(dane, aes(x=alk.phos, y=chol, color=edema)) + geom_point(alpha=0.4)
ggplot(dane, aes(x=bili, y=ast, color=hepato)) + geom_point()
ggplot(dane, aes(x=age, y=chol, color=sex)) + geom_point() + geom_smooth()



doKor <- dane[,c(5, 12,13,14,15,16,17,18)]
corMat <- cor(doKor)
corMat
corrplot(corMat,method="number", tl.col = 'black')

Surv(dane$day,dane$cens)
km <- survfit(Surv(day, cens) ~ 1, conf.type="log", data=dane)
SumKM <- summary(km)
SumKM
PlotKM <- plot(km)

#Właściwa analiza histori zdarzeń
Surv(daneKNN$time, daneKNN$cens)
pbcKM <- survfit(Surv(time, cens) ~ 1, conf.type="log", data=daneKNN)
pbcKM1 <- survfit(Surv(time, cens) ~ 1, conf.type="log-log", data=daneKNN)
pbcKM2 <- survfit(Surv(time, cens) ~ 1, conf.type="plain", data=daneKNN)
summary(pbcKM)
plot(pbcKM)
lines(pbcKM1, col="red")
lines(pbcKM2, col="red")

#Wykresy dla grup
pbcSex = survfit(Surv(time, cens) ~ sex, conf.type="plain", data=daneKNN)
summary(pbcSex)
plot(pbcSex, col=c("red","green"))

pbcHepato = survfit(Surv(time, cens) ~ hepato, conf.type="plain", data=daneKNN)
summary(pbcHepato)
plot(pbcHepato, col=c("red","green"))
legend(50,0.3,c("Enlarged liver","Normal liver"),lty=1, col=c("green","red"),bty="n")

pbcTrt = survfit(Surv(time, cens) ~ trt, conf.type="plain", data=daneKNN)
summary(pbcTrt)
plot(pbcTrt, col=c("red","green"))

#Nelson - Aelen
pbcNELS=survfit(Surv(time,cens) ~ 1, conf.type ="plain", type="fleming-harrington",data=daneKNN)

# skumulowanego hazardu
plot(pbcKM, fun="cumhaz",xlab="Czas", ylab="Skumulowany hazard")
lines(pbcNELS, fun="cumhaz",col="red")

s1=daneKNN[sample(nrow(daneKNN),size=60, replace=FALSE),] #mała próba
pbcKM <- survfit(Surv(time, cens) ~ 1, conf.type="log", data=daneKNN) #estymator KM
pbcNelsSample <- survfit(Surv(time,cens) ~ 1, conf.type ="log-log", type="fleming-harrington",data=s1) # estymator NA dla małej próby
pbcNels <- survfit(Surv(time,cens) ~ 1, conf.type ="log-log", type="fleming-harrington",data=daneKNN) # estymator NA
summary(pbcNels) #podsumowanie N-A
plot(pbcKM) #wykres KM
lines(pbcNels, col="red") #wykres KM + N-A
lines(pbcNelsSample, col="blue") #wykres KM + N-A + N-A dla malej próbki



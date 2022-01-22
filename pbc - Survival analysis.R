install.packages("survival")
install.packages("survminer")
library(survival)
library(survminer)
library(survMisc)
library(dplyr)
library(gtools)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(extrafont)
library(mice)
library(VIM)
library(extrafont)


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
daneKNN$cens <- as.factor(daneKNN$cens)
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
str(daneKNN)
summary(daneKNN)

daneKNN$age <- round(daneKNN$age,0)


dane$cens <- ifelse(dane$status=="0" | dane$status=="1",0,1)
dane$cens <- as.factor(dane$cens)

#Rozkłady zmiennych ilościowych

a <- ggplot(data = daneKNN, aes(x = age)) + geom_histogram(alpha = 0.7, binwidth = 5, fill="darkorange") + theme_minimal() +
  ggtitle('Age') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
        labs(y = "N")
b <- ggplot(data = daneKNN, aes(x = bili)) + geom_histogram(alpha = 0.7, binwidth = 2, fill="mediumpurple3") + theme_minimal() +
  ggtitle('Bilirunbin') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "N")
c <- ggplot(data = daneKNN, aes(x = chol)) + geom_histogram(alpha = 0.7, binwidth = 50, fill="seagreen4") + theme_minimal() +
  ggtitle('Cholesterol') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "N")
d <- ggplot(data = daneKNN, aes(x = albumin)) + geom_histogram(alpha = 0.7, binwidth = 0.2, fill="dodgerblue3") + theme_minimal() +
  ggtitle('Albumin') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "N")
a1 <- ggplot(data = daneKNN, aes(x = copper)) + geom_histogram(alpha = 0.7, binwidth = 30, fill="indianred4") + theme_minimal() +
  ggtitle('Copper') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "N")
b1 <- ggplot(data = daneKNN, aes(x = alk.phos)) + geom_histogram(alpha = 0.7, binwidth = 400, fill="royalblue3") + theme_minimal() +
  ggtitle('Alkaline phos.') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=20, family="serif")) +
  labs(y = "N")
c1 <- ggplot(data = daneKNN, aes(x = ast)) + geom_histogram(alpha = 0.7, binwidth = 20, fill="goldenrod2") + theme_minimal() +
  ggtitle('Aspartate amino.') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=20, family="serif")) +
  labs(y = "N")
d1 <- ggplot(data = daneKNN, aes(x = trig)) + geom_histogram(alpha = 0.7, binwidth = 20, fill="cadetblue3") + theme_minimal() +
  ggtitle('Triglycerides') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=20, family="serif")) +
  labs(y = "N")
a2 <- ggplot(data = daneKNN, aes(x = platelet)) + geom_histogram(alpha = 0.7, binwidth = 25, fill="orangered3") + theme_minimal() +
  ggtitle('Platelet') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=20, family="serif")) +
  labs(y = "N")
b2 <- ggplot(data = daneKNN, aes(x = protime)) + geom_histogram(alpha = 0.7, binwidth = 0.5, fill="darkolivegreen4") + theme_minimal() +
  ggtitle('Protime') + 
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=20, family="serif")) +
  labs(y = "N")
c2 <- ggplot(data = daneKNN, aes(x = time, fill = cens)) + geom_histogram(alpha = 0.7, binwidth = 200) + facet_grid(~cens) + theme_minimal() +
  ggtitle('Time for cens & event') + 
  scale_fill_manual(values = c("coral3","slateblue3")) +
  theme(axis.title.y = element_text(color="Grey23", size=11),
        axis.text.y = element_text(size=9),
        axis.title.x = element_blank(),
        plot.title = element_text(color="gray26", size=20, family="serif")) +
  labs(y = "N")

grid.arrange(a,b,c,d)
grid.arrange(a1,b1,c1,d1)
grid.arrange(a2,b2)
c2


#Rozkłady zmiennych jakościowych

e <- ggplot(data = daneKNN, aes(x=sex, fill = sex)) + geom_bar(alpha = 0.8) + scale_fill_manual(values = c("dodgerblue3","plum4")) +
  theme_minimal() +
  ggtitle('Number of patients by gender') + 
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=19, family="serif")) + 
  labs(y = "N")
f <- ggplot(data = daneKNN, aes(x=trt, fill = trt)) + geom_bar(alpha = 0.9) + scale_fill_manual(values = c("lightseagreen","cornsilk3")) +
  theme_minimal() +
  ggtitle('Number of patients by treatment') + 
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=19, family="serif")) + 
  labs(y = "N")
g <- ggplot(data = daneKNN, aes(x=ascites, fill = ascites)) + geom_bar(alpha = 0.8) + scale_fill_manual(values = c("darkseagreen4","coral3")) +
  theme_minimal() +
  ggtitle('Number of patients by ascites presence') + 
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=19, family="serif")) + 
  labs(y = "N")
h <- ggplot(data = daneKNN, aes(x=hepato, fill = hepato)) + geom_bar(alpha = 0.8) + scale_fill_manual(values = c("palegreen4", "indianred3")) +
  theme_minimal() +
  ggtitle('Number of patients by hepato presence') + 
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=19, family="serif")) + 
  labs(y = "N")
e1 <- ggplot(data = daneKNN, aes(x=edema, fill = edema)) + geom_bar(alpha = 0.8) + scale_fill_manual(values = c("springgreen3","tan3","tomato3")) +
  theme_minimal() +
  ggtitle('Number of patients by edema presence') + 
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=19, family="serif")) + 
  labs(y = "N")
f1 <- ggplot(data = daneKNN, aes(x=stage, fill = stage)) + geom_bar(alpha = 0.8) + scale_fill_manual(values = c("palegreen3", "skyblue3","salmon3","brown3")) +
  theme_minimal() +
  ggtitle('Number of patients by stage of disease') + 
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=19, family="serif")) + 
  labs(y = "N")
g1 <- ggplot(data = daneKNN, aes(x=cens, fill = cens)) + geom_bar(alpha = 0.8) + scale_fill_manual(values = c("coral3","slateblue3")) +
  theme_minimal() +
  ggtitle('Number of patients by cens or event') + 
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=19, family="serif")) + 
  labs(y = "N")

grid.arrange(e,f)
grid.arrange(g,h)
grid.arrange(e1,f1)
g1

#Rozkłady pokazujące niektóre zależności

z <- ggplot(daneKNN, aes(x=stage, y = age, fill=stage)) + geom_boxplot(size=1.2, alpha=0.5) +
  scale_fill_manual(values = c("palegreen3", "skyblue3","salmon3","brown3")) + theme_minimal() +
  ggtitle("Age by stage") +
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "Age")
x <- ggplot(daneKNN, aes(x=stage, y = bili, fill=stage)) + geom_boxplot(size=1.2, alpha=0.5) +
  scale_fill_manual(values = c("palegreen3", "skyblue3","salmon3","brown3")) + theme_minimal() +
  ggtitle("Bilirunbin by stage") +
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "Bilirunbin")
z1 <- ggplot(daneKNN, aes(x=stage, y = time, fill=stage)) + geom_boxplot(size=1.2, alpha=0.5) +
  scale_fill_manual(values = c("palegreen3", "skyblue3","salmon3","brown3")) + theme_minimal() +
  ggtitle("Time by stage") +
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "Time") + facet_grid(~cens)
x1 <- ggplot(daneKNN, aes(x=ascites, y = bili, fill=ascites)) + geom_boxplot(size=1.2, alpha=0.5) +
  scale_fill_manual(values = c("darkseagreen4","coral3")) + theme_minimal() +
  ggtitle("Bilirunbin by ascites presence") +
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "Bilirunbin")

x2 <- ggplot(daneKNN, aes(x=hepato, y = bili, fill=hepato)) + geom_boxplot(size=1.2, alpha=0.5) +
  scale_fill_manual(values = c("palegreen4", "indianred3")) + theme_minimal() +
  ggtitle("Bilirunbin by hepato presence") +
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "Bilirunbin")

z2 <- ggplot(daneKNN, aes(x=hepato, y = copper, fill=hepato)) + geom_boxplot(size=1.2, alpha=0.5) +
  scale_fill_manual(values = c("palegreen4", "indianred3")) + theme_minimal() +
  ggtitle("Copper by hepato presence") +
  theme(axis.title.y = element_text(color="Grey23", size=12),
        axis.title.x=element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=8),
        legend.position = "right",
        legend.justification = c(0.94,0.94),
        legend.background = element_rect(fill="grey88",
                                         size=0.5, linetype="solid", 
                                         colour ="gray26"),
        plot.title = element_text(color="gray26", size=18, family="serif")) +
  labs(y = "Copper")

grid.arrange(z,x)
grid.arrange(z1,x1)
grid.arrange(z2,x2)

#Statystyki względem grup

daneKNN %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(age), Min = min(age), Max = max(age), Sd = sd(age))

daneKNN %>%
  group_by(edema) %>%
  summarise(N = n(), Mean = mean(bili), Min = min(bili), Max = max(bili), Sd = sd(bili))

daneKNN %>%
  group_by(stage) %>%
  summarise(N = n(), Mean = mean(bili), Min = min(bili), Max = max(bili), Sd = sd(bili))

daneKNN %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(chol), Min = min(chol), Max = max(chol), Sd = sd(chol))



#Wykres korelacji

doKor <- daneKNN[,c(5, 12,13,14,15,16,17,18,19)]
corMat <- cor(doKor)
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


#### model PH Coxa ####
# Zbuduj model PH Coxa. Uwzględnij następujące zagadnienia:
  # interpretacja uzyskanych oszacowań parametrów modelu
  # weryfikacja założeń modelu (proporcjonalność hazardów)
  # wybór postaci funkcji zmiennych objaśniających
  # selekcja zmiennych do modelu
  # reszty w modelu
  # wybór najlepszego modelu
  # ocena dopasowania modelu

# estymacja modelu
Cox <- coxph(Surv(time,cens)~trt+age+sex+ascites+hepato+spiders+bili+chol+albumin+copper+alk.phos+ast+trig+platelet+protime, data = daneKNN)
summary(Cox)
#globalny test Walda (p=<2e-16) wskazuje, ze przynajmniej 1 zmienna objasniajaca w modelu statystycznie rozni sie od 0, czyli jest istotna
#zmienne: trt,sex,ascites,spiders,chol,alk.phos,ast,trig,platelet nie sa statystycznie rozne od 0, bo p-value <0,05

# estymacja modelu metoda krokowa 
library(MASS)
stepAIC(Cox,direction="backward") #poszukuje modelu ktory bedzie optymalizowal model ze wzgledu na kryterium akaike AIC (tu - metoda wsteczna)
#w wyniku tej procedury otrzymano model z 7 zmiennymi objasniajacymi: age,hepato,bili,albumin,copper,ast,protime
#wg kryterium AIC zmienna ast mimo p-value <0,06 powinna znalezc sie w modelu
#wszystkie te zmienne na poziomie istotnosci 0,05 sa statystycznie rozne od 0 (czyli istotne), bo p-value >0,05

#otrzymany model
Cox1 <- coxph(Surv(time,cens)~age+hepato+bili+albumin+copper+ast+protime, data = daneKNN)
summary(Cox1)

# INTERPRETACJA PARAMETROW: 
#jezeli wiek wzrosnie o 1rok, hazard (ryzyko) zgonu rośnie o 3,3% u osob bez hepatomegalii lub powiększenia wątroby, ceteris paribus (przy pozostalych wartosciach zmiennych takich samych)
#pacjenci z obecnościa hepatomegalii lub powiększenia wątroby(hepato1) maja hazard o 66,2% wyzszy niz pacjenci z hepato = 0 ceteris paribus
#wraz ze wzrostem bili o 1, hazard (ryzyko) zgonu rosnie o 8,9% ceteris paribus
#wraz ze wzrostem albumin o 1, hazard (ryzyko) zgonu ?
#wraz ze wzrostem copper o 1, hazard (ryzyko) zgonu rosnie o 0,3% ceteris paribus
#wraz ze wzrostem ast o 1, hazard (ryzyko) zgonu rosnie o 0,4% ceteris paribus
#wraz ze wzrostem protime o 1, hazard (ryzyko) zgonu rosnie o 38,0% ceteris paribus

# WERYFIKACJA ZALOZENIA PROPORCJONALNOSCI
Prop <- cox.zph(Cox1, transform = 'km') #Kaplana-Meiera
print(Prop)
plot(Prop)
#jezeli p > 0,05 to zostaly spelnione zalozenia proporcjalosci
#- dla zmiennych: bili,protime nie zostaly spelnione (wyrzucic je z modelu?)
Cox2 <- coxph(Surv(time,cens)~age+hepato+albumin+copper+ast, data = daneKNN)
summary(Cox2)

# graficzna weryfikacja zalozenia proporcjonalnosci
reszty <- resid(Cox1, type="scaledsch") 
Time <- as.numeric(rownames(reszty))
zmienne <- names(daneKNN[,c(5,8,11,13,14,16,19)])

par(mfrow = c(3,3))
for (i in 1:7) {
  plot(log(Time), reszty[,i], xlab="ln(Czas)", main=zmienne[i],
       ylab="Skalowane reszty Schoenfelda", pch=20, cex=0.7)
  lines(smooth.spline(log(Time), reszty[,i] ), lwd=3 )
}



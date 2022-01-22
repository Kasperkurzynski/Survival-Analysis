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
library(mfp)
library(splines)
library(CoxR2)
library(ipred)

#pbcseq
#https://cran.r-project.org/web/packages/survival/survival.pdf

data(pbc)

#Liczba "1" w zmiennej status wynosi 147. Należy uznać te obserwacje jako cenzurowane. 

dane <- pbc
dane <- dane[1:312,, drop=F]
summary(dane)

dane$cens <- ifelse(dane$status=="0" | dane$status=="1",0,1)

NA_count <- colSums(is.na(dane))
NA_count
barplot(NA_count)
NA_obs <- dane[!complete.cases(dane),]
which(!complete.cases(dane))

# KNN_dia <- kNN(KNN_dane, variable = c("price","depth", "x"), k = 5)
?kNN()
daneKNN <- kNN(dane, variable = c("chol", "copper", "trig", "platelet"), k = 5)
daneKNN

daneKNN <- daneKNN[,1:21,drop = F]
daneKNN <- as.data.frame(daneKNN)

# I rozwiązanie
#Imputacja metodą mediany / średniej zmiennej "chol" i "trig", a reszty zmiennych metodą KNN.

# II rozwiązanie
#Imputacja zostanie przeprowadzona metodą KNN w przypadku wzięcia pod uwagę tylko 312 obserwacji.

# III rozwiązanie
#Imputacja metodą regresji przy wykorzystaniu interakcji (sex*age).

daneKNN$age <- round(daneKNN$age,0)
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

#Właściwa analiza histori zdarzeń

Surv(daneKNN$time, daneKNN$cens)
pbcKM <- survfit(Surv(time, cens) ~ 1, conf.type="log", data=daneKNN)
pbcKM1 <- survfit(Surv(time, cens) ~ 1, conf.type="log-log", data=daneKNN)
pbcKM2 <- survfit(Surv(time, cens) ~ 1, conf.type="plain", data=daneKNN)
summary(pbcKM)
plot(pbcKM)
lines(pbcKM1, col="red")
lines(pbcKM2, col="red")

daneKNN <- as.data.frame(daneKNN)
require(reshape2)
daneKNN$id <- rownames(daneKNN) 
melt(daneKNN)

ggsurvplot(
  fit = survfit(Surv(time, cens) ~ 1, data = daneKNN), 
  xlab = "Days", 
  ylab = "Overall survival probability")

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
#pacjenci z obecnościa hepatomegalii lub powiększenia wątroby(hepato1) maja hazard (ryzyko) zgonu o 66,2% wyzszy niz pacjenci z hepato = 0 ceteris paribus
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
#- dla zmiennych: bili,protime nie zostaly spelnione (wyrzucic je z modelu)
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

# jednostki odstajace
deviance <- residuals(Cox2,type="deviance")
s <- Cox2$linear.predictors
plot(s,deviance,xlab="Liniowy predyktor",ylab="Reszty odchylen",cex=0.5, pch=20)
abline(h=c(3,-3),lty=3)
daneKNN$deviance <- deviance
c1 <- which(daneKNN$deviance< c(-3))
#jest 1 jednostka odstajaca (reszty odchylen<-3)

# jednostki wplywowe
dfb <- residuals(Cox2,type="dfbeta")
n <- dim(dfb)[1]
obs.nr <- c(1:n)
par(mfrow = c(2,3))
for (j in 1:5) {
  plot(obs.nr,dfb[,j],xlab="Numer jednostki",ylab="Przyrost oceny parametru",
       main=zmienne[j])
}

a1 <- which(abs(dfb[,1])>(0.004)) #usunac te jednostki
a2 <- which(abs(dfb[,2])>(0.06))
a3 <- which(abs(dfb[,3])>(0.1))
a4 <- which(abs(dfb[,4])>(0.0004))
a5 <- which(abs(dfb[,5])>(0.001))

c <- sort(unique(c(a1,a2,a3,a4,a5,c1)))
daneKNN_1 <- daneKNN[-c,] #zredukowany zbior

Cox3 <- coxph(Surv(time,cens)~age+hepato+albumin+copper+ast, data = daneKNN_1)
summary(Cox3)
library(MASS)
stepAIC(Cox3,direction="backward")

# INTERPRETACJA PARAMETROW: 
#jezeli wiek wzrosnie o 1rok, hazard (ryzyko) zgonu rośnie o 4,9% u osob bez hepatomegalii lub powiększenia wątroby, ceteris paribus (przy pozostalych wartosciach zmiennych takich samych)
#pacjenci z obecnościa hepatomegalii lub powiększenia wątroby(hepato1) maja hazard (ryzyko) zgonu o 100,5% wyzszy niz pacjenci z hepato = 0 ceteris paribus
#wraz ze wzrostem albumin o 1, hazard (ryzyko) zgonu ?
#wraz ze wzrostem copper o 1, hazard (ryzyko) zgonu rosnie o 0,6% ceteris paribus
#wraz ze wzrostem ast o 1, hazard (ryzyko) zgonu rosnie o 0,8% ceteris paribus

# METODA RESZT MARTYNGALOWYCH - badamy czy wystepuje liniowosc
#Jeżeli zmienna objaśniająca 𝑋 jest ilościowa, to wprowadzenie jej do modelu Coxa w 
#jest równoważne przyjęciu założenia, że łączy ją związek liniowy z logarytmem hazardu
zmn1 <- daneKNN_1$age
lab1 <- "WIEK (lata)"
reszty1 <- resid(coxph(Surv(time,cens)~1,data=daneKNN_1),type="martingale")
plot(zmn1, reszty1, xlab=lab1,ylab="Reszty martynga?owe",cex=0.6)
lines(lowess(zmn1, reszty1,delta=1),lwd=2)
#Jeżeli funkcja ta ma kształt zbliżony do liniowego to potwierdza to liniowość 
#relacji między zmienną objaśniającą a logarytmem hazardu, w przeciwnym wypadku kształt 
#funkcji może być wskazówką co do wyboru odpowiedniej metody transformacji zmiennej.
#age - WYSTEPUJE LINIOWOSC

zmn2 <- daneKNN_1$albumin
lab2 <- "albumin"
reszty2 <- resid(coxph(Surv(time,cens)~1,data=daneKNN_1),type="martingale")
plot(zmn2, reszty2, xlab=lab2,ylab="Reszty martynga?owe",cex=0.6)
lines(lowess(zmn2, reszty2,delta=1),lwd=2)
#albumin - WYSTEPUJE LINIOWOSC

zmn3 <- daneKNN_1$copper
lab3 <- "copper"
reszty3 <- resid(coxph(Surv(time,cens)~1,data=daneKNN_1),type="martingale")
plot(zmn3, reszty3, xlab=lab3,ylab="Reszty martynga?owe",cex=0.6)
lines(lowess(zmn3, reszty3,delta=1),lwd=2)
#copper - NIE WYSTEPUJE LINIOWOSC

zmn4 <- daneKNN_1$ast
lab4 <- "ast"
reszty4 <- resid(coxph(Surv(time,cens)~1,data=daneKNN_1),type="martingale")
plot(zmn4, reszty4, xlab=lab4,ylab="Reszty martynga?owe",cex=0.6)
lines(lowess(zmn4, reszty4,delta=1),lwd=2)
#ast - WYSTEPUJE LINIOWOSC

#TRANSFORMACJA ZMIENNEJ copper
#dychotomizacja
Cox4 <- coxph(Surv(time, cens)~zmn3,data=daneKNN_1)
A1 <- round(AIC(Cox3),2) #akaike z modelu coxa
A1
cutpoint <- cutp(Cox4)$zmn3 ## optymalny punkt odciecia
c <- (zmn3>=cutpoint$zmn3[1])*1+0 #jezeli zmienna jest >= punkotwi odciecia to przypisuje 1 w przeciwnym wypadku 0

Cox5 <- coxph(Surv(time, cens)~c,data=daneKNN_1)
A2 <- round(AIC(Cox5),2)
A2
# teraz jest gorzej bo Akaike jest duzo wyzszy niz przed dychotomizacja

#wielomiany ulamkowe
m3 <- mfp(Surv(time,cens)~ fp(zmn3, df = 4, select = 0.05),family=cox, data=daneKNN_1) ## wielomiany ulamkowe
m3
A3 <- round(AIC(m3))
A3

#metoda wielomianowej funkcji sklejanej (splajn)
int <- quantile(na.omit(zmn3),probs=c(0.05,0.275,.5,.725,.95))
Cox6 <- coxph(Surv(time,cens)~ns(zmn3,knots=int),data=daneKNN_1)
pred <- predict(Cox6,type="terms",se.fit=TRUE, terms=1)
plot(na.omit(zmn3),exp(pred$fit),type="n",xlab=lab3,ylim=c(0,2.5),
     ylab="Hazard wzgl?dny")
lines(smooth.spline(na.omit(zmn3),exp(pred$fit+1.96*pred$se.fit)),lty=2)
lines(smooth.spline(na.omit(zmn3),exp(pred$fit-1.96*pred$se.fit)),lty=2)
lines(smooth.spline(na.omit(zmn3),exp(pred$fit)),lty=1)
abline(h=1,lty=3)
legend('topright',2,c("splajn","95% przedzial ufnosci"), lty=1:2, box.lty=0)
A4 <- round(AIC(Cox6),2)
A4

#PODSUMOWANIE TRANSFORMACJI ZMIENNEJ copper
rbind(A1,A2,A3,A4)
#mimo wszystko najlepszym modelem jest model bez transformacji zmiennej copper, czyli:
Cox3 <- coxph(Surv(time,cens)~age+hepato+albumin+copper+ast, data = daneKNN_1)
summary(Cox3)
# INTERPRETACJA PARAMETROW: 
#jezeli wiek wzrosnie o 1rok, hazard (ryzyko) zgonu rośnie o 4,9% u osob bez hepatomegalii lub powiększenia wątroby, ceteris paribus (przy pozostalych wartosciach zmiennych takich samych)
#pacjenci z obecnościa hepatomegalii lub powiększenia wątroby(hepato1) maja hazard (ryzyko) zgonu o 100,5% wyzszy niz pacjenci z hepato = 0 ceteris paribus
#wraz ze wzrostem albumin o 1, hazard (ryzyko) zgonu ?
#wraz ze wzrostem copper o 1, hazard (ryzyko) zgonu rosnie o 0,6% ceteris paribus
#wraz ze wzrostem ast o 1, hazard (ryzyko) zgonu rosnie o 0,8% ceteris paribus

# MIARY DOPASOWANIA
r2_1 <- coxr2(Cox1) #model otrzymany metoda krokowa przed sprawdzeniem zalozen proporcjonalnosci
r2_2 <- coxr2(Cox2) #model po odrzuceniu zmiennych niespelniajacych zalozen proporcjonalnosci
r2_3 <- coxr2(Cox3) #model po odrzuceniu jednostek odstajacych i wplywowych (uznany za najlepszy)
round(rbind(r2_1$rsq, r2_2$rsq, r2_3$rsq),2)

#wskaznik Briera
sbrier(Surv(daneKNN_1$time, daneKNN_1$cens), predict(Cox3), btime = 780) #btime moment do ktorego wskaznik jest liczony
#DLACZEGO WYCHODZI WIEKSZY NIZ 1???? musi byc w przedziale 0-1

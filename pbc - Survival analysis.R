install.packages("survival")
library(survival)
library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)

#pbcseq
#https://cran.r-project.org/web/packages/survival/survival.pdf

data(pbc)

#Liczba "1" w zmiennej status wynosi 147. Należy uznać te obserwacje jako cenzurowane. 
table(pbcseq$status)

dane <- pbcseq
summary(dane)
#Zmienna "chol" zawiera najwięcej braków danych (821). 
#Dla tej zmiennej zostanie przeprowadzona imputacja metodą imputacja średnią.
#Pozostałe zmienne zawierają między 60-80 braków danych. Obserwacje te zostaną usunięte. 

dane$chol[is.na(dane$chol)] <- mean(dane$chol, na.rm = TRUE)
dane <- na.omit(dane)
table(dane$status)

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


dane$cens <- ifelse(dane$status=="0" | dane$status=="1",0,1)
dane$cens <- as.factor(dane$cens)

dane %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(age), Min = min(age), Max = max(age), Sd = sd(age))

dane %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(albumin), Min = min(albumin), Max = max(albumin), Sd = sd(trig))

dane %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(chol, na.rm = T), Min = min(chol, na.rm = T), Max = max(chol, na.rm = T), Sd = sd(chol, na.rm = T))

dane %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(alk.phos), Min = min(alk.phos), Max = max(alk.phos), Sd = sd(alk.phos))

dane %>%
  group_by(sex) %>%
  summarise(N = n(), Mean = mean(protime), Min = min(protime), Max = max(protime), Sd = sd(protime))

dane %>%
  group_by(stage) %>%
  summarise(N = n(), Mean = mean(chol), Min = min(chol), Max = max(chol), Sd = sd(chol))

ggplot(dane, aes(x=stage, fill=stage)) + geom_bar()
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


               
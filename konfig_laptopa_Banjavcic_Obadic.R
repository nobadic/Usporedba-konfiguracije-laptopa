# Projektni zadatak - Banjavcic, Obadic

# install.packages("leaps", "MASS","ggrepel","nnls" ) 
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} 
library(XML)
library(readxl)
laptops <- read_excel("laptops.xlsx")
View(laptops)

# PRIPREMA PODATAKA ------------------------
laptops$CPU.TYPE <- factor(laptops$CPU.TYPE, ordered = TRUE,
                       levels = c("AMD","Celeron", "Core i3","Pentium", "Core i5", "Core i7"),
                       labels = c("AMD","Cel", "i3","Pen", "i5", "i7"))
laptops$CPU.TYPE.num <- laptops$CPU.TYPE
levels(laptops$CPU.TYPE.num) <- c("1","2","3","4","5","7")
laptops$CPU.TYPE.num <- as.numeric.factor(laptops$CPU.TYPE.num)


laptops$RAM <- factor(laptops$RAM, ordered = TRUE,
                      levels = c("4", "8", "12", "16"))
laptops$RAM.num <- laptops$RAM
levels(laptops$RAM.num) <- c("4", "8", "12", "16")
laptops$RAM.num <- as.numeric.factor(laptops$RAM.num)


laptops$HDD <- as.character(laptops$HDD)
laptops$HDD[is.na(laptops$HDD)] <- "0"
laptops$HDD <- factor(laptops$HDD, ordered = TRUE,
                      levels = c("0","16","32","64","128", "256", "500", "1000", "2000"))
laptops$HDD.num <- laptops$HDD
levels(laptops$HDD.num) <- c("0","16","32","64","128", "256","500","1000","2000")
laptops$HDD.num <- as.numeric.factor(laptops$HDD.num)


laptops$SSD <- as.character(laptops$SSD)
laptops$SSD[is.na(laptops$SSD)] <- "0"
laptops$ssd <- factor(laptops$SSD, ordered = TRUE,
                      levels = c("0","32","64", "128", "256", "512"))
laptops$ssd.num <- laptops$ssd
levels(laptops$ssd.num) <- c("0","32","64","128","256","512")
laptops$ssd.num <- as.numeric.factor(laptops$ssd.num)


laptops$GPU <- factor(laptops$GPU, ordered = TRUE,
                      levels = c("HD Graphics","AMD","nVidia","UHD Graphics"))
laptops$GPU.num <- laptops$GPU
levels(laptops$GPU.num) <- c("0","0.5","1","2")
laptops$GPU.num <- as.numeric.factor(laptops$GPU.num)


laptops$OS <- factor(laptops$OS, ordered = TRUE,
                     levels = c("0", "1"),
                     labels = c("FreeDOS", "OS"))
laptops$OS.num <- laptops$OS


# VIZUALIZACIJA PODATAKA --------------------------------------------------
if (0) {
    library(ggplot2)
    library(ggrepel)
    
    windows()
    ggplot(data=laptops, aes(x=CPU.TYPE,y=Cijena))+
      geom_boxplot(fill="lightblue", color="black", notch = TRUE)+
      geom_point(position = "jitter", color="blue", alpha=.5)+
      geom_rug(sides="1", color="black")
    
    windows()
    ggplot(data=laptops, aes(x=RAM,y=Cijena))+
      geom_boxplot(fill="lightblue",
                   color="black", notch = TRUE)+
      geom_point(position = "jitter", color="blue", alpha=.5)+
      geom_rug(sides="1", color="black")
    
    windows()
    ggplot(data=laptops, aes(x=HDD,y=Cijena))+
      geom_boxplot(fill="cornflowerblue",
                   color="black", notch = TRUE)+
      geom_point(position = "jitter", color="blue", alpha=.5)+
      geom_rug(sides="1", color="black")
    
    windows()
    ggplot(data=laptops, aes(x=GPU,y=Cijena))+
      geom_boxplot(fill="cornflowerblue",
                   color="black", notch = TRUE)+
      geom_point(position = "jitter", color="blue", alpha=.5)+
      geom_rug(sides="1", color="black")
    
    windows()
    ggplot(data=laptops,aes(x=Cijena, y=CPU.TYPE, color=SSD, shape=HDD))+
      geom_point(position = "jitter", alpha=1.0)+
      geom_rug(sides="1", color="black")+
      geom_smooth(method = "lm", color = "red", linetype=2)+
      facet_grid(.~laptops$Cijena)
    
    ##windows()
    ##ggplot(data = laptops, aes(x=Cijena, y=Brend))+
      ##geom_boxplot(fill=laptops$Brend, color="black", notch = TRUE)
    
    windows()
    ggplot(data = laptops, aes(x=GPU, fill=Brend))+
      geom_histogram(stat = "count")
    
    windows()
    ggplot(data=laptops,aes(x=Brend, y=GPU))+
      geom_point(position = "jitter", alpha=1.0)+
      geom_rug(sides="1", color="black")+
      geom_smooth(method = "lm", color = "red", linetype=2)
    
    windows()
    ggplot(data=laptops, aes(x = Brend, fill=GPU))+
      geom_histogram(stat = "count")
    
    windows()
    ggplot(data=laptops, aes(x = OS, fill=CPU.TYPE))+
      geom_histogram(stat = "count")
  }

# KORELACIJE --------------------------------------------------------------

rm(laptops.num)
with(laptops, {
  laptops.num <<- data.frame(Cijena, RAM.num, HDD.num, ssd.num, GPU.num, Ekran)  
})

# Test korelacija
options(digits=2)
cor(laptops.num)

library(car)
scatterplotMatrix(laptops.num, spread=FALSE, smoother.args=list(lty=2))

# Korelogram
library(corrgram)
windows()
corrgram(laptops.num, order=TRUE, 
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt)



# MULTIPLA REGRESIJA ------------------------------------------------------

library(MASS)  #za stepwise regresiju
library(leaps) #za all-subsets metodu

# Model fit - nominalne varijable
#Cijena, CPU.TYPE, RAM.num, HDD.num, ssd.num, GPU.num, OS.num, Ekran, Tezina

fit1 <- lm(Cijena ~ CPU.TYPE.num + RAM.num + HDD.num, data=laptops)
fit2 <- lm(Cijena ~ CPU.TYPE.num + RAM.num +OS.num  + HDD.num, data=laptops)
fit3<-lm(Cijena ~ CPU.TYPE.num + RAM.num + ssd.num + GPU.num + HDD.num + OS.num, data=laptops)


summary(fit1)
summary(fit2)
summary(fit3)

#stepAIC(fit3, direction = "backward")


# Model fit - numerièke varijable
fit.num <- lm(Cijena ~ RAM.num , data=laptops)
summary(fit.num)
stepAIC(fit.num, direction = "backward")


# Model fit - testiranje razlièitih modela
fit.test <- lm(Cijena ~ CPU.TYPE.num + RAM.num+ssd.num  + HDD.num + OS.num + Ekran , data=laptops)
summary(fit.test)
stepAIC(fit.test, direction = "backward")

#All-subsets metoda
leaps <- regsubsets(Cijena ~ CPU.TYPE.num + 
                      RAM.num*HDD.num + ssd.num +
                      GPU.num + OS.num + 
                      Ekran, data=laptops, nbest=4)
windows()
plot(leaps, scale="adjr2")

fit.test2 <- lm(Cijena ~ CPU.TYPE.num + 
                  RAM.num:HDD.num + ssd.num + 
                  OS.num + Ekran, data=laptops)
summary(fit.test2)


# PREDIKCIJA --------------------------------------------------------------

testdata.num <- data.frame(CPU.TYPE.num = 5,
                           RAM.num = 8,
                           HDD.num = 1000,
                           ssd.num = 126,
                           GPU.num = 1,
                           Ekran = 15.6,
                           Tezina = 1.8)
predict(fit.num, newdata=testdata.num)

# DIJAGNOSTIKA REGRESIJE --------------------------------------------------

model <- fit.test2

# Normalnost / Linearnost / Homoskedastiènost / Nezavisnost 
windows()
par(mfrow=c(2,2))
plot(model)


# Identifikacija outlier-a
library(car)
outlierTest(model)

# Identifikacija ostalih karakteristièknih toèki
library(car)
windows()
influencePlot(model, id.method = "identify")

# PRECJENJENI I PODCJENJENI PREMA MODELU ----------------------------------

best.model <- fit.test2

fitted(best.model)    
residuals(best.model)  
sort(residuals(best.model)) #reziduali: podcijenjeno (<0), precijenjeno (>0)

laptops$residuals <- residuals(best.model) #pohrana reziduala

# Top lista (od najpodcjenjenijih do najprecjenjenijih)
toplista <- order(residuals(best.model))

# Ispis 
laptops[toplista] #Ispis top liste
toplista


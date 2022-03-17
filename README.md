# My_project_R-Studio
#Portfolio in R Studio with 4 assets
#setwd("/Users/ your library") 
getwd()
#importing data
#ror <- read.csv("projekt.csv", sep=";", dec=",") you must create your csv file with rates of return 
ror
#We choose three assets for further analysis 
portfolio4 <- ror[c(1, 2, 3, 4)]
#Variables for all calculations; here you should change names of assets 
GOOGL <- portfolio4$GOOGL
NVDA <- portfolio4$NVDA
META <- portfolio4$META
EA <- portfolio4$EA

#importing weights from file
weights4inv <- read.table("weights4inv.txt",dec=",", header=TRUE, quote="\"",stringsAsFactors=FALSE)
w1 <- weights4inv$W1
w1 <- as.numeric(w1)
w2 <- weights4inv$W2
w2 <- as.numeric(w2)
w3 <- weights4inv$W3
w3 <- as.numeric(w3)
w4 <- weights4inv$W4
w4 <- as.numeric(w4)

#calculating SD
s1 <- sd(GOOGL)
s2 <- sd(NVDA)
s3 <- sd(META)
s4 <- sd(EA)
#Calculating corellation
corr12 <- cor(GOOGL,NVDA)
corr13 <- cor(GOOGL,META)
corr14 <- cor(GOOGL,EA)
corr23 <- cor(NVDA,META)
corr24 <- cor(NVDA,EA)
corr34 <- cor(META,EA)

#calculating ip
Er <- mean(GOOGL)*w1+mean(NVDA)*w2+mean(META)*w3+mean(EA)*w4
Er
#portfolio risk
sdp <- (w1^2*s1^2 + w2^2*s2^2 + w3^2*s3^2 + w4^2*s4^2 + 2*w1*w2*s1*s2*corr12 + 2*w1*w3*s1*s3*corr13 + 2*w1*w4*s1*s4*corr14 + 
          2*w2*w3*s2*s3*corr23 + 2*w2*w4*s2*s4*corr24 + 2*w3*w4*s3*s4*corr34)^0.5
sdp
#calculating effectivness
rf = 0.0
sharp=(Er-rf)/sdp
sharp
#preparing df with results
data <- cbind(w1, w2, w3, w4, Er, sdp, sharp)
data <- as.data.frame(data)
#finding interesting portfolios
min.risk <- subset(data, data$sdp==min(data$sdp))
max.effectivness <- subset(data, data$sharp==max(data$sharp))
max.er <- subset(data, data$Er==max(data$Er))
max.w1 <- subset(data, data$w1==1)
max.w2 <- subset(data, data$w2==1)
max.w3 <- subset(data, data$w3==1)
max.w4 <- subset(data, data$w4==1)
View(min.risk)
View(max.effectivness)
View(max.er)
View(max.w1)
View(max.w2)
View(max.w3)
View(max.w4)
des <- c("Minimal risk portfolio", "Maximum efficiency portfolio", "Maximum rate of return portfolio", "Max weight one portfolio", "Max weight two portfolio", "Max weight three portfolio", "Max weight four portfolio")

View(des)
#Creating table with results of portfolios and showing results in console
results <- cbind(rbind(min.risk, max.effectivness, max.er, max.w1, max.w2, max.w3, max.w4), des)
results
write.csv(x=results, file = "results.csv", row.names=FALSE)

#install.packages("xts")
#install.packages("PerformanceAnalytics")
#install.packages("quadprog")

library(IntroCompFinR)
erp<-c(mean(GOOGL), mean(NVDA), mean(META), mean(EA))
#COVMAT
covmat = matrix(c(s1^2, corr12*s1*s2, corr13*s1*s3, corr14*s1*s4,
                  corr12*s1*s2, s2^2, corr23*s2*s3, corr24*s2*s4,
                  corr13*s1*s3, corr23*s2*s3, s3^2, corr34*s3*s4,
                  corr14*s1*s4, corr24*s2*s4, corr34*s3*s4, s4^2),
                nrow=4, ncol=4)
#from minimum to maximum Erp
y<-156:487/10000
x<-NA
w_1<-NA
w_2<-NA
w_3<-NA
w_4<-NA
for (i in 1:length(y)){
  x[i]<-efficient.portfolio(erp, covmat, y[i], shorts=FALSE)$sd
  w_1[i]<-efficient.portfolio(erp, covmat, y[i], shorts=FALSE)$weights[1]
  w_2[i]<-efficient.portfolio(erp, covmat, y[i], shorts=FALSE)$weights[2]
  w_3[i]<-efficient.portfolio(erp, covmat, y[i], shorts=FALSE)$weights[3]
  w_4[i]<-efficient.portfolio(erp, covmat, y[i], shorts=FALSE)$weights[4]
}



#Wykres zawierający legendy + PORTFELE MINIMALNEJ WARIANCJI
plot(sdp, Er, type= "p", col = "light yellow",
     xlab="Ryzko portfela [%]", ylab = "Stopa zwrotu z portfela [%]")
title(main="Zbiór możliwości inwestycyjnych dla 4 aktywów")

lines(x,y, lwd=7, col="light coral")

points(min.risk$sdp, min.risk$Er, pch=19, col="green")
points(max.effectivness$sdp, max.effectivness$Er, pch=19, col="blue")

points(max.w1$sdp, max.w1$Er, pch=19, col="black")
points(max.w2$sdp, max.w2$Er, pch=19, col="black")
points(max.w3$sdp, max.w3$Er, pch=19, col="black")
points(max.w4$sdp, max.w4$Er, pch=19, col="black")
abline(c(rf, max.effectivness$sharp), lty=3)

legend(legend = c("Zbiór możliwości bez krótkiej sprzedaży", "Portfel minimalnego ryzyka", "Portfel maksymalnej efektywności", 
                  "Portfel jedno elementowy", "Portfele minimalnej wariancji", "CLM"), 
       pch = c(19, 19, 19, 19, -1, -1, -1, -1), 
       col = c("light yellow", "green", "blue", "black", "light coral", "black", "black"), 
       lty=c(0, 0, 0, 0, 1, 2, 2, 2),
       "bottomright")

erp<-c(mean(GOOGL), mean(NVDA), mean(META), mean(EA))
#COVMAT
covmat = matrix(c(s1^2, corr12*s1*s2, corr13*s1*s3, corr14*s1*s4,
                  corr12*s1*s2, s2^2, corr23*s2*s3, corr24*s2*s4,
                  corr13*s1*s3, corr23*s2*s3, s3^2, corr34*s3*s4,
                  corr14*s1*s4, corr24*s2*s4, corr34*s3*s4, s4^2),
                nrow=4, ncol=4)
#from minimum to maximum Erp
z<-195:487/10000
x<-NA
w_GOOGL<-NA
w_NVDA<-NA
w_META<-NA
w_EA<-NA
for (i in 1:length(y)){
  x[i]<-efficient.portfolio(erp, covmat, z[i], shorts=FALSE)$sd
  w_GOOGL[i]<-efficient.portfolio(erp, covmat, z[i], shorts=FALSE)$weights[1]
  w_NVDA[i]<-efficient.portfolio(erp, covmat, z[i], shorts=FALSE)$weights[2]
  w_META[i]<-efficient.portfolio(erp, covmat, z[i], shorts=FALSE)$weights[3]
  w_EA[i]<-efficient.portfolio(erp, covmat, z[i], shorts=FALSE)$weights[4]
}



#Wykres zawierający legendy + PORTFELE EFEKTYWNE
plot(sdp, Er, type= "p", col = "light yellow",
     xlab="Ryzko portfela [%]", ylab = "Stopa zwrotu z portfela [%]")
title(main="Zbiór możliwości inwestycyjnych dla 4 aktywów")

lines(x,z, lwd=7, col="light coral")

points(min.risk$sdp, min.risk$Er, pch=19, col="green")
points(max.effectivness$sdp, max.effectivness$Er, pch=19, col="blue")

points(max.w1$sdp, max.w1$Er, pch=19, col="black")
points(max.w2$sdp, max.w2$Er, pch=19, col="black")
points(max.w3$sdp, max.w3$Er, pch=19, col="black")
points(max.w4$sdp, max.w4$Er, pch=19, col="black")
abline(c(rf, max.effectivness$sharp), lty=3)

legend(legend = c("Zbiór możliwości bez krótkiej sprzedaży", "Portfel minimalnego ryzyka", "Portfel maksymalnej efektywności", 
                  "Portfel jedno elementowy", "Portfele efektywne", "CLM"), 
       pch = c(19, 19, 19, 19, -1, -1, -1, -1), 
       col = c("light yellow", "green", "blue", "black", "light coral", "black", "black"), 
       lty=c(0, 0, 0, 0, 1, 2, 2, 2),
       "bottomright")



require("tseries")
require("zoo")
require("gplots")

con <- url("https://finance.yahoo.com")
#
INTC <- get.hist.quote(instrument = "INTC", quote = c("Adj", "Vol"), start = "2006-03-20")
MSFT <- get.hist.quote(instrument = "MSFT", quote = c("Adj", "Vol"), start = "2006-03-20")
CSCO <- get.hist.quote(instrument = "CSCO", quote = c("Adj", "Vol"), start = "2006-03-20")
QCOM <- get.hist.quote(instrument = "QCOM", quote = c("Adj", "Vol"), start = "2006-03-20")
#
par(mfrow=c(2,2))
plot(INTC$Adj, main="INTC",  xlab="Date", ylab="Price")
plot(MSFT$Adj, main="MSFT",  xlab="Date", ylab="Price")
plot(CSCO$Adj, main="CSCO",  xlab="Date", ylab="Price")
plot(QCOM$Adj, main="QCOM",  xlab="Date", ylab="Price")
#
par(mfrow=c(1,1))
plot(INTC$Adj, ylim=c(0,100), xlab="Date", ylab="Price", main="Stockprices")
lines(MSFT$Adj, col = "2")
lines(CSCO$Adj, col = "3")
lines(QCOM$Adj, col = "4")
legend("topleft", legend = c("INTEL", "MICROSOFT", "CISCO", "QUALCOMM"), lty=c(1,1), col=c(1,2,3,4))
#
INTC_S <- as.numeric(INTC$Adj)
INTC_R <- (INTC_S[-1] - INTC_S[1:(length(INTC_S)-1)])/INTC_S[1:(length(INTC_S)-1)]
#
MSFT_S <- as.numeric(MSFT$Adj)
MSFT_R <- (MSFT_S[-1] - MSFT_S[1:(length(MSFT_S)-1)])/MSFT_S[1:(length(MSFT_S)-1)]
#
CSCO_S <- as.numeric(CSCO$Adj)
CSCO_R <- (CSCO_S[-1] - CSCO_S[1:(length(CSCO_S)-1)])/CSCO_S[1:(length(CSCO_S)-1)]
#
QCOM_S <- as.numeric(QCOM$Adj)
QCOM_R <- (QCOM_S[-1] - QCOM_S[1:(length(QCOM_S)-1)])/QCOM_S[1:(length(QCOM_S)-1)]
##########################
par(mfrow=c(2,2))
plot(INTC_R, main="Return")
plot(QCOM_R, main="Return")
plot(MSFT_R, main="Return")
plot(CSCO_R, main="Return")
#
########### ------------- ###########
summary(INTC_R)
########### ------------- ###########
summary(MSFT_R)
########### ------------- ###########
summary(CSCO_R)
########### ------------- ###########
summary(QCOM_R)
########### ------------- ###########
#
cor(INTC_R, QCOM_R)
cor(MSFT_R, CSCO_R)
#
quantile(INTC_R, c(0.05, 0.95))
quantile(MSFT_R, c(0.05, 0.95))
quantile(CSCO_R, c(0.05, 0.95))
quantile(QCOM_R, c(0.05, 0.95))
#
par(mfrow=c(2,2))
plot(density(INTC_R))
abline(v = mean(INTC_R), col = 2)
plot(density(QCOM_R))
abline(v = mean(QCOM_R), col = 2)
plot(density(MSFT_R))
abline(v = mean(MSFT_R), col = 2)
plot(density(CSCO_R))
abline(v = mean(CSCO_R), col = 2)
#
########### ------------- ###################### ------------- ###################### ------------- ###########
#
#INTC - QCOM #####################################################
A1 <- seq(-1,0,0.01)
A1 <- A1*-1
B1 <- seq(-0,1,0.01)
RA1 <- mean(INTC_R)
SA1 <- sqrt(var(INTC_R))
RB1 <- mean(QCOM_R)
SB1 <- sqrt(var(QCOM_R))
R1 <- A1*RA1+B1*RB1 # Renditenreihe mit 1% Schritten Start bei 100% A
CORREL1 <- cor(INTC_R, QCOM_R)
V1 <- A1^2*SA1^2+B1^2*SB1^2+2*A1*B1*SA1*SB1*CORREL1
RISK1 <- sqrt(V1)
#
EF1 <- cbind(A1,B1,R1,RISK1)
#
par (mfrow=c(1,2))
plot(RISK1,R1, main="INTC [A] - QCOM [B] ")
#
MRP <- EF1[which.max(EF1[,3]),]
MVP <- EF1[which.min(EF1[,4]),]
#
points(MRP[4],MRP[3], col = 2, lwd =4, pch=16)
points(MVP[4],MVP[3], col = 3, lwd =4, pch=16)
#
legend("topright", legend = c("MAX RETURN", "MIN RISK"), col=c(2,3), lty=0, pch=19)
#
# MSFT - CSCO #####################################################
A2 <- seq(-1,0,0.01)
A2 <- A2*-1
B2 <- seq(0,1,0.01)
RA2 <- mean(MSFT_R)
SA2 <- sqrt(var(MSFT_R))
RB2 <- mean(CSCO_R)
SB2 <- sqrt(var(CSCO_R))
R2 <- A2*RA2+B2*RB2 # Renditenreihe mit 1% Schritten Start bei 100% A
CORREL2 <- cor(MSFT_R, CSCO_R)
V2 <- A2^2*SA2^2+B2^2*SB2^2+2*A2*B2*SA2*SB2*CORREL2
RISK2 <- sqrt(V2)
#
EF2 <- cbind(A2,B2,R2,RISK2)
#par (mfrow=c(1,1))
plot(RISK2,R2, main="MSFT [A] - CSCO [B] ")
#
MRP2 <- EF2[which.max(EF2[,3]),]
MVP2 <- EF2[which.min(EF2[,4]),]
#
points(MRP2[4],MRP2[3], col = 2, lwd =4, pch=16)
points(MVP2[4],MVP2[3], col = 3, lwd =4, pch=16)
#
legend("topright", legend = c("MAX RETURN", "MIN RISK"), col=c(2,3), lty=0, pch=19)
######################################################
########### ------------- ###########
######## INTC [A] - QCOM [B] ########
MRP # - Max Return
MVP # - Min Risk
########### ------------- ###########
######## MSFT [A] - CSCO [B] ########
MRP2 # - Max Return
MVP2 # - Min Risk


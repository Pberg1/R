#
MV_1 <- function(c=5, NV=100, s1=0.04, s2=0.06) c/(1+s1) + (c+NV)/(1+s2)^2
MV_2 <- function(c=5, x, NV=100) c/(1+x) + (c+NV)/(1+x)^2
#
fooX <- function(c=5 , NV=100 , s1=0.04, s2=0.06, tl=0.0001){
  #
  x <- 0 #Startwert (Zinssatz)
  #Coupon
  #SpotRate1
  #SpotRate2
  #Nominale
  #
  X1 <- round(MV_1(c, NV, s1, s2), 5)
  X2 <- round(MV_2(c, x, NV), 5)
  #
  while (X1 < X2){
    #
    X1 <- round(MV_1(c, NV, s1, s2), 5)
    X2 <- round(MV_2(c, x, NV), 5)
    #
    x <- x + tl #steigerung in 0.0001er Schritten
  }
  #
x - tl}
#
fooX() #0.0595 für 5,100,0.004,0.006,0.0001
#
############################################### advanced ###############################################
#
#
#
MV_1_1 <- function(c2=5, NV2=100, T=2, s=c(0.04,0.06)) {#T == Anzahl an s
  #
  k <- seq(-T+1,0,1) #Hilfsfkt
  k <- k * -1 # Hilfsfkt
  #
  sum(c2/((1+s[T-k])^(T-k))) + (NV2/(1+s[T])^T)
}
MV_1_1()
#
MV_2_2 <- function(c2=5, y=0.0595, NV2=100, T=2) {  
  #
  k <- seq(-T+1,0,1) #Hilfsfkt
  k <- k * -1 # Hilfsfkt
  z1 <- sum(c2/(1+y)^(T-k))
  z2 <- (NV2)/(1+y)^T
  z1+z2
}
MV_2_2()
  #
fooY <- function(c2=5, NV2=100, T=2, s=c(0.04,0.06), tl2=0.0001) { #T == Anzahl an s !!
  #
  y <- 0 #Startwert (Zinssatz)
  #
  k <- seq(-T+1,0,1) #Hilfsfkt
  k <- k * -1 # Hilfsfkt
  #
  X3 <- round(MV_1_1(c2, NV2, T, s), 5)
  X4 <- round(MV_2_2(c2, y, NV2), 5)
  #
   while (X3 < X4){
    #
    X3 <- round(MV_1_1(c2, NV2, T, s), 5)
    X4 <- round(MV_2_2(c2, y, NV2, T), 5)
    #
    y <- y + tl2 #steigerung in z.B. 0.0001er Schritten
  }
  
  y - tl2}
#
fooY()
# s und T in fooY eingeben -> Ergebniss in MV_2_2 einsetzten -> s und T in MV1_1 einsetzten -> Ergebnisse
# sollten gleich sein (Fehler durch runden)

# (5/(1.04^1))+(5/(1.06^2))+(100/(1.06^2))=(5/((1+x)^1))+(5/((1+x)^2))+(100/((1+x)^2)) -> x=0.059499
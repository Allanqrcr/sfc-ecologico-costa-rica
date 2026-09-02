suppressMessages(library(sfcr))
PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848
W<-0.0404327375264023; RHO<-0.218080022778761
KAPPA<-6.20802562807628E-07*1.4071907; A1<-0.7219; A2<-0.3681
PHI<-2.07644144656066; SIG<-0.0410390766992924
S0<-SIG*PHI*PIB22/RHO; D0<-KAPPA*S0
H0<-(PIB22*(1-A1*(1-TH-D0))-G)/A2
YD0<-PIB22*(1-TH-D0); C0<-A1*YD0+A2*H0

eqs <- sfcr_set(
  TXs~TXd, Nd~Y/W, Ns~Nd, DAM~damage*Y,
  YD~W*Ns-TXs-DAM, Cd~alpha1*YD+alpha2*Hh[-1], Cs~Cd, Gs~Gd, Y~Cs+Gs,
  TXd~theta*W*Ns, Hh~YD-Cd+Hh[-1], Hs~Gd-TXd-DAM+Hs[-1],
  E~phi*Y, CO2~sigma*E, Cstock~Cstock[-1]+CO2-rho*Cstock[-1],
  damage~kappa*Cstock, Y_eff~Y*(1-damage))
ext <- eval(parse(text=sprintf(
 "sfcr_set(Gd~%.15g, W~%.15g, alpha1~%.15g, alpha2~%.15g, theta~%.15g,
   phi~%.15g, sigma~%.15g, rho~%.15g, kappa~%.15g)",G,W,A1,A2,TH,PHI,SIG,RHO,KAPPA)))
ini <- eval(parse(text=sprintf(
 "sfcr_set(Y~%.15g, Hh~%.15g, Hs~%.15g, Cstock~%.15g, Cd~%.15g, Ns~%.15g)",
  PIB22,H0,H0,S0,C0,PIB22/W)))

r <- tryCatch({m<-sfcr_baseline(eqs,ext,periods=29,initial=ini,hidden=c("Hh"="Hs"))
  sprintf("ACEPTADO por sfcr. max|Hh - Hs| = %.3e", max(abs(m$Hh-m$Hs)))},
  error=function(e) paste("RECHAZADO:",conditionMessage(e)))
cat("V1. Ecuacion redundante con hidden = c('Hh'='Hs'):\n    ", r, "\n")
m <- sfcr_baseline(eqs,ext,periods=29,initial=ini)
cat(sprintf("V2. Producto del periodo 1 = %.5f  vs PIB observado %.5f\n", m$Y[1], PIB22))
cat(sprintf("V3. Estado estacionario analitico = %.1f  vs simulado en 2050 = %.1f\n",
    G/(TH+m$damage[29]), m$Y[29]))

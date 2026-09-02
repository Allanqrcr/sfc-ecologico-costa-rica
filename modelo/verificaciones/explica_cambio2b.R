suppressMessages(library(sfcr))
## Especificacion CORREGIDA
eqs <- sfcr_set(
  TXs~TXd, Nd~Y/W, Ns~Nd, DAM~damage*Y,
  YD~W*Ns-TXs-DAM, Cd~alpha1*YD+alpha2*Hh[-1], Cs~Cd, Gs~Gd, Y~Cs+Gs,
  TXd~theta*W*Ns, Hh~YD-Cd+Hh[-1], Hs~Gd-TXd-DAM+Hs[-1],
  E~phi*Y, CO2~sigma*E, Cstock~Cstock[-1]+CO2-rho*Cstock[-1],
  damage~kappa*Cstock, Y_eff~Y*(1-damage))
PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848
A1<-0.7219; A2<-0.3681; RHO<-0.218080022778761; KA<-6.20802562807628E-07*1.159
S0<-0.0410390766992924*2.07644144656066*PIB22/RHO; D0<-KA*S0
H0<-(PIB22*(1-A1*(1-TH-D0))-G)/A2
ext <- eval(parse(text=sprintf(
 "sfcr_set(Gd~%.15g, W~0.0404327375264023, alpha1~%.15g, alpha2~%.15g, theta~%.15g,
  phi~2.07644144656066, sigma~0.0410390766992924, rho~%.15g, kappa~%.15g)",G,A1,A2,TH,RHO,KA)))
ini <- eval(parse(text=sprintf(
 "sfcr_set(Y~%.15g, Hh~%.15g, Hs~%.15g, Cstock~%.15g, Cd~%.15g, Ns~%.15g)",
  PIB22, H0, H0, S0, PIB22-G, PIB22/0.0404327375264023)))

cat("=== VERIFICACION: la libreria ya NO rechaza el modelo ===\n")
r <- tryCatch({m<-sfcr_baseline(eqs, ext, periods=29, initial=ini, hidden=c("Hh"="Hs"))
               sprintf("ACEPTADO. max|Hh - Hs| = %.2e", max(abs(m$Hh-m$Hs)))},
  error=function(e) paste("RECHAZADO:", conditionMessage(e)))
cat("  Con hidden = c('Hh'='Hs'):", r, "\n")

m <- sfcr_baseline(eqs, ext, periods=29, initial=ini)
cat("\n=== LA CONTABILIDAD DEL PERIODO 15, YA CUADRADA ===\n")
i<-15
cat(sprintf("  Producto                 Y      = %10.1f\n", m$Y[i]))
cat(sprintf("  Masa salarial            W*Ns   = %10.1f   <- ahora IGUAL al producto\n", m$W[i]*m$Ns[i]))
cat(sprintf("  menos impuestos          TXd    = %10.1f\n", m$TXd[i]))
cat(sprintf("  menos costo del danio    DAM    = %10.1f\n", m$DAM[i]))
cat(sprintf("  = ingreso disponible     YD     = %10.1f\n", m$YD[i]))
cat(sprintf("  menos consumo            Cd     = %10.1f\n", m$Cd[i]))
cat(sprintf("  = ahorro de los hogares         = %10.1f\n", m$YD[i]-m$Cd[i]))
cat(sprintf("  Gasto publico - impuestos - danio = %8.1f   <- la contrapartida del gobierno\n",
    G-m$TXd[i]-m$DAM[i]))
cat(sprintf("  Riqueza hogares  Hh = %10.1f  |  Emision gobierno  Hs = %10.1f\n", m$Hh[i], m$Hs[i]))
cat(sprintf("  Diferencia = %.2e\n", m$Hh[i]-m$Hs[i]))

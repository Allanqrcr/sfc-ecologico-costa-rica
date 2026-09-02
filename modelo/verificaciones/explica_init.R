suppressMessages(library(sfcr))
eqs <- sfcr_set(
  TXs~TXd, YD~W*Ns-TXs, Cd~alpha1*YD+alpha2*Hh[-1], Hh~YD-Cd+Hh[-1],
  Ns~Nd, Nd~Y_eff/W, Cs~Cd, Gs~Gd, Y~Cs+Gs, TXd~theta*W*Ns, Hs~Gd-TXd+Hs[-1],
  E~phi*Y, CO2~sigma*E, Cstock~Cstock[-1]+CO2-rho*Cstock[-1],
  damage~kappa*Cstock, Y_eff~Y*(1-damage))
ext <- sfcr_set(Gd~13527.2509115691, W~0.0404327375264023, alpha1~0.7707,
  alpha2~0.3595, theta~0.129036878894848, phi~2.07644144656066,
  sigma~0.0410390766992924, rho~0.218080022778761, kappa~6.20802562807628E-07)

m <- sfcr_baseline(eqs, ext, periods=5)   # SIN argumento 'initial', como el codigo original
cat("=== PERIODO 1 CUANDO NO SE PASA 'initial' ===\n")
print(round(as.data.frame(m[1, c("Y","Cd","Hh","Hs","Ns","Cstock","damage")]), 8))
cat("\n=== PERIODOS 1 A 5 (variable Y) ===\n")
print(round(m$Y, 2))
cat("\nPIB observado de 2022 =", 87862.81, "\n")

cat("\n\n=== LA RED DE SEGURIDAD QUE NO SE ACTIVO: argumento 'hidden' ===\n")
cat("sfcr puede verificar la ecuacion redundante si se le indica cual es.\n")
r <- tryCatch({
  sfcr_baseline(eqs, ext, periods=5, hidden=c("Hh"="Hs")); "sin aviso" },
  error=function(e) paste("ERROR:", conditionMessage(e)),
  warning=function(w) paste("AVISO:", conditionMessage(w)))
cat("Resultado de correr el modelo ORIGINAL con hidden=c('Hh'='Hs'):\n  ", r, "\n")

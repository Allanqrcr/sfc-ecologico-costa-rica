suppressMessages(library(sfcr))
eqs <- sfcr_set(
  TXs~TXd, YD~W*Ns-TXs, Cd~alpha1*YD+alpha2*Hh[-1], Hh~YD-Cd+Hh[-1],
  Ns~Nd, Nd~Y_eff/W, Cs~Cd, Gs~Gd, Y~Cs+Gs, TXd~theta*W*Ns, Hs~Gd-TXd+Hs[-1],
  E~phi*Y, CO2~sigma*E, Cstock~Cstock[-1]+CO2-rho*Cstock[-1],
  damage~kappa*Cstock, Y_eff~Y*(1-damage))
ext <- sfcr_set(Gd~13527.2509115691, W~0.0404327375264023, alpha1~0.7707,
  alpha2~0.3595, theta~0.129036878894848, phi~2.07644144656066,
  sigma~0.0410390766992924, rho~0.218080022778761, kappa~6.20802562807628E-07)
m <- sfcr_baseline(eqs, ext, periods=60)

cat("=== EL DESAJUSTE, PERIODO A PERIODO ===\n")
cat("En el modelo SIM debe cumplirse  Hh = Hs  en todo periodo.\n\n")
p <- c(2,3,5,10,20,40,60)
cat(sprintf("%7s %11s %11s %11s %11s %11s\n","periodo","Y","Y_eff","Hh","Hs","Hh - Hs"))
for(i in p) cat(sprintf("%7d %11.1f %11.1f %11.1f %11.1f %11.1f\n",
  i, m$Y[i], m$Y_eff[i], m$Hh[i], m$Hs[i], m$Hh[i]-m$Hs[i]))

cat("\n=== DE DONDE SALE EXACTAMENTE LA BRECHA ===\n")
cat(sprintf("  Brecha en el periodo 60          = %12.1f\n", m$Hh[60]-m$Hs[60]))
cat(sprintf("  Suma acumulada de (Y - Y_eff)    = %12.1f\n", -sum(m$Y-m$Y_eff)))
cat(sprintf("  Diferencia entre ambas           = %12.6f\n",
    (m$Hh[60]-m$Hs[60]) + sum(m$Y-m$Y_eff)))

cat("\n=== EL ORIGEN: DOS PRODUCTOS DISTINTOS EN EL MISMO SISTEMA ===\n")
i <- 29
cat(sprintf("  Periodo %d:\n", i))
cat(sprintf("    El mercado de bienes cierra con   Y     = Cs + Gs      = %10.1f\n", m$Y[i]))
cat(sprintf("    El empleo se determina con        Y_eff = Y*(1-dano)   = %10.1f\n", m$Y_eff[i]))
cat(sprintf("    La masa salarial resulta          W*Ns                 = %10.1f\n", m$W[i]*m$Ns[i]))
cat(sprintf("    Se produce por valor de %.1f pero solo se reparte %.1f.\n", m$Y[i], m$W[i]*m$Ns[i]))
cat(sprintf("    Los %.1f de diferencia no los registra ningun sector.\n", m$Y[i]-m$W[i]*m$Ns[i]))

cat("\n=== LO QUE LA LIBRERIA HABRIA DICHO ===\n")
r <- tryCatch({sfcr_baseline(eqs, ext, periods=10, hidden=c("Hh"="Hs")); "sin aviso"},
  error=function(e) conditionMessage(e))
cat("  Con hidden = c('Hh' = 'Hs'):\n  ", r, "\n")

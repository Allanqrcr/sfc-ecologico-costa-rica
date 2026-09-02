suppressMessages(library(sfcr))
eqs <- sfcr_set(TXs~TXd, YD~W*Ns-TXs, Cd~alpha1*YD+alpha2*Hh[-1], Hh~YD-Cd+Hh[-1],
  Ns~Nd, Nd~Y_eff/W, Cs~Cd, Gs~Gd, Y~Cs+Gs, TXd~theta*W*Ns, Hs~Gd-TXd+Hs[-1],
  E~phi*Y, CO2~sigma*E, Cstock~Cstock[-1]+CO2-rho*Cstock[-1],
  damage~kappa*Cstock, Y_eff~Y*(1-damage))
ext <- sfcr_set(Gd~13527.2509115691, W~0.0404327375264023, alpha1~0.7707,
  alpha2~0.3595, theta~0.129036878894848, phi~2.07644144656066,
  sigma~0.0410390766992924, rho~0.218080022778761, kappa~6.20802562807628E-07)
m <- sfcr_baseline(eqs, ext, periods=60)
cat("Ventanas de 29 periodos cercanas al 25% reportado:\n")
for(a in 6:9) cat(sprintf("  periodos %2d a %2d : %5.1f%%\n",a,a+28,100*(m$Y[a+28]/m$Y[a]-1)))
cat("\nY el PIB observado de 2022 (87.863) aparece en el modelo original\n")
i<-which.min(abs(m$Y-87862.8)); cat(sprintf("  alrededor del periodo %d (Y = %.0f)\n",i,m$Y[i]))

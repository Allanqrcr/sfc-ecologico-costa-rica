PIB22 <- 87862.80504186137; G <- 13527.2509115691; TH <- 0.129036878894848
A1 <- 0.8011; A2 <- 0.1555; RHO <- 0.218080022778761
KA <- 6.20802562807628E-07 * 1.159
PHI <- 2.07644144656066; SIG <- 0.0410390766992924

cat("PASO 1. Emisiones del anio base\n")
CO2 <- SIG*PHI*PIB22
cat(sprintf("  CO2(2022) = sigma * phi * PIB = %.7f * %.7f * %.2f = %.2f Gg\n\n",SIG,PHI,PIB22,CO2))

cat("PASO 2. Acervo de carbono inicial: el nivel de reposo\n")
cat("  Si las emisiones se mantuvieran constantes, el acervo converge a S* tal que\n")
cat("     S* = (1-rho)*S* + CO2   =>   S* * rho = CO2   =>   S* = CO2 / rho\n")
S0 <- CO2/RHO
cat(sprintf("  S(2021) = %.2f / %.6f = %.1f Gg\n\n",CO2,RHO,S0))

cat("PASO 3. Danio del anio base\n")
D0 <- KA*S0
cat(sprintf("  d(2022) = kappa * S = %.6e * %.1f = %.6f  ->  %.2f%% del producto\n\n",KA,S0,D0,100*D0))

cat("PASO 4. Riqueza inicial: se DESPEJA para que el modelo reproduzca el PIB de 2022\n")
cat("  La ecuacion del producto en el estado del periodo 1 es\n")
cat("     Y = ( alpha2*H(-1) + G ) / ( 1 - alpha1*(1 - theta - d) )\n")
den <- 1 - A1*(1-TH-D0)
cat(sprintf("  denominador = 1 - %.4f*(1 - %.6f - %.6f) = %.6f\n",A1,TH,D0,den))
cat("  Se despeja H(-1) imponiendo Y = PIB observado de 2022:\n")
cat("     H(-1) = ( Y*denominador - G ) / alpha2\n")
H0 <- (PIB22*den - G)/A2
cat(sprintf("  H(2021) = ( %.2f * %.6f - %.2f ) / %.4f = %.1f millones USD\n",PIB22,den,G,A2,H0))
cat(sprintf("            equivale al %.1f%% del PIB\n\n",100*H0/PIB22))

cat("COMPROBACION. Se vuelve a calcular Y con esos valores:\n")
Yc <- (A2*H0 + G)/den
cat(sprintf("  Y(2022) = ( %.4f*%.1f + %.2f ) / %.6f = %.5f\n",A2,H0,G,den,Yc))
cat(sprintf("  PIB observado de 2022                                = %.5f\n",PIB22))
cat(sprintf("  diferencia = %.2e\n",abs(Yc-PIB22)))

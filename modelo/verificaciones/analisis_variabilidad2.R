G<-13527.2509115691; TH<-0.129036878894848
phG<-2.07644144656066; sgG<-0.0410390766992924
phR<-1.28975587832427; sgR<-0.00882132065193575
PHI_P<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
PIB22<-87862.80504186137; SIG_P<-META/(PHI_P*PIB22)
phP<-PHI_P[6]; sgP<-SIG_P[6]     # composicion energetica de 2050 del plan
Yss<-function(a) (-TH+sqrt(TH^2+4*G*a))/(2*a)

cat("=== ANCLAJE ROBUSTO: la razon entre escenarios NO depende de kappa ni de rho ===\n")
cat("El parametro compuesto es a = kappa*(sigma*phi)/rho. Entre escenarios, kappa y rho\n")
cat("son comunes, asi que la razon a_i/a_j = (sigma_i*phi_i)/(sigma_j*phi_j) depende\n")
cat("SOLO de los datos nacionales de energia y emisiones.\n\n")
spG<-sgG*phG; spR<-sgR*phR; spP<-sgP*phP
cat(sprintf("  sigma*phi  continuidad = %.6f\n",spG))
cat(sprintf("  sigma*phi  renovable   = %.6f   -> divide 'a' por %.2f\n",spR,spG/spR))
cat(sprintf("  sigma*phi  plan (2050) = %.6f   -> divide 'a' por %.2f\n",spP,spG/spP))

cat("\n=== VENTAJA SEGUN EL NIVEL DE DANO QUE SE SUPONGA ===\n")
cat("Se parametriza por el dano del escenario de continuidad, que es lo unico\n")
cat("realmente incierto. Todo lo demas queda fijado por los datos nacionales.\n\n")
cat(sprintf("%-12s %10s %10s %10s %10s %10s\n","dano cont.","PIB cont.","PIB renov","vent.renov","PIB plan","vent.plan"))
for(dG in c(0.010,0.015,0.0247,0.035,0.050,0.070)){
  YG<-G/(TH+dG); aG<-dG*(TH+dG)/G
  aR<-aG*spR/spG; YR<-Yss(aR)
  aP<-aG*spP/spG; YP<-Yss(aP)
  cat(sprintf("%11.2f%% %10.0f %10.0f %9.1f%% %10.0f %9.1f%%\n",
      100*dG,YG,YR,100*(YR/YG-1),YP,100*(YP/YG-1)))}
cat("\nLa fila de 2,47% es la calibracion central adoptada.\n")

cat("\n=== DESCOMPOSICION DE LA INCERTIDUMBRE ===\n")
cat("Fuente                                  Efecto sobre la ventaja renovable\n")
cat("---------------------------------------------------------------------\n")
cat("Datos de energia y emisiones (sigma,phi)  NULO sobre la razon; fijan el factor 7.49\n")
cat("Gasto publico G y tasa impositiva theta   Comunes a todos los escenarios; se cancelan\n")
cat("Propensiones alpha1, alpha2               NULO sobre el estado estacionario\n")
cat("Nivel del dano (kappa y rho conjuntos)    UNICA fuente relevante\n")

PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848; W<-0.0404327375264023
RHO<-0.218080022778761; KA<-6.20802562807628E-07*1.159; A1<-0.7219; A2<-0.3681
PHI<-2.07644144656066; SIG<-0.0410390766992924
CO2<-SIG*PHI*PIB22; S0<-CO2/RHO; D0<-KA*S0
den<-1-A1*(1-TH-D0); H0<-(PIB22*den-G)/A2
YD<-PIB22*(1-TH-D0); Cd<-A1*YD+A2*H0; Hstar<-(1-A1)*YD/A2
cat("=== VALORES INICIALES AJUSTADOS (periodo 1 = 2022) ===\n")
cat(sprintf("  Producto              Y   = %12.2f  (PIB observado %.2f)\n",(A2*H0+G)/den,PIB22))
cat(sprintf("  Energia               E   = %12.1f TJ\n",PHI*PIB22))
cat(sprintf("  Emisiones             CO2 = %12.1f Gg\n",CO2))
cat(sprintf("  Acervo de carbono     S   = %12.1f Gg   (= CO2/rho)\n",S0))
cat(sprintf("  Danio                 d   = %12.4f     (%.2f%% del producto)\n",D0,100*D0))
cat(sprintf("  Riqueza inicial       H   = %12.1f      (%.1f%% del PIB)\n",H0,100*H0/PIB22))
cat(sprintf("  Ingreso disponible    YD  = %12.1f\n",YD))
cat(sprintf("  Consumo               C   = %12.1f\n",Cd))
cat(sprintf("  Gasto publico         G   = %12.1f\n",G))
cat(sprintf("  Recaudacion           T   = %12.1f\n",TH*PIB22))
cat(sprintf("  Deficit                   = %12.1f      (%.3f%% del PIB)\n",G-TH*PIB22,100*(G-TH*PIB22)/PIB22))
cat(sprintf("  Empleo                N   = %12.0f personas\n",PIB22/W))
cat("\n=== COHERENCIA PATRIMONIAL ===\n")
cat(sprintf("  Riqueza inicial calibrada   H(2021) = %9.1f\n",H0))
cat(sprintf("  Riqueza de estado estacionario H*   = %9.1f   ((1-a1)*YD*/a2)\n",Hstar))
cat(sprintf("  Diferencia = %.2f%%  -> la economia de 2022 arranca practicamente\n",100*(H0/Hstar-1)))
cat("  en su estado estacionario tambien en la dimension patrimonial.\n")
cat("\n=== EVOLUCION DE LA RIQUEZA INICIAL SEGUN LA CALIBRACION ===\n")
cat(sprintf("%-40s %8s %8s %12s %9s\n","calibracion","alpha1","alpha2","H(2021)","%PIB"))
for(p in list(c("Codigo original (sin inicializar)",0.7707,0.3595,"0","0"),
              c("Manuscrito, ya inicializado",0.7707,0.3595,"",""),
              c("Etapa intermedia (serie de ahorro)",0.8011,0.1555,"",""),
              c("Actual: deficit acumulado",0.7219,0.3681,"",""))){
  a1<-as.numeric(p[2]); a2<-as.numeric(p[3])
  h<-(PIB22*(1-a1*(1-TH-D0))-G)/a2
  if(p[4]=="0") cat(sprintf("%-40s %8.4f %8.4f %12s %9s\n",p[1],a1,a2,"0","0%"))
  else cat(sprintf("%-40s %8.4f %8.4f %12.1f %8.1f%%\n",p[1],a1,a2,h,100*h/PIB22))}

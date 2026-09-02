PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848; W<-0.0404327375264023
RHO<-0.218080022778761; KA<-6.20802562807628E-07*1.158355; A1<-0.7219; A2<-0.3681
phG<-2.07644144656066; sgG<-0.0410390766992924
PHI<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
SIGC<-META/(PHI*PIB22); AN<-c(2025,2030,2035,2040,2045,2050)
S0<-sgG*phG*PIB22/RHO; D0<-KA*S0; H0<-(PIB22*(1-A1*(1-TH-D0))-G)/A2
CO2_22<-sgG*phG*PIB22

## OPCION A (vigente): emisiones endogenas, CO2 = sigma*phi*Y
pt<-approx(c(2022,AN),c(phG,PHI),xout=2022:2050)$y
st<-approx(c(2022,AN),c(sgG,SIGC),xout=2022:2050)$y
simA<-function(){Hh<-H0;Cs<-S0;Y<-numeric(29);E<-numeric(29)
 for(t in 1:29){f<-function(y){dd<-KA*((1-RHO)*Cs+st[t]*pt[t]*y);(A2*Hh+G)/(1-A1*(1-TH-dd))-y}
  y<-uniroot(f,c(1,1e7),tol=1e-10)$root; co2<-st[t]*pt[t]*y; Cs<-(1-RHO)*Cs+co2; dd<-KA*Cs
  YD<-y*(1-TH-dd); Hh<-Hh+YD-(A1*YD+A2*Hh); Y[t]<-y; E[t]<-co2}
 list(Y=Y,E=E)}

## OPCION B: emisiones EXOGENAS, fijadas a la trayectoria del plan
meta_t<-approx(c(2022,AN),c(CO2_22,META),xout=2022:2050)$y
simB<-function(){Hh<-H0;Cs<-S0;Y<-numeric(29)
 for(t in 1:29){Cs<-(1-RHO)*Cs+meta_t[t]; dd<-KA*Cs
  y<-(A2*Hh+G)/(1-A1*(1-TH-dd)); YD<-y*(1-TH-dd); Hh<-Hh+YD-(A1*YD+A2*Hh); Y[t]<-y}
 list(Y=Y,E=meta_t)}

a<-simA(); b<-simB()
cat("=== EMISIONES: OPCION A (vigente) vs OPCION B (exogenas a la meta) ===\n")
cat(sprintf("%6s %10s %12s %9s %12s\n","anio","meta","A endogenas","desvio","B exogenas"))
for(y in AN){i<-y-2021
 cat(sprintf("%6d %10.1f %12.1f %8.1f%% %12.1f\n",y,meta_t[i],a$E[i],100*(a$E[i]/meta_t[i]-1),b$E[i]))}

## escenario de continuidad para las comparaciones
simG<-function(){Hh<-H0;Cs<-S0;Y<-numeric(29)
 for(t in 1:29){f<-function(y){dd<-KA*((1-RHO)*Cs+sgG*phG*y);(A2*Hh+G)/(1-A1*(1-TH-dd))-y}
  y<-uniroot(f,c(1,1e7),tol=1e-10)$root; Cs<-(1-RHO)*Cs+sgG*phG*y; dd<-KA*Cs
  YD<-y*(1-TH-dd); Hh<-Hh+YD-(A1*YD+A2*Hh); Y[t]<-y}; Y}
g<-simG()

cat("\n=== EFECTO SOBRE LOS RESULTADOS DEL PLAN EN 2050 ===\n")
cat(sprintf("  Continuidad                     : %9.1f\n", g[29]))
cat(sprintf("  Plan, opcion A (vigente)        : %9.1f   ventaja %+.2f%%\n", a$Y[29], 100*(a$Y[29]/g[29]-1)))
cat(sprintf("  Plan, opcion B (meta exacta)    : %9.1f   ventaja %+.2f%%\n", b$Y[29], 100*(b$Y[29]/g[29]-1)))
cat(sprintf("  Mejora que aporta la opcion B   : %9.1f   (%+.2f%% de PIB, %+.2f pp de ventaja)\n",
  b$Y[29]-a$Y[29], 100*(b$Y[29]/a$Y[29]-1), 100*(b$Y[29]/g[29]-1)-100*(a$Y[29]/g[29]-1)))

cat("\n=== QUE MEZCLA ENERGETICA EXIGIRIA LA OPCION B ===\n")
cat(sprintf("%6s %14s %16s %10s\n","anio","sigma opcion A","sigma que exige B","diferencia"))
for(y in c(2030,2040,2050)){i<-y-2021
 sB<-meta_t[i]/(pt[i]*b$Y[i])
 cat(sprintf("%6d %14.7f %16.7f %9.1f%%\n",y,st[i],sB,100*(sB/st[i]-1)))}

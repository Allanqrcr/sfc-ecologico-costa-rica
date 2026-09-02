PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848
RHO<-0.218080022778761; KA<-6.20802562807628E-07*1.159; A1<-0.7219; A2<-0.3681
phG<-2.07644144656066; sgG<-0.0410390766992924
PHI<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
SIG<-META/(PHI*PIB22); ANIOS<-c(2025,2030,2035,2040,2045,2050)
S0<-sgG*phG*PIB22/RHO; D0<-KA*S0; H0<-(PIB22*(1-A1*(1-TH-D0))-G)/A2

## (a) METODO ORIGINAL: cada tramo como linea base independiente desde acervos CERO
tramo_aislado<-function(ph,sg,n=60){
  Hh<-0; Cs<-0; Y<-numeric(n); St<-numeric(n)
  for(t in 1:n){f<-function(y){dd<-KA*((1-RHO)*Cs+sg*ph*y); (A2*Hh+G)/(1-A1*(1-TH-dd))-y}
    y<-uniroot(f,c(1e-9,1e7),tol=1e-10)$root; Cs<-(1-RHO)*Cs+sg*ph*y; dd<-KA*Cs
    YD<-y*(1-TH-dd); Hh<-Hh+YD-(A1*YD+A2*Hh); Y[t]<-y; St[t]<-Cs}
  list(Y=Y,S=St)}

## (b) METODO CORREGIDO: una sola trayectoria continua desde el ano base
pt<-approx(c(2022,ANIOS),c(phG,PHI),xout=2022:2050)$y
st<-approx(c(2022,ANIOS),c(sgG,SIG),xout=2022:2050)$y
Hh<-H0; Cs<-S0; Yc<-numeric(29); Sc<-numeric(29)
for(t in 1:29){f<-function(y){dd<-KA*((1-RHO)*Cs+st[t]*pt[t]*y); (A2*Hh+G)/(1-A1*(1-TH-dd))-y}
  y<-uniroot(f,c(1,1e7),tol=1e-10)$root; Cs<-(1-RHO)*Cs+st[t]*pt[t]*y; dd<-KA*Cs
  YD<-y*(1-TH-dd); Hh<-Hh+YD-(A1*YD+A2*Hh); Yc[t]<-y; Sc[t]<-Cs}

cat("=== ACERVO DE CARBONO: TRAMO AISLADO vs TRAYECTORIA CONTINUA ===\n")
cat(sprintf("%6s %14s %16s %14s %10s\n","anio","S tramo aislado","S continua","diferencia","%"))
for(i in seq_along(ANIOS)){
  a<-tramo_aislado(PHI[i],SIG[i]); idx<-ANIOS[i]-2021
  Sa<-a$S[60]                       # el tramo aislado converge a su propio reposo
  Sb<-Sc[idx]
  cat(sprintf("%6d %14.1f %16.1f %14.1f %9.1f%%\n",ANIOS[i],Sa,Sb,Sa-Sb,100*(Sa/Sb-1)))}

cat("\n=== PRODUCTO EN 2050 ===\n")
a50<-tramo_aislado(PHI[6],SIG[6])
cat(sprintf("  Tramo 2050 aislado, en su estado de reposo : %9.1f\n", a50$Y[60]))
cat(sprintf("  Trayectoria continua, ano 2050             : %9.1f\n", Yc[29]))
cat(sprintf("  Sobreestimacion del metodo original        : %9.1f  (%.2f%%)\n",
    a50$Y[60]-Yc[29], 100*(a50$Y[60]/Yc[29]-1)))

cat("\n=== POR QUE: EL PRIMER PERIODO DE CADA TRAMO AISLADO ===\n")
cat(sprintf("%6s %12s %12s %12s\n","tramo","S periodo 1","dano periodo 1","Y periodo 1"))
for(i in c(1,6)){a<-tramo_aislado(PHI[i],SIG[i],3)
  cat(sprintf("%6d %12.1f %11.4f%% %12.1f\n",ANIOS[i],a$S[1],100*KA*a$S[1],a$Y[1]))}
cat("  Cada tramo arranca con acervo practicamente nulo y por tanto sin dano.\n")

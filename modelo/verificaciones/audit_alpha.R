suppressMessages(library(readxl))
d<-as.data.frame(read_excel("../Datos parámetros/BD para propensiones.xlsx",sheet=1))
names(d)[1:7]<-c("t","C","Y","G","YD","S","H"); d$H_lag<-c(NA,head(d$H,-1)); d<-d[!is.na(d$H_lag),]
r_all<-mean(d$H_lag/d$YD); r_last<-mean(tail(d$H_lag/d$YD,5))
cat("Razon H/YD: media 1992-2024 =",round(r_all,4)," | media 2020-2024 =",round(r_last,4),"\n")
cat("Razon H/Y  en 2022 =",round(d$H_lag[d$t==2022]/d$Y[d$t==2022],3),"\n\n")
cat("alpha2 = (1-alpha1)/(H/YD):\n")
for(a1 in c(0.7707,0.8011)) cat(sprintf("  a1=%.4f -> a2=%.4f (muestra completa) | %.4f (ultimo quinquenio)\n",
  a1,(1-a1)/r_all,(1-a1)/r_last))
cat("\nEl script usa a1=0.8011 con a2=0.1793, que corresponde a a1=0.7707. INCONSISTENTE.\n")

## Efecto sobre los resultados
G<-13527.2509115691; TH<-0.129036878894848; W<-0.0404327375264023; PIB22<-87862.80504186137
KA<-6.20802562807628E-07*1.159; RHO<-0.218080022778761
sim<-function(ph,sg,a1,a2,n=29){
  CO2_22<-0.0410390766992924*2.07644144656066*PIB22; Cs0<-CO2_22/RHO; d0<-KA*Cs0
  Hh<-(PIB22*(1-a1*(1-TH-d0))-G)/a2; Cs<-Cs0; o<-numeric(n)
  if(length(ph)==1) ph<-rep(ph,n); if(length(sg)==1) sg<-rep(sg,n)
  for(t in 1:n){f<-function(Y){dd<-KA*((1-RHO)*Cs+sg[t]*ph[t]*Y); (a2*Hh+G)/(1-a1*(1-TH-dd))-Y}
    Y<-uniroot(f,c(1,1e7),tol=1e-10)$root; Cs<-(1-RHO)*Cs+sg[t]*ph[t]*Y; dd<-KA*Cs
    YD<-Y*(1-TH-dd); Hh<-Hh+YD-(a1*YD+a2*Hh); o[t]<-Y}
  list(Y=o, H0=(PIB22*(1-a1*(1-TH-d0))-G)/a2)}
PHI<-c(2.07644144656066,1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
SIG<-c(0.0410390766992924, META/(PHI[-1]*PIB22))
pt<-approx(c(2022,2025,2030,2035,2040,2045,2050),PHI,xout=2022:2050)$y
st<-approx(c(2022,2025,2030,2035,2040,2045,2050),SIG,xout=2022:2050)$y
cat("\n=== EFECTO SOBRE EL PIB DE 2050 ===\n")
for(p in list(c(0.8011,0.1793),c(0.8011,0.1555),c(0.8011,0.0822))){
  g<-sim(2.07644144656066,0.0410390766992924,p[1],p[2])
  r<-sim(1.28975587832427,0.00882132065193575,p[1],p[2]); n<-sim(pt,st,p[1],p[2])
  cat(sprintf("a1=%.4f a2=%.4f | H0=%8.1f (%.0f%% del PIB) | Global %8.1f | Renov %8.1f | PND %8.1f\n",
    p[1],p[2],g$H0,100*g$H0/PIB22,g$Y[29],r$Y[29],n$Y[29]))}

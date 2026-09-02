PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848; W<-0.0404327375264023
RHO<-0.218080022778761; KA<-6.20802562807628E-07*1.159
phG<-2.07644144656066; sgG<-0.0410390766992924
phR<-1.28975587832427; sgR<-0.00882132065193575
PHI_P<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
SIG_P<-META/(PHI_P*PIB22)
pt<-approx(c(2022,2025,2030,2035,2040,2045,2050),c(phG,PHI_P),xout=2022:2050)$y
st<-approx(c(2022,2025,2030,2035,2040,2045,2050),c(sgG,SIG_P),xout=2022:2050)$y

sim<-function(ph,sg,a1,a2,n=29,ka=KA){
  S0<-sgG*phG*PIB22/RHO; d0<-ka*S0
  Hh<-(PIB22*(1-a1*(1-TH-d0))-G)/a2; Cs<-S0
  if(length(ph)==1) ph<-rep(ph,n); if(length(sg)==1) sg<-rep(sg,n)
  Y<-numeric(n); H<-numeric(n)
  for(t in 1:n){f<-function(y){dd<-ka*((1-RHO)*Cs+sg[t]*ph[t]*y); (a2*Hh+G)/(1-a1*(1-TH-dd))-y}
    y<-uniroot(f,c(1,1e7),tol=1e-10)$root; Cs<-(1-RHO)*Cs+sg[t]*ph[t]*y; dd<-ka*Cs
    YD<-y*(1-TH-dd); Hh<-Hh+YD-(a1*YD+a2*Hh); Y[t]<-y; H[t]<-Hh}
  list(Y=Y,H=H,H0=(PIB22*(1-a1*(1-TH-ka*S0))-G)/a2)}

cat(sprintf("%-9s %10s %8s %9s %9s %9s %8s %8s\n",
  "alpha2","H(2021)","%PIB","Global50","Renov50","PND50","v.Renov","v.PND"))
for(a2 in c(0.1555,0.0855)){
  g<-sim(phG,sgG,0.8011,a2); r<-sim(phR,sgR,0.8011,a2); p<-sim(pt,st,0.8011,a2)
  cat(sprintf("%-9.4f %10.0f %7.1f%% %9.0f %9.0f %9.0f %7.2f%% %7.2f%%\n",
    a2,g$H0,100*g$H0/PIB22,g$Y[29],r$Y[29],p$Y[29],
    100*(r$Y[29]/g$Y[29]-1),100*(p$Y[29]/g$Y[29]-1)))}

cat("\n=== ESTABILIDAD DE LA RIQUEZA A LO LARGO DE LA SIMULACION (escenario global) ===\n")
for(a2 in c(0.1555,0.0855)){
  g<-sim(phG,sgG,0.8011,a2)
  cat(sprintf("alpha2=%.4f: H(2022)=%9.0f  H(2035)=%9.0f  H(2050)=%9.0f  variacion %+.1f%%\n",
    a2,g$H[1],g$H[14],g$H[29],100*(g$H[29]/g$H[1]-1)))}

cat("\n=== INTERPRETACION CONDUCTUAL DE alpha2 ===\n")
for(a2 in c(0.3595,0.1793,0.1555,0.0855))
  cat(sprintf("  alpha2=%.4f -> los hogares gastan %.1f centavos por cada dolar de riqueza al anio\n",a2,100*a2))
cat("  Referencia empirica habitual para riqueza financiera: entre 3 y 5 centavos.\n")

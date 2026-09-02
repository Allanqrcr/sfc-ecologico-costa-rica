PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848
RHO<-0.218080022778761; KA<-6.20802562807628E-07*1.4071907
phG<-2.07644144656066; sgG<-0.0410390766992924
phR<-1.28975587832427; sgR<-0.00882132065193575
PHI<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
SIG<-META/(PHI*PIB22); AN<-c(2025,2030,2035,2040,2045,2050)
pt<-approx(c(2022,AN),c(phG,PHI),xout=2022:2050)$y
st<-approx(c(2022,AN),c(sgG,SIG),xout=2022:2050)$y
sim<-function(ph,sg,a1,a2,n=29){
  S0<-sgG*phG*PIB22/RHO; d0<-KA*S0; Hh<-(PIB22*(1-a1*(1-TH-d0))-G)/a2; Cs<-S0; Y<-numeric(n)
  if(length(ph)==1) ph<-rep(ph,n); if(length(sg)==1) sg<-rep(sg,n)
  for(t in 1:n){f<-function(y){dd<-KA*((1-RHO)*Cs+sg[t]*ph[t]*y);(a2*Hh+G)/(1-a1*(1-TH-dd))-y}
    y<-uniroot(f,c(1,1e7),tol=1e-10)$root; Cs<-(1-RHO)*Cs+sg[t]*ph[t]*y; dd<-KA*Cs
    YD<-y*(1-TH-dd); Hh<-Hh+YD-(a1*YD+a2*Hh); Y[t]<-y}
  list(Y=Y,H0=(PIB22*(1-a1*(1-TH-KA*S0))-G)/a2)}

cat("=== INVARIANCIA DEL ESTADO ESTACIONARIO (calibracion vigente) ===\n")
cat(sprintf("%-22s %11s %11s %11s\n","alpha1 / alpha2","H(2021)","Y en 2050","Y estacionario"))
for(p in list(c(0.60,0.20),c(0.7219,0.3681),c(0.90,0.50))){
  s<-sim(phG,sgG,p[1],p[2]); s200<-sim(phG,sgG,p[1],p[2],200)
  cat(sprintf("%.4f / %.4f       %11.0f %11.1f %11.1f\n",p[1],p[2],s$H0,s$Y[29],s200$Y[200]))}

cat("\n=== EFECTO SOBRE LAS CIFRAS DE 2050 ===\n")
cat(sprintf("%-34s %10s %10s %10s %8s %8s\n","calibracion","Global","Renov","PND","vRenov","vPND"))
for(p in list(c("Manuscrito original",0.7707,0.3595),
              c("Adoptada",0.7219,0.3681),
              c("Godley y Lavoie (texto)",0.60,0.40))){
  a1<-as.numeric(p[2]); a2<-as.numeric(p[3])
  g<-sim(phG,sgG,a1,a2); r<-sim(phR,sgR,a1,a2); n<-sim(pt,st,a1,a2)
  cat(sprintf("%-34s %10.0f %10.0f %10.0f %7.2f%% %7.2f%%\n",p[1],g$Y[29],r$Y[29],n$Y[29],
    100*(r$Y[29]/g$Y[29]-1),100*(n$Y[29]/g$Y[29]-1)))}

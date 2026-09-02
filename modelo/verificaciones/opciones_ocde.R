PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848
RHO<-0.218080022778761; A1<-0.7219; A2<-0.3681
phG<-2.07644144656066; sgG<-0.0410390766992924
phR<-1.28975587832427; sgR<-0.00882132065193575
PHI<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
SIGC<-META/(PHI*PIB22); AN<-c(2025,2030,2035,2040,2045,2050); CO2<-sgG*phG*PIB22
pt<-approx(c(2022,AN),c(phG,PHI),xout=2022:2050)$y
st<-approx(c(2022,AN),c(sgG,SIGC),xout=2022:2050)$y
sim<-function(ph,sg,KA,n=29){
  S0<-CO2/RHO; d0<-KA*S0; Hh<-(PIB22*(1-A1*(1-TH-d0))-G)/A2; Cs<-S0; Y<-numeric(n)
  if(length(ph)==1) ph<-rep(ph,n); if(length(sg)==1) sg<-rep(sg,n)
  for(t in 1:n){f<-function(y){dd<-KA*((1-RHO)*Cs+sg[t]*ph[t]*y);(A2*Hh+G)/(1-A1*(1-TH-dd))-y}
    y<-uniroot(f,c(1,1e7),tol=1e-10)$root; Cs<-(1-RHO)*Cs+sg[t]*ph[t]*y; dd<-KA*Cs
    YD<-y*(1-TH-dd); Hh<-Hh+YD-(A1*YD+A2*Hh); Y[t]<-y}
  list(Y=Y,H0=(PIB22*(1-A1*(1-TH-KA*S0))-G)/A2, d0=KA*S0)}
cat(sprintf("%-46s %7s %9s %9s %9s %8s %8s %8s\n",
 "conversion","USD/t","H0 %PIB","Global50","Renov50","PND50","vRenov","vPND"))
for(v in list(c("60 x 1,053  tipo de cambio 2022 (actual)",60*1.05305),
              c("60 x 1,142  tipo de cambio 2020",60*1.14220),
              c("60 x 1,142 y expresado en dolares de 2022",60*1.14220*1.12))){
  pr<-as.numeric(v[2]); KA<-(pr*CO2*1000/1e6/PIB22)/CO2
  g<-sim(phG,sgG,KA); r<-sim(phR,sgR,KA); p<-sim(pt,st,KA)
  cat(sprintf("%-46s %7.2f %8.1f%% %9.0f %9.0f %9.0f %7.2f%% %7.2f%%\n",
    v[1],pr,100*g$H0/PIB22,g$Y[29],r$Y[29],p$Y[29],
    100*(r$Y[29]/g$Y[29]-1),100*(p$Y[29]/g$Y[29]-1)))}

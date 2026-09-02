PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848
RHO<-0.218080022778761; KA<-6.20802562807628E-07*1.159
phG<-2.07644144656066; sgG<-0.0410390766992924
phR<-1.28975587832427; sgR<-0.00882132065193575
PHI_P<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
SIG_P<-META/(PHI_P*PIB22)
pt<-approx(c(2022,2025,2030,2035,2040,2045,2050),c(phG,PHI_P),xout=2022:2050)$y
st<-approx(c(2022,2025,2030,2035,2040,2045,2050),c(sgG,SIG_P),xout=2022:2050)$y
sim<-function(ph,sg,a1,a2,n=29){
  S0<-sgG*phG*PIB22/RHO; d0<-KA*S0
  H<-(PIB22*(1-a1*(1-TH-d0))-G)/a2; Cs<-S0; Y<-numeric(n)
  if(length(ph)==1) ph<-rep(ph,n); if(length(sg)==1) sg<-rep(sg,n)
  for(t in 1:n){f<-function(y){dd<-KA*((1-RHO)*Cs+sg[t]*ph[t]*y); (a2*H+G)/(1-a1*(1-TH-dd))-y}
    y<-uniroot(f,c(1,1e7),tol=1e-10)$root; Cs<-(1-RHO)*Cs+sg[t]*ph[t]*y; dd<-KA*Cs
    YD<-y*(1-TH-dd); H<-H+YD-(a1*YD+a2*H); Y[t]<-y}
  list(Y=Y,H0=(PIB22*(1-a1*(1-TH-KA*S0))-G)/a2)}
cat(sprintf("%-32s %8s %8s %8s %9s %9s %9s %8s %8s\n",
  "calibracion","alpha1","alpha2","H0 %PIB","Global50","Renov50","PND50","v.Renov","v.PND"))
for(p in list(c("Manuscrito original",0.7707,0.3595),
              c("Version revisada actual",0.8011,0.1555),
              c("Coherente: deficit acumulado",0.7219,0.3681),
              c("Coherente: dinero amplio",0.7219,0.4418))){
  a1<-as.numeric(p[2]); a2<-as.numeric(p[3])
  g<-sim(phG,sgG,a1,a2); r<-sim(phR,sgR,a1,a2); n<-sim(pt,st,a1,a2)
  cat(sprintf("%-32s %8.4f %8.4f %7.0f%% %9.0f %9.0f %9.0f %7.2f%% %7.2f%%\n",
    p[1],a1,a2,100*g$H0/PIB22,g$Y[29],r$Y[29],n$Y[29],
    100*(r$Y[29]/g$Y[29]-1),100*(n$Y[29]/g$Y[29]-1)))}

G<-13527.2509115691; TH<-0.129036878894848; RHO<-0.218080022778761
KA<-6.20802562807628E-07*1.4071907
spG<-0.0410390766992924*2.07644144656066; spR<-0.00882132065193575*1.28975587832427
PHI<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
PIB22<-87862.80504186137; spP<-(META/(PHI*PIB22))[6]*PHI[6]
Yss<-function(a)(-TH+sqrt(TH^2+4*G*a))/(2*a)
cat("9.3 cocientes sigma*phi:\n")
cat(sprintf("  continuidad %.6f | renovable %.6f (div %.2f) | plan 2050 %.6f (div %.2f)\n",
  spG,spR,spG/spR,spP,spG/spP))
cat("\n9.4 sensibilidad a kappa:\n")
for(f in c(0.25,0.5,1,2,4)){a<-KA*f*spG/RHO
  cat(sprintf("  x%.2f -> Y* = %8.1f  dano %.2f%%\n",f,Yss(a),100*a*Yss(a)))}
cat("\n9.5 segun el nivel de dano supuesto:\n")
for(d in c(0.010,0.015,0.0292,0.035,0.050,0.070)){
  YG<-G/(TH+d); aG<-d*(TH+d)/G
  YR<-Yss(aG*spR/spG); YP<-Yss(aG*spP/spG)
  cat(sprintf("  dano %5.2f%% -> cont %8.0f | renov %8.0f (%+.1f%%) | plan %8.0f (%+.1f%%)\n",
    100*d,YG,YR,100*(YR/YG-1),YP,100*(YP/YG-1)))}
cat("\n9.2 especificacion vigente vs alternativa (rho):\n")
a<-KA*spG/RHO; ar<-KA*spR/RHO
cat(sprintf("  vigente rho=0.218: cont %8.1f dano %.2f%% | renov %9.1f ventaja %+.1f%%\n",
  Yss(a),100*a*Yss(a),Yss(ar),100*(Yss(ar)/Yss(a)-1)))
for(dl in c(0.005,0.01,0.02,0.05)){
  b1<-KA*spG*(1-RHO)/dl; b2<-KA*spR*(1-RHO)/dl
  cat(sprintf("  alternativa delta=%.3f: cont %8.1f dano %.2f%% | renov %9.1f ventaja %+.1f%%\n",
    dl,Yss(b1),100*b1*Yss(b1),Yss(b2),100*(Yss(b2)/Yss(b1)-1)))}

G<-13527.2509115691; TH<-0.129036878894848; RHO<-0.218080022778761
KA<-6.20802562807628E-07*1.158355; spG<-0.0410390766992924*2.07644144656066
spR<-0.00882132065193575*1.28975587832427
Yss<-function(a) (-TH+sqrt(TH^2+4*G*a))/(2*a)
cat("Sensibilidad a kappa (tabla 9.4 de la bitacora):\n")
for(f in c(0.25,0.5,1,2,4)){a<-KA*f*spG/RHO
  cat(sprintf("  x%.2f -> Y* = %8.1f  dano = %.2f%%\n",f,Yss(a),100*a*Yss(a)))}
cat("\nEspecificacion vigente (tabla 9.2):\n")
a<-KA*spG/RHO; ar<-KA*spR/RHO
cat(sprintf("  Global Y* = %8.1f  dano %.2f%%  | Renovable Y* = %9.1f  ventaja %+.1f%%\n",
  Yss(a),100*a*Yss(a),Yss(ar),100*(Yss(ar)/Yss(a)-1)))

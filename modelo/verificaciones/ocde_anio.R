PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848
RHO<-0.218080022778761; phG<-2.07644144656066; sgG<-0.0410390766992924
CO2<-sgG*phG*PIB22; Yss<-function(a)(-TH+sqrt(TH^2+4*G*a))/(2*a)
cat("\n=== ALTERNATIVAS DE CONVERSION DEL DATO DE LA OCDE ===\n")
cat(sprintf("%-52s %8s %9s %11s %10s\n","supuesto","USD/t","kappa","dano*","PIB*"))
for(v in list(c("60/1,1 (hoja original)",60/1.1),
              c("60 x 1,053 (tipo de cambio 2022) ADOPTADO",60*1.05305),
              c("60 x 1,142 (tipo de cambio 2020, ano del referente)",60*1.14220),
              c("60 x 1,142 y ajuste de precios EE.UU. 2020-2022",60*1.14220*1.1200))){
  pr<-as.numeric(v[2]); ka<-(pr*CO2*1000/1e6/PIB22)/CO2; a<-ka*sgG*phG/RHO
  cat(sprintf("%-52s %8.2f %9.3e %10.2f%% %11.1f\n",v[1],pr,ka,100*a*Yss(a),Yss(a)))}

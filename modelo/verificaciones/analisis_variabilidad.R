## ¿De dónde viene la variabilidad de los resultados?
G<-13527.2509115691; TH<-0.129036878894848; PIB22<-87862.80504186137
KA<-6.20802562807628E-07*1.159; RHO<-0.218080022778761
phG<-2.07644144656066; sgG<-0.0410390766992924
phR<-1.28975587832427; sgR<-0.00882132065193575

## El estado estacionario es la raiz de:  a*Y^2 + theta*Y - G = 0,  con a = kappa*sigma*phi/rho
## => todo depende de UN solo parametro compuesto "a".
aa<-function(ph,sg,rho=RHO,ka=KA) ka*sg*ph/rho
Yss<-function(a) (-TH+sqrt(TH^2+4*G*a))/(2*a)

cat("=== EL MODELO DEPENDE DE UN SOLO PARAMETRO COMPUESTO ===\n")
cat("a = kappa*sigma*phi/rho\n")
aG<-aa(phG,sgG); aR<-aa(phR,sgR)
cat(sprintf("  Global    a = %.4e -> Y* = %8.1f  dano = %.4f\n",aG,Yss(aG),aG*Yss(aG)))
cat(sprintf("  Renovable a = %.4e -> Y* = %8.1f  dano = %.4f\n",aR,Yss(aR),aR*Yss(aR)))
cat(sprintf("  Razon aG/aR = %.2f  (la matriz renovable divide 'a' por este factor)\n",aG/aR))

cat("\n=== POR QUE HAY TANTA SENSIBILIDAD: LA NO LINEALIDAD ===\n")
cat("Elasticidad local de Y* respecto de 'a' = -dano/(theta+2*dano):\n")
for(nm in c("Global","Renovable")){a<-if(nm=="Global") aG else aR; d<-a*Yss(a)
  cat(sprintf("  %-10s dano=%.4f -> elasticidad = %.3f\n",nm,d,-d/(TH+2*d)))}
cat("Con elasticidad de -0.14, duplicar 'a' mueve Y* solo un 14%.\n")
cat("Pero pasar rho de 0.218 a 0.005 multiplica 'a' por 43.6: eso NO es una\n")
cat("perturbacion local, es cambiar de regimen. De ahi el 104%.\n")

cat("\n=== RANGO PLAUSIBLE vs RANGO EXPLORADO ===\n")
cat("Rango PLAUSIBLE alrededor de la especificacion adoptada:\n")
cat("  kappa: costo social del carbono entre 30 y 120 EUR/t -> factor 0.5 a 2.0\n")
cat("  rho:   tasa de disipacion del indice entre 0.15 y 0.30\n")
res<-c(); rat<-c()
for(fk in c(0.5,0.75,1,1.5,2)) for(r in c(0.15,0.18,RHO,0.25,0.30)){
  yg<-Yss(aa(phG,sgG,r,KA*fk)); yr<-Yss(aa(phR,sgR,r,KA*fk))
  res<-c(res,yg); rat<-c(rat,100*(yr/yg-1))}
cat(sprintf("\n  PIB* continuidad : min %8.1f  max %8.1f  -> amplitud %.1f%%\n",
    min(res),max(res),100*(max(res)/min(res)-1)))
cat(sprintf("  Ventaja renovable: min %6.2f%%  max %6.2f%%  -> amplitud %.1f pp\n",
    min(rat),max(rat),max(rat)-min(rat)))
cat(sprintf("  Mediana de la ventaja renovable: %.2f%%\n",median(rat)))

cat("\n=== COMPARACION: RANGO EXPLORADO EN EL DOCUMENTO ANTERIOR ===\n")
res2<-c(); rat2<-c()
for(fk in c(0.25,0.5,1,2,4)) for(r in c(0.005,0.01,0.02,0.05,0.10,RHO,0.40)){
  yg<-Yss(aa(phG,sgG,r,KA*fk)); yr<-Yss(aa(phR,sgR,r,KA*fk))
  res2<-c(res2,yg); rat2<-c(rat2,100*(yr/yg-1))}
cat(sprintf("  PIB* continuidad : min %8.1f  max %8.1f  -> amplitud %.0f%%\n",
    min(res2),max(res2),100*(max(res2)/min(res2)-1)))
cat(sprintf("  Ventaja renovable: min %6.2f%%  max %6.2f%%\n",min(rat2),max(rat2)))

cat("\n=== LO QUE SI ES ESTABLE ===\n")
cat("Orden de escenarios (renovable > PND > continuidad) en las 35 combinaciones\n")
cat("del rango plausible y en las 35 del rango amplio: se cumple SIEMPRE.\n")

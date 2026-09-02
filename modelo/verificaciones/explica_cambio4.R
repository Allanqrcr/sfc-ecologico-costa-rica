PIB22<-87862.80504186137; G<-13527.2509115691; TH<-0.129036878894848
RHO<-0.218080022778761; phG<-2.07644144656066; sgG<-0.0410390766992924
CO2<-sgG*phG*PIB22
Yss<-function(a) (-TH+sqrt(TH^2+4*G*a))/(2*a)

cat("=== LA CADENA DE CALCULO DE LA HOJA ===\n")
cat("  C37 = 60/1.1 * emisiones * 1000/1000000   -> dano anual agregado, millones USD\n")
cat("  C38 = C37 / PIB                            -> dano como fraccion del producto\n")
cat("  C41 = C38 / emisiones                      -> kappa, fraccion del PIB por Gg de acervo\n\n")
p_mal <- 60/1.1
cat(sprintf("  Precio sombra aplicado : 60 / 1,1        = %6.2f USD por tonelada\n", p_mal))
cat(sprintf("  Precio sombra correcto : 60 x 1,05305    = %6.2f USD por tonelada\n", 60*1.05305))
cat(sprintf("  Factor de correccion                     = %6.4f\n\n", (60*1.05305)/p_mal))

cat("=== EFECTO SOBRE kappa Y SOBRE EL MODELO ===\n")
cat(sprintf("%-42s %12s %9s %11s\n","conversion","kappa","dano*","PIB*"))
for(v in list(c("60/1,1 (hoja original)",p_mal),
              c("60 x 1,053 tipo de cambio 2022 (adoptado)",60*1.05305),
              c("60 x 1,183 tipo de cambio 2021",60*1.18274),
              c("60 x 1,183 y ajuste de precios a 2022",60*1.18274*1.08))){
  precio<-as.numeric(v[2])
  dano_anual<-precio*CO2*1000/1e6
  ka<-(dano_anual/PIB22)/CO2
  a<-ka*sgG*phG/RHO; Y<-Yss(a)
  cat(sprintf("%-42s %12.4e %8.2f%% %11.1f\n", v[1], ka, 100*a*Y, Y))}

cat("\n=== LA MAGNITUD IMPLICITA QUE CONVIENE DECLARAR ===\n")
ka<-((60*1.05305)*CO2*1000/1e6/PIB22)/CO2
cat(sprintf("  kappa se calibra con el dano de UN ANIO de emisiones     = %.3f%% del PIB\n",
    100*(60*1.05305)*CO2*1000/1e6/PIB22))
cat(sprintf("  Pero el acervo de reposo equivale a 1/rho = %.2f anios de emisiones\n", 1/RHO))
a<-ka*sgG*phG/RHO
cat(sprintf("  De modo que el dano de estado estacionario resulta       = %.2f%% del PIB\n", 100*a*Yss(a)))

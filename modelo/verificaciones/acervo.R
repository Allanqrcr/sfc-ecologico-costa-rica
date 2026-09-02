G<-13527.2509115691; TH<-0.129036878894848; PIB22<-87862.80504186137
RHO<-0.218080022778761; KA<-6.20802562807628E-07*1.4071907
spG<-0.0410390766992924*2.07644144656066; spR<-0.00882132065193575*1.28975587832427
PHI<-c(1.94815033931687,1.8455609450008,1.75313427460553,1.66961697683439,1.59369517358915,1.52437777251918)
META<-c(6852.646424416003,6281.7042423699995,5710.091940392017,5139.149758346013,4568.207576300009,3997.2653942540055)
spP<-(META/(PHI*PIB22))[6]*PHI[6]
Yss<-function(a)(-TH+sqrt(TH^2+4*G*a))/(2*a)
fila<-function(nom,aG,aR,aP) cat(sprintf("%-52s %9.1f %8.2f%% %10.1f %8.1f%% %8.1f%%\n",
  nom,Yss(aG),100*aG*Yss(aG),Yss(aR),100*(Yss(aR)/Yss(aG)-1),100*(Yss(aP)/Yss(aG)-1)))

cat(sprintf("%-52s %9s %9s %10s %9s %9s\n","opcion","PIB cont","dano","PIB renov","vRenov","vPND"))
cat("--- 1. ADOPTADA: acervo como indice de degradacion domestica, rho = 0.218 ---\n")
fila("   d = kappa*S, S=(1-rho)S+CO2, rho=0.218",KA*spG/RHO,KA*spR/RHO,KA*spP/RHO)

cat("--- 3. ACERVO ATMOSFERICO con permanencia de literatura ---\n")
for(dl in c(0.005,0.01,0.02)) fila(sprintf("   rho compensa flujo, delta=%.3f",dl),
  KA*spG*(1-RHO)/dl, KA*spR*(1-RHO)/dl, KA*spP*(1-RHO)/dl)

cat("--- 4. SIN ACERVO: el dano depende del FLUJO anual de emisiones ---\n")
## d = kappa_f * CO2 = kappa_f*sigma*phi*Y, con kappa_f tal que en el ano base
## el dano sea el costo social de las emisiones de ese ano (0.654% del PIB)
kaf<-(76.75584*1000/1e6)/PIB22    # fraccion del PIB por Gg de emisiones anuales
fila("   d = kappa_f * CO2 (costo social del ano corriente)",kaf*spG,kaf*spR,kaf*spP)
cat(sprintf("\n   Con esta opcion el dano del ano base es %.3f%% del PIB y desaparecen rho y el acervo.\n",
  100*kaf*spG*PIB22/PIB22*1))
cat(sprintf("   Dano de estado estacionario: %.3f%%\n",100*kaf*spG*Yss(kaf*spG)))

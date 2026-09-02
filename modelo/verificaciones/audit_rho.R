G<-13527.2509115691; TH<-0.129036878894848; PIB22<-87862.80504186137
KA<-6.20802562807628E-07*1.159; RHO<-0.218080022778761
phG<-2.07644144656066; sgG<-0.0410390766992924
phR<-1.28975587832427; sgR<-0.00882132065193575

## (A) Especificacion vigente: rho decae el acervo -> S* = CO2/rho
ssA<-function(ph,sg,rho=RHO,ka=KA){f<-function(Y) G/(TH+ka*sg*ph*Y/rho)-Y
  Y<-uniroot(f,c(1,1e7))$root; c(Y=Y,d=ka*sg*ph*Y/rho)}
## (B) Alternativa: rho compensa el flujo, delta decae el acervo -> S* = CO2*(1-rho)/delta
ssB<-function(ph,sg,delta,rho=RHO,ka=KA){f<-function(Y) G/(TH+ka*sg*ph*Y*(1-rho)/delta)-Y
  Y<-uniroot(f,c(1,1e7))$root; c(Y=Y,d=ka*sg*ph*Y*(1-rho)/delta)}

cat("=== (A) VIGENTE: S(t) = (1-rho)S(t-1) + CO2 ===\n")
a1<-ssA(phG,sgG); a2<-ssA(phR,sgR)
cat(sprintf("  Global    Y*=%8.1f  dano=%6.2f%%\n",a1[1],100*a1[2]))
cat(sprintf("  Renovable Y*=%8.1f  dano=%6.2f%%   brecha=%+.2f%%\n",a2[1],100*a2[2],100*(a2[1]/a1[1]-1)))

cat("\n=== (B) ALTERNATIVA: S(t) = (1-delta)S(t-1) + (1-rho)CO2 ===\n")
for(dl in c(0.005,0.01,0.02,0.05,0.10,0.218)){
  b1<-ssB(phG,sgG,dl); b2<-ssB(phR,sgR,dl)
  cat(sprintf("  delta=%.3f  Global Y*=%8.1f dano=%6.2f%% | Renov Y*=%8.1f dano=%6.2f%% | brecha=%+.2f%%\n",
      dl,b1[1],100*b1[2],b2[1],100*b2[2],100*(b2[1]/b1[1]-1)))}

cat("\n=== SENSIBILIDAD CON kappa FINAL (x1.159) ===\n")
for(r in c(0.005,0.01,0.02,0.05,0.10,RHO,0.40)){
  x<-ssA(phG,sgG,rho=r); cat(sprintf("  rho=%.5f -> Y*=%8.1f  dano=%6.2f%%\n",r,x[1],100*x[2]))}
cat("\n=== SENSIBILIDAD A kappa (rho vigente) ===\n")
for(fk in c(0.25,0.5,1,2,4)){
  x<-ssA(phG,sgG,ka=KA*fk); cat(sprintf("  kappa x%.2f -> Y*=%8.1f  dano=%6.2f%%\n",fk,x[1],100*x[2]))}

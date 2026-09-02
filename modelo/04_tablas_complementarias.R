# =====================================================================
#  Tablas complementarias citadas en el articulo.
#  Se calculan con los parametros finales, a partir de la solucion cerrada
#  del estado estacionario, y de las series que produce 01_modelo_SFCE.R.
#
#    coherencia_metas_PND.csv        seccion 4.4
#    sensibilidad_rho_kappa.csv      seccion 3.11
#    comparacion_especificacion_rho.csv   seccion 3.7
# =====================================================================
OUT <- "salidas"; if (!dir.exists(OUT)) dir.create(OUT)

PIB22 <- 87862.80504186137
G     <- 13527.2509115691
TH    <- 0.129036878894848
RHO   <- 0.218080022778761
KAPPA <- 6.20802562807628E-07 * 1.4071907
PHI_G <- 2.07644144656066; SIG_G <- 0.0410390766992924
PHI_R <- 1.28975587832427; SIG_R <- 0.00882132065193575

## Producto de estado estacionario: raiz de a*Y^2 + theta*Y - G = 0
Yss <- function(a) (-TH + sqrt(TH^2 + 4 * a * G)) / (2 * a)

## --- 1. Coherencia entre las emisiones del escenario y las metas del plan ---
META_PND <- c(6852.646424416003, 6281.7042423699995, 5710.091940392017,
              5138.479638414033, 4566.86733643605, 3995.255034458066)
ser  <- read.csv(file.path(OUT, "series_FINAL.csv"), stringsAsFactors = FALSE)
pnd  <- ser[ser$esc == "4. PND", ]
anios <- c(2025, 2030, 2035, 2040, 2045, 2050)
idx  <- match(anios, pnd$anio)

coherencia <- data.frame(
  anio       = anios,
  meta_Gg    = round(META_PND, 1),
  modelo_Gg  = round(pnd$CO2[idx], 1),
  desvio_pct = round(100 * (pnd$CO2[idx] / META_PND - 1), 1))
write.csv(coherencia, file.path(OUT, "coherencia_metas_PND.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\n=== EMISIONES DEL ESCENARIO DEL PLAN FRENTE A SUS METAS ===\n")
print(coherencia, row.names = FALSE)
cat("\nLa desviacion mide la brecha entre la meta absoluta del plan y las emisiones\n",
    "que resultan de aplicar sus intensidades a un producto endogeno.\n", sep = "")

## --- 2. Sensibilidad al par (rho, kappa) -----------------------------------
## rho gobierna la disipacion del acervo; el factor multiplica a kappa.
rhos    <- c(0.005, 0.01, 0.02, 0.05, 0.10, RHO, 0.40)
factores <- c(0.25, 0.5, 1, 2, 4)
esc <- list("Continuidad" = SIG_G * PHI_G, "Totalmente renovable" = SIG_R * PHI_R)

sens <- do.call(rbind, lapply(names(esc), function(e)
  do.call(rbind, lapply(rhos, function(r)
    do.call(rbind, lapply(factores, function(f) {
      a <- KAPPA * f * esc[[e]] / r; Y <- Yss(a)
      data.frame(escenario = e, rho = r, factor_kappa = f,
                 PIB_estacionario = round(Y, 1),
                 dano_pct = round(100 * a * Y, 3))
    }))))))
write.csv(sens, file.path(OUT, "sensibilidad_rho_kappa.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\n=== SENSIBILIDAD AL PAR (rho, kappa) ===\n")
cat("Filas de la calibracion central (rho = 0.2181, factor = 1):\n")
print(sens[abs(sens$rho - RHO) < 1e-9 & sens$factor_kappa == 1, ], row.names = FALSE)

## --- 3. La especificacion adoptada frente a la alternativa ------------------
## Adoptada:    rho disipa el acervo         -> a = kappa*sigma*phi/rho
## Alternativa: rho compensa el flujo anual y el acervo decae a una tasa delta
##              tomada de la literatura del ciclo del carbono
##                                           -> a = kappa*(1-rho)*sigma*phi/delta
DELTA <- 0.01
comp <- do.call(rbind, lapply(names(esc), function(e) {
  sp <- esc[[e]]
  aA <- KAPPA * sp / RHO
  aB <- KAPPA * (1 - RHO) * sp / DELTA
  rbind(
    data.frame(especificacion = "Adoptada: rho disipa el acervo",
               escenario = e, PIB_estacionario = round(Yss(aA), 1),
               dano_pct = round(100 * aA * Yss(aA), 3),
               parametro_compuesto = signif(aA, 4)),
    data.frame(especificacion = "Alternativa: rho compensa el flujo, delta = 1 %",
               escenario = e, PIB_estacionario = round(Yss(aB), 1),
               dano_pct = round(100 * aB * Yss(aB), 3),
               parametro_compuesto = signif(aB, 4)))
}))
comp <- comp[order(comp$especificacion, comp$escenario), ]
write.csv(comp, file.path(OUT, "comparacion_especificacion_rho.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\n=== LA ESPECIFICACION ADOPTADA FRENTE A LA ALTERNATIVA ===\n")
print(comp, row.names = FALSE)
cat(sprintf("\nLa alternativa multiplica el parametro compuesto por %.1f,\n",
            (1 - RHO) * RHO / DELTA))
cat("es decir mas de un orden de magnitud. La diferencia entre ambas cifras no\n",
    "refleja sensibilidad parametrica sino un cambio de interpretacion del\n",
    "bloque biofisico, discutido en la seccion 3.7 del articulo.\n", sep = "")

cat("\nTablas complementarias escritas en", OUT, "\n")

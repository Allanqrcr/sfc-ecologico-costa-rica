# =====================================================================
#  Figuras del articulo, generadas a partir de series_FINAL.csv
#  (salida del script SFC-E_version_revisada.R)
#  Sustituyen a las figuras de la version original, que provenian de
#  simulaciones previas a las correcciones de la revision.
# =====================================================================
sal <- "salidas"
d   <- read.csv(file.path(sal, "series_FINAL.csv"), stringsAsFactors = FALSE)

ESC <- c("1. Sin componente ecologico", "2. Global",
         "3. Totalmente renovable", "4. PND")
LEY <- c("Sin bloque ecológico", "Continuidad de la matriz actual",
         "Totalmente renovable", "Plan Nacional de Descarbonización")
COL <- c("#8c8c8c", "#c0392b", "#1e8449", "#1f4e79")
LTY <- c(2, 1, 1, 1)
LWD <- c(2, 2.4, 2.4, 2.4)

serie <- function(e, v) d[d$esc == e, v]
anios <- serie(ESC[1], "anio")

fig <- function(archivo, var, titulo, ejey, escala = 1, pos = "topleft",
                escenarios = 1:4) {
  png(file.path(sal, archivo), width = 2000, height = 1250, res = 240, type = "cairo")
  par(mar = c(4.2, 6.2, 3.2, 1.2), mgp = c(4.4, 0.8, 0), family = "sans")
  Y <- lapply(escenarios, function(k) serie(ESC[k], var) / escala)
  plot(range(anios), range(unlist(Y)), type = "n",
       xlab = "", ylab = ejey, main = titulo, cex.main = 1.0, cex.lab = 0.9,
       cex.axis = 0.85, las = 1)
  grid(col = "grey88", lty = 1)
  for (j in seq_along(escenarios)) {
    k <- escenarios[j]
    lines(anios, Y[[j]], col = COL[k], lty = LTY[k], lwd = LWD[k])
  }
  legend(pos, LEY[escenarios], col = COL[escenarios], lty = LTY[escenarios],
         lwd = LWD[escenarios], bty = "o", bg = "white", box.col = NA,
         cex = 0.8)
  dev.off()
  cat("  ", archivo, "\n")
}

cat("Generando figuras en", sal, "\n")

fig("Figura_2_PIB.png", "Y",
    "Producto por escenario, 2022–2050",
    "Millones de dólares constantes de 2022")

fig("Figura_3_PIB_efectivo.png", "Y_eff",
    "Producto neto de daño por escenario, 2022–2050",
    "Millones de dólares constantes de 2022")

fig("Figura_4_emisiones.png", "CO2",
    "Emisiones anuales de dióxido de carbono por escenario, 2022–2050",
    "Gigagramos de CO₂ por año")

fig("Figura_5_acervo.png", "Cstock",
    "Acervo de emisiones acumuladas por escenario, 2022–2050",
    "Gigagramos de CO₂", escenarios = 2:4)

fig("Figura_6_deficit.png", "def",
    "Déficit primario por escenario, 2022–2050",
    "Porcentaje del producto", pos = "topright")

# --- Figura 7: comparacion PND vs totalmente renovable -----------------
png(file.path(sal, "Figura_7_PND_vs_renovable.png"),
    width = 2000, height = 1250, res = 240, type = "cairo")
par(mfrow = c(1, 2), mar = c(4.2, 5.8, 3.0, 1.0), mgp = c(4.0, 0.8, 0), family = "sans")
for (v in c("Y", "CO2")) {
  a <- serie(ESC[4], v); b <- serie(ESC[3], v)
  esc <- if (v == "Y") 1 else 1
  plot(range(anios), range(c(a, b)) / esc, type = "n", xlab = "",
       ylab = if (v == "Y") "Millones de USD de 2022" else "Gigagramos de CO₂",
       main = if (v == "Y") "Producto" else "Emisiones anuales",
       cex.main = 1.0, cex.lab = 0.85, cex.axis = 0.8, las = 1)
  grid(col = "grey88", lty = 1)
  lines(anios, b / esc, col = COL[3], lwd = 2.4)
  lines(anios, a / esc, col = COL[4], lwd = 2.4)
  legend(if (v == "Y") "topleft" else "topright",
         LEY[c(3, 4)], col = COL[c(3, 4)], lwd = 2.4, bty = "o",
         bg = "white", box.col = NA, cex = 0.75)
}
dev.off()
cat("   Figura_7_PND_vs_renovable.png \n")

cat("\nListo. Seis figuras nuevas; la Figura 1 es el diagrama del modelo,\n")
cat("generado por diagrama_modelo.R\n")

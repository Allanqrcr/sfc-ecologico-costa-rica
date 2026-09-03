# =====================================================================
#  Figuras 2 a 7 del articulo, generadas a partir de series_FINAL.csv
#  (salida de 01_modelo_SFCE.R).
#
#  Diseno: ggplot2, leyenda horizontal debajo del panel (nunca sobre las
#  curvas), ejes con separador de miles, sin titulo interno (el titulo va
#  en la leyenda de la figura dentro del articulo). PNG a 300 ppp via ragg.
#
#    Figura_2_PIB.png              producto por escenario
#    Figura_3_PIB_efectivo.png     producto neto de dano por escenario
#    Figura_4_emisiones.png        emisiones anuales por escenario
#    Figura_5_acervo.png           acervo de emisiones acumuladas (esc. 2-4)
#    Figura_6_deficit.png          deficit primario por escenario
#    Figura_7_PND_vs_renovable.png plan vs totalmente renovable (2 paneles)
# =====================================================================
suppressPackageStartupMessages({ library(ggplot2); library(scales) })
sal <- "salidas"
d   <- read.csv(file.path(sal, "series_FINAL.csv"), stringsAsFactors = FALSE,
                encoding = "UTF-8")

ESC <- c("1. Sin componente ecologico", "2. Global",
         "3. Totalmente renovable", "4. PND")
LEY <- c("Sin bloque ecológico", "Continuidad de la matriz actual",
         "Totalmente renovable", "Plan Nacional de Descarbonización")
COL <- c("#7F7F7F", "#B03A2E", "#1E8449", "#1F4E79")
LTY <- c("22", "solid", "solid", "solid")
names(COL) <- names(LTY) <- LEY
d$esc_ley <- factor(LEY[match(d$esc, ESC)], levels = LEY)

tema <- theme_minimal(base_size = 12, base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey88", linewidth = 0.35),
        panel.border     = element_rect(colour = "grey40", fill = NA, linewidth = 0.4),
        axis.title.x     = element_blank(),
        axis.title.y     = element_text(size = 10.5, margin = margin(r = 8)),
        axis.text        = element_text(size = 10, colour = "grey20"),
        legend.position  = "bottom",
        legend.title     = element_blank(),
        legend.text      = element_text(size = 10),
        legend.key.width = unit(1.3, "cm"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.key.spacing.y = unit(0.15, "cm"),
        legend.margin    = margin(t = 2),
        plot.margin      = margin(8, 14, 6, 8))

miles <- label_number(big.mark = " ", decimal.mark = ",", accuracy = 1)
ejes_x <- scale_x_continuous(breaks = seq(2022, 2050, 4), expand = expansion(mult = 0.015))

fig <- function(archivo, var, ejey, escenarios = 1:4, escala = 1,
                etiquetas = miles, ancho = 16, alto = 9.5) {
  dd <- d[d$esc %in% ESC[escenarios], ]
  dd$y <- dd[[var]] / escala
  g <- ggplot(dd, aes(anio, y, colour = esc_ley, linetype = esc_ley)) +
    geom_line(linewidth = 1.05, lineend = "round") +
    scale_colour_manual(values = COL[LEY[escenarios]], drop = TRUE) +
    scale_linetype_manual(values = LTY[LEY[escenarios]], drop = TRUE) +
    scale_y_continuous(labels = etiquetas) + ejes_x +
    labs(y = ejey) + tema +
    guides(colour = guide_legend(nrow = if (length(escenarios) > 2) 2 else 1, byrow = TRUE),
           linetype = guide_legend(nrow = if (length(escenarios) > 2) 2 else 1, byrow = TRUE))
  ragg::agg_png(file.path(sal, archivo), width = ancho, height = alto,
                units = "cm", res = 300)
  print(g); dev.off()
  cat("  ", archivo, "\n")
}

cat("Generando figuras en", sal, "\n")
fig("Figura_2_PIB.png", "Y", "Millones de dólares constantes de 2022")
fig("Figura_3_PIB_efectivo.png", "Y_eff", "Millones de dólares constantes de 2022")
fig("Figura_4_emisiones.png", "CO2", expression("Gigagramos de CO"[2]*" por año"))
fig("Figura_5_acervo.png", "Cstock", expression("Gigagramos de CO"[2]), escenarios = 2:4)
fig("Figura_6_deficit.png", "def", "Porcentaje del producto",
    etiquetas = label_number(accuracy = 0.1, decimal.mark = ",", suffix = " %"))

## Figura 7: dos paneles con leyenda comun, escenarios 3 y 4
dd <- d[d$esc %in% ESC[3:4], ]
d7 <- rbind(data.frame(anio = dd$anio, esc_ley = dd$esc_ley, panel = "Producto (millones de dólares de 2022)", y = dd$Y),
            data.frame(anio = dd$anio, esc_ley = dd$esc_ley, panel = "Emisiones anuales (gigagramos de CO2)", y = dd$CO2))
d7$panel <- factor(d7$panel, levels = unique(d7$panel))
g7 <- ggplot(d7, aes(anio, y, colour = esc_ley)) +
  geom_line(linewidth = 1.05, lineend = "round") +
  facet_wrap(~ panel, scales = "free_y") +
  scale_colour_manual(values = COL[LEY[3:4]]) +
  scale_y_continuous(labels = miles) +
  scale_x_continuous(breaks = seq(2022, 2050, 7), expand = expansion(mult = 0.02)) + tema +
  theme(strip.text = element_text(size = 9.5, colour = "grey15", margin = margin(b = 5)),
        axis.title.y = element_blank(), panel.spacing = unit(1.2, "cm")) +
  guides(colour = guide_legend(nrow = 1))
ragg::agg_png(file.path(sal, "Figura_7_PND_vs_renovable.png"), width = 17.5, height = 9,
              units = "cm", res = 300)
print(g7); dev.off()
cat("   Figura_7_PND_vs_renovable.png \n")
cat("\nListo. Seis figuras; la Figura 1 es el diagrama del modelo (03_diagrama.R).\n")

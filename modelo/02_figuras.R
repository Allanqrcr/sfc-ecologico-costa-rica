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
#    Figura_8_producto_por_rama.png  producto por rama, areas apiladas, 4 escenarios
#    Figura_9_emisiones_por_rama.png emisiones por rama, areas apiladas, 4 escenarios
#    Figura_2_trayectorias.png       compuesta de 4 paneles (version resumida del articulo)
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
## Figuras 8 y 9: composición por ramas en cada escenario (áreas apiladas)
COL_RAMA <- c("Rama verde (energía renovable)" = "#1E8449",
              "Rama no verde (energía fósil)"  = "#8C6D1F")
fig_ramas <- function(archivo, vg, vc, ejey, escala = 1) {
  dl <- rbind(data.frame(anio = d$anio, esc_ley = d$esc_ley, rama = names(COL_RAMA)[2], y = d[[vc]] / escala),
              data.frame(anio = d$anio, esc_ley = d$esc_ley, rama = names(COL_RAMA)[1], y = d[[vg]] / escala))
  dl$rama <- factor(dl$rama, levels = rev(names(COL_RAMA)))
  g <- ggplot(dl, aes(anio, y, fill = rama)) +
    geom_area(alpha = 0.92, colour = "white", linewidth = 0.25, position = "stack") +
    facet_wrap(~ esc_ley, ncol = 2) +
    scale_fill_manual(values = COL_RAMA) +
    scale_y_continuous(labels = miles, expand = expansion(mult = c(0, 0.04))) +
    scale_x_continuous(breaks = seq(2022, 2050, 7), expand = expansion(mult = 0.01)) +
    labs(y = ejey) + tema +
    theme(strip.text = element_text(size = 9.5, colour = "grey15", margin = margin(b = 4)),
          panel.spacing = unit(0.9, "cm"), legend.key.width = unit(0.9, "cm")) +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))
  ragg::agg_png(file.path(sal, archivo), width = 16, height = 12.5, units = "cm", res = 300)
  print(g); dev.off()
  cat("  ", archivo, "\n")
}
fig_ramas("Figura_8_producto_por_rama.png", "Y_g", "Y_c", "Millones de dólares constantes de 2022")
fig_ramas("Figura_9_emisiones_por_rama.png", "CO2_g", "CO2_c", expression("Gigagramos de CO"[2]*" por año"))
## Figura compuesta (version resumida del articulo): producto, emisiones, acervo y
## deficit primario en cuatro paneles con leyenda comun.
d4 <- rbind(
  data.frame(anio = d$anio, esc_ley = d$esc_ley, panel = "Producto (millones de dólares de 2022)", y = d$Y),
  data.frame(anio = d$anio, esc_ley = d$esc_ley, panel = "Emisiones anuales (gigagramos de CO2)", y = d$CO2),
  data.frame(anio = d$anio, esc_ley = d$esc_ley, panel = "Acervo de emisiones acumuladas (gigagramos de CO2)", y = d$Cstock),
  data.frame(anio = d$anio, esc_ley = d$esc_ley, panel = "Déficit primario (porcentaje del producto)", y = d$def))
d4$panel <- factor(d4$panel, levels = unique(d4$panel))
g4 <- ggplot(d4, aes(anio, y, colour = esc_ley, linetype = esc_ley)) +
  geom_line(linewidth = 0.95, lineend = "round") +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = COL) + scale_linetype_manual(values = LTY) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ",", accuracy = 1)) +
  scale_x_continuous(breaks = seq(2022, 2050, 7), expand = expansion(mult = 0.02)) + tema +
  theme(strip.text = element_text(size = 9, colour = "grey15", margin = margin(b = 4)),
        axis.title.y = element_blank(), panel.spacing = unit(0.8, "cm"),
        axis.text = element_text(size = 9)) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))
ragg::agg_png(file.path(sal, "Figura_2_trayectorias.png"), width = 16, height = 13, units = "cm", res = 300)
print(g4); dev.off()
cat("   Figura_2_trayectorias.png (compuesta, version resumida)\n")
cat("
Listo. Nueve figuras mas la compuesta; la Figura 1 es el diagrama del modelo (03_diagrama.R).
")

# Modelo Stock-Flow Consistente con extensión ecológica para Costa Rica

Código en R, datos derivados y resultados del artículo **«Descarbonización y desempeño
macroeconómico en Costa Rica: evidencia desde un modelo Stock-Flow Consistente con
extensión ecológica»**.

El repositorio permite reproducir íntegramente, desde los datos hasta las figuras, los
cuatro escenarios que el artículo compara. Todo el material está disponible para su
verificación, reutilización y crítica.

---

## Qué hace el modelo

Un núcleo macroeconómico-financiero de dos sectores —hogares y gobierno— con coherencia
contable entre flujos y acervos, acoplado a un bloque biofísico que vincula el nivel de
actividad con la demanda de energía, las emisiones de dióxido de carbono y un acervo de
presión ambiental acumulada de origen doméstico, cuyo costo retroalimenta el ingreso
disponible de los hogares.

La calibración se fija al año base 2022. El horizonte de simulación llega a 2050.

**Escenarios**

| | Composición energética |
|---|---|
| 1. Sin bloque ecológico | modelo contable puro, sin retroalimentación ambiental |
| 2. Continuidad | se mantiene la matriz energética observada en 2022 |
| 3. Totalmente renovable | sustitución completa del componente fósil |
| 4. Plan de Descarbonización | transición gradual conforme a las metas oficiales |

**Resultados en 2050** (millones de dólares constantes de 2022)

| Indicador | Sin bloque ecol. | Continuidad | Renovable | Plan |
|---|---:|---:|---:|---:|
| Producto | 104 465 | 85 499 | 100 797 | 90 739 |
| Producto neto de daño | 104 465 | 83 003 | 100 334 | 89 102 |
| Emisiones (Gg CO₂/año) | 8 902 | 7 286 | 1 147 | 4 128 |
| Daño (% del producto) | 0,00 | 2,92 | 0,46 | 1,80 |
| Déficit primario (% del producto) | 0,05 | 2,92 | 0,52 | 2,00 |

Los cuatro escenarios parten del producto observado de 2022 (87 863).

---

## Cómo reproducirlo

Requiere R (probado en 4.5.0) y los paquetes `sfcr`, `readxl`, `lmtest` y `sandwich`.

```r
install.packages(c("sfcr", "readxl", "lmtest", "sandwich"))
```

Desde la raíz del repositorio:

```
Rscript ejecutar_todo.R
```

El proceso estima las propensiones a consumir, resuelve los cuatro escenarios, ejecuta
las verificaciones de consistencia y genera las figuras. Todo se escribe en
`modelo/salidas/`. Los resultados versionados en este repositorio son la salida de esa
misma ejecución, de modo que cualquier discrepancia es detectable de inmediato.

---

## Verificación de la consistencia contable

Un modelo stock-flow consistente correctamente especificado contiene una ecuación
redundante que no se impone y que, sin embargo, debe cumplirse. El script principal la
verifica con el paquete `sfcr`, que rechaza el modelo si la identidad no se satisface:

```
V1. Ecuación redundante Hh = Hs, verificada con hidden = c('Hh'='Hs'):
     sfcr acepta el modelo. max|Hh - Hs| = 6.01e-09
V2. Producto del período 1 = 87862.80504  |  PIB observado de 2022 = 87862.80504
V3. Estado estacionario G/(theta+daño) = 85491.2  |  simulado en 2050 = 85498.8
V4. Discrepancia relativa máxima en Y entre ambos métodos = 1.30e-11
```

La cuarta comprobación contrasta el solucionador analítico del script principal contra
la resolución independiente del sistema con `sfcr`.

---

## Estructura

```
ejecutar_todo.R              reproduce el ejercicio completo
modelo/
  01_modelo_SFCE.R           parámetros, escenarios, verificaciones y sensibilidad
  02_figuras.R               figuras 2 a 7 del artículo
  03_diagrama.R              figura 1: diagrama del modelo
  04_tablas_complementarias.R  coherencia con las metas del plan, sensibilidad
                               al par (rho, kappa) y comparación entre la
                               especificación adoptada y la alternativa
  salidas/                   resultados y figuras (salida de ejecutar_todo.R)
  verificaciones/            comprobaciones auxiliares del proceso de revisión
propensiones/
  Propensiones_reestimacion.R          ocho especificaciones econométricas
  Propensiones_riqueza_alternativas.R  construcciones alternativas de la riqueza
  FUENTES.md                           procedencia de cada serie
  *.csv, *.xlsx                        datos derivados de estadística pública
```

Los archivos con sufijo `FINAL` en `modelo/salidas/` son los que alimentan el artículo.
La carpeta `modelo/verificaciones/` conserva las comprobaciones intermedias del proceso
de revisión; se publican por transparencia y no forman parte de la cadena de resultados.

---

## Datos

Las series incluidas son derivadas y fueron construidas por los autores a partir de
estadística pública costarricense (Banco Central, Ministerio de Hacienda, INEC, MINAE).
La procedencia de cada una está documentada en [`propensiones/FUENTES.md`](propensiones/FUENTES.md).

Las publicaciones oficiales de las que se extraen los parámetros físicos —Balance
Nacional de Energía, Inventario Nacional de Gases de Efecto Invernadero, Matriz
Insumo-Producto y Plan Nacional de Descarbonización— no se redistribuyen aquí. Los
valores que de ellas se derivan figuran como parámetros explícitos en
`modelo/01_modelo_SFCE.R` y en `modelo/salidas/tabla_parametros_FINAL.csv`, con su
fuente indicada en cada caso.

---

## Alcance y limitaciones

El modelo no representa la formación de capital ni, por tanto, los costos de inversión
de la transición energética; no incluye sector externo ni desagregación sectorial; y no
contiene nivel de precios. El único canal por el que la composición energética afecta al
nivel de actividad es la reducción del daño ambiental. Estas limitaciones se exponen en
detalle en el artículo y condicionan la lectura de todos los resultados.

El acervo que el modelo acumula no representa el acervo atmosférico global de carbono,
sino la presión ambiental acumulada de origen doméstico asociada al uso de energía.

---

## Licencia

Código bajo licencia [MIT](LICENSE). Los datos derivados y las figuras se ofrecen bajo
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/deed.es), con atribución a los
autores y a las fuentes primarias señaladas en `propensiones/FUENTES.md`.

## Cómo citar

Véase [`CITATION.cff`](CITATION.cff). Al citar resultados obtenidos con este material,
por favor cite también el artículo.

## Autores

Allan Quesada Rojas y Marco Otoya Chavarría — Universidad Nacional, Costa Rica.

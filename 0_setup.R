library(plotly)
library(tidyverse)
library(glue)

format_n <- function(x, y = 0L) {format(round(x, y), nsmall = y, decimal.mark=",", big.mark=".",
                                        justify = "right")}

# Base de datos
df_cursadas <- read_csv("data/fce_tablero - cursado por materia_v2.csv")

#secinstitucional@eco.uncor.ar


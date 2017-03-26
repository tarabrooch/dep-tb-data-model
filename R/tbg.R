#' Funciones para graficar
#'
#' Grafica de datos tidy
#' @param data datos tidy
#' @param tipo "line" o "column"
#' @import highcharter
#' @import dplyr
#' @export
tbg_hc_pagos <- function(data, tipo){

  hc <- highchart() %>%
    hc_tidy(data = data,
            group = FormaPago,
            nombres = gsub(pattern = "Pago_",
                           x = unique(data$FormaPago), replacement = ""),
            values = Pesos) %>%
    hc_chart(type = tipo)
  hc
}
#' Funciones para graficar
#'
#' Grafica de inventario físico
#' @param data datos tidy de tmb
#' @param tipo "line" o "column"
#' @import highcharter
#' @import dplyr
#' @export
tbg_hc_fisico_unit <- function(data, tipo){
  data <- data %>% filter(MARGEN_PROM<5)
  d <- data %>%
    filter(CORTE == "TOTAL") %>%
    left_join(.,
              data %>%
                filter(CORTE == "M90") %>%
                mutate("ARTICULOS_NUEVOS" = ARTICULOS,
                       "MARGEN_NUEVO" = MARGEN_PROM) %>%
                select(c(DESC_FAM, ARTICULOS_NUEVOS, MARGEN_NUEVO)))

  hc <- highchart() %>%
    hc_yAxis_multiples(
      list(
        title = list(text = "Unidades")),
      list(
        title = list(text = "Margen"),
        opposite = TRUE)
      ) %>%
    hc_add_serie(data = d$ARTICULOS,
                 name = "Más de 90 días") %>%
    hc_add_serie(data = d$ARTICULOS_NUEVOS,
                 name = "Menos de 90 días") %>%
    hc_add_serie(data = d$MARGEN_PROM,
                 name = "Margen (> 90 días)",
                 type = "line", yAxis = 1) %>%
    hc_add_serie(data = d$MARGEN_NUEVO,
                 name = "Margen (< 90 días)",
                 type = "line", yAxis = 1) %>%
    hc_chart(type = tipo) %>%
    hc_xAxis(categories = d$DESC_FAM) %>%
    # hc_yAxis(title = list(text = "Unidades")) %>%
    hc_plotOptions(series = list(stacking = "normal"))
  hc
}
#' Funciones para graficar
#'
#' Colores
#' @export
colores_nsdl4tb <- c("#521421", "#70592d", "#556b2f", "#bedcb6", "#d5c8b8",
                     "#f29576", terrain.colors(50))
#' Funciones para graficar
#'
#' Tidy graph
#' @export
hc_tidy <- function(hc, data,
                    group,
                    nombres,
                    values, ...){

  arguments <- as.list(match.call())
  cats <- eval(arguments$group, data)

  n <- length(unique(as.character(cats)))
  if (n > 0) {
    for (i in 1:n) {
      nm <- as.character(unique(cats)[i])
      dat <- eval(arguments$values, data)
      dat <- dat[cats == nm]
      hc <- hc_add_series(hc, name = nombres[i], data = dat, ...)
    }
  }
  hc
}

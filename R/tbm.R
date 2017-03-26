#' Funciones para manipular
#'
#'ojo, cambiar el nombre!
#' Manipula datos de corte, produce tipo de objeto tidy para gráficar
#' @param data Datos
#' @param tipo Actualmente por: "pagos"
#' @import dplyr
#' @import tidyr
#' @export
tbm_crear_graficar <- function(data, tipo){
  if(tipo == "pagos"){
    d <- data %>%
      select(c(FechaCorte, starts_with("Pago_"))) %>%
      tidyr::gather(key = FechaCorte)
    names(d) <- c("FechaCorte", "FormaPago", "Pesos")
    # d <- d %>% filter(Pesos>0)
  }else{
    if(tipo == "total"){
      d <- data %>%
        group_by(FechaCorte) %>%
        summarise(VentaTotal = sum(VentaTotal))
  }else{
    #otros tipos
    }
  }
  return(d)
}
#' Funciones para manipular
#'
#' Manipula datos de inventario fisico para gráficar
#' @param data Datos
#' @import dplyr
#' @import tidyr
#' @export
tbm_grafica_fisicos <- function(data){
  d <- data %>%
    group_by(DESC_FAM) %>%
    summarise("ARTICULOS" = n_distinct(ID),
              "PRECIO_PROM" = mean(PRECIO),
              "MARGEN_PROM" = mean(PRECIO)/mean(COSTO),
              "DIAS_FISICO_PROM" = mean(DIAS_FISICO)) %>%
    mutate("CORTE" = "TOTAL")

  d_nuevos <- data %>%
    filter(DIAS_FISICO<90) %>%
    group_by(DESC_FAM) %>%
    summarise("ARTICULOS" = n_distinct(ID),
              "PRECIO_PROM" = mean(PRECIO),
              "MARGEN_PROM" = mean(PRECIO)/mean(COSTO),
              "DIAS_FISICO_PROM" = mean(DIAS_FISICO)) %>%
    mutate("CORTE" = "M90")

  d <- rbind.data.frame(d, d_nuevos)

  return(d)
}
#' Funciones para manipular
#'
#' Manipula datos de ventas para gráficar
#' @param venta Datos de venta (no headers) de tbi
#' @import dplyr
#' @export
tbm_limpiar_ventas <- function(venta){
  venta %>% select(c(ID_MOV, FECHA, ID, ID_TIPO_MOV, MOVIMIENTO, FUENTE_MOV,
                     TIPO_SALIDA, VENDEDOR, DESC_MOV, COSTO, VALOR_MOV,
                     MARGEN, PROV, DESC, COSTO_OFICIAL, PRECIO, FECHA_ALTA,
                     LINEA, DESC_FAM, DESC_MET, DIAS_NUM, METAL_GRUPO, KILATAJE,
                     VALOR, INGRESO, TIPO_INGRESO, FECHA_VENCESEP))
}
#' Funciones para manipular
#'
#' Une cortes con ventas y obtiene diferencias atribuibles a otros tipos de ingresos.
#' @param cortes tabla de cortes
#' @param venta venta (header)
#' @import dplyr
#' @export
tbm_modelar_ingreso <- function(cortes, venta){
  venta <- venta %>%
    mutate(SEPARADO = ifelse(is.na(NUM_SEP), 1, 0)) %>%
    group_by(FECHA_CORTE, SEPARADO) %>%
    summarise(INGRESO = sum(IMPORTE)) %>%
    tidyr::spread(key = SEPARADO,
                  value = INGRESO)

  names(venta) <- c("FechaCorte", "Separados", "Venta")

  d <- cortes %>%
    left_join(venta, by = c("FechaCorte" = "FechaCorte")) %>%
    select(c(FechaCorte, VentaTotal, Venta, Separados)) %>%
    mutate(Venta = ifelse(is.na(Venta), 0, Venta),
           Separados = ifelse(is.na(Separados), 0, Separados)) %>%
    mutate(Otros = VentaTotal-Venta-Separados) %>%
    group_by(FechaCorte) %>% summarise_each(funs(mean(.))) %>%
    mutate(Otros = ifelse(Otros<0, 0, Otros))
  d
}

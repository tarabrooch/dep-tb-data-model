#' Funciones para inicializar
#'
#' Importa y guarda en data.frame objetos de diferentes bases de datos.
#' @param db base de datos ("cortes")
#' @param l Local (Computadora): 1 = Negocio local, 2 = Casa (Personal), 3 = Usuario
#' @export
tbi_importar <- function(db, l){
  if(db == "cortes"){
    s <- tbi_h_escogerlocal(l)
    archivo <- paste0(s, "Tara Brooch/TotalControl/BackEnd.xlsm")
    print(paste0(" >> Usando: tbi_importer_cortes en local: ", l))
    d <- tbi_importer_cortes(archivo)
  }
  if(db == "inventario"){
    s <- tbi_h_escogerlocal(l)
    archivo <- paste0(s, "Sistema/DATOS2/ACARTI00.DBF")
    m_log <- paste0(s, "Sistema/DATOS2/ADMOIN00.DBF")
    m_fam <- paste0(s, "Sistema/DATOS2/ACGRUP00.DBF")
    m_met <- paste0(s, "Sistema/DATOS2/ACMATE00.DBF")

    print(paste0(" >> Usando: tbi_importer_inv en local: ", l))
    d <- tbi_importer_inv(archivo = archivo,
                          cat_metales = m_met,
                          cat_fam = m_fam,
                          logs = m_log)
    }
  if(db == "ventas"){
    s <- tbi_h_escogerlocal(l)
    #archivo <- paste0(s, "Sistema/DATOS2/ADTISE00.DBF")
    archivo <- paste0(s, "Sistema/DATOS2/ADTIMA00.DBF")
    inventarios <- paste0(s, "Sistema/DATOS2/ACARTI00.DBF")
    m_fam <- paste0(s, "Sistema/DATOS2/ACGRUP00.DBF")
    m_met <- paste0(s, "Sistema/DATOS2/ACMATE00.DBF")
    tickets <- paste0(s, "Sistema/DATOS2/ADMOCL00.DBF")
    m_log <- paste0(s, "Sistema/DATOS2/ADMOIN00.DBF")

    print(paste0(" >> Usando: tbi_importer_ticks en local: ", l))

    d <- tbi_importer_ticks(archivo = archivo,
                            ticks = tickets,
                            logs = m_log,
                            inventarios = inventarios,
                          cat_metales = m_met,
                          cat_fam = m_fam)
  }
return(d)
}
#' Funciones para inicializar
#'
#' Importar y unir cortes diarios
#' @param archivo archivo de cortes
#' @import readxl
#' @import magrittr
#' @import reshape2
#' @import dplyr
#' @export
tbi_importer_cortes <- function(archivo){

  # 1. Importar
  header <- readxl::read_excel(archivo, sheet = "BD_Cortes_Header")
    print("Header importado...")
  clientes <- readxl::read_excel(archivo, sheet = "BD_Cortes_Clientes")
    print("Clientes importados...")
  vendedor <- readxl::read_excel(archivo, sheet = "BD_Cortes_Vendedor")
    print("Vendedores importados...")
  pagos <- readxl::read_excel(archivo, sheet = "BD_Cortes_Pagos")
    print("Pagos importados...")

  # 2. Limpiar
  header %<>% filter(VentaTotal!=0)
  clientes %<>% filter(Compra!=0)
  vendedor %<>% filter(Venta!=0)
  pagos %<>% filter(Venta!=0)

  # 3. Abrir las categorias, para hacer bind...
  # -----
    pagos <- reshape2::dcast(data = pagos,
                           formula = Hora_Fecha + Dia + Mes + Year + Sucursal ~ Pago,
                           fill = 0, value.var = "Venta", fun.aggregate = sum)
    pagos <- tbi_h_renombrar(pagos,
                             6:length(names(pagos)),
                             tipo = "pre", char = "Pago_")
  # -----
    clientes <- reshape2::dcast(data = clientes,
                                formula = Hora_Fecha + Dia + Mes + Year + Sucursal ~ TipoCliente,
                                fill = 0, value.var = "Compra", fun.aggregate = sum)
    clientes <- tbi_h_renombrar(clientes,
                                6:length(names(clientes)),
                                tipo = "pre", char = "Cliente_")
  # -----
    vendedor <- reshape2::dcast(data = vendedor,
                                formula = Hora_Fecha + Dia + Mes + Year + Sucursal ~ Vendedora,
                                fill = 0, fun.aggregate = sum, value.var = "Venta")
    vendedor <- tbi_h_renombrar(vendedor,
                                6:length(names(vendedor)),
                                tipo = "pre", char = "Vendedora_")
  # 4. Homologar algunas columnas
  names(clientes)[names(clientes)=="Cliente_Los demás"] <- "Cliente_LosDemas"

  header %<>% mutate("FechaCorte" = as.Date(paste0(Year,"/", Mes, "/", Dia)))

  vendedor %<>% mutate("FechaCorte" = as.Date(paste0(Year,"/", Mes, "/", Dia)),
                       "TimeStamp_v" = Hora_Fecha) %>%
    select(-c(Hora_Fecha, Dia, Mes, Year))

  clientes %<>% mutate("FechaCorte" = as.Date(paste0(Year,"/", Mes, "/", Dia)),
                       "TimeStamp_c" = Hora_Fecha) %>%
    select(-c(Hora_Fecha, Dia, Mes, Year))

  pagos %<>% mutate("FechaCorte" = as.Date(paste0(Year,"/", Mes, "/", Dia)),
                       "TimeStamp_p" = Hora_Fecha) %>%
    select(-c(Hora_Fecha, Dia, Mes, Year))

  # 5. Unir y exportar
  d <- header %>%
    left_join(., pagos) %>%
    left_join(., vendedor) %>%
    left_join(., clientes)
return(d)
}
#' Funciones para inicializar
#'
#' Importar y unir, con catalogo, inventario físico
#' @param archivo archivo de inventario
#' @param cat_metales archivo de catalogo de metales
#' @param cat_fam archivo de catalogo de familias
#' @import foreign
#' @import magrittr
#' @import reshape2
#' @import dplyr
#' @export
tbi_importer_inv <- function(archivo, cat_metales, cat_fam, logs){

  # totalidad de inventario...
  inv <- foreign::read.dbf(file = archivo, as.is = TRUE)
  names_inv <- c("ID", "PROV", "MODELO", "DESC", "GRAMO",
                 "FAM", "METAL", "V1", "COSTO", "PRECIO",
                 "V2", "V3", "V4", "V5", "V6", "V7", "V8",
                 "V9", "V10", "V11", "V12", "V13", "FECHA_ALTA",
                 "V14", "V15", "V16", "LINEA", "V17",
                 "V18", "V19", "V20","V21","V22",
                 "V23","V24","V25","V26","V27","V28","V29",
                 "V30","V31","V32")
  names(inv) <- names_inv
  inv %<>% dplyr::select(c(ID, PROV, MODELO,
                    DESC, GRAMO, FAM, METAL, COSTO, PRECIO,
                    FECHA_ALTA, LINEA))
  # movimientos...
  movs <- foreign::read.dbf(file = logs, as.is = TRUE)
  movs %<>% select(c(ADMOIN01, ADMOIN02, ADMOIN03,
                     ADMOIN04, ADMOIN10, ADMOIN11, ADMOIN17, ADMOIN18))
  names(movs) <- c("ID", "ID_TIPO_MOV", "MOVIMIENTO", "FUENTE_MOV",
                   "ID_MOV", "FECHA", "USER", "DESC_MOV")
  # familia
  fam <- foreign::read.dbf(file = cat_fam, as.is = TRUE)
  names(fam) <- c("ID_FAM", "DESC_FAM", "GRUPO_FAM_ORIGEN",
                  paste0("V", 1:8))
  fam %<>% dplyr::select(c(ID_FAM, DESC_FAM))

  # metal
  met <- foreign::read.dbf(file = cat_metales, as.is = TRUE)
  names(met) <- c("MET", "DESC_MET", "ABR_MET", paste0("V", 1:8))
  met %<>% dplyr::select(c(MET, DESC_MET))

  # preparar
  movs %<>%
    mutate("VENTA_FECHA" = ifelse(MOVIMIENTO == "VENTA", FECHA, Sys.Date()))
  class(movs$VENTA_FECHA) <- "Date"

  # las ventas x dia
  ventas_dias <- movs %>%
    group_by(ID) %>%
    summarise("M_UNICOS" = n_distinct(MOVIMIENTO)) %>%
    filter(M_UNICOS>1) %>%
    inner_join(., movs %>%
                 group_by(ID) %>%
                 summarise("DIAS_PASADOS" = mean(VENTA_FECHA)-mean(FECHA),
                           "ENTRADA" = min(FECHA))) %>%
    arrange(desc(ENTRADA)) %>%
    # agregar los demas indicadores que ya teníamos...
    inner_join(., inv) %>%
    left_join(., fam, by = c("FAM" = "ID_FAM")) %>%
    left_join(., met, by = c("METAL" = "MET"))

  # productos que siguen aquí...
  inv_fisico <- inv %>%
    filter(!(ID %in% ventas_dias$ID)) %>%
    left_join(., fam, by = c("FAM" = "ID_FAM")) %>%
    left_join(., met, by = c("METAL" = "MET")) %>%
    mutate("DIAS_FISICO" = as.numeric(Sys.Date()-as.Date(FECHA_ALTA)),
           "SEG_DIASF" = ifelse(DIAS_FISICO>365, "MAS DE 1 AÑO",
                                ifelse(DIAS_FISICO>180, "MAS DE 6 MESES",
                                       ifelse(DIAS_FISICO>90, "MAS DE 90 DIAS",
                                              ifelse(DIAS_FISICO>45, "MAS DE 45 DIAS",
                                                     ifelse(DIAS_FISICO>30,"MAS DE 30 DIAS",
                                                            "MENOS DE 30 DIAS"))))))

  list_inv <- list("FISICO" = inv_fisico,
                   "VENTAS" = ventas_dias)
  return(list_inv)
}
#' Funciones para inicializar
#'
#' Importar y unir, con catalogo, ventas
#' @param archivo archivo de inventario
#' @param cat_metales archivo de catalogo de metales
#' @param cat_fam archivo de catalogo de familias
#' @import foreign
#' @import magrittr
#' @import reshape2
#' @import dplyr
#' @import stringi
#' @export
tbi_importer_ticks <- function(archivo,
                               inventarios,
                               ticks,
                               logs,
                               cat_metales,
                               cat_fam){
  # headers de tickets
  v_headers <- foreign::read.dbf(file = archivo, as.is = TRUE)
  names(v_headers) <- c("PARTIDA", "V1", "V2", "V3", "FECHA_CORTE",
                "VENDEDOR", paste0("V", 5:12), "FECHA_REAL", "V13",
                "V14", "TIMESTAMP", "SUBTOTAL", "IVA", "IMPORTE",
                "V15", "V16", "V17", "V18", "DESC_NOTA_CLIENTE",
                "FORMA_PAGO", "TIPO_PAGO_GPO", "TOTALPARTIDA",
                "NO_ABONO_NS", "V19", "V20", "NS2", "TOTAL_NS",
                "TIPO_TICKET", "RUTA_ORDEN", "NUM_SEP",
                paste0("V", 21:28))
  v_headers %<>% select(PARTIDA, FECHA_CORTE, VENDEDOR, FECHA_REAL,
                      TIMESTAMP, SUBTOTAL, IVA, IMPORTE, DESC_NOTA_CLIENTE,
                      FORMA_PAGO, TIPO_PAGO_GPO, TOTALPARTIDA, TIPO_TICKET,
                      RUTA_ORDEN, NUM_SEP)

  # tickets sin inv
  v_ticks <- foreign::read.dbf(ticks, as.is = TRUE)
  names(v_ticks) <- c("V1", "FECHA1", "FECHA2", "ID_MOV",
                      "TIPO_MOV_ID", "VALOR", "INGRESO",
                      "V2", "V3", "VENDEDOR_TCKS", "ID_MOV_2",
                      "V4", "ID_MOV_3", "DESC_MOV_NOTA", "ID_MOV_4",
                      "ID_MOV_5", "ID_MOV_6", "TIPO_INGRESO", "V5",
                      "V6", "FECHA_VENCESEP", paste0("V", 7:16))
  v_ticks %<>% filter(TIPO_INGRESO == "VENTAS")

  # caracteristicas de inventarios...
  inv <- foreign::read.dbf(file = inventarios, as.is = TRUE)
  names(inv) <- c("ID", "PROV", "MODELO", "DESC", "GRAMO",
                 "FAM", "METAL", "V1", "COSTO_OFICIAL", "PRECIO",
                 "V2", "V3", "V4", "V5", "V6", "V7", "V8",
                 "V9", "V10", "V11", "V12", "V13", "FECHA_ALTA",
                 "V14", "V15", "V16", "LINEA", "V17",
                 "V18", "V19", "V20","V21","V22",
                 "V23","V24","V25","V26","V27","V28","V29",
                 "V30","V31","V32")
  inv %<>% dplyr::select(c(ID, PROV, MODELO,
                           DESC, GRAMO, FAM, METAL,
                           COSTO_OFICIAL, PRECIO,
                           FECHA_ALTA, LINEA))

  # familia
  fam <- foreign::read.dbf(file = cat_fam, as.is = TRUE)
  names(fam) <- c("ID_FAM", "DESC_FAM", "GRUPO_FAM_ORIGEN",
                  paste0("V", 1:8))
  fam %<>% dplyr::select(c(ID_FAM, DESC_FAM))

  # metal
  met <- foreign::read.dbf(file = cat_metales, as.is = TRUE)
  names(met) <- c("MET", "DESC_MET", "ABR_MET", paste0("V", 1:8))
  met %<>% dplyr::select(c(MET, DESC_MET))

  # movimientos
  movs <- foreign::read.dbf(file = logs, as.is = TRUE)
  names(movs) <- c("ID", "ID_TIPO_MOV", "MOVIMIENTO",
                   "FUENTE_MOV", "TIPO_SALIDA", "COSTO",
                   "V3", "ID_NS", "V5", "ID_MOV",
                   "FECHA", "V6", "V7", "VALOR_MOV",
                   "V9", "V10", "VENDEDOR", "DESC_MOV",
                   "V12", "FECHA2", paste0("V",14:24))
  movs %<>% dplyr::select(c(ID_MOV, FECHA, ID, ID_TIPO_MOV,
                            MOVIMIENTO,FUENTE_MOV,
                            TIPO_SALIDA, ID_NS, VENDEDOR,
                            DESC_MOV, COSTO, VALOR_MOV))
  # solo ventas
  movs %<>% dplyr::filter(MOVIMIENTO == "VENTA")

  # preparar - agregar los catalogos a las ventas...
  movs %<>%
  left_join(., inv, by = c("ID" = "ID")) %>%
  left_join(., fam, by = c("FAM" = "ID_FAM")) %>%
    left_join(., met, by = c("METAL" = "MET")) %>%
    mutate("DIAS_DESDE" = difftime(as.Date(FECHA), as.Date(FECHA_ALTA), units = "d")) %>%
    mutate("DIAS_NUM" = as.numeric(DIAS_DESDE)) %>%
    mutate("METAL_GRUPO" = ifelse(METAL == "PLA", "PLATA",
                                  ifelse(substr(METAL, 1,2) %in% c("OA","OB","OC","OF", "AB"), "ORO",
                                         "OTRO")))
  movs$KILATAJE <- ifelse(stringi::stri_extract(str = movs$METAL, regex = "[0-9]$") == "4", "14K",
                          ifelse(stringi::stri_extract(str = movs$METAL, regex = "[0-9]$")== "0", "10k",
                                 ifelse(stringi::stri_extract(str = movs$METAL, regex = "[0-9]$")=="8", "18k", "Otro")
                                 ))
  movs %<>% left_join(., v_ticks, by = c("ID_MOV" = "ID_MOV")) %>%
    mutate("MARGEN" = VALOR_MOV / COSTO_OFICIAL)


  list_exp <- list("HEADERS" = v_headers,
                   "VENTAS_REAL" = movs)
}
#' Funciones para inicializar
#'
#' Retorna string de path a archivos, dependiendo el local
#' @param l Local (Computadora): 1 = Negocio local, 2 = Casa (Personal), 3 = Usuario
tbi_h_escogerlocal <- function(l){
  if(l == 1){
    s <- "c:/usuario.joyeria/"
  }else{
    if(l == 2){
      s <- "/Users/eduardoflores/Documents/Dropbox/Proyectos/"
    }else{
      if(l == 3){
        s <- "c:/usuario.mama/"
      }else{
        stop("local incorrecto, escoger 1, 2 o 3")
      }
    }
  }
  return(s)
}
#' Funciones para inicializar
#'
#' Importar y unir, con catalogo, ventas
#' @param archivo archivo de inventario
#' @param cat_metales archivo de catalogo de metales
#' @param cat_fam archivo de catalogo de familias
#' @import foreign
#' @import magrittr
#' @import reshape2
#' @import dplyr
#' @import stringi
#' @export
tbi_importer_seps <- function(archivo,
                               inventarios,
                               ticks,
                               logs,
                               cat_metales,
                               cat_fam){

}
#' Funciones para inicializar
#'
#' Renombra una serie de columnas, manteniendo el nombre original
#' @param df data.frame
#' @param columnas Numeros de columnas a renombrar
#' @param tipo tipo de renombre ("pre" = antes o "pos" = despues)
#' @param char caracteres a poner antes o después
#' @export
tbi_h_renombrar <- function(df, columnas, tipo = "pre", char){

  if(tipo == "pre"){
    s <- names(df)[columnas]
    s_n <- paste0(char, s)
  }else{
    if(tipo == "pos"){
      s <- names(df)[columnas]
      s_n <- paste0(s, char)
    }else{
      stop("Escoger pre o pos")
    }
  }
  names(df)[columnas] <- s_n
  return(df)
}

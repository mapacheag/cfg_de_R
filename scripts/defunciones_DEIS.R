# CARGAR LIBRERIAS Y PAQUETES

library(dplyr)
library(readr)
library(ggplot2)

# CARGAR BASE DE DATOS


##### LINUX  #########
#df_main <- readr::read_csv2(
 # "bases_de_datos/DEFUNCIONES_FUENTE_DEIS_1990_2022_CIFRAS_OFICIALES.csv",
  #locale = readr::locale(encoding = "ISO-8859-1")
#)

##### WINDOWS  #########
df_main <- readr::read_csv2(
  "../bases_de_datos/DEFUNCIONES_FUENTE_DEIS_1990_2022_CIFRAS_OFICIALES.csv",
  locale = readr::locale(encoding = "ISO-8859-1")
)


#head(df_main)
#View(head(df_main))

# FILTRAR POR CODIGO DIAG2 ENTRE X60 Y X84
# filtrado primario antiguo
#df_main$DIAG2 <- as.character(df_main$DIAG2)

#df_filtrado <- df_main %>%
 # filter(DIAG2 >= "X60" & DIAG2 <= "X84")

######## CODIGO CORREGIDO PARA FILTRAR SUICIDIOS ####

#df_filtrado <- df_main %>%
 # grepl("^X(6[0-9]7[0-9]8[0-4])", diag2)
# sugerencia en correo
#dplyr::mutate(suicide= dplyr::case_when(grepl(""^X(6[0-9]|7[0-9]|8[0-4])"", diag2) ~ 1,T~0))

df_filtrado <- df_main %>%
  dplyr::mutate(DIAG2 = as.character(DIAG2)) %>%
  dplyr::filter(
    grepl("^X(6[0-9]|7[0-9]|8[0-4])", DIAG2)
  )


#View(head(df_filtrado, 100))

# SE ELIMINAN VARIABLES SIN USO EN INVESTIGACION

df_filtrado2 <- df_filtrado %>%
  dplyr::select(
    -EDAD_TIPO,
    -COD_COMUNA,
    -COMUNA,
    -DIAG1,
    -CAPITULO_DIAG1,
    -GLOSA_CAPITULO_DIAG1,
    -CODIGO_GRUPO_DIAG1,
    -GLOSA_GRUPO_DIAG1,
    -CODIGO_CATEGORIA_DIAG1,
    -GLOSA_CATEGORIA_DIAG1,
    -CODIGO_SUBCATEGORIA_DIAG1,
    -GLOSA_SUBCATEGORIA_DIAG1,
    -DIAG2,
    -CAPITULO_DIAG2,
    -GLOSA_CAPITULO_DIAG2,
    -CODIGO_GRUPO_DIAG2,
    -GLOSA_GRUPO_DIAG2,
    -CODIGO_CATEGORIA_DIAG2,
    -GLOSA_CATEGORIA_DIAG2,
    -CODIGO_SUBCATEGORIA_DIAG2,
    -GLOSA_SUBCATEGORIA_DIAG2,
  )
    
#View(head(df_filtrado2, 100))
# SEPARACION DE DATA FRAMES
# MIXTO = df_final
# MUJER = df_fem
# HOMBRE = df_masc

df_final <- df_filtrado2

#View(head(df_final, 100))

df_fem <- df_final %>%
  dplyr::filter(SEXO_NOMBRE == "Mujer")

df_masc <- df_final %>%
  dplyr::filter(SEXO_NOMBRE == "Hombre")

#View(head(df_fem, 50))

#View(head(df_masc, 50))

# TOTAL DE DEFUNCIONES POR AÑO    ############# AJUSTE PARA INDICE CADA 100.000 HABITANTES#####

#df_defunciones_anuales <- df_main %>%
 # dplyr::mutate(AÑO = as.numeric(AÑO)) %>%
  #dplyr::filter(AÑO >= 2012 & AÑO <= 2022) %>%
  #dplyr::group_by(AÑO) %>%
  #dplyr::summarise(
   # N_DEFUNCIONES = n()
  #)

#print(df_defunciones_anuales)

df_conteo_fem <- df_fem %>%
  dplyr::mutate(AÑO = as.numeric(AÑO)) %>%
  dplyr::filter(AÑO >= 2012 & AÑO <= 2022) %>%
  dplyr::group_by(AÑO) %>%
  dplyr::summarise(
    N_SUICIDIOS = n()
  )

print(df_conteo_fem)

df_conteo_masc <- df_masc %>%
  dplyr::mutate(AÑO = as.numeric(AÑO)) %>%
  dplyr::filter(AÑO >= 2012 & AÑO <= 2022) %>%
  dplyr::group_by(AÑO) %>%
  dplyr::summarise(
    N_SUICIDIOS = n()
  )

print(df_conteo_masc)

df_conteo_mixto <- df_final %>%
  dplyr::mutate(AÑO = as.numeric(AÑO)) %>%
  dplyr::filter(AÑO >= 2012 & AÑO <= 2022) %>%
  dplyr::group_by(AÑO) %>%
  dplyr::summarise(
    N_SUICIDIOS = n()
  )

print(df_conteo_mixto)



#####################################################################
########### CATEGORIZAR EN RANGOS ETARIOS ###########################
#####################################################################


categorizar_edad_suicidios <- function(df, clumna_edad = "EDAD_CANT") {
  
  limites_rangos <- c(0,14,64,Inf)
  etiquetas_rangos <- c("0-14", "15-64", ">65")
  
  df %>%
    dplyr::mutate(
      RANGO_ETARIO = cut(
        x = EDAD_CANT,
        breaks = limites_rangos,
        labels = etiquetas_rangos,
        right = TRUE,
        include.lowest = TRUE
      )
    ) %>%
    dplyr::filter(!is.na(RANGO_ETARIO))
}

df_etario_conteo_mixto <- df_final %>%
  categorizar_edad_suicidios() %>%
  dplyr::group_by(RANGO_ETARIO) %>%
  dplyr::summarise(N_SUICIDIOS = n()) %>%
  dplyr::ungroup()

print("Cantidad de suicidios generales por rango etario (2012-2022)")
print(df_etario_conteo_mixto)

df_etario_conteo_fem <- df_fem %>%
  categorizar_edad_suicidios() %>%
  dplyr::group_by(RANGO_ETARIO) %>%
  dplyr::summarise(N_SUICIDIOS = n()) %>%
  dplyr::ungroup()

print("Cantidad de sucidios en mujeres por rango etario (2012-2022)")
print(df_etario_conteo_fem)

df_etario_conteo_masc <- df_masc %>%
  categorizar_edad_suicidios() %>%
  dplyr::group_by(RANGO_ETARIO) %>%
  dplyr::summarise(N_SUICIDIOS = n()) %>%
  dplyr::ungroup()

print("Cantidad de suicidios en hombres por rango etario (2012-2022)")
print(df_etario_conteo_masc)


############# CALCULO DE LA TASA DE SUICIDIOS CADA 100.000 HABITANTES; UTILIZANDO PROYECCIONES ANUALES
############# DE POBLACION DEL INE

df_poblacion_anual <- tibble::tibble(
  AÑO = 2012:2022,
  POBLACION_TOTAL = c(
    17443491, #2012
    17611902, #2013
    17787617, #2014
    17971423, #2015
    18167147, #2016
    18419192, #2017
    18751405, #2018
    19107216, #2019
    19458310, #2020
    19678363, #2021
    19828563 #2022
  )
)


calcular_tasa_anual <- function(df_conteo, df_poblacion_anual){
  df_conteo %>%
    dplyr::left_join(df_poblacion_anual, by = "AÑO") %>%
    
    dplyr::mutate(
      TASA = round(
        (N_SUICIDIOS / POBLACION_TOTAL) * 100000,
        digits = 5
      )
    ) %>%
    dplyr::select(AÑO,TASA)
}


# DF con incoporacion de indice
df_fem_i <- calcular_tasa_anual(df_conteo_fem, df_poblacion_anual)
df_masc_i <- calcular_tasa_anual(df_conteo_masc, df_poblacion_anual)
df_conteo_mixto_i <- calcular_tasa_anual(df_conteo_mixto, df_poblacion_anual)

print(df_fem_i)

print(df_masc_i)

print(df_conteo_mixto_i)

#################################################################
######## EN LA NOCHE AGREGO LA TASA PARA RANGOS ETARIOS #########
#################################################################



#df_conteo_fem_i <- df_conteo_fem %>%
 # dplyr::left_join(df_defunciones_anuales, by = "AÑO") %>%
  #dplyr::mutate(
   # PORCENTAJE_SUICIDIO = round((N_SUICIDIOS / N_DEFUNCIONES) * 100,
    #                            digits = 2)
#  ) %>%
 # dplyr::select(-N_DEFUNCIONES,-N_SUICIDIOS)
#print(df_conteo_fem_i)

#df_conteo_masc_i <- df_conteo_masc %>%
 # dplyr::left_join(df_defunciones_anuales, by = "AÑO") %>%
#  dplyr::mutate(
 #   PORCENTAJE_SUICIDIO = round((N_SUICIDIOS / N_DEFUNCIONES) * 100,
  #                              digits = 2)
#  ) %>%
 # dplyr::select(-N_DEFUNCIONES,-N_SUICIDIOS)
#print(df_conteo_masc_i)
#
#df_conteo_mixto_i <- df_conteo_mixto %>%
 # dplyr::left_join(df_defunciones_anuales, by = "AÑO") %>%
  #dplyr::mutate(
   # PORCENTAJE_SUICIDIO = round((N_SUICIDIOS / N_DEFUNCIONES) * 100,
   #                             digits = 2)
  #) %>%
  #dplyr::select(-N_DEFUNCIONES,-N_SUICIDIOS)
#print(df_conteo_mixto_i)



#####################################################################################
#####################################################################################
################### SECCIÓN DE GRÁFICOS - SE REEMPLAZA POR EL USO DE SHINY ##########
#####################################################################################
#####################################################################################



#grafico_suicidios_fem <- ggplot2::ggplot(
 # data = df_conteo_fem_i,
  #mapping = ggplot2::aes(x = AÑO, y = PORCENTAJE_SUICIDIO)
#)+
#  ggplot2::geom_line(color = "blue", size = 1) +
#  ggplot2::geom_point(color = "blue", size = 3) +
#  ggplot2::scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +
#  ggplot2::scale_y_continuous(limits = c(0.20, 0.40),breaks = seq(0.00, 2.00, by = 0.10)) +
 # ggplot2::labs(
  #  title = "Tasa de suicidios en mujeres (2012-2022)",
  #  x = "Año",
   # y = "Suicidios por cada 100.000 habitantes"
  #) +
  #ggplot2::theme_minimal()

#print(grafico_suicidios_fem)

#grafico_suicidios_masc <- ggplot2::ggplot(
 # data = df_conteo_masc_i,
  #mapping = ggplot2::aes(x = AÑO, y = PORCENTAJE_SUICIDIO)
#)+
 # ggplot2::geom_line(color = "green", size = 1) +
  #ggplot2::geom_point(color = "green", size = 3) +
  #ggplot2::scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +
 # ggplot2::scale_y_continuous(limits = c(0.70, 1.70),breaks = seq(0.00, 2.00, by = 0.10)) +
  #ggplot2::labs(
   # title = "Tasa de suicidios en hombres (2012-2022)",
    #x = "Año",
    #y = "Suicidios por cada 100.000 habitantes"
#  ) +
 # ggplot2::theme_minimal()

#print(grafico_suicidios_masc)


#grafico_suicidios_mixto <- ggplot2::ggplot(
 # data = df_conteo_mixto_i,
  #mapping = ggplot2::aes(x = AÑO, y = PORCENTAJE_SUICIDIO)
#)+
 # ggplot2::geom_line(color = "red", size = 1) +
  #ggplot2::geom_point(color = "red", size = 2) +
  #ggplot2::scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +
#  ggplot2::scale_y_continuous(limits = c(1.10, 1.90),breaks = seq(0.00, 2.00, by = 0.10)) +
  #ggplot2::labs(
   # title = "Tasa de suicidios general (2012-2022)",
    #x = "Año",
    #y = "Suicidios por cada 100.000 habitantes"
  #) +
  #ggplot2::theme_minimal()

#print(grafico_suicidios_mixto)


# GRAFICO DE MEZCLAS (FEM Y MASC)

#df_etiqueta_f <- df_conteo_fem_i %>%
 # dplyr::mutate(SEXO = "Mujer")
#df_etiqueta_m <- df_conteo_masc_i %>%
 # dplyr::mutate(SEXO ="Hombre")
#df_comparacion_de_etiquetas <- dplyr::bind_rows(df_etiqueta_f,df_etiqueta_m)
#print(df_comparacion_de_etiquetas)

#library(ggplot2)
#grafico_comparativo <- ggplot2::ggplot(
 # data = df_comparacion_de_etiquetas,
  #mapping = ggplot2::aes(x=AÑO, y=PORCENTAJE_SUICIDIO, color=SEXO)
#) +
 # ggplot2::geom_line(size=1) +
  #ggplot2::geom_point(size=3)+
  #ggplot2::scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +
 # ggplot2::scale_y_continuous(
  #  limits = c(0.00, 2.00),
   # breaks = seq(0.00, 2.00, by = 0.10)
  #) +
  #ggplot2::scale_color_manual(
   # values = c("Hombre" = "green", "Mujer" = "blue"),
    #name = "Sexo"
  #) +
  #ggplot2::labs(
   # title = "Comparativa de Tasa de Suicidio por Sexo (2012-2022)",
  #  x = "Año",
   # y = "Suicidios por cada 100.000 habitantes"
  #) +
  #ggplot2::theme_minimal()

#print(grafico_comparativo)





###################################################################################
###################### INICIO DE PLANTILLA SERIE DE TIEMPO ########################

#si no existe el paquete plotly, lo instalamos
if(!require(plotly)){install.packages("plotly")}                 # Verifica e instala plotly si es necesario

#Formatea el rango de fechas desde el 2012 hasta el 2022, en rangos de 1 año
fechas_comparativo <- seq(as.Date("2012-01-01"), as.Date("2022-01-01"), by = "1 year")  # Crea secuencia de fechas anuales
n_comparativo <- length(fechas_comparativo)                     # Cuenta número de años (11)

#Series de datos
serie_verde_comparativo <- df_fem_i$TASA                         # Extrae tasas de suicidio femeninas
serie_azul_comparativo  <- df_masc_i$TASA                        # Extrae tasas de suicidio masculinas

#Une los datos a graficar
datos_comparativo <- tibble::tibble(                             # Crea tibble con datos
  año = fechas_comparativo,                                      # Columna de fechas
  verde = serie_verde_comparativo,                               # Columna de datos femeninos
  azul  = serie_azul_comparativo                                 # Columna de datos masculinos
) |>
  dplyr::mutate(                                                 # Transforma datos
    año_anio = format(año, "%b %Y"),                            # Formatea fecha como "Mes Año"
    # Crear columnas de tooltip
    text_mujeres = paste0("Año: ", format(año, "%Y"), "<br>",   # Tooltip para mujeres con etiquetas personalizadas
                          "Grupo: Mujeres<br>",
                          "Tasa: ", round(verde, 2)),
    text_hombres = paste0("Año: ", format(año, "%Y"), "<br>",   # Tooltip para hombres con etiquetas personalizadas
                          "Grupo: Hombres<br>",
                          "Tasa: ", round(azul, 2))
  ) |>
  dplyr::select(año, año_anio, verde, azul, text_mujeres, text_hombres)  # Selecciona columnas relevantes

# Gráfico ggplot básico 
p3 <- ggplot2::ggplot(datos_comparativo, ggplot2::aes(x = año)) +        # Inicia gráfico ggplot con eje X de fechas
  
  geom_line(
    ggplot2::aes(y = verde, color = "Mujeres"),                         # Línea para datos femeninos
    size = 1                                                            # Grosor de línea
  ) +
  
  geom_line(
    ggplot2::aes(y = azul, color = "Hombres"),                          # Línea para datos masculinos
    size = 1                                                            # Grosor de línea
  ) +
  
  scale_color_manual(                                                    # Define colores de las líneas
    name = "Leyenda",                                                   # Título de la leyenda
    values = c("Mujeres" = "#01c9ad", "Hombres" = "#9682fc")            # Códigos hexadecimales para colores
  ) +
  
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +            # Formato eje X: solo año, marcas anuales
  
  labs(y = "Suicidios por cada 100.000 habitantes", x = "Año") +        # Etiquetas de ejes
  
  theme_minimal()                                                       # Tema minimalista para gráfico

# Mostrar gráfico estático
print(p3)                                                               # Imprime gráfico ggplot estático

# Convertir a plotly - Asegurar que plotly_p3 use datos_comparativo
plotly_p3 <- plotly::ggplotly(p3) |>                                    # Convierte ggplot a plotly interactivo
  plotly::style(                                                        # Personaliza primera traza (mujeres)
    # Tooltip para línea de mujeres (trace 1)
    text = datos_comparativo$text_mujeres,                             # Texto personalizado para tooltip
    hoverinfo = "text",                                                 # Solo muestra texto personalizado
    traces = 1                                                          # Aplica a primera línea (mujeres)
  ) |>
  plotly::style(                                                        # Personaliza segunda traza (hombres)
    # Tooltip para línea de hombres (trace 2)  
    text = datos_comparativo$text_hombres,                             # Texto personalizado para tooltip
    hoverinfo = "text",                                                 # Solo muestra texto personalizado
    traces = 2                                                          # Aplica a segunda línea (hombres)
  ) |>
  plotly::layout(                                                       # Personaliza layout del gráfico
    font = list(family = "Arial, sans-serif"),                         # Fuente de todo el texto
    paper_bgcolor = "rgba(0,0,0,0)",                                   # Fondo transparente del área exterior
    plot_bgcolor = "rgba(0,0,0,0)",                                    # Fondo transparente del área del gráfico
    hoverlabel = list(                                                  # Personaliza etiquetas al pasar cursor
      bgcolor = "white",                                               # Color de fondo del tooltip
      font = list(size = 12),                                          # Tamaño de fuente del tooltip
      bordercolor = "black"                                        # Color del borde del tooltip
      # PARA CAMBIAR COLOR DEL TEXTO DEL TOOLTIP: añadir color = "color_deseado"
      # Ejemplo: font = list(size = 12, color = "black")
    ),
    hovermode = "x"                                                     # Muestra tooltips alineados verticalmente en misma X
  )

# Mostrar gráfico interactivo
print(plotly_p3)                                                        # Imprime gráfico plotly interactivo

######################## FIN DE PLANTILLA SERIE DE TIEMPO #########################
###################################################################################

warnings()

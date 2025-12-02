# CARGAR LIBRERIAS Y PAQUETES

library(dplyr)
library(readr)
library(ggplot2)

# CARGAR BASE DE DATOS


##### LINUX  #########
df_main <- readr::read_csv2(
  "bases_de_datos/DEFUNCIONES_FUENTE_DEIS_1990_2022_CIFRAS_OFICIALES.csv",
  locale = readr::locale(encoding = "ISO-8859-1")
)

##### WINDOWS  #########
#df_main <- readr::read_csv2(
 # "../bases_de_datos/DEFUNCIONES_FUENTE_DEIS_1990_2022_CIFRAS_OFICIALES.csv",
  #locale = readr::locale(encoding = "ISO-8859-1")
#)


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

# GRAFICO SUICIDIOS/TIEMPO (2012-2022) FEM y MASC

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
if(!require(plotly)){install.packages("plotly")}

# Datos tipo "tibble" (generados de forma reproducible)
set.seed(123)
fechas <- seq(as.Date("2012-01-01"), as.Date("2022-01-01"), by = "1 year")
n <- length(fechas)

# Series que imitan patrones y rangos de IVE en el período (verde ~40–70; azul ~2–16)
set.seed(2125)
#datos aleatorios. de ahi, reemplazar por sus datos
serie_verde <- rnorm(n= n, 
                     mean=.28, 
                     sd=.05)

datos <- tibble::tibble(
  mes = fechas,
  verde = serie_verde#,
  #azul  = serie_azul #sacamos la otra serie
) |>
  dplyr::mutate(mes_anio = format(mes, "%b %Y")) |>
  dplyr::select(mes, mes_anio, verde) #, azul) #sacamos la otra serie

p3<- 
  ggplot2::ggplot(datos, ggplot2::aes(x = mes)) + #formato wide. cada variable es una columna, en este caso
  geom_line(ggplot2::aes(y = verde, color = "Mujeres"), size = 1) + #añadimos una capa de línea con las interrupciones
  #geom_line(ggplot2::aes(y = azul, color = "Continuaciones"), size = 1) + #añadimos una capa de línea con las continuaciones
  scale_color_manual(#generamos la leyenda
    name = "Leyenda",#título de la leyenda
    values = c("Mujeres" = "#01c9ad", "Continuaciones" = "#9682fc"))+#colores de las líneas e identificador de las líneas
  labs(y = "Suicidios por cada 100.000 habitantes", x = "Año")+ #Definimos las etiquetas de ejes
  theme_minimal(base_family = "storia-sans") #+ #tema minimalista, con la fuente personalizada

p3 #grafico estático

plotly::ggplotly(p3) |> #convertimos a plotly. ojo que el tooltip se genera por defecto, pero se puede personalizar
  layout(font=list(family="storia-sans"), # fuente personalizada
         paper_bgcolor = "rgba(0,0,0,0)",  plot_bgcolor  = "rgba(0,0,0,0)") # → fondo del lienzo y fondo del área de trazado


######################## FIN DE PLANTILLA SERIE DE TIEMPO #########################
###################################################################################

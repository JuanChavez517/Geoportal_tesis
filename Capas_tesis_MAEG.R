#cargar las librerias
library("sf")
library("leaflet")
library("leaflegend")
library("leaflet.extras")
library("openxlsx")
library("dplyr")
library("htmlwidgets")


#espacio de trabajo
setwd("C:/Users/juamc/OneDrive/Documentos/Juan_Chavez/Tesis_maestria/Geoportal")

#archivo shapefile de los municipios del Estado de Mexico
ruta_mun_edomex = ("Mun_edomex_capitulo_3/Mun_edomex.shp")
shp_mun_edomex = st_read(ruta_mun_edomex)

#reproyectar a WGS84
shp_mun_edomex = st_transform(shp_mun_edomex, crs = 4326)
print(shp_mun_edomex)

#importar los resulados de LISA
ruta_lisa_clusters = ("Clusters/Clusters_ArcMap.xlsx")
xlsx_lisa_clusters = read.xlsx(ruta_lisa_clusters)
xlsx_lisa_clusters$CVEGEO = as.character(xlsx_lisa_clusters$CVEGEO)
colnames(xlsx_lisa_clusters)[colnames(xlsx_lisa_clusters) == "2020"] = "lisa20"
colnames(xlsx_lisa_clusters)[colnames(xlsx_lisa_clusters) == "2021"] = "lisa21"
colnames(xlsx_lisa_clusters)[colnames(xlsx_lisa_clusters) == "2022"] = "lisa22"

#union resultados LISA
shp_mun_edomex = left_join(shp_mun_edomex, xlsx_lisa_clusters, by = "CVEGEO")

#importar los resulados de GWR
ruta_gwr = ("GWR/2022_6_par_listwise.xlsx")
xlsx_gwr = read.xlsx(ruta_gwr)
View(xlsx_gwr)

#union resultados GWR
shp_mun_edomex = left_join(shp_mun_edomex, xlsx_gwr, by = "CVEGEO")

#definir la paleta de colores de LISA
colores_lisa = c("0" = "#FFFFFF", "1" = "#FF0000", "2" = "#0070FF",
                "3" = "#AFC1EA", "4" = "#FFBEBE")

#plot autocorrelacion
plot(shp_mun_edomex["lisa22"], pal = colores_lisa)

#crear nueva columna con la etiqueta de lisa
shp_mun_edomex$label_lisa20 <- ifelse(shp_mun_edomex$lisa20 == 0,
                                      "No significativo",
                                      ifelse(shp_mun_edomex$lisa20 == 1,
                                      "Alto-Alto",
                                      ifelse(shp_mun_edomex$lisa20 == 2,
                                      "Bajo-Bajo",
                                      ifelse(shp_mun_edomex$lisa20 == 3,
                                      "Bajo-Alto",
                                      ifelse(shp_mun_edomex$lisa20 == 4,
                                      "Alto-Bajo",
                                      "NA")))))

shp_mun_edomex$label_lisa21 <- ifelse(shp_mun_edomex$lisa20 == 0,
                                      "No significativo",
                                      ifelse(shp_mun_edomex$lisa20 == 1,
                                      "Alto-Alto",
                                      ifelse(shp_mun_edomex$lisa20 == 2,
                                      "Bajo-Bajo",
                                      ifelse(shp_mun_edomex$lisa20 == 3,
                                      "Bajo-Alto",
                                      ifelse(shp_mun_edomex$lisa20 == 4,
                                      "Alto-Bajo",
                                      "NA")))))

shp_mun_edomex$label_lisa22 <- ifelse(shp_mun_edomex$lisa20 == 0,
                                      "No significativo",
                                      ifelse(shp_mun_edomex$lisa20 == 1,
                                      "Alto-Alto",
                                      ifelse(shp_mun_edomex$lisa20 == 2,
                                      "Bajo-Bajo",
                                      ifelse(shp_mun_edomex$lisa20 == 3,
                                      "Bajo-Alto",
                                      ifelse(shp_mun_edomex$lisa20 == 4,
                                      "Alto-Bajo",
                                      "NA")))))

#-----------------GWR Sal-------------------#

matrix_GWR_sal = matrix(c(
                  0.127816, 0.175688, 1,
                  0.175689, 0.244825, 2,
                  0.244826, 0.333512, 3,
                  0.333513, 0.425117, 4
                  ), ncol = 3, byrow = TRUE)

print(matrix_GWR_sal)

#crear nueva columna de salarios
shp_mun_edomex$GWR_sal_reclass = NA

#Aplicar la condicional para asignar los valores basados en la matriz
for (i in 1:nrow(matrix_GWR_sal)) {
  shp_mun_edomex$GWR_sal_reclass[shp_mun_edomex$est_Sal22 >=
                                   as.numeric(matrix_GWR_sal[i, 1]) & 
                                   shp_mun_edomex$est_Sal22 <= as.numeric
                                 (matrix_GWR_sal[i, 2])] = as.numeric(matrix_GWR_sal[i, 3])
}

#definir la paleta de colores del coeficiente
colores_coef_sal = c("1" = "#ECFCCC", "2" = "#D5ED8E",
                 "3" = "#B9DB53", "4" = "#9ABB59")

plot(shp_mun_edomex["GWR_sal_reclass"], pal = colores_coef_sal)

#definir la paleta de colores de la significancia
colores_sig = c("0" = "#FF0000", "1" = "#9ABB59")
plot(shp_mun_edomex["t_Sal22_sig"], pal = colores_sig)

#-----------------GWR VAB-------------------#

matrix_GWR_vab = matrix(c(
  -Inf, 0.000000, 1,
  0.000001, 0.003679, 2,
  0.003680, 0.006346, 3,
  0.006347, Inf, 4
), ncol = 3, byrow = TRUE)

print(matrix_GWR_vab)

#crear nueva columna de salarios
shp_mun_edomex$GWR_vab_reclass = NA

#Aplicar la condicional para asignar los valores basados en la matriz
for (i in 1:nrow(matrix_GWR_vab)) {
  shp_mun_edomex$GWR_vab_reclass[shp_mun_edomex$est_Vab22 >=
                                   as.numeric(matrix_GWR_vab[i, 1]) & 
                                   shp_mun_edomex$est_Vab22 <= as.numeric
                                 (matrix_GWR_vab[i, 2])] = as.numeric(matrix_GWR_vab[i, 3])
}

#definir la paleta de colores del coeficiente
colores_coef_vab = c("1" = "#FF0000", "2" = "#D5ED8E",
                     "3" = "#B9DB53", "4" = "#9ABB59")

plot(shp_mun_edomex["GWR_vab_reclass"], pal = colores_coef_vab)

#plot significancia
plot(shp_mun_edomex["t_Vab22_sig"], pal = colores_sig)

#-----------------GWR Cen-------------------#

matrix_GWR_cen = matrix(c(
  -Inf, -14.868962, 1,
  -14.868961, -9.668452, 2,
  -9.668451, 0.000000, 3,
  0.000001, Inf, 4
), ncol = 3, byrow = TRUE)

print(matrix_GWR_cen)

#crear nueva columna de salarios
shp_mun_edomex$GWR_cen_reclass = NA

#Aplicar la condicional para asignar los valores basados en la matriz
for (i in 1:nrow(matrix_GWR_cen)) {
  shp_mun_edomex$GWR_cen_reclass[shp_mun_edomex$est_Cen22 >=
                                   as.numeric(matrix_GWR_cen[i, 1]) & 
                                   shp_mun_edomex$est_Cen22 <= as.numeric
                                 (matrix_GWR_cen[i, 2])] = as.numeric(matrix_GWR_cen[i, 3])
}

#definir la paleta de colores del coeficiente
colores_coef_cen = c("1" = "#FF0000", "2" = "#F2553D",
                     "3" = "#FF907D", "4" = "#9ABB59")

plot(shp_mun_edomex["GWR_cen_reclass"], pal = colores_coef_cen)

#plot significancia
plot(shp_mun_edomex["t_Cen22_sig"], pal = colores_sig)

#-----------------GWR Inv-------------------#

matrix_GWR_inv = matrix(c(
  -Inf, -0.139413, 1,
  -0.139413, 0.000000, 2,
  0.000001, 0.243411, 3,
  0.243412, Inf, 4
), ncol = 3, byrow = TRUE)

print(matrix_GWR_inv)

#crear nueva columna de salarios
shp_mun_edomex$GWR_inv_reclass = NA

#Aplicar la condicional para asignar los valores basados en la matriz
for (i in 1:nrow(matrix_GWR_inv)) {
  shp_mun_edomex$GWR_inv_reclass[shp_mun_edomex$est_Inv22 >=
                                   as.numeric(matrix_GWR_inv[i, 1]) & 
                                   shp_mun_edomex$est_Inv22 <= as.numeric
                                 (matrix_GWR_inv[i, 2])] = as.numeric(matrix_GWR_inv[i, 3])
}

#definir la paleta de colores del coeficiente
colores_coef_inv = c("1" = "#FF0000", "2" = "#FF907D",
                     "3" = "#D5ED8E", "4" = "#9ABB59")

plot(shp_mun_edomex["GWR_inv_reclass"], pal = colores_coef_inv)

#plot significancia
plot(shp_mun_edomex["t_Inv22_sig"], pal = colores_sig)

#-----------------GWR Edu-------------------#

matrix_GWR_edu = matrix(c(
  -Inf, 0.325329, 1,
  0.325330, 0.527012, 2,
  0.527013, 0.778826, 3,
  0.778827, Inf, 4
), ncol = 3, byrow = TRUE)

print(matrix_GWR_edu)

#crear nueva columna de salarios
shp_mun_edomex$GWR_edu_reclass = NA

#Aplicar la condicional para asignar los valores basados en la matriz
for (i in 1:nrow(matrix_GWR_edu)) {
  shp_mun_edomex$GWR_edu_reclass[shp_mun_edomex$est_Edu22 >=
                                   as.numeric(matrix_GWR_edu[i, 1]) & 
                                   shp_mun_edomex$est_Edu22 <= as.numeric
                                 (matrix_GWR_edu[i, 2])] = as.numeric(matrix_GWR_edu[i, 3])
}

#definir la paleta de colores del coeficiente
colores_coef_edu = c("1" = "#ECFCCC", "2" = "#D5ED8E",
                     "3" = "#B9DB53", "4" = "#9ABB59")

plot(shp_mun_edomex["GWR_edu_reclass"], pal = colores_coef_edu)

#plot significancia
plot(shp_mun_edomex["t_Edu22_sig"], pal = colores_sig)

#crear nueva columna con la etiqueta de la significancia de los coeficientes
shp_mun_edomex$label_sig_sal = ifelse(shp_mun_edomex$t_Sal22_sig == 0,
                                      "No significativo",
                                      ifelse(shp_mun_edomex$t_Sal22_sig == 1,
                                             "Significativo", "NA"))

shp_mun_edomex$label_sig_vab = ifelse(shp_mun_edomex$t_Vab22_sig == 0,
                                       "No significativo",
                                       ifelse(shp_mun_edomex$t_Vab22_sig == 1,
                                              "Significativo", "NA"))

shp_mun_edomex$label_sig_cen = ifelse(shp_mun_edomex$t_Cen22_sig == 0,
                                       "No significativo",
                                       ifelse(shp_mun_edomex$t_Cen22_sig == 1,
                                              "Significativo", "NA"))

shp_mun_edomex$label_inv_cen = ifelse(shp_mun_edomex$t_Inv22_sig == 0,
                                       "No significativo",
                                       ifelse(shp_mun_edomex$t_Inv22_sig == 1,
                                              "Significativo", "NA"))

shp_mun_edomex$label_inv_edu = ifelse(shp_mun_edomex$t_Edu22_sig == 0,
                                       "No significativo",
                                       ifelse(shp_mun_edomex$t_Edu22_sig == 1,
                                              "Significativo", "NA"))

#Tres decimales en los coeficientes
shp_mun_edomex$localR2 = round(shp_mun_edomex$localR2, 3)
shp_mun_edomex$std_residual = round(shp_mun_edomex$std_residual, 3)
shp_mun_edomex$est_Sal22 = round(shp_mun_edomex$est_Sal22, 3)
shp_mun_edomex$t_Sal22_sig = round(shp_mun_edomex$t_Sal22_sig, 3)
shp_mun_edomex$est_Vab22 = round(shp_mun_edomex$est_Vab22, 3)
shp_mun_edomex$t_Vab22_sig = round(shp_mun_edomex$t_Vab22_sig, 3)
shp_mun_edomex$est_Cen22 = round(shp_mun_edomex$est_Cen22, 3)
shp_mun_edomex$t_Cen22_sig = round(shp_mun_edomex$t_Cen22_sig, 3)
shp_mun_edomex$est_Inv22 = round(shp_mun_edomex$est_Inv22, 3)
shp_mun_edomex$t_Inv22_sig = round(shp_mun_edomex$t_Inv22_sig, 3)
shp_mun_edomex$est_Edu22 = round(shp_mun_edomex$est_Edu22, 3)
shp_mun_edomex$t_Edu22_sig = round(shp_mun_edomex$t_Edu22_sig, 3)

#-----------------Leaflet-------------------#

mun = "#73B273"
lisa1 = c("#FFFFFF","#FF0000","#0070FF","#AFC1EA","#FFBEBE")
lisa2 = c("#FF0000","#FFBEBE","#AFC1EA","#0070FF","#FFFFFF")
coef_sal = c("#ECFCCC","#D5ED8E","#B9DB53","#9ABB59")
coef_vab = c("#FF0000","#D5ED8E","#B9DB53","#9ABB59")
coef_cen = c("#FF0000","#F2553D","#FF907D","#9ABB59")
coef_inv = c("#FF0000","#FF907D","#D5ED8E","#9ABB59")
coef_edu = c("#ECFCCC","#D5ED8E","#B9DB53","#9ABB59")
sig = c("#FF0000", "#9ABB59")
ajuste_error = c("#FFFF80", "#F7C348", "#C46D1B", "#A83800")

#convertir simbologia de R a simbologia para Leaflet
#LISA

palette_fill_lisa20 = colorNumeric(palette = lisa1,
                                 domain = shp_mun_edomex$lisa20)

palette_fill_lisa21 = colorNumeric(palette = lisa1,
                                 domain = shp_mun_edomex$lisa21)

palette_fill_lisa22 = colorNumeric(palette = lisa1,
                                 domain = shp_mun_edomex$lisa22)

palette_leg_lisa20 = colorFactor(palette = lisa2,
                             domain = shp_mun_edomex$label_lisa20)

palette_leg_lisa21 = colorFactor(palette = lisa2,
                             domain = shp_mun_edomex$label_lisa21)

palette_leg_lisa22 = colorFactor(palette = lisa2,
                             domain = shp_mun_edomex$label_lisa22)

#GWR

#ajuste error

palette_aju = colorQuantile(palette = ajuste_error,
                                     domain = shp_mun_edomex$localR2)

palette_error = colorQuantile(palette = ajuste_error,
                                domain = shp_mun_edomex$std_residual)

#salarios
palette_fill_coef_sal = colorNumeric(palette = coef_sal,
                                 domain = shp_mun_edomex$GWR_sal_reclass)

palette_leg_coef_sal = colorQuantile(palette = coef_sal,
                                 domain = shp_mun_edomex$est_Sal22)

palette_fill_sig_sal = colorNumeric(palette = sig,
                                domain = shp_mun_edomex$t_Sal22_sig)

palette_leg_sig_sal = colorFactor(palette = sig,
                                     domain = shp_mun_edomex$label_sig_sal)

#vab
palette_fill_coef_vab = colorNumeric(palette = coef_vab,
                                     domain = shp_mun_edomex$GWR_vab_reclass)

palette_leg_coef_vab = colorQuantile(palette = coef_vab,
                                     domain = shp_mun_edomex$est_Vab22)

palette_fill_sig_vab = colorNumeric(palette = sig,
                                    domain = shp_mun_edomex$t_Vab22_sig)

palette_leg_sig_vab = colorFactor(palette = sig,
                                  domain = shp_mun_edomex$label_sig_vab)

#centralidad
palette_fill_coef_cen = colorNumeric(palette = coef_cen,
                                     domain = shp_mun_edomex$GWR_cen_reclass)

palette_leg_coef_cen = colorQuantile(palette = coef_cen,
                                     domain = shp_mun_edomex$est_Cen22)

palette_fill_sig_cen = colorNumeric(palette = sig,
                                    domain = shp_mun_edomex$t_Cen22_sig)

palette_leg_sig_cen = colorFactor(palette = sig,
                                  domain = shp_mun_edomex$label_sig_cen)

#inversion
palette_fill_coef_inv = colorNumeric(palette = coef_inv,
                                     domain = shp_mun_edomex$GWR_inv_reclass)

palette_leg_coef_inv = colorQuantile(palette = coef_inv,
                                     domain = shp_mun_edomex$est_Inv22)

palette_fill_sig_inv = colorNumeric(palette = sig,
                                    domain = shp_mun_edomex$t_Inv22_sig)

palette_leg_sig_inv = colorFactor(palette = sig,
                                  domain = shp_mun_edomex$label_sig_inv)

#educacion
palette_fill_coef_edu = colorNumeric(palette = coef_edu,
                                     domain = shp_mun_edomex$GWR_edu_reclass)

palette_leg_coef_edu = colorQuantile(palette = coef_edu,
                                     domain = shp_mun_edomex$est_Edu22)

palette_fill_sig_edu = colorNumeric(palette = sig,
                                    domain = shp_mun_edomex$t_Edu22_sig)

palette_leg_sig_edu = colorFactor(palette = sig,
                                  domain = shp_mun_edomex$label_sig_edu)

mapas = leaflet() %>%
#agregar capas base
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Satélite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") %>%
  
#agregar poligonos municipios
  addPolygons(
    data = shp_mun_edomex,
    fillColor = "#73B273",
    fillOpacity = 0.8,
    color = "white",
    weight = 0.7,
    opacity = 1,
    label = ~paste(NOMMUN),
    highlightOptions = highlightOptions(color = "yellow", weight = 2),
    group = "Municipios del Estado de México"
  ) %>%
  
#agregar poligonos para Clusters LISA 2020
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_lisa20(lisa20),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Clusters LISA 2020"
  ) %>%
  
#agregar poligonos para Clusters LISA 2021
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_lisa21(lisa21),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Clusters LISA 2021"
  ) %>%
  
#agregar poligonos para Clusters LISA 2022
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_lisa22(lisa22),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Clusters LISA 2022"
  ) %>%
  
#agregar polígonos para ajuste local
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_aju(localR2),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN, localR2),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "R2 local"
  ) %>%
  
#agregar polígonos para error estandar
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_error(std_residual),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN, std_residual),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Error"
  ) %>%
  
#agregar polígonos para coeficiente salarios
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_coef_sal(GWR_sal_reclass),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN, est_Sal22),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Coeficiente salarios"
  ) %>%
  
#agregar poligonos para significancia salarios
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_sig_sal(t_Sal22_sig),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN, t_Sal22_sig),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Significancia salarios"
  ) %>%
  
#agregar polígonos para coeficiente vab
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_coef_vab(GWR_vab_reclass),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN, est_Vab22),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Coeficiente valor agregado bruto"
  ) %>%
  
#agregar poligonos para significancia vab
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_sig_sal(t_Vab22_sig),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Significancia valor agregado bruto"
  ) %>%
  
#agregar polígonos para coeficiente centralidad
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_coef_cen(GWR_cen_reclass),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN, est_Cen22),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Coeficiente centralidad"
  ) %>%
  
#agregar poligonos para significancia centralidad
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_sig_cen(t_Cen22_sig),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Significancia centralidad"
  ) %>%
  
#agregar polígonos para coeficiente inversion
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_coef_inv(GWR_inv_reclass),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN, est_Inv22),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Coeficiente inversion publica"
  ) %>%
  
#agregar poligonos para significancia inversion
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_sig_inv(t_Inv22_sig),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Significancia inversion publica"
  ) %>%
  
#agregar polígonos para coeficiente educacion
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_coef_edu(GWR_edu_reclass),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN, est_Edu22),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Coeficiente educacion superior"
  ) %>%
  
  #agregar poligonos para significancia educacion
  addPolygons(
    data = shp_mun_edomex,
    fillColor = ~palette_fill_sig_edu(t_Edu22_sig),
    fillOpacity = 1,
    color = "black",
    weight = 0.7,
    opacity = 0.6,
    label = ~paste(NOMMUN),
    highlightOptions = highlightOptions(color = "black", weight = 5),
    group = "Significancia educacion superior"
  ) %>%
  
#agregar controles de capas
  addLayersControl(
    baseGroups = c("ESRI Satelite", "Open Street Map"),
    overlayGroups = c("Municipios del Estado de México", "Clusters LISA 2020",
                      "Clusters LISA 2021", "Clusters LISA 2022", "R2 local", "Error",
                      "Coeficiente salarios", "Significancia salarios",
                      "Coeficiente valor agregado bruto", "Significancia valor agregado bruto",
                      "Coeficiente centralidad", "Significancia centralidad",
                      "Coeficiente inversion publica", "Significancia inversion publica",
                      "Coeficiente educacion superior", "Significancia educacion superior")
  ) %>%
  
hideGroup(c("Clusters LISA 2020","Clusters LISA 2021", "Clusters LISA 2022",
            "R2 local", "Error",
            "Coeficiente salarios", "Significancia salarios",
            "Coeficiente valor agregado bruto", "Significancia valor agregado bruto",
            "Coeficiente centralidad", "Significancia centralidad",
            "Coeficiente inversion publica", "Significancia inversion publica",
            "Coeficiente educacion superior", "Significancia educacion superior"
            )) %>% 

#agregar leyenda para Clusters LISA 2020
  addLegendFactor(
    pal = palette_leg_lisa20,
    values = shp_mun_edomex$label_lisa20,
    group = "Clusters LISA 2020",
    position = "bottomright",
    title = "Cluster LISA: IGE total 2020",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%

#agregar leyenda para Clusters LISA 2021  
  addLegendFactor(
    pal = palette_leg_lisa21,
    values = shp_mun_edomex$label_lisa21,
    group = "Clusters LISA 2021",
    position = "bottomright",
    title = "Cluster LISA: IGE total 2021",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>% 

#agregar leyenda para Clusters LISA 2022  
addLegendFactor(
  pal = palette_leg_lisa22,
  values = shp_mun_edomex$label_lisa22,
  group = "Clusters LISA 2022",
  position = "bottomright",
  title = "Cluster LISA: IGE total 2022",
  opacity = 1,
  width = 30,
  height = 30,
) %>%
  
#agregar leyenda para ajuste
  addLegendQuantile(
    pal = palette_aju,
    values = shp_mun_edomex$localR2,
    group = "R2 local",
    position = "bottomright",
    title = "R cuadrada local",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para error estandar
  addLegendQuantile(
    pal = palette_error,
    values = shp_mun_edomex$std_residual,
    group = "Error",
    position = "bottomright",
    title = "Error estándar",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para coeficiente salarios 
  addLegendQuantile(
    pal = palette_leg_coef_sal,
    values = shp_mun_edomex$est_Sal22,
    group = "Coeficiente salarios",
    position = "bottomright",
    title = "Coeficiente salarios",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para significancia salarios 
  addLegendFactor(
    pal = palette_leg_sig_sal,
    values = shp_mun_edomex$label_sig_sal,
    group = "Significancia salarios",
    position = "bottomright",
    title = "Significancia al 3%",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para coeficiente vab
  addLegendQuantile(
    pal = palette_leg_coef_vab,
    values = shp_mun_edomex$est_Vab22,
    group = "Coeficiente valor agregado bruto",
    position = "bottomright",
    title = "Coeficiente valor agregado bruto",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para significancia vab 
  addLegendFactor(
    pal = palette_leg_sig_vab,
    values = shp_mun_edomex$label_sig_vab,
    group = "Significancia valor agregado bruto",
    position = "bottomright",
    title = "Significancia al 3%",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para coeficiente centralidad
  addLegendQuantile(
    pal = palette_leg_coef_cen,
    values = shp_mun_edomex$est_Cen22,
    group = "Coeficiente centralidad",
    position = "bottomright",
    title = "Coeficiente centralidad",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para significancia centralidad 
  addLegendFactor(
    pal = palette_leg_sig_cen,
    values = shp_mun_edomex$label_sig_cen,
    group = "Significancia centralidad",
    position = "bottomright",
    title = "Significancia al 3%",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para coeficiente inversion
  addLegendQuantile(
    pal = palette_leg_coef_inv,
    values = shp_mun_edomex$est_Inv22,
    group = "Coeficiente inversion publica",
    position = "bottomright",
    title = "Coeficiente inversion publica",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para significancia inversion 
  addLegendFactor(
    pal = palette_leg_sig_inv,
    values = shp_mun_edomex$label_sig_inv,
    group = "Significancia inversion publica",
    position = "bottomright",
    title = "Significancia al 3%",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para coeficiente educacion
  addLegendQuantile(
    pal = palette_leg_coef_edu,
    values = shp_mun_edomex$est_Edu22,
    group = "Coeficiente educacion superior",
    position = "bottomright",
    title = "Coeficiente educacion superior",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%
  
#agregar leyenda para significancia educacion 
  addLegendFactor(
    pal = palette_leg_sig_edu,
    values = shp_mun_edomex$label_sig_edu,
    group = "Significancia educacion superior",
    position = "bottomright",
    title = "Significancia al 3%",
    opacity = 1,
    width = 30,
    height = 30,
  ) %>%

#agregar minimapa
addMiniMap(position = "bottomleft", tiles = providers$Esri.WorldImagery,
           width = 200, height = 200, zoomAnimation = TRUE,
           zoomLevelOffset = -4,
           aimingRectOptions = list (color = "red", weight = 3)
           ) %>% 

#agregar barra de escala
addScaleBar(position = "topleft", scaleBarOptions(maxWidth = 100))

mapas

mapas_html = saveWidget(mapas, file = "mapas_tesis_MAEG.html")
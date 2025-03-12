---
title: "TF_CCDATOSII"
author: "Mercedes Rodriguez"
date: "2024-08-20"
output:
  html_document: default
  pdf_document: default
---

## Análisis de Centros de Educación Media Tecnológicos en las Localidades de Atlantida y Parque del Plata del Departamento de Canelones, Uruguay 

En este trabajo se reaizará el análisis de la oferta de Centros de Educación Media Pública para estas dos localidades, teniendo en cuenta la población total y los tramos de edad correspondientes a este mivel educativo.

Para esto se calcularán;  

- Totales de población para las localidades 

- Totales de polación por tramos etáreos.

- Oferta de Centros Educativos en las Localidades

Pueden acceder a esta oferta educativa aquellas personas que tengan Educación Media Básica aprobada (Ciclo Básico de Liceo, UTU o Formación Profesional Básica).

```{r, Cargo librerías, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

#install.packages("survey", repos = "https://rstd.io/r-survey-es")
library(survey)
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
library(sf)
if (!requireNamespace("leaflet", quietly = TRUE)) {
 # install.packages("leaflet")
}
library(leaflet)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  #install.packages("dplyr")
}
library(dplyr)
if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
  #install.packages("tidygeocoder")
}
library(tidygeocoder)

if (!requireNamespace("osrm", quietly = TRUE)) {
  #install.packages("osrm")
}
library(osrm)

if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  #install.packages("RColorBrewer")
}
library(RColorBrewer)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  #install.packages("ggplot2")
}
library(ggplot2)
library(tmap)
library(stringi)

```


```{r, Cargo datos de la Encuesta Continúa de Hogares, echo=TRUE, message=TRUE, warning=TRUE, results="show"} 

ECH_2023=read.csv("ECH_implantacion_2023.csv")

```

```{r, Exploro datos de la Encuesta Contínua de Hogares, echo=TRUE, message=TRUE, warning=TRUE, results="show"} 

dim(ECH_2023)
glimpse(ECH_2023)


```



## Creo las sintáxis para ponderar la base y las variables a utilizar

```{r, Pondero y ordeno los datos, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

diseño <- svydesign(ids = ~1, data = ECH_2023, weights = ~ECH_2023$W_ANO)

tabla_ponderada_depto <- svytable(~ECH_2023$nom_dpto, diseño)
print(tabla_ponderada_depto)

tabla_ponderada_loc <- svytable(~ECH_2023$NOM_LOC_AGR_13, diseño)
print(tabla_ponderada_loc)

tabla_ponderada_loc <- as.data.frame(tabla_ponderada_loc)

canelones_ech <- tabla_ponderada_loc %>%
 filter(ECH_2023.NOM_LOC_AGR_13 %in% c("Agrupación localidades entre 5.000 y 20.000 habitantes de Canelones", 
 "Agrupación localidades menores a 5.000 habitantes de Canelones","Costa de Oro Oeste"))

```

## Cargo el archivo espacial (Polígonos) correspondiente a los límites Municipales del Departamento de Canelones

```{r, Cargo archivo de polígonos municipales, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

canelones_mun <- st_read("canelones_ine_limites_municipales-20211109.shp")

```

## Gráfico de Polígono de Municipios

```{r, Gráfico de Polígonos de Municipios, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

ggplot()+
  geom_sf(data=canelones_mun)

```

## Cargo el archivo espacial con los Centros educativos y filtro los correspondientes al Departamento de Canelones.

```{r, Centros educativos, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

centros <- st_read("educacionmediatecnologicabachilleratotecnologico.shp")
                       

centros_depto<- centros %>% 
  filter(DEPARTAMEN == "CANELONES")


```

## Visualizo los Centros Técnicos Educativos georreferenciados del Departamento de Canelones

```{r, Mapa de los centros educativos georreferenciados, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

ggplot()+
  geom_sf(data=centros_depto)

```



```{r, Cargo el mapa departamental desde OSM, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

canelones <- geo(address = "Canelones, Uruguay", method = "osm")

```

```{r, Visualizo el mapa, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

leaflet(canelones) %>%
  addTiles() %>% 
  addCircleMarkers(~long, ~lat)

```


```{r, Visualizo y edito el mapa, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

centros_depto <- centros_depto %>%
  mutate(
    LON = as.numeric(LON),
    LAT = as.numeric(LAT)
  )
st_crs(centros_depto)


if (is.na(st_crs(centros_depto))) {
  centros_depto <- st_set_crs(centros_depto, 4326)
}

centros_depto <- st_transform(centros_depto, crs = 4326)

```
## Gráfico de Centros Educativos Técnicos en Canelones

```{r, Mapa de centros edúcativos en el Departamento, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

centros_depto_sp <- as_Spatial(centros_depto)

mapa_centros <- leaflet(centros_depto_sp) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircleMarkers(~LON, ~LAT,
                   color = "white",
                   popup = ~NOMBRE)
mapa_centros

```


## Gráfico de Municpios del Departamento de Canelones Uruguay


```{r, Mapa de municipios en el departamento, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

st_crs(canelones_mun)

canelones_mun <- st_transform(canelones_mun, crs = 4326)

mapa_mun <- leaflet(canelones) %>%
  addTiles() %>%  # Añadir capa base
  addPolygons(data = canelones_mun, 
              color = "blue",    # Color del borde del polígono
              weight = 2,        # Grosor del borde del polígono
              opacity = 1.0,     # Opacidad del borde
              fillColor = "lightblue", # Color de relleno
              fillOpacity = 0.5,
              label = ~canelones_mun$MUNICIPIO)  # Etiqueta tomada del campo 'nombre'
             

mapa_mun

```

## Gráfico de Municipios y Centros Técnicos Educativos en Canelones 

```{r, Mapa de municipios y centros edúcativos, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

mapa_mun_centros <- leaflet(centros_depto_sp) %>%
  addTiles() %>%  # Añadir capa base
  addPolygons(data = canelones_mun, 
              color = "blue",    # Color del borde del polígono
              weight = 2,        # Grosor del borde del polígono
              opacity = 1.0,     # Opacidad del borde
               # Color de relleno
              fillOpacity = 0,
              label = ~canelones_mun$MUNICIPIO) %>%
   addCircleMarkers(~LON, ~LAT,
                  color = "red",
                  popup = ~NOMBRE)
mapa_mun_centros
```



## Gráfico de habitantes por localidades agrupadas de Canelones según la ECH 2023

```{r, Genero mapa de puntos de localidades con cantidad de hábitantes, echo=TRUE, message=TRUE, warning=TRUE, results="show"}

canelones_ech<- canelones_ech %>% rename(LOCALIDAD = ECH_2023.NOM_LOC_AGR_13)

canelones_ech <- canelones_ech %>%
  mutate(LOCALIDAD = recode(as.character(LOCALIDAD),
    "1" = "Agrupación localidades entre 5.000 y 20.000 habitantes de Canelones",
    "2" = "Agrupación localidades menores a 5.000 habitantes de Canelones",
    "3" = "Costa de Oro Oeste"
  ))

LOCALIDAD <- st_read("canelones_atic_uy_loc_pt-20240825.shp")

LOCALIDAD <- st_transform(LOCALIDAD, crs = 4326)

LOCALIDAD_sp <- as_Spatial(LOCALIDAD)

canelones_ech <- canelones_ech %>%
  rename(NOMBRE = LOCALIDAD)

LOCALIDAD <- LOCALIDAD %>%
  left_join(canelones_ech, by = "NOMBRE")

canelones_ech <- canelones_ech %>%
  geocode(NOMBRE, method = "osm", lat = lat, long = lon)


canelones_ech_geo <- data.frame(
  LOCALIDAD = c("Agrupacion localidades entre 5.000 y 20.000 habitantes de Canelones
", "Agrupacion localidades menores a 5.000 habitantes de Canelones
", "Costa de Oro Oeste"),
  lon = c(-55.738333, -55.630555, -55.701944),
  lat = c(-34.764722, -34.488055, -34.684166)
)

canelones_ech <- canelones_ech %>%
  rename(LOCALIDAD = NOMBRE)

canelones_ech <- canelones_ech %>%
  select(-lat, -lon)

canelones_ech$LOCALIDAD <- stri_trans_general(canelones_ech$LOCALIDAD, "Latin-ASCII")

canelones_ech_geo$LOCALIDAD <- stri_trans_general(canelones_ech_geo$LOCALIDAD, "Latin-ASCII")

canelones_ech$LOCALIDAD <- trimws(tolower(canelones_ech$LOCALIDAD)) 
canelones_ech_geo$LOCALIDAD <- trimws(tolower(canelones_ech_geo$LOCALIDAD))

canelones_ech_geo <- left_join(canelones_ech, canelones_ech_geo, by = "LOCALIDAD")

canelones_ech_geo<- st_as_sf(canelones_ech_geo, coords = c("lon", "lat"), crs = 4326)

```


```{r, Uno el mapa de puntos de cantidad de habitantes con el de polígonos del departamento para mostrar la cantidad de hábitantes y agrego los centros educativos, echo=TRUE, message=TRUE, warning=TRUE, results="show"}


canelones_ech_geo <- canelones_ech_geo %>%
  mutate(lon = st_coordinates(.)[,1], 
         lat = st_coordinates(.)[,2])

canelones_ech_geo_sp <- as_Spatial(canelones_ech_geo)


mapa_mun_hab <- leaflet(canelones_ech_geo) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 3,  # Escala el tamaño del círculo
    color = "blue",
    popup = ~as.character(Freq)  # Muestra la frecuencia al hacer clic
  ) %>%
  addCircleMarkers(data = centros_depto_sp,
                   lng = ~LON, lat = ~LAT,
                   color = "red",
                   popup = ~NOMBRE,
                   radius = 4) 

mapa_mun_hab

```

## Conclusión

En este análisis, se observó la oferta educativa en relación con la población en las localidades de Atlántida y Parque del Plata. Se trata de un estudio exploratorio ya que las limitaciones son varias; faltan datos actualizados de población para los municipos y las localidades de manera individual ya que en esta encuesta las localidades aparecen agrupadas y por la tanto la estimación de la población también.

Un análisis completo, tendría que utilizar otra base de datos en donde se encuentre disponible la variable población por localidad y municipio, como podría ser en el último censo realizado, Censo 2023, pero aún no se encuentra disponible. 

Como análisis preliminar igual podemos decir que la oferta es pequeña ya que solo hay 1 centro educativo para ambos municipios dentro del Departamento, para un análisis mejorado podemos agregar tramos etáreos de los residentes de todo el departamento para ajustar el análisis con la población objetivo(si bien los centros no poseen límite de edad para concurrir). 





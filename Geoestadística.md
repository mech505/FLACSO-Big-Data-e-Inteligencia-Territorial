---
title: "Análisis del Empleo en Servicios Públicos en Uruguay"
author: "[Mercedes Rodríguez Trujillo]"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```

## 1️⃣ Introducción

Este informe analiza la distribución espacial del empleo en servicios públicos en Uruguay utilizando la Encuesta Continua de Hogares (ECH) 2023.

## 2️⃣ Carga de librerías y datos
```{r librerias}
library(sf)
library(tidyverse)
library(tmap)
library(spdep)
library(reldist)
library(tidyr)
 if (!requireNamespace("survey", quietly = TRUE)) {
      install.packages("survey")
    }
library(survey)
```

### 📂 **Carga de Datos**

2. **Carga de datos de la ECH 2023**

```{r, include=TRUE, echo=FALSE, message=FALSE, warning=FALSE}
    
# Cargar la Encuesta Continua de Hogares (ECH)

ech <- read.csv("C:/Users/merodriguez/Desktop/Geoestadistica/tp_geoestadistica/ECH_implantacion_2023.csv")
rm(departamentos)

# Cargar shapefiles
departamentos <- st_read("C:/Users/merodriguez/Desktop/Geoestadistica/tp_geoestadistica/ine_depto.shp") %>%
  st_make_valid() %>%  
  filter(!st_is_empty(.)) %>%  
  st_cast("MULTIPOLYGON", warn = FALSE) %>%  
  st_buffer(0) %>%  
  rename(nom_dpto = NOMBRE) %>%  
  mutate(nom_dpto = tolower(iconv(trimws(nom_dpto), from = "UTF-8", to = "ASCII//TRANSLIT"))) %>%
  filter(nom_dpto != "limite contestado")

# Verificar y corregir geometrías POLYGON
departamentos <- if(any(st_geometry_type(departamentos) == "POLYGON")) {
  st_cast(departamentos, "MULTIPOLYGON", warn = FALSE)
} else {
  departamentos
}

# Asegurar que el CRS está bien definido después de corregir geometrías
if (is.na(st_crs(departamentos))) {
  departamentos <- st_set_crs(departamentos, 32721) %>% st_transform(4326)
} else {
  departamentos <- st_transform(departamentos, 4326)
}
  
```

```{r ponderación de la encuesta}

    diseno <- svydesign(ids = ~1, data = ech, weights = ~ech$W_ANO)
    tabla_ponderada_depto <- svytable(~ech$nom_dpto, diseno)
    tabla_ponderada_loc <- svytable(~ech$NOM_LOC_AGR_13, diseno)
    tabla_ponderada_loc <- as.data.frame(tabla_ponderada_loc)
    tabla_ponderada_depto <- as.data.frame(tabla_ponderada_depto)
    
```    




```{r recodificar categoria ocupacional}

ech <- ech %>%
  mutate(
    categoria_ocupacional = case_when(
      f73 == 1 ~ "Asalariado privado",
      f73 == 2 ~ "Asalariado publico",
      f73 == 3 ~ "Socio cooperativa",
      f73 == 4 ~ "Patron",
      f73 == 9 ~ "Cuenta propia",
      f73 == 7 ~ "Miembro hogar no remunerado",
      f73 == 8 ~ "Trabajador programa social",
      TRUE ~ "Otro"
    )
  )
```

### 📊 **Cálculo de empleo **

```{r empleo}

empleo <- ech %>% mutate(nom_dpto = tolower(iconv(trimws(nom_dpto), from = "UTF-8", to = "ASCII//TRANSLIT")))

# Verificar y corregir codificación
departamentos$nom_dpto <- iconv(departamentos$nom_dpto, from = "UTF-8", to = "ASCII//TRANSLIT")
empleo$nom_dpto <- iconv(empleo$nom_dpto, from = "UTF-8", to = "ASCII//TRANSLIT")

# Eliminar caracteres no imprimibles
departamentos$nom_dpto <- gsub("[^[:alnum:] ]", "", departamentos$nom_dpto)
empleo$nom_dpto <- gsub("[^[:alnum:] ]", "", empleo$nom_dpto)

# Eliminar valores NA
departamentos <- departamentos %>% filter(!is.na(nom_dpto))
empleo <- empleo %>% filter(!is.na(nom_dpto)) %>%
  group_by(nom_dpto, categoria_ocupacional) %>%
  summarise(prop_empleo = n() / nrow(ech), .groups = 'drop') %>%
  pivot_wider(names_from = categoria_ocupacional, values_from = prop_empleo, values_fill = 0)

```

### 📈 **Índices de desigualdad: Gini y Coeficiente de Variación**

```{r indices_desigualdad}

gini_index <- gini(rowSums(empleo[,-1], na.rm = TRUE))
cv <- sd(rowSums(empleo[,-1], na.rm = TRUE)) / mean(rowSums(empleo[,-1], na.rm = TRUE))

gini_index <- ifelse(is.na(gini_index) | is.nan(gini_index), 0, gini_index)
cv <- ifelse(is.na(cv) | is.nan(cv), 0, cv)

cat("Índice de Gini:", gini_index, "Coeficiente de Variación:", cv)

```

## 4️⃣ Mapeo de la distribución del empleo

```{r mapa_empleo}


# Unir con shapefile
departamentos <- left_join(departamentos, empleo, by = "nom_dpto")

# Corregir geometrías inválidas y eliminar vacías
departamentos <- st_make_valid(departamentos)
departamentos <- departamentos[!st_is_empty(departamentos), ]

departamentos <- st_buffer(departamentos, 0)  # Eliminar vértices duplicados

departamentos <- st_cast(departamentos, "MULTIPOLYGON", warn = FALSE)

#Mapa de cuantiles

mapa_cuantiles <- tm_shape(departamentos) +
  tm_polygons(
    col = "Asalariado publico", 
    style = "quantile",          
    n = 4,                       
    palette = "Blues",           
    title = "Distribución del empleo público",  
    border.col = "white",        
    lwd = 0.5                    
  ) +
  tm_layout(
    main.title = "1. Mapa de Cuantiles Empleo Público por Departamentos para el 2023",  
    main.title.size = 1.2,       
    legend.outside = TRUE,       
    legend.outside.position = "right",  
    frame = FALSE               
  ) +
  tm_compass(type = "arrow", position = c("right", "bottom")) +  
  tm_scale_bar(position = c("left", "bottom"))  

# Mostrar el mapa
 
mapa_cuantiles

```

## 5️⃣ Análisis de autocorrelación espacial

### 🧩 **Matriz de pesos espaciales y Moran I**

```{r moran_test}
# Corregir geometrías inválidas y eliminar vacías
departamentos <- st_make_valid(departamentos)
departamentos <- departamentos[!st_is_empty(departamentos), ]

# Aplicar un pequeño buffer de 0 para corregir errores topológicos
departamentos <- st_buffer(departamentos, 0)

departamentos <- st_cast(departamentos, "MULTIPOLYGON", warn = FALSE)

sum(!st_is_valid(departamentos))


vecinos <- poly2nb(departamentos)
pesos <- nb2listw(vecinos, style = "W")

moran_test <- moran.test(departamentos$`Asalariado publico`, pesos)

moran_test
```

### 🔥 **Clusters espaciales (LISA)**

El análisis LISA permite identificar clusters espaciales de empleo público:

- **Alta-Alta (HH):** Departamentos con alta concentración de empleo público rodeados por otros similares.
- **Baja-Baja (LL):** Departamentos con baja concentración de empleo público en regiones homogéneas.
- **Alta-Baja (HL):** Departamentos con alta concentración rodeados de baja presencia (valores atípicos).
- **Baja-Alta (LH):** Departamentos con baja concentración rodeados de alta presencia (valores atípicos).

#### 📌 **Departamentos en cada cluster**

- **Alta-Alta (HH):** `r departamentos$nom_dpto[departamentos$lisa > quantile(departamentos$lisa, 0.75)]`
- **Baja-Baja (LL):** `r departamentos$nom_dpto[departamentos$lisa < quantile(departamentos$lisa, 0.25)]`
- **Alta-Baja (HL) y Baja-Alta (LH):** Departamentos con valores extremos detectados en el análisis LISA.

```{r lisa_clusters}

departamentos$`Asalariado publico` <- as.numeric(departamentos$`Asalariado publico`)
departamentos$`Asalariado publico`[is.na(departamentos$`Asalariado publico`)] <- 0

departamentos$`Asalariado publico`[is.na(departamentos$`Asalariado publico`)] <- 0

local_moran <- localmoran(pull(departamentos, `Asalariado publico`), pesos)

# Verificar si local_moran generó valores válidos

if (!is.null(local_moran) && nrow(local_moran) == nrow(departamentos) && !all(is.na(local_moran[,1]))) {
  departamentos$lisa <- as.numeric(local_moran[,1])
} else {
  stop("Error: LISA no generó valores válidos. Verificar pesos y datos de entrada.")
}
if (!all(is.na(local_moran[,1]))) {
  departamentos$lisa <- as.numeric(local_moran[,1])
} else {
  stop("Error: LISA no generó valores válidos.")
}

departamentos$lisa[is.na(departamentos$lisa)] <- 0

tm_shape(departamentos) +
  tm_polygons(
    col = "lisa", 
    style = "quantile",          
    n = 4,                        
    palette = "Reds",           
    title = "Clusters de empleo publico",  
    border.col = "white",        
    lwd = 0.5                     
  ) +
  tm_layout(title = "2. Mapa de Clusters de Empleo Pùblico", legend.outside = TRUE) +
  tm_compass(type = "arrow", position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"))


```


## 6️⃣ Conclusiones

- La concentración del empleo varía significativamente entre departamentos y categorías ocupacionales.
- Se identifican **clusters espaciales** donde la presencia del empleo público es más alta o baja de lo esperado, lo que sugiere un patrón de distribución territorial no homogéneo.
- Los departamentos con **alta concentración de empleo público** están agrupados en ciertas regiones, mientras que otros presentan una baja presencia relativa.
- La existencia de valores atípicos sugiere posibles desigualdades en la distribución del empleo público, que podrían ser clave para diseñar políticas de descentralización.







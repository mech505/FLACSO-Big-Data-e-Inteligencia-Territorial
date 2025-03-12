---
title: "cdatosI_TF"
author: "Mercedes Rodriguez"
date: "2024-09-19"
output:
  pdf_document: default
  #html_document:
   # df_print: paged
---

**Tarea final del Módulo II Ciencia de datos I**

Pregunta problema ¿Cual es el departamento con mayor nivel de pobreza de Uruguay? 

*Análisis de la Encuesta Continua de Hogares (ECH) proveniente del Instituto Nacional de Estadística de Uruguay (INE)* 

Procesamiento de ECH 2023, exploración de frecuencias por Departamento y Pobreza, 
La Encuesta Continua de Hogares (ECH) es una encuesta a hogares particulares, que se realiza sin interrupciones por parte del Instituto Nacional de Estadística desde el año 1968; constituye una de las fuentes de información más importante que posee el país en materia socioeconómica.
La variable "pob_06" se define como la cuantificación de hogares indigentes y de la brecha que los separa de los no indigentes a partir del método del ingreso basado exclusivamente en los hábitos de consumo de la población del estrato de referencia.


```{r setup, include=FALSE}

#install.packages("survey", repos = "https://rstd.io/r-survey-es")
library(survey)
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
library(sf)
#if (!requireNamespace("leaflet", quietly = TRUE)) {
 
install.packages("leaflet", repos = "https://cloud.r-project.org/")

library(leaflet)
 
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
 install.packages("dplyr")
}
library(dplyr)
if (!requireNamespace("tidygeocoder", quietly = TRUE)) {
 install.packages("tidygeocoder")
}
library(tidygeocoder)

if (!requireNamespace("osrm", quietly = TRUE)) {
 install.packages("osrm")
}
library(osrm)

if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
 install.packages("RColorBrewer")
}
library(RColorBrewer)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
 install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("kableExtra", quietly = TRUE)) {
  install.packages("kableExtra")
}
library(kableExtra)
library(tinytex)


```

Cargo la base y visualizo los nombres

```{r, include=TRUE, echo=FALSE, message=FALSE, warning=FALSE} 

ECH_2023=read.csv("C:\\Users\\merodriguez\\Desktop\\tf_ccdd1_mercedes_rodríguez\\entradas\\ECH_implantacion_2023.csv")

```

Creo las síntaxis para ponderar la base y las variables a utilizar

```{r, include=TRUE, echo=FALSE, message=FALSE, warning=FALSE}

diseño <- svydesign(ids = ~1, data = ECH_2023, weights = ~ECH_2023$W_ANO)

tabla_ponderada_depto <- svytable(~ECH_2023$nom_dpto, diseño)
print(tabla_ponderada_depto)

tabla_ponderada_pobreza <- svytable(~ECH_2023$pobre06, diseño)
print(tabla_ponderada_pobreza)

```

Creo los Dataframes correspondientes a Departamentos y Pobreza ponderadas

```{r, include=TRUE, echo=FALSE, message=FALSE, warning=FALSE}

tabla_ponderada_depto <- as.data.frame(tabla_ponderada_depto)
total<- sum(tabla_ponderada_depto$Freq)  
colnames(tabla_ponderada_depto) <- c("depto", "freq")

fila_total <- data.frame(tabla_ponderada_depto = "Total", Frecuencia = total)

colnames(fila_total) <- c("depto", "freq")
tabla_ponderada_depto <- rbind(tabla_ponderada_depto, fila_total)

tabla_ponderada_pobreza <- as.data.frame(tabla_ponderada_pobreza)
rownames(tabla_ponderada_pobreza) <- c("nopobre", "pobre")
colnames(tabla_ponderada_pobreza) <- c("pobreza", "freq")
```

Creo las Tabla cruzada de Pobreza por Departamentos

```{r, include=TRUE, echo=FALSE, message=FALSE, warning=FALSE}

tabla_ponderada <- svytable(~ ECH_2023$nom_dpto + ECH_2023$pobre06, diseño)
print(tabla_ponderada)
colnames(tabla_ponderada) <- c("nopobre","pobre")
tabla_ponderada_df <- as.data.frame(tabla_ponderada)
tabla_ponderada_df$ECH_2023.pobre06 <- as.numeric(tabla_ponderada_df$ECH_2023.pobre06)

datos <- tabla_ponderada_df %>%
  mutate(pobre = factor(ifelse(tabla_ponderada_df$ECH_2023.pobre06 > 1, "si","no")))

pobres_xdepto<- filter(tabla_ponderada_df, tabla_ponderada_df$ECH_2023.pobre06 > 1)
nopobres_xdepto<- filter(tabla_ponderada_df, tabla_ponderada_df$ECH_2023.pobre06 < 2)
total_pobres_xdepto <- sum(pobres_xdepto$Freq)
names(pobres_xdepto) <- c("Depto", "Pobre", "Freq")

fila_total <- data.frame(pobres_xdepto = "Total", Frecuencia = total_pobres_xdepto)
datos_con_columnas_vacias <- fila_total %>%
  mutate(
    Columna3 = NA
  )
names(datos_con_columnas_vacias) <- c("Depto", "Freq", "Pobre")
pobres_xdepto <- rbind(pobres_xdepto, datos_con_columnas_vacias)

```


Creo Tabla de Personas Pobres por Departamento

```{r, include=TRUE, echo=FALSE, message=FALSE, warning=FALSE}

pobres_xdepto <- pobres_xdepto[, -which(names(pobres_xdepto) == "Pobre")]

kable(pobres_xdepto, caption = "Tabla de Personas Pobres por Departamento") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) 

```

Agrego el gráfico de Pobreza por Departamentos con valores ponderados (pobres_xdepto) y podemos Observar que es Montevideo el Departamento con mayor Población Pobre con un total de 177052 en su territorio frente a un total de 360047 de personas pobres en todo el País (tabla_ponderada_pobreza)

```{r, include=TRUE, echo=FALSE, message=FALSE, warning=FALSE}

ggplot(pobres_xdepto, aes(x = pobres_xdepto$Freq, y = pobres_xdepto$Depto, fill = pobres_xdepto$Freq)) +
  geom_col(stat = "identity", fill = 4) +
  labs(title = "Gráfico de Pobreza por Departamentos Ponderado",
       x = "Pobreza",
       y = "Departamento") 
   
```

  
```{r, include=TRUE, echo=FALSE, message=FALSE, warning=FALSE}

ggplot(datos, aes(y = datos$ECH_2023.nom_dpto, x = datos$Freq, fill = datos$pobre)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    labs(title = "Cantidad de Pobres (Sí y No) por Departamento",
       x = "Frecuencia",
       y = "Departamento") +
  scale_fill_manual(values = c("Sí" = "red", "No" = "blue")) + 
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal()

```
  
Agrego el gráfico de Pobreza Total con valores ponderados (tabla_ponderada_pobreza) y podemos Observar que el total de población pobre es de 360047 frente a un total de de personas de 320585 en todo el País (tabla_ponderada_pobreza)

```{r, include=TRUE, echo=FALSE, message=FALSE, warning=FALSE}

ggplot(tabla_ponderada_pobreza, aes(y = tabla_ponderada_pobreza$freq, x = tabla_ponderada_pobreza$pobreza)) +
  geom_col(stat = "identity", fill="red") +
   geom_text(aes(label=freq), vjust=-0.3, color="black", size=5) +
  labs(title = "Gráfico de Pobreza Total",
       y= "Frecuencia",
       x = "Pobreza")
```




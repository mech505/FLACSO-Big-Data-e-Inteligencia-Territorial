---
title: "Estadística computacional"
author: "Mercedes Rodríguez"
date: "2025-02-11"
output: html_document
---



# ***¿Existe correlación entre el nivel educativo de la persona y el nivel socioeconómico de la familia en Uruguay?***

## Introducción

Según el estudio  ([**"La desigualdad educativa en la Educación Media uruguaya"**](http://www.scielo.edu.uy/scielo.php?script=sci_arttext&pid=S1688-74682020000200034)), existe una relación entre el nivel socioeconómico de las familias y el desempeño educativo de los estudiantes. En este trabajo, exploraremos esta relación utilizando datos de PISA para Uruguay en los años 2009 y 2018.

"...Se observó que sistemáticamente los jóvenes provenientes de los hogares más vulnerables transitaban trayectorias educativas incompletas, promediando malas calificaciones en relación a sus pares provenientes de hogares de mayores ingresos. Esta tendencia observada desde mediados de la década de los 40 persiste y se observa más nítidamente en los 90, en una serie de trabajos de la CEPAL dedicados a Educación Media." 

El autor en el siguiente párrafo concluye:

"...Treinta años después la situación no es mucho más alentadora, y si bien hay avances en términos de ***inclusión educativa*** en Educación Media (Cardozo, 2008; De Armas y Retamoso, 2010; Menese y Ríos, 2013), los aprendizajes continúan mostrando jóvenes provenientes de estratos altos que logran resultados significativamente mejores que sus pares de estratos bajos (ANEP, 2019; INEEd, 2020). En términos de acreditación, los porcentajes de culminación de la educación formal obligatoria continúan segmentados, donde el momio de acreditación del quintil 5 es 7/10, mientras en el quintil 1 es 1/10 (Menese y Ríos, 2013), y esto es desde hace al menos 40 años (De los Campos, 2020)."

En este trabajo se realizarán 2 índices ***(socioeconómico y notas promedio)*** que nos permitirán acercarnos a una respuesta fundada a la pregunta inicial. 

## Carga de Librerías

```{r warning = FALSE, message=FALSE, echo = TRUE}


library(tidyverse)
library(learningtower)
library(car)
library(nortest)
library(lmtest)
library(sandwich)

```

```{r warning = FALSE, message=FALSE, echo = TRUE}


Sys.setlocale("LC_ALL", "es_ES.UTF-8")

```
## Exploración de Datos

Cargamos los datos y exploramos las variables relevantes.

```{r warning = FALSE, message=FALSE, echo = TRUE}

data(countrycode)
head(countrycode)

student_2009_2018 <- load_student(c(2009, 2018))
table(student_2009_2018$country)
```

## Exploración de Datos

Cargamos los datos y exploramos las variables relevantes.

```{r warning = FALSE, message=FALSE, echo = TRUE}


data(countrycode)
head(countrycode)

student_2009_2018 <- load_student(c(2009, 2018))
table(student_2009_2018$country)

```

## Transformación de Variables

Transformamos las variables categóricas en numéricas para facilitar el análisis.

```{r warning = FALSE, message=FALSE, echo = TRUE}


student_2009_2018 <- student_2009_2018 %>% 
  mutate(book = case_when(book == "0-10" ~ 1,
                          book == "11-25" ~ 2,
                          book == "26-100" ~ 3,
                          book == "101-200" ~ 4,
                          book == "201-500" ~ 5,
                          book == "more than 500" ~ 6,
                          TRUE ~ NA_real_)) %>% 
  mutate(television = case_when(television == "0" ~ 1,
                                television == "1" ~ 2,
                                television == "2" ~ 3,
                                television == "3+" ~ 4,
                                TRUE ~ NA_real_)) %>% 
  mutate(car = case_when(car == "0" ~ 1,
                         car == "1" ~ 2,
                         car == "2" ~ 3,
                         car == "3+" ~ 4,
                         TRUE ~ NA_real_)) %>% 
  mutate(desk = case_when(desk == "no" ~ 0,
                          desk == "yes" ~ 1,
                          TRUE ~ NA_real_)) %>% 
  mutate(computer = case_when(computer == "no" ~ 0,
                              computer == "yes" ~ 1,
                              TRUE ~ NA_real_)) %>% 
  mutate(dishwasher = case_when(dishwasher == "no" ~ 0,
                                dishwasher == "yes" ~ 1,
                                TRUE ~ NA_real_)) %>%
  mutate(nota_promedio = (math + science + read) / 3) %>% 
  mutate(internet = case_when(internet == "yes" ~ 1,
                              internet == "no" ~ 0,
                              TRUE ~ NA_real_))
```

## Cálculo de Índices

Calculámos el índice socioeconómico y el acceso digital.

```{r warning = FALSE, message=FALSE, echo = TRUE}


student_2009_2018$indice_socioeconomico <- rowMeans(student_2009_2018[, c("dishwasher", "desk", "television", "computer", "internet", "book", "wealth")], na.rm = TRUE)
student_2009_2018$acceso_digital <- rowMeans(student_2009_2018[, c("computer", "internet")], na.rm = TRUE)

```

## Filtrado de Datos

Filtramos los datos para Uruguay en los años 2009 y 2018

```{r warning = FALSE, message=FALSE, echo = TRUE}


student_2009 <- student_2009_2018 %>% 
  filter(year == 2009, country == "URY") %>% 
  select(nota_promedio, gender, internet, wealth, acceso_digital, indice_socioeconomico)

student_2018 <- student_2009_2018 %>% 
  filter(year == 2018, country == "URY") %>% 
  select(nota_promedio, gender, internet, wealth, acceso_digital, indice_socioeconomico)

```

```{r warning = FALSE, message=FALSE, echo = TRUE}



student_ury <- student_2009_2018 %>% filter(!is.na(indice_socioeconomico))

student_ury <- student_2009_2018 %>% 
  filter(country == "URY", !is.na(indice_socioeconomico)) %>% 
  mutate(year = as.factor(year))

histo_socio <- ggplot(student_ury, aes(x = indice_socioeconomico, fill = as.factor(year))) +
  geom_histogram(alpha = 0.6, bins = 30) + 
  facet_wrap(~year) + 
  scale_fill_manual(values = c("2009" = "blue", "2018" = "orange")) + 
  labs(
    title = "Distribución del Índice Socioeconómico por Año",
    x = "Índice Socioeconómico",
    y = "Frecuencia",
    fill = "Año"
  ) +
  theme_minimal()

print(histo_socio)

```

**Interpretación del histograma socioeconómico:**

***Distribuciones por año:***

2009: La mayoría de los valores están concentrados cerca del centro, con pocos valores extremos hacia ambos lados.
2018: También tiene una concentración similar al centro, pero parece estar más superpuesta con la distribución de 2009.
Comparación entre años:

Las distribuciones de ambos años son bastante similares, lo que sugiere que no hubo cambios significativos en el índice socioeconómico entre 2009 y 2018.

Ambas distribuciones son más estrechas, lo que indica que los índices socioeconómicos están mayormente concentrados en un rango limitado

```{r warning = FALSE, message=FALSE, echo = TRUE}



student_ury <- student_ury %>% filter(!is.na(nota_promedio))

student_ury <- student_2009_2018 %>% 
  filter(country == "URY", !is.na(nota_promedio)) %>% 
  mutate(year = as.factor(year))


histo_nota <- ggplot(student_ury, aes(x = nota_promedio, fill = year)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) + 
  scale_fill_manual(values = c("2009" = "blue", "2018" = "orange")) +
  facet_wrap(~year) +
  labs(
    title = "Comparación de la nota promedio entre 2009 y 2018",
    x = "Nota Promedio",
    y = "Frecuencia",
    fill = "Año"
  ) +
  theme_minimal()


print(histo_nota)
```

**Análisis del histograma Nota Proemdio**
Este histograma compara la distribución de las notas promedio (o alguna métrica similar) para dos años diferentes (representados por azul y naranja). Aquí está el análisis basado en las características observadas:

***Forma de las distribuciones***
Ambas distribuciones tienen una forma aproximadamente normal (campana), lo que indica que las notas promedio tienden a agruparse cerca de un valor central.
La simetría de las distribuciones sugiere que las notas promedio están equilibradas alrededor de un promedio para ambos años.
***Comparación entre los años***
Las distribuciones de los dos años están ampliamente superpuestas. Esto sugiere que no hay diferencias significativas en las notas promedio entre los dos periodos.
Podría observarse que el año azul tiene un ligero sesgo hacia valores más altos en comparación con el naranja, aunque esta diferencia es pequeña.
***Dispersión y concentración***

Ambas distribuciones tienen su pico (moda) cerca del centro del rango, lo que sugiere que la mayoría de las notas promedio están en un rango similar.
Los valores extremos (colas) son escasos, lo que indica que las notas promedio fuera del rango central son poco presentes.
Si este gráfico representa una variable dependiente (nota promedio), su distribución normal es adecuada para el modelo de regresión.
Dado que las distribuciones de los años son similares, podría esperarse que el año no sea un predictor altamente significativo.

  La estabilidad en la distribución sugiere que las políticas educativas, el rendimiento estudiantil o las evaluaciones no cambiaron significativamente entre los dos años.



## Correlación de Pearson

Calculamos la correlación entre el índice socioeconómico y la nota promedio.

```{r warning = FALSE, message=FALSE, echo = TRUE}


student_correlation <- student_2009_2018 %>% 
  filter(country == "URY", !is.na(indice_socioeconomico), !is.na(nota_promedio))

correlation <- cor(student_correlation$nota_promedio, student_correlation$indice_socioeconomico, method = "pearson")
correlation

```


## Interpretación de la Correlación:

El coeficiente de correlación de Pearson obtenido es 0.3857. Esto indica una correlación positiva moderada entre el índice socioeconómico y la nota promedio. En otras palabras, a medida que aumenta el nivel socioeconómico de las familias, tiende a aumentar el desempeño educativo de los estudiantes. Sin embargo, la correlación no es fuerte, lo que sugiere que otros factores también influyen en las notas promedio.

### Gráfico de Dispersión

Visualizamos la relación entre el índice socioeconómico y la nota promedio, separando los datos por año.

```{r warning = FALSE, message=FALSE, echo = TRUE}




cor_plot <- ggplot(student_correlation, aes(x = indice_socioeconomico, y = nota_promedio, color = year)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  facet_wrap(~year) +  
  labs(
    title = "Relación entre nivel socioeconómico y nota promedio en Uruguay (2009 vs. 2018)",
    x = "Índice Socioeconómico",
    y = "Nota Promedio",
    color = "Año"
  ) +
  theme_minimal()  



print(cor_plot)

```

## Interpretación del Gráfico:

El gráfico muestra una relación positiva entre el índice socioeconómico y la nota promedio en ambos años (2009 y 2018). Sin embargo, la dispersión de los puntos indica que la correlación no es muy fuerte. Además, se observa que la relación es similar en ambos años, lo que sugiere que no hubo cambios significativos en esta dinámica entre 2009 y 2018.

### Modelo de Regresión Lineal

Ajustamos un modelo de regresión lineal para cuantificar la relación entre el índice socioeconómico, el año y la nota promedio.


```{r warning = FALSE, message=FALSE, echo = TRUE}


model <- lm(nota_promedio ~ indice_socioeconomico + year, data = student_correlation)
summary(model)

```

## Interpretación del Modelo de Regresión:

***Intercepto (Intercept)***: El valor estimado es 1972.31. Esto representa la nota promedio esperada cuando el índice socioeconómico es 0 y el año no afecta (referencia base).

***indice_socioeconomico***: El coeficiente es 69.41. Por cada unidad adicional en el índice socioeconómico, las notas promedio aumentan en 69.41 puntos, manteniendo el año constante. Este efecto es altamente significativo (p-value < 2e-16).

***year***: El coeficiente es -0.81. Esto indica que, con el paso del tiempo (de 2009 a 2018), las notas promedio disminuyen en 0.81 puntos por año, manteniendo constante el índice socioeconómico. Este efecto también es significativo (p-value = 2.48e-06).

## Bondad del Ajuste:

***R² (Multiple R-squared)***: 0.1505. Esto indica que el modelo explica aproximadamente el 15.05% de la variabilidad en las notas promedio. Aunque no es un ajuste alto, es común en datos sociales que otros factores no incluidos en el modelo también influyan.

***Error estándar residual***: 80.27. Esto refleja el promedio de las desviaciones de los valores observados respecto a los valores ajustados por el modelo.

***F-Statistic***: 970.5, p-value < 2.2e-16. Esto confirma que el modelo es globalmente significativo.

### Evaluación de Supuestos del Modelo

## 1. Normalidad de los Residuales

Verificamos la normalidad de los residuales utilizando el test de Shapiro-Wilk.


```{r warning = FALSE, message=FALSE, echo = TRUE}


# Prueba de Kolmogorov-Smirnov para normalidad
ks.test(residuals(model), "pnorm", mean = mean(residuals(model)), sd = sd(residuals(model)))

```

***Interpretación***:

El p-value es mayor que 0.05, no podemos rechazar la hipótesis nula de que los residuales siguen una distribución normal.



## 2. Homocedasticidad

Verificamos la homocedasticidad utilizando el test de Breusch-Pagan

```{r warning = FALSE, message=FALSE, echo = TRUE}

bptest(model)
```

***Interpretación***:

El p-value es mayor que 0.05, no podemos rechazar la hipótesis nula de que los residuales tienen varianza constante (homocedasticidad). 


***Interpretación***:

Si el VIF es mayor que 5, hay evidencia de multicolinealidad, lo que podría afectar la interpretación de los coeficientes del modelo.


## Test de Hipótesis

Realizamos un test de hipótesis para comparar las medias de las notas promedio entre 2009 y 2018.

```{r warning = FALSE, message=FALSE, echo = TRUE}


t.test(nota_promedio ~ year, data = student_correlation)

```

***Interpretación***:

El p-value = 0.0869 es mayor que 0.05, lo que indica que no hay evidencia suficiente para rechazar la hipótesis nula al nivel de significancia del 5%. Es decir, la diferencia en las notas promedio entre 2009 y 2018 no es estadísticamente significativa.

Aunque la media de 2009 es ligeramente mayor que la de 2018, esta diferencia podría deberse a otras variables en lugar de un cambio real en el desempeño educativo.

### Conclusiones

***Correlación Moderada***: Existe una correlación positiva moderada entre el índice socioeconómico y la nota promedio (0.3857). Esto sugiere que el nivel socioeconómico tiene un impacto significativo, pero no determinante, en el desempeño educativo.

***Modelo de Regresión***: El modelo de regresión confirma que el índice socioeconómico tiene un efecto positivo y significativo en las notas promedio. Sin embargo, el año también tiene un efecto negativo, lo que sugiere que el desempeño educativo ha disminuido ligeramente con el tiempo.








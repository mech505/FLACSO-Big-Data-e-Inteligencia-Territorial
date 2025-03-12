---
title: "Evaluación de Programas del Gobierno de Uqbar con Inteligencia Territorial"
author: "Mercedes Rodríguez Trujillo"
date: "`r Sys.Date()`"
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## **Resumen Ejecutivo**

Este informe presenta un enfoque basado en **Inteligencia Territorial (IT)** para evaluar la efectividad de tres programas heredados del gobierno anterior de Uqbar. Se propone el uso de herramientas georreferenciadas y modelos predictivos para optimizar la toma de decisiones y garantizar la eficiencia de las políticas públicas. Esta estrategia se inspira en experiencias internacionales como **CitiStat en Baltimore (Behn, 2007) y Delivery Units en el Reino Unido (Clase 2 - FLACSO, 2024)**, que han demostrado la importancia del monitoreo en tiempo real y la evidencia en la gestión pública.

## **Antecedentes y Problema**

El Gobierno de Uqbar necesita determinar si los siguientes programas cumplen con sus objetivos:

1. **Programa de Densificación Urbana**: Incentivos para reducir la expansión de la mancha urbana en 20 de las 50 ciudades más grandes.

2. **Programas de Mejora del Transporte Público**: Implementación aleatoria de carriles exclusivos en 500 de las 1.000 principales avenidas.

3. **Programa de Apoyo a PyMES**: Beneficios fiscales a empresas de hasta 25 empleados.

El desafío radica en evaluar estas políticas de manera objetiva y basada en datos.

## **Análisis y Evaluación de Programas**

### **1. Programa de Densificación Urbana**

#### **Método: Análisis de Imágenes Satelitales y Regresión Geográfica Ponderada**
- **Objetivo**: Determinar si las ciudades con incentivos han reducido su expansión urbana en comparación con otras.
- **Datos Necesarios**:
  - Imágenes satelitales antes y después de la intervención.
  - Registros catastrales de cada municipio.
  - Datos poblacionales y de densidad urbana.
- **Fuentes de Datos**:
  - Programas satelitales (NASA, ESA, Google Earth Engine).
  - Institutos de estadística y catastro.
- **Limitaciones**:
  - Diferencias en la calidad y disponibilidad de imágenes satelitales.
  - Influencia de otras regulaciones urbanísticas que puedan sesgar los resultados.

### **2. Programas de Mejora del Transporte Público**

#### **Método: Análisis de Datos de Movilidad y Machine Learning Espacial**
- **Objetivo**: Evaluar la reducción en tiempos de viaje en avenidas con carriles exclusivos.
- **Datos Necesarios**:
  - Tiempos de viaje antes y después de la implementación.
  - Datos de GPS de flotas de transporte público.
  - Encuestas a usuarios sobre percepción del servicio.
  - Datos de congestión vehicular.
- **Fuentes de Datos**:
  - Aplicaciones de movilidad (Google Maps, Waze, Moovit).
  - Registros de transporte público.
  - Sensores de tráfico instalados en avenidas.
- **Limitaciones**:
  - La congestión puede estar influenciada por otros factores externos (obras, crecimiento del parque automotor).
  - Sesgo en encuestas debido a la percepción subjetiva de los usuarios.

### **3. Programa de Apoyo a PyMES**

#### **Método: Georreferenciación y Evaluación de Clusters Empresariales**
- **Objetivo**: Comparar el crecimiento de PyMES beneficiadas con un grupo de control.
- **Datos Necesarios**:
  - Información fiscal de las empresas beneficiadas y no beneficiadas.
  - Datos de empleo y facturación de cada PyME.
  - Localización geográfica y sector de actividad.
- **Fuentes de Datos**:
  - Registros fiscales nacionales y locales.
  - Encuestas empresariales.
  - Bases de datos de comercio y actividad económica.
- **Limitaciones**:
  - Falta de acceso a datos financieros detallados de algunas empresas.
  - Posibles efectos externos como cambios en la demanda del sector o crisis económicas que influyan en los resultados.

## **Recomendaciones y Conclusión**
- Se recomienda implementar los métodos propuestos para evaluar de manera rigurosa cada programa.
- La **Inteligencia Territorial** permite realizar análisis basados en datos georreferenciados y machine learning espacial.
- La implementación de un modelo de **Chief Data Officer (CDO)**, siguiendo experiencias internacionales [(Wiseman, 2018)](file:///C:/Users/merodriguez/Desktop/machine%20learning/tp_final/Wiseman%202018%20Chief%20Data%20Officers.pdf), facilitaría la sistematización del uso de datos en la gestión pública.
- Se sugiere desarrollar una unidad especializada en análisis territorial dentro del gobierno para optimizar la toma de decisiones basada en evidencia.

---

## **Anexos**
- [Behn, R. (2007). *The Core Drivers of CitiStat: It's Not Just About the Meetings and the Maps.* International Public Management Journal.](file:///C:/Users/merodriguez/Desktop/machine%20learning/tp_final/Behn%20CitiStat%202007.pdf)
- [Gertler, P. et al. (2017). *Evaluación de Impacto en la Práctica.* Banco Mundial.](file:///C:/Users/merodriguez/Desktop/machine%20learning/tp_final/Gertler%20Evaluacion%20de%20Impacto%202017.pdf)
- [Wiseman, J. (2018). *Data-Driven Government: The Role of Chief Data Officers.* Harvard Kennedy School.](file:///C:/Users/merodriguez/Desktop/machine%20learning/tp_final/Wiseman%202018%20Chief%20Data%20Officers.pdf)
- [FLACSO (2024). Clases 1-4. *Big Data e Inteligencia Territorial en la Gestión Pública.*](file:///C:/Users/merodriguez/Desktop/machine%20learning/tp_final/Clase%202%20-%20FLACSO%202024.pdf)

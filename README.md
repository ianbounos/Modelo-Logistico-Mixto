# Modelo-Logistico-Mixto
Implementamos un modelo lineal generalizado (logístico) con la incorporación de efectos mixtos para la detección de fraudes. Esto está realizado en el contexto de la tesis de maestría en estadística de la Facultad de ciencias exactas de la Universidad de Buenos Aires.

La **tesis** puede verse [aquí](https://github.com/ianbounos/Modelo-Logistico-Mixto/blob/main/TESIS_Modelo_Logistico_Mixto.pdf) y las diapositivas de la defensa oral  [aquí](https://github.com/ianbounos/Modelo-Logistico-Mixto/blob/main/Presentacion_defensa_tesis_Bounos.pdf)

Además, se dispone del [Script comentado utilizado para las aplicaciones prácticas de la tesis](https://github.com/ianbounos/Modelo-Logistico-Mixto/blob/main/Script_Final.R) y el [dataset correspondiente](https://github.com/ianbounos/Modelo-Logistico-Mixto/blob/main/insurance_claims_original.csv). 
Finalmente, se adjunta [Script en R del ejemplo ilustrativo de elecciones presidenciales](https://github.com/ianbounos/Modelo-Logistico-Mixto/blob/main/Script%20Teorico%20Elecciones.R) y su respectivo [dataset](https://github.com/ianbounos/Modelo-Logistico-Mixto/blob/main/eleccion.txt).


# Resumen de la tesis


Los Modelos Lineales Generalizados (GLM) son ampliamente utilizados en diversos campos de los seguros, sobre todo en el cálculo de frecuencia y severidad de siniestros. Muchas de las covariables son cualitativas con múltiples posibles valores, por ejemplo, la marca del automóvil en el contexto de un seguro automotor. Esto es un problema para los modelos lineales generalizados con efectos fijos porque aumentan la varianza de las predicciones y dificultan la estimación cuando tenemos datos desbalanceados. Más aún, resulta imposible hacer proyecciones sin modificar el modelo en el caso en el que no se dispone de experiencia previa del comportamiento de cierto nivel de la variable cualitativa en cuestión (como podría ser una determinada marca en nuestro ejemplo). Estos problemas pueden ser abordados incorporando efectos aleatorios a los GLM, los cuales consideran el efecto de ciertas variables como algo aleatorio en lugar de un parámetro fijo a estimar como se realiza en los GLM usuales.
En este trabajo se estudia la incorporación de efectos aleatorios al modelo logístico en el contexto de la detección de Fraudes en Seguros Automotores y se estudian múltiples métricas del rendimiento de la clasificación para seleccionar el modelo.


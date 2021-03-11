#Practica Matricas de Satisfaccion del Consumidor - NPS
#NPS - Net Promoter Score - Score promedio de recomendacion
#En R existe una libreria llamada NPS para calcularlo automaticamente
#Deberán trabajar el calculo NPS sobre un dataset y analizar correlaciones con cluster de RFM


#PASO 1: Deben cargar las librerias tidyverse, lubridate, caret, corrplot y NPS

install.packages("NPS")
library(tidyverse)
library(dplyr)
library(lubridate)
library(caret)  
library(corrplot)
library(NPS)

#PASO 2: Generen un dataset llamado ventas2020 cargando el archivo "NPS_T1.csv" eliminando primera columna
#Eliminen aquellos registros que figuren con un NPS NA utilizando filter
#Modifiquen la columna nps a columna numerica utilizando mutate y as.numeric
#Ayuda: al utilizar select si escriben select(-1) entonces se seleccionan todas las columnas excepto la primera

ventas2020 <- read.csv(choose.files()) %>%
  select(-1)
  
  #*Importamos la base de datos eliminando la primera columna 

#Calculen el NPS agrupado por cada tienda, utilizando la función nps del paquete NPS dentro de la funcion summarise
#¿cual es la tienda con mejor performance?

ventas2020 <- na.omit(ventas2020); ventas2020

  # Eliminamos los NA de la base 

nps_tiendas <- ventas2020 %>%
  group_by(store) %>%
  summarise(nps_result = nps(nps));nps_tiendas

  #* Creamos un nuevo dataframe en el cual calculamos el NPS agrupadando cada una de las tiendas

#Realizaremos un analisis entre los meses del año y el NPS para cada tienda
#para ello deben crear una columna mes en el dataframe de ventas2020 
#y agrupar tanto por mes como por store y calcular el nps en un nuevo data frame

ventas2020$fecha_compra <-as.Date(ventas2020$fecha_compra)
ventas2020$mes <- months(ventas2020$fecha_compra)

  #*Es este paso cambiamos el tipo de datos almacenado al interior del dataframe
  #*por fecha poder extraer el mes y realizar un posterior calculo con los datos

nps_mes <- ventas2020 %>%
  group_by(mes) %>%
  summarise(nps_mes = nps(nps));nps_mes

  #Calculamos el NPS por cada mes de la base de datos 

#visualizamos la comparación de NPS de cada tienda para cada mes
#utilicen gráfico de scatter (geom_point) y den color a los puntos con
#columna store

nps_txmes <- ventas2020 %>%
  group_by(mes,store) %>%
  summarise(nps_3 = nps(nps))

ggplot(nps_txmes) + geom_point(mapping = aes(x=mes, y=nps_3, colour = store))

  #*Gracifacamos los NPS obtenidos por tienda durante el periodo de tiempo de la base
  #*(ene-may) lo que podemos denotar es bastante variaci�n en los datos por tienda

#Desarrollar el cálculo de RFM para cada comprador en otro dataframe 
#sin olvidar de modificar la columna de  fecha para que R la reconozca como tal utilizando as.Date
#Generen 5 clusters a traves de kmean para identificar segmentos de consumidores
#pueden utilizar de referencia el script visto en el modulo II

Calculo_RFM <- ventas2020 %>% 
  group_by(id_member) %>%
  summarise(Recency=as.numeric(as.Date(Sys.Date())-max(fecha_compra)),
            Frequency=length(id_member), 
            Monetary_Value= sum(gasto),
            NPSmedia = mean(nps));Calculo_RFM

  #*Posteriormente creamos un nuevo dataframe donde calculamos el recency, frequency, 
  #*monetary value y el la Media de los NPS  para posteriormente calcular el RFM del ejercicio
  #*

set.seed(1234)
SegmentacionRFM <- kmeans(scale(Calculo_RFM[,2:4]), 5, nstart = 1) 

Calculo_RFM$segmentacion <- as.factor(SegmentacionRFM$cluster)

  #*Realizamos la segmentacion con la funcion kmeans y lo agregamos a nuestro datafreme 
  #*creado en el paso anterior con ccategorias que van del 1 al 5 

#Calcular nps agrupando por segmento de consumidores en un nuevo data frame

nps_segmento <- Calculo_RFM %>%
  group_by((segmentacion))%>%
  summarise(nps_segmento = nps(NPSmedia))

  #*A traves del segmento realizamos un nuevo calculo del NPS a nivel de segmentaci�n

#Ahora realicen una correlacion entre NPS y  los segmentos de consumidores de RFM
#Existen mayor correlacion con aquellos consumidores que gastan mas dinero o menos dinero?

correlacion <- Calculo_RFM %>% 
  group_by(segmentacion) %>%
  summarise(Recency_mean=mean(Recency),
            Frequency_mean=mean(Frequency), 
            Monetary_mean= mean(Monetary_Value),
            NPSmedia = mean(NPSmedia))

  #*Para realizar el analisis de correlacion creamos un nuevo objeto
  #*donde calculamos la media de los indicadores y agrupamos por segmento

corrplot.mixed(corr= cor(correlacion[,-1]))

  #*En el grafico de correlaci�n podemos notar 2 correlaciones positivas muy fuertes
  #*entre las variables:
    #* - Frecunency y Monetary Value: 0.99
    #* - Recency y NPSmean : 0.81
    
  #*Existen otras correlaciones negativas en la muestra de las variables sin 
  #*Embargo no llega a ser tan determinante como las 2 inicialmente comentadas

#Que sucede si realizamos un promedio de NPS por cada segmentos para cada tienda?
#los segmentos puntúan muy diferente a cada tienda? Observamos algun patron?


ventas2020 <- merge(ventas2020,Calculo_RFM,by="id_member", all.x = TRUE) 

  #Incluimos el dataframe calculado del RMF con los datos iniciales

segmento_tiendas <- ventas2020 %>% 
  group_by(segmentacion,store) %>%
  summarise(nps = nps(nps))

  #*Calculamos un nuevo dataframe donde por realizamos la agrupaci�n por tienda y 
  #*segmento de la misma, observamos un patron creciente en la mayoria de las segmentaciones

ggplot(segmento_tiendas) +
  geom_point(aes(x=store, y=nps, color=segmentacion))

  #*Al graficar encontramos menor dispersi�n de los datos a nivel de tiendas y sus segmentos
  #*donde la tienda umero 4 posee la mayor media de NPS entre todas las categorias

#Que sucede si correlacionamos frecuencia de compra de los 172 ids con el NPS? 
#Los consumidores que tienen mayor frecuencia de compra puntúan mas y mejor?

ggplot(Calculo_RFM) +
  geom_point(aes(x=Frequency, y=NPSmedia, color=segmentacion))

  #*Notamos que los clientes pertenecientes a la categoria de segmentacion numero 2
  #*poseen mayor frecuencia de compra seguidos de los pertenecientes a la categoria 1 

#En líneas generales luego del análisis exploratorio, ¿podriamos identificar tiendas que sobresalen
#por una buena o una mala performance en terminos de NPS?

corrplot(corr = cor(Calculo_RFM[,2:5]))

  #*Despues de realizar el analisis podemos determinar que un NPS con un valor por 
  #*encima del 0.17 es un NPS optimo y lo cual determina que se debe tomar un plan de accion
  #*con aquellas segmentaciones y tiendas que posean un NPS medio > al 0.17.






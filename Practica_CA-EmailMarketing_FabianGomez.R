#Análisis explotatorio de campañas de E-mail Marketing

#PASO 1: Deben cargar las librerias:
#tidyverse, lubridate, corrplot

library(tidyverse)
library(lubridate)
library(corrplot)
library(ggplot2)

#PASO 2: Generar un dataset llamado email_analysis cargando el archivo "dataset_email-mkt.csv" eliminando primera columna
email_analysis <- read.csv(choose.files())

  #Importamos los datos 

#Modificar la columna de sendout_date para que sea fecha y no character
email_analysis$sendout_date <- as.Date(email_analysis$sendout_date, "%Y-%m-%d")

class(email_analysis$sendout_date)

  #*Cambiamos el formato de del campo del dataset sendout_date a "%Y-%m-%d"
  #*y corroboramos la clase del campo 

#PASO 3: Generar un segundo dataset email_campaign filtrando la columna
#email_scope == "Campaign"


email_campaign <- email_analysis%>%
  filter(email_scope == "Campaign")

  #*Filstramos el dataset con todos aquellos datos que tengan "Campaign" en su escope

#Calculen los datos agregados de todas las columnas que comienzan con "total_"
#agrupando por journeys_id

range(email_analysis$journeys_ids)

journeys <- email_analysis %>% 
  select(contains("total_",email_analysis$journeys_ids)) %>%
  group_by(email_analysis$journeys_ids) %>%
  summarise_all(sum,na.rm=T)

  #*calculamos el rango de los ids para deteerminar el largo del nuevo dataset que 
  #*creamos seleccionando aquellas columnas que contengan en total_ en su nombre
  #*y agrupando por los ids (90)

#Realicen un plot de la cantidad de envios de mails para cada journeys_id

colnames(journeys)


ggplot(data = journeys, mapping = aes(x = `email_analysis$journeys_ids`, y = total_email_sent)) + 
  geom_point()

  #*Realizamos el grafico que nos permite determinar que los Ids que son mayores a
  #*55 reducen criticamente los email enviados los que mayor distribuci�n tienen son
  #*el grupo de ids entre 0 y 25 .

#PASO 4: Realizar los cálculos de open_rate y ctor para cada journeys_id
#OR: el porcentaje de emails que fueron abiertos por los
#destinatarios sobre el total de emails enviados.
#Click to Open Rate (CTOR): El porcentaje de usuarios que
#recibieron el mail, lo abrieron y realizaron clic en el link deseado.

journeys$open_rate <- (journeys$total_email_open/journeys$total_email_sent)*100
journeys$ctor <- (journeys$total_email_clicks/journeys$total_email_open)*100;journeys

  #*Calculamos el open rate y el CTOR los cuales incluimos en nuestro dataset Journeys  

#Cual es el OR y CTOR promedio de todas las campañas realizadas?

mean(journeys$open_rate)
mean(na.omit(journeys$ctor))

  #*Calculamos las respectivas medias de las variables anteriores

#Cuales son las campañas que mejor han performado?

journeys <- arrange(journeys,-journeys$ctor)
head(journeys)

  #Las campa�as 79, 88, 50, 76, 73 y 66, son aquellas que mejor CTOR poseen indicando las que mejor han performado

#Las campañas que peor performan son aquellas donde más "flag_unsubscribe"
#con valor TRUE existen?

flags <- email_analysis %>% 
  group_by(email_analysis$journeys_ids) %>%
  summarise(sum(flag_unsubscribe==TRUE))

flags <- arrange(flags, -flags$`sum(flag_unsubscribe == TRUE)`)
head(flags)

  #Los Journeys 8,9,10,2,5 y 14 son las campa�as que mas unsuscribe tuvieron

#PASO 5: Realizar análisis de los usuarios según su género, realizando un nuevo
#dataset que agregue los datos según género
#Calcular métricas de OR y CTOR para cada género e identificar si se perciben
#diferencias de comportamiento en relación a la tasa de apertura y clics

genero <- email_campaign %>%
  group_by(gender) %>%
  summarise(enviados= sum(total_email_sent),
            abiertos =sum(na.omit(total_email_open)),
            clicks= sum(na.omit(total_email_clicks)),
            or=(abiertos/enviados)*100,
            ctor=(clicks/abiertos)*100,
            paginas_vistas=mean(na.omit(total_pageviews)));genero

  #*Al calcular un nuevo dataframe por genero evidenciamos que tan solo el 5%
  #*de los correos abiertos se les da click  es una tasa muy baja

#Qué sucede con la cantidad promedio de páginas vistas por género?
#Los hombres o las mujeres exhiben un comportamiento diferencial?

head(genero)
  #En promedio los hombres visitan 3.25 paginas, mientras que las mujeres 4.36
  #por ultimo los undefined visitan en promedio 3.25 paginas



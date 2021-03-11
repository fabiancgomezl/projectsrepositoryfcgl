#M脫DULO IV - SATISFACCI脫N DEL CONSUMIDOR
#En este ejercicio estaremos trabajando con data t铆pica que uno puede extraer del modulo de Support Service
#de un CRM sobre los problemas que se reportan con el producto o servicio
#Y contiene una encuesta de satisfaccion tanto para el producto como para el servicio brindado por el equipo de custome support

#Para completar el ejercicio deberan cargar las siguientes librerias: tidyverse, corrplot, psych

library("tidyverse")
library("corrplot")
library("psych")
library("qplot")
library("hexbin")
install.packages("hexbin")
#PISTA: Las librerias de corrplot y psych fueron utilizadas en el ejercicio de Percepcion de Marca. 
#Pueden revisar ese script como referencia para esta tarea


#PASO 1
#Cargamos el dataset "data_CRM.csv" eliminando columna 1 que es innecesaria
#Inspecciones en dataset con summary, describe y head para comprender la informaci贸n que contiene, entender missing values en las columnas
#Realicen un plot para entender la distribuci贸n de las quejas a lo largo del per铆odo, 
#selecciondo el tipo de gr谩fico m谩s 贸ptimo para representar la data

data_CRM <- read.csv(choose.files()) %>%
  select(-1)

  #*Cargamos los datos del correspondiente dataset y retiamos la primera columna ya que som datos  innecesarios

summary(data_CRM) 
describe(data_CRM)
head(data_CRM)
  
  #*Utilizamos la funcion sumary, describe y head para dar un vistazo inicial al datset
  #*y determinar si encontramos algo erroneo con el mismo

table(data_CRM$complaint_reason)
length(data_CRM$complaint_reason)

  #* Con la funcion table acotado a las razones de queja encontramos que hay 23
  #* tipos diferentes de quejas distriyendo los 12.000 datos de data set

quejas <- data.frame(Indice = seq(1,23,1),table(data_CRM$complaint_reason))
quejas

ggplot(data = quejas, aes(x = Indice, y =Freq))+ 
  geom_point() + theme_classic()  +
  scale_x_continuous(limit = c(1,23), breaks = seq(1, 23, 1)) +
  labs(title="Complaint Reasons",
       x="Complaint Reason",
       y="Numbers of complaint") 

  #*Creamos un nuevo dataframe en el cual agrupamos la frecuencia de sus quejas y
  #*al grficarlo podemos observar que  los datos se distribuyen de aleatoria


#PASO 2
#El dataset presenta todos los casos de los usuarios que han contactado con Customer Support en el per韔do enero-abril 2020
#Pero nos interesa hacer el an醠isis sobre complaint_reason, por lo cual es necesario crear un nuevo dataset, 
#agrupar los datos por complaint_reason y realizar las siguientes operaciones para las columnas relevantes: 
#generar una columna que cuente la cantidad de llamadas para cada tipo de complaint_reason llamada "num_casos"
#generar una columna que cuente la cantidad de llamadas pendientes para cada tipo de complaint_reason contando la cantidad de "y" llamada pend_calls
#calcular el promedio de time_to_resolution_min para cada tipo de complaint_reason en una columna nueva llamada avg_time_to_resolution
#generar una columna que cuente la cantidad de need_replace para cada tipo de complaint_reason contando la cantidad de "TRUE" llamada n_replacements
#generar una nueva columna que calcule el Prod_CSAT para cada tipo de complaint_reason en una columna nueva llamada Prod_CSAT
#generar una nueva columna que calcule el Serv_CSAT para cada tipo de complaint_reason en una columna nueva llamada Serv_CSAT
#De esta forma el dataset nuevo debe contener las siguientes columnas: complaint_reason, num_casos, pend_calls, 
#avg_time_to_resolution, n_replacements, Prod_CSAT, Serv_CSAT


range(data_CRM$create_date)

data_CRM$prodCSAT1[data_CRM$ProdCSAT == "4" | data_CRM$ProdCSAT =="5"] <- 1
data_CRM$servCSAT1[data_CRM$ServCSAT == "4" | data_CRM$ServCSAT =="5"] <- 1

  #*creamos 2 nuevas columnas al interior de nuestro dataframe  donde evaluamos con un 1
  #*1 todos los datos que sean iguales a 4 o 5  esto con el fin de poder carlular los CSAT
  #*de una forma m醩 facil con el mismo dataframe

quejas2 <- data_CRM %>%
  group_by(complaint_reason) %>%
  summarise(num_casos=length(complaint_reason),
            pend_calls=sum(pending_call=="y"),
            avg_time_to_resolution=mean(time_to_resolution_min),
            n_replacement=sum(need_replace==TRUE),
            prod_CSAT=((sum(prodCSAT1, na.rm = T)/sum(ProdCSAT>0, na.rm = T))*100),
            serv_CSAT=((sum(servCSAT1, na.rm = T)/sum(ServCSAT>0, na.rm = T))*100))

  #*Al crear este segundo dataframe creamos todas las columnas solicitadas 
  #*donde tuvimos especial cuidado en los CSAT de prod y serv ya que se tenian que
  #*incluir solo aquellos datos que huebieran respondido la encuesta 

#PASO 3
#Seleccionar un plot idoneo para poder realizar una comparativa de C-SATs para cada problema t茅cnico
#Justificar la selecci贸n de dicho plot brevemente

ggplot(data=quejas2) + 
  geom_col(aes(x = complaint_reason, y =  serv_CSAT ), fill = "blue", position = "dodge", alpha=0.6) +
  geom_col(aes(x = complaint_reason, y =  prod_CSAT ), fill = "green", position = "dodge", alpha = 0.5) +theme(axis.text.x = element_text(angle = 90, hjust = 1))

  #* "Main switch does not on" es la queja que mayor CSAT tanto de producto como 
  #* de servicio se representa en el grafico

#PASO 4
#Realizar una correlaci贸n entre las variables num茅ricas y analizar si existen correlaciones fuertes entre
#alguna de las variables presentadas. 
#las funciones de correlaci贸n poseen un argumento llamado use que permite excluir los NA para que el computo sea
#posible. Para ello incluyan como argumento use = "complete.obs" ya que por default es use = "everything" 
#驴La columna de Serv_CSAT muestra correlaciones con alguna otra columna?

Correlacion <- quejas2[,2:7]

corrplot.mixed(cor(Correlacion, use ="complete.obs"))

  #* La columna de serv_CSAT tiene un correlaci髇 positiva alta con:
    #*Time to resolution y Serv_csat  = 0.75
    #*prod_csat y serv_csat = 0.83
    #*

  #*Otras correlaciones que existe  al interior de los datos son entre las siguientes
  #*variables las cuales tambien son positivas  y altas:
    #*Num_casos y replacemente = 0.72
    #*Num_casos y pendind call = 1
    #*pendind call y replacement = 0.73
    #*Time to resolution y prod_csat = 0.81


#Inspeccionen la funcion cor.test para entender su funcionamiento y apliquenla sobre aquellas correlaciones
#que ustedes opinaron anteriormente que tienen correlaci贸n con la columna de Serv_CSAT para verificar si su hipotesis es correcta
#IMPORTANTE: pueden explorar los diferentes m茅todos, pero el que utilizamos de forma gen茅rica es pearson
##a su vez es importante que comprendan y utilicen el argumento exact con l贸gica FALSE

?cor.test

cor.test(quejas2$avg_time_to_resolution,quejas2$serv_CSAT)

  #*Al realizar el analisis de correlacion entre las variables:
  #*Tiempo de respuesta y Serv_CSAT y podemos determinar que:
    #*Obtenemos un P-Valos de 4.431e-05 lo cual
    #*nos indica que tomamos la hipotesis alterna y determinamos que la correlacion
    #*es positiva y no es igual a 0  por lo tanto existe una correlacion (+) de 0.74 

cor.test(quejas2$prod_CSAT,quejas2$serv_CSAT)

  #*Al realizar el analisis de correlacion entre las variables:
  #*prod_CSAT y Serv_CSAT y podemos determinar que:
  #*Obtenemos un P-Valos de 9.957e-07 lo cual
  #*nos indica que tomamos la hipotesis alterna y determinamos que la correlacion
  #*es positiva y no es igual a 0  por lo tanto existe una correlacion (+) de 0.82

#Por 煤ltimo utilicen la funci贸n corrplot.mixed() para realizar el plot de todas las correlaciones juntas
#Intenten utilizar algunas de las opciones que presenta para embellecer el gr谩fico (colores, formas, tama帽os, etc)
#La forma de aplicaci贸n ser铆a corrplot.mixed(corr = (correlacion que quieren hacer con sus argumentos incluido use = "complete.obs")) 
#y el resto de argumentos que quieran incluir


corrplot.mixed(cor(Correlacion, use ="complete.obs"),
               tl.col = "red", 
               lower.col = "black",
               number.cex = .7,
               tl.cex=0.8,
               upper =  "square") 

corrplot(cor(Correlacion, use ="complete.obs"),
               tl.col = "red", 
               number.cex = .7,
               tl.cex=0.8,
               method =  "ellipse") 


#PASO 5
#Repetir el paso 4 pero enfocando el analisis en la columna Prod_CSAT en vez de Serv_CSAT: realicen hipotesis sobre correlaciones,
#apliquen cor.test para validarlas y corrplot.mixed() para representarlo.

cor.test(quejas2$avg_time_to_resolution,quejas2$prod_CSAT)

  #*Al realizar el analisis de correlacion entre las variables:
  #*prod_CSAT y Serv_CSAT y podemos determinar que:
  #*Obtenemos un P-Valos de 2.376e-06 lo cual
  #*nos indica que tomamos la hipotesis alterna y determinamos que la correlacion
  #*es positiva y no es igual a 0  por lo tanto existe una correlacion (+) de 0.81
  #*con un intervalo de confianza del 95%

cor.test(quejas2$prod_CSAT,quejas2$prod_CSAT)

  #*Al realizar el analisis de correlacion entre las variables:
  #*prod_CSAT y Serv_CSAT y podemos determinar que:
  #*Obtenemos un P-Valos de 2.2e-16 lo cual
  #*nos indica que tomamos la hipotesis alterna y determinamos que la correlacion
  #*es positiva y no es igual a 0  por lo tanto existe una correlacion (+) de 1
  #*con un intervalo de confuianza del 95%


corrplot.mixed(cor(Correlacion, use ="complete.obs"),
               tl.col = "red", 
               lower.col = "black",
               number.cex = .7,
               tl.cex=0.8,
               upper =  "square")

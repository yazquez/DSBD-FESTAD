# En este trabajo se va a trabajar con datos procedentes de la liga de béisbol de los Estados Unidos recopilados por
# Sean Lahman desde 1871 a nuestros días. Descargue el fichero que aparece como 2014 – comma-delimited
# version – Updated January 24, 2015 (lahman-csv_2015-01-24.zip) en la dirección http://seanlahman.
# com/baseball-archive/statistics/.
# Cómo entregar el trabajo: Cree un fichero con código R en el que se obtenga respuesta a cada uno de los
# siguientes ejercicios que se plantean, y envíeme antes del día 1 de abril de 2016 por email a calvo@us.es
# como adjunto (en un fichero comprimido) todo el material necesario para que se puedan reproducir todos sus
# cálculos y los ficheros resultantes. Nota: también podría ser un fichero Rmarkdown (Rmd).


setwd("/home/yazquez/Dev/R/dsbd-festad/data")

# ------------------------------------------------------------------------------------------------------------------------------------------
# Ejercicio 1
# Cargue los ficheros: Master.csv y Batting.csv en los objetos R: master (datos de jugadores) y bateos
# (información sobre el juego de estos jugadores), con al menos dos métodos distintos (utilice funciones de paquetes distintos).
library(rio) 
library(readr)

master = rio::import("Master.csv") 
bateos = readr::read_csv("Batting.csv") 

# Apartado (a)
# Extraiga los nombres de las variables que contienen los dos ficheros.
str(master)
str(bateos)

# Apartado (b)
# Muestre las primeras 6 filas de los dos objetos R creados.
# NOTAS:
#   Alternativamente podriamos haber usado algo como master[1:6,] y bateos[1:6,]
head(master)
head(bateos)

# Apartado (c)
# Cree un data.frame que contenga solamente las siguientes variables del objeto master (llámelo: master2):
# "lahmanID"
# "birthCountry"
# "deathYear"
# "nameFirst"
# "nameNick"
# "bats"
# "finalGame"
# "playerID"
# "birthYear"
# "birthState"
# "deathCountry" "deathState"
# "nameLast"
# "weight"
# "height"
# "throws"
# "debut"
# "college"

#Columnas no existentes
# lahmanID
# nameNick
# college

master2 = data.frame(master["birthCountry"],master["deathYear"],master["nameFirst"], master["bats"],master["finalGame"],
                     master["playerID"],master["birthYear"],master["birthState"],master["deathCountry"], master["deathState"],
                     master["nameLast"],master["weight"],master["height"],master["throws"],master["debut"])


# Apartado (d)
# ¿De cuántos países distintos hay jugadores de béisbol? Muestre el peso (weight) y la altura (height) de los
# jugadores de “W.Germany”. Represente esos puntos con la ayuda de plot.

#Cuenta el numero de paises en los que han nacido los jugadores
numero_paises_distintos = length(unique(master$birthCountry))
numero_paises_distintos
#Muestra el peso y altura de los jugadores nacidos en "Germany"
peso_altura_germany = subset(master, master$birthCountry=="Germany", select = c("weight","height"))
peso_altura_germany
#Representa los puntos (peso,altura) correspondientes a los jugadores nacidos en "Germany"
plot(peso_altura_germany)

# Apartado (e)
# ¿Cuántos jugadores son de “England” y tienen un peso mayor que 180 (libras)?
england_peso_mayor_180 = subset(master, master$birthCountry=="United Kingdom" & master$weight>180)
nrow(england_peso_mayor_180)

# Apartado (f)
# Seleccione 200 jugadores al azar y calcule el siguiente índice:
# Guarde los datos en un fichero Excel datos200.xlsx.

#Seleccionamos los 200 jugadores
azar_200_jugadores = master[sample(nrow(master), 200), ]

#Calculamos el indice:
#Paso 1) Definimios la función que realiza dicho cálculo
get_index = function(w, h) (w/(h**2))*100
#Paso 2) Usando cbind, unimoos al dataframe original la columna, a la que llamaremos index, que nos devuelve la función definida en el paso anterior
azar_200_jugadores = cbind(azar_200_jugadores, index=mapply(get_index, azar_200_jugadores$weight, azar_200_jugadores$height))

#Guardamos los datos en un fichero 
library(openxlsx)
openxlsx::write.xlsx(azar_200_jugadores, file = "datos200.xlsx", colNames = TRUE)


# Apartado (g)
# ¿De qué clase R es la variable nameFirst? Conviértela a clase character. Recodifique en la variable
# nameFirst (en master2) para que en lugar de Charlie aparezca Carlos. ¿Cuántos jugadores tienen como
# nameFirst el valor Carlos ahora? ¿Y antes?

#Determinamos la clase dela variable nameFirst
class(master$nameFirst)
#Convertimos la clase a character
master$nameFirst = as.character(master$nameFirst)

#Numero de jugadores con el nombre Carlos antes del cambio
nrow(subset(master2, master2$nameFirst=="Carlos"))
#Realizamos el cambio
#    Con la expresion which(master2$nameFirst=="Charlie") obtenemos la posicion de todas las filas que cumplen que nameFirst=="Charlie"
#    Usamos ese array para recuperar todas las filas, de entre ellas solo necesitamos la variables nameFirst por lo que añadimos ["nameFirst"]
#    Finalmente asignamos el valor "Carlos" a esa variable
master2[which(master2$nameFirst=="Charlie"),]["nameFirst"]="Carlos"
#Numero de jugadores con el nombre Carlos despues del cambio
nrow(subset(master2, master2$nameFirst=="Carlos"))


# ------------------------------------------------------------------------------------------------------------------------------------------
# Ejercicio 2
# Combine las dos data.frame en un único data.frame (llámelo todos) uniéndolos por la variable que los
# relaciona playerID.
todos = merge(master,bateos,by="playerID")

# Apartado (a)
# Guarde los 2000 primeros registros de todos en un fichero csv.
todos_2000_primeros = todos[1:2000,]
write.csv(todos_2000_primeros,"todos_2000_primeros.csv")


# ------------------------------------------------------------------------------------------------------------------------------------------
# Ejercicio 3
# Cree una función que calcule el momento de orden k de una variable, que por defecto calcule el momento de
# orden 2 (nota: elimine en la función los datos faltantes o NA que puediera tener la variable). Utilícela para
# calcular el momento de orden 2, 3 y 4, de las variables peso y altura de todos los jugadores, y de la variable
# RBI pero únicamente de los jugadores nacidos en USA con yearID igual a 2008 (usa la función subset).

library(e1071)

moment_of_order = function (x,k=2){
  #  Omitimos los datos sin valor, para ello usamos is.na(x) el cual nos devolverá un array de booleanos que nos indica
  #  cuales de ellos tienen el valor NA, para obtener los que no lo tienen simplemente negamos la condición !(is.na(x))
  #  Por ultimo usamos el mapa resultante para obtener los datos concretos
  x = x[!(is.na(x))] 
  #  Llamamos a la función moment de la librería e1071 pasando el valor k que hemos recibido como parámetro, 
  #  el cual tiene 2 como valor por defecto
  return (e1071::moment(x, order=k, center=TRUE))
}

#Calculo del momento de orden 2, para peso y altura. Al ser 2 el valor por defecto, no es necesario pasarlo
moment_of_order(master$weight)
moment_of_order(master$height)

#Calculo del momento de orden 3, para peso y altura
moment_of_order(master$weight,3)
moment_of_order(master$height,3)

#Calculo del momento de orden 4, para peso y altura
moment_of_order(master$weight,4)
moment_of_order(master$height,4)

#Calculo del momento de orden 2,3,4, para variable RBI con las restricciones indicadas
# Usamos el dataframe todos, porque necesitamos filtrar por el año de nacimiento y esa variables no está 
# en el dataframe bateos
rbi_usa_year2008 = subset(todos, todos$birthCountry =="USA" & todos$yearID==2008, select = c("RBI"))
moment_of_order(rbi_usa_year2008$RBI)
moment_of_order(rbi_usa_year2008$RBI,3)
moment_of_order(rbi_usa_year2008$RBI,4)



# ------------------------------------------------------------------------------------------------------------------------------------------
# Ejercicio 4
# NOTA: Añada este cuarto ejercicio libre con algún tipo de manipulación sobre estos datos que
# le resulte de interés.

# 1) Estudio de los jugadores según su pais de nacimiento
#----------------------------------------------------------------
# Obtenemos un vector con los paises de los jugadores
players_by_Country = master$birthCountry
# Normalizamos todos los paises que no son USA
players_by_Country[which(players_by_Country!="USA")]="RESTO DEL MUNDO"

# Obtenemos las frecuencias relativas, en forma de %
FrePor = round(prop.table(table(players_by_Country))*100,1)
# Configuramos y mostramos la gráfica
etiquetas = paste(rownames(FrePor)," ",FrePor,"%",sep="")
pie(FrePor,labels=etiquetas,col = rainbow(length(etiquetas)),cex=0.8, main="Distribución jugadores USA/Resto del mundo")


# 2) Estudio de los jugadores según su pais de nacimiento. 
#    para jugadores foraneos
#----------------------------------------------------------------

foreign_players = subset(master, master$birthCountry!="USA", select = c("birthCountry"))
FrePor  = round(prop.table(table(foreign_players))*100,1)
# Mostraremos solo los que superen el 5%, agrupando el resto
over_5  = FrePor[which(FrePor>=5)]
under_5 = FrePor[which(FrePor<5)]
#Calcularmos la suma de los porcentajes de los paises con menos de un 5%
under_5_sum = sum(under_5)

#Añadimos el nuevo dato a mostrar 
l = length(over_5)+1
names = rownames(over_5)
over_5[l]=under_5_sum
names[l]="Otros paises"

# Configuramos y mostramos la gráfica
etiquetas = paste(names," ",over_5,"%",sep="")
pie(over_5,labels=etiquetas,col = rainbow(length(etiquetas)),cex=0.8, main="Distribución jugadores Foráneos")

# 3) Estudio de la evolución, en cuanto a numero de jugadores debutantes
#    comparando los jugadores locales (nacidos en uS) con los foraneos
#----------------------------------------------------------------

# Creamos una función que extrae el año de la fecha de debut,
# teniendo en cuenta los distintos formatos presentes.
get_debut_year = function(d) {
  if (grepl("-", d)) {
    # Fechas tipo 1890-04-21 
    return(as.integer(substr(d,1,4)))
  } else{
    # Fechas tipo 1890-04-21 
    return(as.integer(substr(d,(nchar(d)+1)-4,nchar(d))))
  }
}

# Obtenemos todos los jugadores nacidos fuera de USA
foreign_players = subset(master, master$birthCountry!="USA", select = c("debut"))

# Añadimos la columna "year" a nuestro dataframe
foreign_players = cbind(foreign_players, year=mapply(get_debut_year, foreign_players$debut))

# Obtenemos la tabla de frecuencias absolutas, agrupando en 40 grupos para hacer la grafica más manejable
frecAbs_foreign_players = table(cut(foreign_players$year,breaks = 40))

# Obtenemos todos los jugadores nacidos en de USA
local_players = subset(master, master$birthCountry=="USA", select = c("debut"))

# Añadimos la columna "year" a nuestro dataframe
local_players = cbind(local_players, year=mapply(get_debut_year, local_players$debut))

# Obtenemos la tabla de frecuencias absolutas, agrupando en 40 grupos para hacer la grafica más manejable
frecAbs_local_players = table(cut(local_players$year,breaks = 40))

#Pintamos la grafica 
yrange<-range(c(frecAbs_foreign_players,frecAbs_local_players))
plot(frecAbs_foreign_players, type="l", ylim=yrange, col=1, xlab="Año de debut", ylab="Número de jugadores", main="Evolución de debuts de jugadores (Locales vs Foraneos)")
lines(frecAbs_local_players, type="l", col=2)



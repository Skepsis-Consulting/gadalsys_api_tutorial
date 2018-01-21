## la primera vez instalar los paquetes
install.packages("httr")
install.packages("jsonlite")


### si ya estan instalados arranca aqui 

require(httr)
require(jsonlite)


url_base = "https://gadalsys.com"
url_metodo = "/sales_ml"
### sustituir TOKEN por el token que generas en Gadalsys > Administracion > Credenciales
token = TOKEN   ### sustituir por tu nombre de usuario de gadalsys
frequency = "diario" ### acepta diario, mensual, semanal, anual
username = "NOMBRE DE USUARIO" ## cambiar por tu username
site = "MLM" ### acepta "MLV", "MLC"
### fecha de inicio
inicio = "2017-01-01" ### formato YYYY-MM-DD
fin = "2018-01-20" ### formato YYYY-MM-DD


traer_ventas_x_periodo <- function(url_base, url_metodo, token, site, frequency, username,  inicio, fin){

	url_final = paste(url_base, url_metodo, ".json", "?", 
	"token=", token, "&site=", site, "&frequency=", frequency, "&username=", username, 
	 "&inicio=", inicio, "&fin=", fin, collapse=NULL, sep="")
	### hacer el request al api de meli
	data <- GET(url_final)
	## parsear el response
	data_parse <- jsonlite::fromJSON(content(data, "text"), simplifyVector = FALSE)


	periodos = vector(mode='character', length=length(data_parse))
	num_ventas = vector(mode='numeric', length=length(data_parse))
	ventas_periodo = vector(mode='numeric', length=length(data_parse))


	for(i in 1:length(data_parse)){
		## guardar el periodo
		periodos[i] = data_parse[[i]]$periodo 
		## guardar el numero de ventas del periodo
		if(is.null(data_parse[[i]]$num_ventas)){
			num_ventas[i] = 0
		}else{
			num_ventas[i] = as.numeric(data_parse[[i]]$num_ventas)
		}
		## guardar el valor de las ventas del periodo
		if(is.null(data_parse[[i]]$ventas_periodo)){
			ventas_periodo[i] = 0
		}else{
			ventas_periodo[i] = as.numeric(data_parse[[i]]$ventas_periodo)
		}
	}


	### hacemos un dataframe con los 3 vectores
	salida <- data.frame(periodos, num_ventas, ventas_periodo)
	### mandar el resultado	
	return(salida)

}

### ejecutar la funcion y crear un dataframe
df <- traer_ventas_x_periodo(url_base, url_metodo, token, site, frequency, username,  inicio, fin)

## ver los primeros resultados
head(df)

### ver todos los resultados
df 

### explorar valor de las ventas
summary(df$ventas_periodo)


### explorar numero de ventas 
summary(df$num_ventas)

### FILTRO para solo los dias de semana (lunes a viernes)
dia_semana = as.POSIXlt(df$periodos)$wday
filtro_dia_semana = ((dia_semana > 0) & (dia_semana < 6))


### histograma de ventas diarias de lunes a viernes
hist(df$num_ventas[filtro_dia_semana])
### excluir los dias sin ventas de lunes a viernes
hist(df$num_ventas[filtro_dia_semana])

## incluyendo fines de semana
hist(df$num_ventas)

## filtrar dias con ventas 
filtro_dia_con_venta = df$num_ventas > 0 

### histograma de dias de la semana con ventas
hist(df$num_ventas[filtro_dia_semana & filtro_dia_con_venta])

### dividir en menor frequency el histograma
hist(df$num_ventas[filtro_dia_semana & filtro_dia_con_venta], breaks=20)
## ver la tabla 
df[filtro_dia_semana & filtro_dia_con_venta,]

### filtrar por fechas 
df[as.Date(df$periodos) >= as.Date("2017-12-01"),]

## fecha, dia semana y dia con venta 
df[as.Date(df$periodos) >= as.Date("2017-10-01") & filtro_dia_semana & filtro_dia_con_venta,]

## histograma fecha, dia semana y dia con venta 
hist(df[as.Date(df$periodos) >= as.Date("2017-10-01") & filtro_dia_semana & filtro_dia_con_venta, 2])


### exportar a csv (ej. para abrir en Excel)
nombre_para_el_archivo_csv = "ventas_diarias.csv"
write.csv(nombre_para_el_archivo_csv, df, row.names=FALSE) 








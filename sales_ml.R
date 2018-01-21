
url_base = "https://gadalsys.com"
url_metodo = "/sales_ml"
### sustituir TOKEN por el token que generas en Gadalsys > Administracion > Credenciales
token = TOKEN
### sustituir por tu nombre de usuario de gadalsys
username = "mexico"
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
			num_ventas[i] = data_parse[[i]]$num_ventas
		}
		## guardar el valor de las ventas del periodo
		if(is.null(data_parse[[i]]$ventas_periodo)){
			ventas_periodo[i] = 0
		}else{
			ventas_periodo[i] = data_parse[[i]]$ventas_periodo
		}
	}


	### hacemos un dataframe con los 3 vectores
	salida <- data.frame(periodos, num_ventas, ventas_periodo)
	### mandar el resultado	
	return(salida)

}

df <- traer_ventas_x_periodo(url_base, url_metodo, token, site, frequency, username,  inicio, fin)
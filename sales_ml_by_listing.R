## la primera vez instalar los paquetes
install.packages("httr")
install.packages("jsonlite")


### si ya estan instalados arranca aqui 

require(httr)
require(jsonlite)


url_base = "https://gadalsys.com"
url_metodo = "/sales_ml_by_listing"
### sustituir TOKEN por el token que generas en Gadalsys > Administracion > Credenciales
token = TOKEN   ### sustituir por tu nombre de usuario de gadalsys
frequency = "diario" ### acepta diario, mensual, semanal, anual
username = "NOMBRE DE USUARIO" ## cambiar por tu username
site = "MLM" ### acepta "MLV", "MLC"
### fecha de inicio
inicio = "2017-01-01" ### formato YYYY-MM-DD
fin = "2018-01-20" ### formato YYYY-MM-DD


traer_ventas_x_listing <- function(url_base, url_metodo, token, site, frequency, username,  inicio, fin){

	url_final = paste(url_base, url_metodo, ".json", "?", 
	"token=", token, "&site=", site, "&frequency=", frequency, "&username=", username, 
	 "&inicio=", inicio, "&fin=", fin, collapse=NULL, sep="")
	### hacer el request al api de meli
	data <- GET(url_final)
	## parsear el response
	data_parse <- jsonlite::fromJSON(content(data, "text"), simplifyVector = FALSE)


	gadalsys_listing_id = vector(mode='character', length=length(data_parse))
	ventas_periodo = vector(mode='numeric', length=length(data_parse))
	listing_name = vector(mode='character', length=length(data_parse))


	for(i in 1:length(data_parse)){
		## guardar el periodo
		gadalsys_listing_id[i] = data_parse[[i]]$reference_id 
		## guardar el numero de ventas del periodo
		if(is.null(data_parse[[i]]$ventas_periodo)){
			ventas_periodo[i] = 0
		}else{
			ventas_periodo[i] = as.numeric(data_parse[[i]]$ventas_periodo)
		}
		## guardar el valor de las ventas del periodo
		if(is.null(data_parse[[i]]$nombre)){
			listing_name[i] = 0
		}else{
			listing_name[i] = (data_parse[[i]]$nombre)
		}
	}


	### hacemos un dataframe con los 3 vectores
	salida <- data.frame(gadalsys_listing_id, ventas_periodo, listing_name)
	### mandar el resultado	
	return(salida)

}

data <- traer_ventas_x_listing(url_base, url_metodo, token, site, frequency, username,  inicio, fin)



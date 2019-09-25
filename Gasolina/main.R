# URL para hacer ranking de gadolinas
# http://m.facilito.gob.pe/facilito-mobile/pages/public/buscarCombustible?idsCombustible=74&idsCombustible=78&idsCombustible=73&idsCombustible=03&masCercano=0&usarMiUbicacion=0&area=5&miLatitud=-12.089684&miLongitud=-77.033528&distrito=150136&provincia=150100&departamento=150000&distritoText=SAN%20MIGUEL&provinciaText=LIMA&departamentoText=LIMA&codigoUnidad=&startIndex=0&recordsPerPage=50&back=0

#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scraped
url = 'http://m.facilito.gob.pe/facilito-mobile/pages/public/buscarCombustible?idsCombustible=74&idsCombustible=78&idsCombustible=73&idsCombustible=03&idsCombustible=72&idsCombustible=04&masCercano=0&usarMiUbicacion=0&area=10&miLatitud=-12.089684&miLongitud=-77.033528&distrito=150136&provincia=150100&departamento=150000&distritoText=SAN%20MIGUEL&provinciaText=LIMA&departamentoText=LIMA&codigoUnidad=&startIndex=0&recordsPerPage=200&back=0'
download.file(url, destfile = "gasolina.html", quiet=TRUE)

#Reading the HTML code from the website
webpage <- read_html("gasolina.html")

####################################################################
prices_data_html <- html_nodes(webpage,'.texto_3')
#Converting the prices data to text
prices_data <- html_text(prices_data_html)

#Let's have a look at the rankings
head(prices_data)
  
#Data-Preprocessing: Converting rankings to numerical
prices_data<- gsub("S/.","",prices_data)
prices_data<-as.numeric(prices_data)

#Let's have another look at the rankings
head(prices_data)

####################################################################
company_data_html <- html_nodes(webpage,'.texto_1')
#Converting the companies data to text
company_data <- html_text(company_data_html)

#Let's have a look at the companies
head(company_data)

#Data-Preprocessing: Converting companies
company_data<- sub("\r\n","",company_data)
company_data<- substr(company_data,1,regexpr('\r', company_data))
company_data<- gsub("  ","",company_data)
company_data<- gsub("\r","",company_data)
#Let's have another look at the companies
head(company_data)

####################################################################
tipo_data_html <- html_nodes(webpage,'.texto_2')
#Converting the tipo gas data to text
tipo_data <- html_text(tipo_data_html)

#Let's have a look at the tipo
head(tipo_data)

#Data-Preprocessing: Converting tipo
tipo_data<- gsub("\r\n","",tipo_data)
tipo_data<- gsub("  ","",tipo_data)
tipo_data<- as.factor(tipo_data)
#Let's have another look at the tipo
show(tipo_data)
length(tipo_data)
####################################################################
direccion_data_html <- html_nodes(webpage,'.texto_1')
#Converting the direcciones data to text
direccion_data <- html_text(direccion_data_html)

#Let's have a look at the direcciones
head(direccion_data)

#Data-Preprocessing: Converting direcciones
direccion_data<- sub("\r\n","",direccion_data)
direccion_data<- substr(direccion_data,regexpr('\r', direccion_data),regexpr(')', direccion_data))
direccion_data<- sub("\r\n","",direccion_data)
direccion_data<- gsub("  ","",direccion_data)

#Let's have another look at the direcciones
head(direccion_data)

#let's have the distance in KM also:
posicion1<- regexpr("\\(", direccion_data)
posicion2<- regexpr("\\)", direccion_data)
distancia_data<- substr(direccion_data, posicion1, posicion2)
distancia_data<- gsub("\\(Distancia: ","",distancia_data)
distancia_data<- gsub(" Km\\)","",distancia_data)
distancia_data<- as.numeric(distancia_data)
show(distancia_data)

#############juntamos la informacion
gasolina_df<-data.frame(Precio = prices_data, 
                      Tipo = tipo_data,
                      Empresa = company_data, 
                      Direccion = direccion_data,
                      Distancia = distancia_data)
str(gasolina_df)

############## Analisis ##################
#Aplicamos filtros
gasolina_df2 <- as.data.frame(gasolina_df)
transform.data.frame(gasolina_df2, ) ############## falta aplicar descuento de repsol


library('ggplot2')

qplot(data = gasolina_df,Precio,fill = Tipo,bins = 25)

ggplot(gasolina_df,aes(x=Precio,y=Tipo))

ggplot(gasolina_df,aes(x=Precio,y=Tipo))+
  geom_point(aes(Empresa))


ggplot(gasolina_df, aes(x=Empresa, y=Precio,color=Tipo)) +
  geom_point()

gasolina_df <- filter(gasolina_df,Empresa=="REPSOL COMERCIAL S.A.C.")

library(gapminder)
library(plotly)

p <- gapminder %>%
  ggplot(gasolina_df,aes(x=Empresa,y=Tipo,size=Precio)) +
  geom_point() +
  scale_x_log10() +
  theme_bw()

ggplotly(p)


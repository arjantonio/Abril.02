library(httr)
library(XML)

# Pregunta 1.1 
# Almacenando la URl 
html <- GET("https://www.mediawiki.org/wiki/MediaWiki")
contenido <- content(html, as = "text")
parsedHtml <- htmlParse(contenido)

# Pregunta 1.2
# Obteniendo el título de la web
xpath <- "//title"
# Utilizando la función: xpathSApply()
titulos <- xpathSApply(parsedHtml, xpath, xmlValue)
# Se muestra el Título de la Pagina Web
cat("Título de la Página Web:\n")
cat(titulos, sep = "\n")

# Pregunta 1.3 

# Extrayendo los nombres de la etiqueta <a></a>
name_hyperlink <- xpathSApply(parsedHtml, "//a", xmlValue)
#name_hyperlink 
# Extrayendo los valores del atributo href de la etiqueta <a></>
url_hyperlink <- xpathSApply(parsedHtml, "//a", xmlGetAttr, "href")
#url_hyperlink
# Validando valores null

# Validando si los nombres de la etiqueta y los valores del atributo href tienen los valores null
null_name <- sapply(name_hyperlink, is.null)
null_href <- sapply(url_hyperlink, is.null)
# Reemplazando los valores null por el valor NA
name_hyperlink[null_name] <- NA
url_hyperlink[null_href] <- NA

name_hyperlink <- unlist(name_hyperlink)
url_hyperlink  <- unlist(url_hyperlink)

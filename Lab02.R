library(httr)
library(XML)
library(stringr)
library(dplyr) 
library(purrr)
library(ggplot2)

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

# Pregunta 1.4 
# Creando una tabla con el texto y su respectivo url del enlace
links_tables <- data.frame(Text = name_hyperlink, Url = url_hyperlink)

# Convirtiendo a data.frame
concurrences <- as.data.frame(table(links_tables))

# Filtrando solo los enlaces existentes, es decir todos lo que tenga >0 en la columna freqq
links_data <- filter(concurrences, Freq > 0) %>% arrange(desc(Freq))


# Pregunta 1.5
# Agregando la URL completa 
base_url <- "https://www.mediawiki.org"

# Validando los carácteres al inicio de la columna URL
links_data$Final_Url <- case_when(
  # validando los carácteres al inicio de la URL
  grepl("^/wiki/", links_data$Url) ~ paste0(base_url, links_data$Url),
  grepl("^/w/", links_data$Url) ~ paste0(base_url, links_data$Url),
  grepl("^//", links_data$Url) ~ paste0(base_url, links_data$Url),
  grepl("^https", links_data$Url) ~ links_data$Url,
  grepl("^#", links_data$Url) ~ paste0(base_url,"/wiki/MediaWiki", links_data$Url),
  TRUE ~ NA_character_
)

# Incorporando el valor status_code 
# Demora 1 minuto
# Recorriendo los datos para incorporar HEAD y hallar el status_code
status_codes <- map(links_data$Final_Url, HEAD)
# Agregando una columna con el status_code respectivo
links_data$Status_Code <- map(status_codes, status_code)
# Convirtiendo de lista a character
links_data$Status_Code <- as.character(links_data$Status_Code)
View(links_data)
cat("Proceso terminado")

# Pregunta 2
# Pregunta 2.1
# Validando si la URL es absoluta o relativa
links_data$Url_type <- ifelse(grepl("^http", links_data$Url), "Absoluta", "Relativa")
# Agregando la gráfica histograma
ggplot(links_data, aes(x=Freq)) + 
  geom_histogram(aes(fill=Url_type), 
                 binwidth = 1, 
                 position = "dodge") +
  scale_fill_manual(values=c("#FF5733", "#6B33FF")) +
  labs(x = "Frecuencia de apariciones", y = "N° de enlaces", title ="Enlaces MediaWiki") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10), 
                     expand = c(0, 0)) +
  theme_light()



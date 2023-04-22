library(GET)
library(readr)
library(rvest)
library(httr)
library(XML)
library(stringr)
library(tidyr)
library(dplyr) 
library(purrr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
# Pregunta 1.1 
# Almacenando la URl 
html <- GET("https://www.mediawiki.org/wiki/MediaWiki")
# Descargando contenido de la página web
contenido <- content(html, as = "text")
# Analizando el contenido de la página web
parsedHtml <- htmlParse(contenido, asText = TRUE)

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

# Validando si los nombres de la etiqueta tienen valores null
null_name <- sapply(name_hyperlink, is.null)
# Validando si los valores del atributo href tienen valores null
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
cat("En Proceso... \n")
status_codes <- map(links_data$Final_Url, HEAD)
# Agregando una columna con el status_code respectivo
links_data$Status_Code <- map(status_codes, status_code)
# Convirtiendo de lista a character
links_data$Status_Code <- as.character(links_data$Status_Code)

cat("Proceso terminado \n")

# Pregunta 2
# Pregunta 2.1
# Validando si la URL es absoluta o relativa
links_data$Url_type <- ifelse(grepl("^http", links_data$Url), "URL_Absoluta", "URL_Relativa")
# Agregando la gráfica histograma
histogram <- ggplot(links_data, aes(x=Freq)) + 
  geom_histogram(aes(fill=Url_type), 
                 binwidth = 1, 
                 position = "dodge") +
  scale_fill_manual(name ="Tipo de Url", values=c("#FF5733", "#6B33FF")) +
  labs(x = "Frecuencia de apariciones", y = "N° de enlaces", title ="Enlaces MediaWiki") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10), 
                     expand = c(0, 0)) +
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, size = 18))
 
grid.arrange(histogram, ncol=1)
# Pregunta 2.2 

# Añadiendo si el link es interno o externo
links_data$Domain_Type <- ifelse(grepl("^https://www.mediawiki.org", links_data$Final_Url), "Interno", "Externo")

# Hallando la frecuencia
freq_link <- table(links_data$Domain_Type)
# Mostrando gráfica de barras
bar_graphic <- ggplot(data.frame(Domain_Type = names(freq_link), count = as.numeric(freq_link)), aes(x=Domain_Type, y=count, fill = Domain_Type)) +  
  geom_bar(stat="identity") +
  labs(title="Enlaces Internos vs Externos", x="Tipo de enlace", y="Cantidad") +
  theme_light() +
  scale_fill_manual( name = "Tipo de dominio", values = c("#6833FF", "#D433FF")) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 20)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18))

grid.arrange(bar_graphic, ncol=1)

# Pregunta 2.3
# Hallando la frecuencia del status_code
code_freq <- table(links_data$Status_Code)
# Hallando el porcentaje en % ( o.9 a 90%)
percentage_value <- round(prop.table(code_freq) * 100, 2)
percentage_value <- as.numeric(percentage_value)
# data 
code_data <- data.frame(Status_Code = names(code_freq),Percentage = percentage_value)

# Mostrando pie chart
chart_graphic <- ggplot(code_data, aes(x="", y=Percentage, fill=Status_Code)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ggtitle("Códigos de estados de respuesta") +
  theme_void() +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(name = "Código de respuesta" ,values=c("#cf3a69", "#7caa96")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 22, size = 18))
grid.arrange(chart_graphic, ncol=1)

# Mostrando las tres gráficas en una sola figura
grid.arrange(histogram, bar_graphic, chart_graphic, ncol=3)
cat("Fin\n")
View(links_data)

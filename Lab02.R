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
library(knitr)
library(stringr)
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
# Extrayendo los valores del atributo href de la etiqueta <a></a>
url_hyperlink <- xpathSApply(parsedHtml, "//a", xmlGetAttr, "href")

# Validando si los nombres de la etiqueta tienen valores null
null_name <- sapply(name_hyperlink, is.null)
# Validando si los valores del atributo href tienen valores null
null_href <- sapply(url_hyperlink, is.null)
# Reemplazando los valores null por el valor NA
name_hyperlink[null_name] <- NA
url_hyperlink[null_href] <- NA
# Convirtiendo de lista a vector los nombres y el valor del href del la etiqueta <a></a>
name_hyperlink <- unlist(name_hyperlink)
url_hyperlink  <- unlist(url_hyperlink)

# Se valida que hay texto que tiene al principio espacios pero son iguales
# Se realiza la eliminación de los espacios al inicio
name_hyperlink <- str_replace(name_hyperlink, "^\\s+", "")
# Creando una tabla con el texto y su respectivo url del enlace
links_tables <- data.frame(Text = name_hyperlink, Url = url_hyperlink) %>% arrange(Text)

# Pregunta 1.4 

# Convirtiendo a data.frame y hallando el n° de repeticiones
concurrences <- as.data.frame(table(links_tables))

# Filtrando solo los enlaces existentes, es decir todos lo que tenga >0 en la columna freq y lo ordenamos en orden alfábetico
links_data <- filter(concurrences, Freq > 0) %>% arrange(Text)


# Pregunta 1.5
# Agregando la URL completa 
base_url <- "https://www.mediawiki.org"

# Validando los carácteres al inicio de la columna URL y uniendolo con la url base
links_data$Final_Url <- case_when(
  # validando los carácteres al inicio de la URL
  grepl("^/wiki/|^/w/|^//", links_data$Url) | grepl("^#", links_data$Url) ~ paste0(base_url, links_data$Url),
  grepl("^https", links_data$Url) ~ links_data$Url,
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

# Agregando la gráfica histograma general
histogram <- ggplot(links_data, aes(x=Freq)) + 
  geom_histogram(aes(fill=Url_type), 
                 binwidth = 1, 
                 position = "dodge") +
  # Cambiando el nombre de la leyenda
  scale_fill_manual(values=c("#fe8b05","#fe0557"), name = "Tipo de Url") +
  # Incroporando los  nombres para los ejes x y y, y el título del gráfico
  labs(x = "Frecuencia", y = "N° de enlaces", title ="Enlaces absolutos vs relativos") +
  # Escala del número de enlaces
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10), 
                     expand = c(0, 0)) +
  # Creando fondo con líneas de cuadrícula
  theme_light() +
  # Ajustando la ubicación del título
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  # Incorporando valores a cada barra
  geom_text(aes(x = Freq, y = ..count.., label = ..count..,group = Url_type), 
            stat = "count", vjust= -0.5, hjust = 0.7, size=4)


# Mostrando el histograma<
grid.arrange(histogram, ncol=1)


# Pregunta 2.2 

# Añadiendo si el link es interno o externo
links_data$Domain_Type <- ifelse(grepl("^https://www.mediawiki.org", links_data$Final_Url), "Interno", "Externo")

# Hallando la frecuencia
freq_link <- table(links_data$Domain_Type)
# Mostrando gráfica de barras
# Dibujando la gráfica en base al tipo de dominio si es externo o interno y su cantidad
bar_graphic <- ggplot(data.frame(Domain_Type = names(freq_link), count = as.numeric(freq_link)), aes(x=Domain_Type, y=count, fill = Domain_Type)) +  
  # Creación del gráfico de barras
  geom_bar(stat="identity") +
  # Títulos para los ejes x e y, y el título de la gráfica
  labs(title="Enlaces Internos vs Externos", x="Tipo de dominio", y="Cantidad") +
  theme_light() +
  # Cambiando el nombre de la leyenda
  scale_fill_manual( name = "Tipo de dominio", values = c("#5e5473", "#19b5a5")) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 20)) +
  # Incorporando valores en cada barra
  geom_text(aes(label=count), vjust=-0.5, size=5) +
  # Ajustando ubicación del título
  theme(plot.title = element_text(hjust = 0.5, size = 18))
# Mostrando la gráfica
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
  # Gráfico chart pie
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  # Titulo del gráfico
  ggtitle("Códigos de estados de respuesta") +
  theme_void() +
  # Incorporando los porcentajes
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size = 3) +
  # Nombre de la leyenda y colores del gráfico
  scale_fill_manual(name = "Código de respuesta" ,values=c("#cf3a69", "#7caa96")) +
  # Ubicación del título
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 18))

# Mostrando la gráfica
grid.arrange(chart_graphic, ncol=1)

# Mostrando las tres gráficas en una sola figura
grid.arrange(histogram, bar_graphic, chart_graphic, ncol=1)
cat("Fin\n")
# Visualización de la tabla final
View(links_data)

html <- GET("https://www.mediawiki.org/wiki/MediaWiki")
content <- content(html, as = "text")
parsedHtml <- htmlParse(content)
xpath <- "//title"
# Utilizando la función: xpathSApply()
Titulos <- xpathSApply(parsedHtml, xpath, xmlValue)
# Se muestra el Título de la Pagina Web
cat("Título de la Página Web:\n")
cat(titulos, sep = "\n")
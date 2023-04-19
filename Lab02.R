html <- GET("https://www.mediawiki.org/wiki/MediaWiki")
content <- content(html, as = "text")
parsedHtml <- htmlParse(content)
xpath <- "//title"
Titulos <- xpathSApply(parsedHtml, xpath, xmlValue)
cat(titulos, sep = "\n")
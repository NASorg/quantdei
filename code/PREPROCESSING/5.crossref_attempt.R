library(rcrossref)
library(curl)
file.edit("~/.Renviron")

rcrossref::cr_works_(query = "astronomy 'critical race theory'", cursor = "*", cursor_max = 1, works = TRUE, .progress = TRUE)

cr_works(query="NSF")


cr_works(query="global state", filter=c(has_orcid=TRUE), limit=3)



res <- curl_fetch_memory("https://api.crossref.org/works?filter=has-full-text:true&mailto=GroovyBib@example.org")

readLines(res$content)
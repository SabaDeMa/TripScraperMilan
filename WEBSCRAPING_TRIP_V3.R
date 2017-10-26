## TRIP-SCRAPING
library(XML)
library(reshape2)
library(tidyr)
library(dplyr)
library(RCurl)


# costruzione URL
url_primo_pezzo <- "https://www.tripadvisor.it/RestaurantSearch-g187849"
url_secondo_pezzo <- "-a_date.2016__2D__03__2D__18-a_people.2-a_time.20%3A00%3A00-Milan_Lombardy.html#EATERY_LIST_CONTENTS"
pezzo_in_mezzo_1 <- "-oa"
sequenza_di <- seq(31, 6000, by = 30)
pezzo_in_mezzo2 <- paste0(pezzo_in_mezzo_1, sequenza_di)
prima_pagina <- paste0(url_primo_pezzo, url_secondo_pezzo)
url1 <- paste0(url_primo_pezzo, pezzo_in_mezzo2, url_secondo_pezzo)
url <- c(prima_pagina, url1)

# LETTURA HTML

padpa <- getURL(url[1:5],  ssl.verifypeer = FALSE)


# PARSING
parsing_1 <- lapply(, htmlParse, useInternalNodes = TRUE)

# ESTRAZIONE LINK
link <- lapply(parsing_1, function(x) getHTMLLinks(x, xpQuery = "//a[@class='property_title']/@href") )

# CREAZIONE LINK TOTALI
root_url <- "https://www.tripadvisor.it"
tree_root <- unlist(link)
total_url <- paste0(root_url, tree_root)

## LETTURA HTML LINKS
lfc <- function(x){
    con <- url(x, "r")
    x <- readLines(con)
    close(con)
    return(x)
}

lettura1 <- lapply(total_url, lfc )

# PARSING 2
parsing_1 <- lapply(lettura1,
                    function(x) htmlTreeParse(x[-1], useInternalNodes = T))


##################################################
## APPROCCIO TOTAL URL DIVERSO
# forse https non sono visti da htmlTreeParse quindi tolto la s
# PROVATO E NON FUNZIONA total_url_1 <- sub("s", "", total_url)




# SALVATAGGIO WORKSPACE
# save(list = ls(all.names = TRUE), file = "C:/users/sdemaio/Desktop/estrazioni_trip/trip.RData")
load("trip.Rdata")




## estrazione brutale con GREPL


parsa <- lapply(lettura1[[1]],
                function(x) xpathApply(
                    htmlParse(x,
                              useInternalNodes = TRUE, asText = TRUE),
                    "//h1[@id='HEADING']",
                xmlValue))




library(dplyr)
library(stringr)
library(rvest)

# Pegando nomes dos episodios e href dos transcripts:

epi_nodes = read_html("http://avatar.wikia.com/wiki/Avatar_Wiki:Transcripts") %>%
  html_nodes("td > a") %>% 
  .[c(2:62)] # episodios do atla

epi_names = epi_nodes %>%
  html_text()

epi_href = epi_nodes %>%
  as.character() %>%
  str_match('a href="(.*?)" title=') %>%
  .[,2]

scrape_avatar <- function(href) {
  
  pag = "http://avatar.wikia.com/" %>%
    paste0(href) %>%
    read_html()
  
  speaker = pag %>%
    html_nodes("th") %>%
    html_text(trim = TRUE)
  
  text = pag %>%
    html_nodes("th+ td") %>%
    html_text()
  
  tibble(speaker, text)
}

raw_transcripts = lapply(epi_href, scrape_avatar)

for(i in 1:length(raw_transcripts)) {
  raw_transcripts[[i]] = cbind(raw_transcripts[[i]], 
                               epi_num = i,
                               epi_name = epi_names[i])
}

transcripts = do.call(rbind, raw_transcripts) 

dir.create("data")
save(transcripts, file = "data/transcripts.RData")

# Coletando as notas dos episódios no IMDB

scrape_ratings <- function(season) {
  rating = "https://www.imdb.com/title/tt0417299/episodes?season=" %>%
    paste0(as.character(season)) %>%
    read_html() %>%
    html_nodes(".ipl-rating-widget > .ipl-rating-star .ipl-rating-star__rating") %>%
    html_text() %>%
    as.numeric()
}

scrape_desc <- function(season) {
  rating = "https://www.imdb.com/title/tt0417299/episodes?season=" %>%
    paste0(as.character(season)) %>%
    read_html() %>%
    html_nodes(".item_description") %>%
    html_text(trim = TRUE)
}

rat = c(scrape_ratings(1), scrape_ratings(2), scrape_ratings(3))[-1] # o primeiro do site não conta
desc = c(scrape_desc(1), scrape_desc(2), scrape_desc(3))[-1]

ratings = tibble(epi_num = 1:61, epi_name = epi_names, rating = rat, description = desc)

save(ratings, file = "data/ratings.RData")
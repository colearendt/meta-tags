library(tidyverse)

get_meta_tags <- function(url) {
  res <- httr::GET(url)
  ct <- httr::content(res)
  tmpfile <- tempfile(fileext = ".html")
  write_lines(ct, tmpfile)
  rawhtml <- xml2::read_html(tmpfile)

  meta_tags <- rvest::html_nodes(rawhtml, "meta")

  meta_attrs <- meta_tags %>% rvest::html_attrs()

  meta_attrs %>% map_df(identity)
}


all_urls <- read_lines("urls.txt")

all_tags <- purrr::map_df(all_urls, function(.x) {get_meta_tags(.x) %>% mutate(url = .x)})


all_tags %>% filter(!is.na(name)) %>% group_by(name) %>% summarize(content = paste(unique(content), collapse = "/")) %>%
  View()

all_tags %>% filter(!is.na(name)) %>% distinct(name)
all_tags %>% filter(!is.na(property)) %>% distinct(property)

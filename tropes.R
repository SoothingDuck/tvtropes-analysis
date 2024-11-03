library(polite)
library(rvest)
library(tidyverse)
library(stringr)

## Fonctions Categories
get_subpage_links_from_page <- function(page) {
    session <- bow("https://tvtropes.org/pmwiki/pmwiki.php/WebVideo/TheOldestView", force = TRUE)

    result <- scrape(session, query = list(t = "semi-soft", per_page = 100))

    tmp <- tibble(
        subpage_name = result %>% html_node(".subpage-links") %>% html_nodes("a") %>% html_text() %>% str_trim(),
        subpage_ref = result %>% html_node(".subpage-links") %>% html_nodes("a") %>% html_attr("href") %>% paste0("https://tvtropes.org", .)
    ) %>% filter(subpage_name != "Create Subpage")

    return(tmp)
}

## Fonctions Bottom Links
get_bottom_links_from_page <- function(page, col_number) {
    ## Retourne les pages "Index" en bas de page
    session <- bow(page, force = TRUE)

    result <- scrape(session, query = list(t = "semi-soft", per_page = 100))

    tmp <- tibble(
        link_name = (result %>% html_node(".links") %>% html_nodes("a") %>% html_text()),
        link_ref = (result %>% html_node(".links") %>% html_nodes("a") %>% html_attr("href") %>% paste0("https://tvtropes.org", .))
    ) %>% filter(as.numeric(rownames(.)) %% 3 == col_number)

    return(tmp)
}

get_previous_links_from_page <- function(page) {
    ## Retourne les pages "Index" en bas de page
    return(get_bottom_links_from_page(page, col_number = 1))
}

get_index_links_from_page <- function(page) {
    ## Retourne les pages "Index" en bas de page
    return(get_bottom_links_from_page(page, col_number = 2))
}

get_next_links_from_page <- function(page) {
    ## Retourne les pages "Index" en bas de page
    return(get_bottom_links_from_page(page, col_number = 0))
}

## MAIN
page <- "https://tvtropes.org/pmwiki/pmwiki.php/WebVideo/TheOldestView"

get_previous_links_from_page(page)

get_index_links_from_page(page)

get_next_links_from_page(page)

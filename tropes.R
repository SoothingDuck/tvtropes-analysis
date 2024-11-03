library(polite)
library(rvest)
library(tidyverse)
library(stringr)

## Fonctions Main article

main - article

mainarticle_links_from_page <- function(page) {
    session <- bow("https://tvtropes.org/pmwiki/pmwiki.php/WebVideo/TheOldestView", force = TRUE)

    result <- scrape(session, query = list(t = "semi-soft", per_page = 100))

    tmp <- tibble(
        main_article_href = result %>%
            html_node("#main-article") %>%
            html_nodes("a.twikilink") %>%
            html_attr("href") %>%
          paste0("https://tvtropes.org", .)
    ) %>%
        unique() %>%
        mutate(main_article_name = str_split_i(main_article_href, "/", i = -1)) %>%
        select(main_article_name, main_article_href)

    return(tmp)
}


tmp <- tibble(
    subpage_name = ,
    subpage_ref = result %>%
        html_node(".subpage-links") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        paste0("https://tvtropes.org", .)
) %>%
    filter(subpage_name != "Create Subpage")

## Fonctions Categories
subpage_links_from_page <- function(page) {
    session <- bow(page, force = TRUE)

    result <- scrape(session, query = list(t = "semi-soft", per_page = 100))

    tmp <- tibble(
        subpage_name = result %>%
            html_node(".subpage-links") %>%
            html_nodes("a") %>%
            html_text() %>%
            str_trim(),
        subpage_ref = result %>%
            html_node(".subpage-links") %>%
            html_nodes("a") %>%
            html_attr("href") %>%
            paste0("https://tvtropes.org", .)
    ) %>%
        filter(subpage_name != "Create Subpage")

    return(tmp)
}

## Fonctions Bottom Links
bottom_links_from_page <- function(page, col_number) {
    ## Retourne les pages "Index" en bas de page
    session <- bow(page, force = TRUE)

    result <- scrape(session, query = list(t = "semi-soft", per_page = 100))

    tmp <- tibble(
        link_name = (result %>% html_node(".links") %>% html_nodes("a") %>% html_text()),
        link_ref = (result %>%
            html_node(".links") %>%
            html_nodes("a") %>%
            html_attr("href") %>%
            paste0("https://tvtropes.org", .)
        )
    ) %>%
        filter(as.numeric(rownames(.)) %% 3 == col_number)

    return(tmp)
}

previous_links_from_page <- function(page) {
    ## Retourne les pages "Index" en bas de page
    return(bottom_links_from_page(page, col_number = 1))
}

index_links_from_page <- function(page) {
    ## Retourne les pages "Index" en bas de page
    return(bottom_links_from_page(page, col_number = 2))
}

next_links_from_page <- function(page) {
    ## Retourne les pages "Index" en bas de page
    return(bottom_links_from_page(page, col_number = 0))
}

## MAIN
page <- "https://tvtropes.org/pmwiki/pmwiki.php/WebVideo/TheOldestView"

subpage_links_from_page(page)

previous_links_from_page(page)

index_links_from_page(page)

next_links_from_page(page)

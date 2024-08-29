#' Give the years present on the FFCK website
#'
#' This function scrapes the FFCK website for the years listed their database.
#'
#' @return A vector of integers representing the years mentioned on the FFCK website.
#' @export
#' @import rvest
#' @import dplyr
#'
#' @examples
#' give_years()
give_years <- function(){

  link='http://www.ffcanoe.asso.fr/eau_vive/slalom/classement/evenements/index/annee:2001'

  content <- read_html(link) %>%
    html_nodes(xpath = '//*[@id="accueil"]/div[1] | //*[@id="accueil"]/div[2]') %>%
    html_nodes("a") %>%
    html_text()%>%
    as.integer()

  content <- c(2001, content)

  return(content)
}

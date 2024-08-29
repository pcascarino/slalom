#' Extract race IDs from the FFCK website for given years
#'
#' This function scrapes the FFCK website to extract competition and race IDs for the specified years.
#'
#' @param years_tab A vector of integers representing the years to scrape.
#' @return A data frame with columns `annee`, `ID_competition`, and `ID_course` representing the year, competition ID, and race ID respectively.
#' @export
#' @import rvest
#' @import dplyr
#' @import stringr
#'
#' @examples
#' give_id_race(c(2001, 2002))
give_id_race <- function(years_tab){

  ID_competition_df <- data.frame()

  for(year in years_tab){

    # create the link with each years
    year_link <- paste0('http://www.ffcanoe.asso.fr/eau_vive/slalom/classement/evenements/index/annee:', year)


    content <- read_html(year_link) %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset('voir') %>%
      str_subset('course|evenements')

    evenements <- 0

    for (i in content){

      if (str_detect(i, 'evenements')){
        evenements <- str_extract(i, "\\d+$") %>%
          as.integer()
      }
      else if(str_detect(i, 'courses')){
        course <- str_extract(i, "\\d+$") %>%
          as.integer()
        temp_df <- data.frame(annee=year, ID_competition = evenements, ID_course = course)

        ID_competition_df <- bind_rows(ID_competition_df, temp_df)
      }
      else{
        print(paste('BUG-01 : no race detected', i))
      }
    }


  }

  return(ID_competition_df)
}

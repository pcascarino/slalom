library(tidyverse)
library(dplyr)
library(docstring)
library(rvest)
library(stringr)









#' Scrape Competition Results and Build Dataframe
#'
#' This function scrapes competition data from the specified URL based on the provided
#' `ID_compet` dataframe. For each competition, it retrieves competition details, 
#' participant results, and additional information from the web page.
#'
#' @param ID_compet A dataframe containing the details of each competition. 
#'                  It should include the columns:
#'                  - `ID_competition`: An identifier for the competition.
#'                  - `ID_course`: An identifier for the course.
#'                  - `annee`: The year of the competition.
#'
#' @return A dataframe (`df_final`) containing the following columns for each participant:
#'         - `Place`: The place of the participant.
#'         - `Nom`: The name of the participant.
#'         - `Club`: The club of the participant.
#'         - `Cat`: The category of the participant.
#'         - `Temps`: The time of the participant.
#'         - `Pen`: Penalties incurred by the participant.
#'         - `Total`: The total score of the participant.
#'         - `Valeur`: The value associated with the participant's result.
#'         - `Pts_course`: Points awarded in the course.
#'         - `ID_competition`: Identifier of the competition.
#'         - `ID_course`: Identifier of the course.
#'         - `annee`: The year of the competition.
#'         - `Type`: The type of competition.
#'         - `NomCompet`: Name of the competition.
#'         - `Date`: The date of the competition.
#'         - `lien`: Links to the related content.
#'         - `Embarcation`: The first 3 characters of the link.
#'
#' @examples
#' # Example dataframe
#' example_df <- data.frame(
#'   ID_competition = c(1, 2),
#'   ID_course = c(101, 202),
#'   annee = c(2024, 2024)
#' )
#' result_df <- give_df_final(example_df)
#' print(result_df)
#'
#' @import rvest
#' @import stringr
#' @import dplyr
#'
#' @export
give_df_final <- function(ID_compet){
  
  df_final <- data.frame()
  
  for(i in (1:nrow(ID_compet))){
    
    id_c <- ID_compet$ID_course[i]
    print(paste(i, ' :', nrow(ID_compet), ' =>', id_c))
    link_course <- paste0('http://www.ffcanoe.asso.fr/eau_vive/slalom/classement/courses/voir/', id_c)
    
    ###
    ### Scraping des infos de la course
    ###
    content_name <- read_html(link_course)%>%
      html_nodes(".pagetitle") %>%
      html_text() %>%
      str_trim()
    
    
    compet_info <- str_split(content_name, " - ", simplify = TRUE)
    
    ###
    ### Scraping des résultats
    ###
    content <- read_html(link_course)%>%
      html_nodes("td") %>%
      html_text() %>%
      str_trim()
    
    temp_matrix <- matrix(content, ncol = 9, byrow = TRUE) # On regroupe les données des competiteurs
    df <- as.data.frame(temp_matrix, stringsAsFactors = FALSE)
    colnames(df) <- c("Place", "Nom", "Club", "Cat", "Temps", "Pen", "Total", "Valeur", "Pts_course")
    
    
    content_link <- read_html(link_course)%>%
      html_nodes("a") %>% 
      html_attr("href") %>%
      str_subset('embarcations/voir')%>%
      str_extract(basename(.))
    
    df$ID_competition <- ID_compet$ID_competition[i]
    df$ID_course <- ID_compet$ID_course[i]
    df$annee <- ID_compet$annee[i]
    df$Type <- compet_info[1]
    df$NomCompet <- compet_info[2]
    df$Date <- compet_info[3]
    df$lien <- content_link
    df$Embarcation <- substr(df$lien, 1, 3)
    
    
    df_final <- bind_rows(df_final, df)
    
    save(df_final, file = './App/Data/df_final.RData')
  }
  
  return(df_final)
}






#' Extract competition and course IDs from the FFCanoe website for multiple years.
#'
#' This function iterates over a vector of years, constructs URLs to access competition and 
#' course data for each year, and extracts the IDs of competitions and courses from those URLs.
#' It returns a data frame containing the year, competition ID, and course ID.
#'
#' For each year, the function creates a URL, retrieves the content, and parses the href attributes 
#' to find links related to either 'evenements' or 'courses'. It then extracts the IDs and 
#' combines them into a single data frame.
#'
#' @param years_tab A numeric vector of years for which the data should be extracted. Each year is used
#'                  to construct a URL from which competition and course IDs are extracted.
#' @return A data frame with columns 'annee', 'ID_competition', and 'ID_course'. Each row represents
#'         a course with its associated competition ID and the year of the event.
#' @examples
#' years_tab <- c(2020, 2021)
#' ID_competition_df <- give_id_race(years_tab)
#' print(ID_competition_df)
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
      
      # Si la ligne est une compétition
      if (str_detect(i, 'evenements')){
        evenements <- str_extract(i, "\\d+$") %>%
          as.integer()
      }
      # Si la ligne une course
      else if(str_detect(i, 'courses')){
        course <- str_extract(i, "\\d+$") %>%
          as.integer()
        temp_df <- data.frame(annee=year, ID_competition = evenements, ID_course = course)
        
        ID_competition_df <- bind_rows(ID_competition_df, temp_df)
      }
      else{
        print(paste('BUG-01 : Pas de course ou compétition détecté', i))
      }
    }
    
    
  }
  
  return(ID_competition_df)
}
  




#' Extract and return the years from the given URL.
#'
#' This function fetches the content from the provided URL, 
#' extracts the years listed in the web page, and returns them 
#' as a vector of integers. The function automatically adds the 
#' base year (2001) to the beginning of the returned list.
#'
#' @param link A string. The URL of the web page to extract years from. 
#'             Defaults to 'http://www.ffcanoe.asso.fr/eau_vive/slalom/classement/evenements/index/annee:2001'.
#' @return A vector of integers containing the base year (2001) followed by the extracted years.
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



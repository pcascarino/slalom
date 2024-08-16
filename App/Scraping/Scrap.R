library(tidyverse)
library(dplyr)
library(docstring)
library(rvest)
library(stringr)





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



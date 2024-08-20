library(tidyverse)
library(dplyr)
library(docstring)
library(rvest)
library(stringr)



scrap_course <- function(ID_compet, isN1=FALSE, link_rdata='./Data/2024_N1.RData'){
  
  df_final <- data.frame()
  
  
  for(i in 1:nrow(ID_compet)){   
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
    
    content <- content[!str_detect(content, 'MancheTempsPénalités')]
    
    
    index_empty <- which(content == "")[1]
    
    if(isN1 == TRUE){
      index_empty <- index_empty+1
    }
    
    
    abd_nbr <- length(which(content == "Abd" | content == "Des" | content == "Dsq"))/2
    
    
    delete_link <- c()
    
    if(abd_nbr != 0){
      for(abd_n in 1:abd_nbr){
        
        abd <- which(content == "Abd" | content == "Des" | content == "Dsq")[1]
        start_index <- abd - 6
        
        end_index <- start_index
        
        while (!is.na(end_index) && end_index <= (length(content) - 3)) {
          
          # We delete all before the first place of the next categorie
          if (content[end_index] == "" && content[end_index + 1] == "" && content[end_index + 3] == "1") {
            break
          }
          end_index <- end_index + 1
        }
        
        if (!is.na(end_index) && end_index < length(content) - 3) {
          # Supprimer les valeurs entre start_index et end_index (inclus)
          content <- content[-(start_index:end_index)]
        }
      }
      
    }
    
    
    
    
    content <- content[content != ""]
    
    temp_matrix <- matrix(content, ncol = index_empty-1, byrow = TRUE) # On regroupe les données des competiteurs
    
    
    
    
    df_temp <- as.data.frame(temp_matrix, stringsAsFactors = FALSE)
    colnames(df_temp) <- c("Dossard", "Place", "Del", "Nom", "Club", "Cat", "Resultats", "Valeur", "Pts_course",
                           "Del", "Temps_pur", "Penalites")
    
    df_temp <- df_temp[, -ncol(df_temp)]
    
    colnames(df_temp)[is.na(colnames(df_temp))] <- paste0("Unnamed_", which(is.na(colnames(df_temp))))
    
    df_temp <- df_temp[, !grepl('Del', colnames(df_temp))]
    
    na_cols <- grep('NA|Unnamed_', colnames(df_temp), value = TRUE)
    
    df_temp$Portes <- apply(df_temp[, na_cols, drop = FALSE], 1, function(row) {
      paste(na.omit(row), collapse = "-")
    })
    
    df_temp <- df_temp[, !grepl('NA|Unnamed_', colnames(df_temp))]
    
    content_link <- read_html(link_course)%>%
      html_nodes("a") %>%
      html_attr("href") %>%
      str_subset('embarcations/voir')%>%
      str_extract(basename(.))
    
    df_temp$ID_competition <- ID_compet$ID_competition[i]
    df_temp$ID_course <- ID_compet$ID_course[i]
    df_temp$annee <- ID_compet$annee[i]
    df_temp$Type <- compet_info[1]
    df_temp$NomCompet <- compet_info[2]
    df_temp$NumCourse<- compet_info[3]
    df_temp$Date <- compet_info[4]
    
    
    df_temp$Embarcation <- ''
    
    content_emb <- read_html(link_course) %>%
      html_nodes("th") %>%
      html_text() %>%
      str_trim() %>%
      str_subset("^.{3}$") %>%
      str_subset("[A-Z]$")
    
    
    n <- length(df_temp$Place)
    embarcation_count <- length(content_emb)
    embarcation_index <- 1
    
    # Boucle pour remplir la colonne Embarcation
    for (i in 1:n) {
      if (df_temp$Place[i] == "1" && i != 1) {
        # Chaque fois que Place revient à "1" (sauf pour le premier élément)
        embarcation_index <- embarcation_index + 1
        
        # Si l'indice dépasse le nombre d'embarcations disponibles, il revient au début
        if (embarcation_index > embarcation_count) {
          embarcation_index <- 1
        }
      }
      
      # Remplissage de la colonne Embarcation
      df_temp$Embarcation[i] <- content_emb[embarcation_index]
    }
    
    df_final <- bind_rows(df_final, df_temp)
    
    save(df_final, file = link_rdata)  
    
  }
  
  return(df_final)
}


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



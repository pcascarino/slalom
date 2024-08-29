#' Scrap Course Results from FFCK
#'
#' This function scrapes the course results from the French canoeing federation website
#' using the specified course ID, competition ID, year, and whether it's a national event (N1).

#' @param ID_course A numeric or character ID representing the course to be scraped.
#' @param ID_competition A numeric or character ID representing the competition to which the course belongs.
#' @param year A numeric year for the competition.
#' @param isN1 A logical value indicating whether the competition is a national event (N1). Default is FALSE.
#'
#' @return A data frame containing the scraped course results, including competitor information
#' and relevant statistics.
#' @export
#'
scrap_course <- function(ID_course, ID_competition, year, isN1=FALSE){

  link_course <- paste0('http://www.ffcanoe.asso.fr/eau_vive/slalom/classement/courses/voir/', ID_course)

  # scrap informations of the course
  content_name <- read_html(link_course)%>%
    html_nodes(".pagetitle") %>%
    html_text() %>%
    str_trim()

  # Scrap results of the course
  content <- read_html(link_course) %>%
    html_nodes("td") %>%
    html_text() %>%
    str_trim()

  # Replace non-ASCII characters
  content <- iconv(content, from = "UTF-8", to = "ASCII//TRANSLIT")

  # Optionally, remove entries containing 'MancheTempsPénalités' (using ASCII version)
  content <- content[!str_detect(content, 'MancheTempsPenalites')]



  compet_info <- str_split(content_name, " - ", simplify = TRUE)


  # Permits having the number of stats for each competitor
  index_empty <- which(content == "")[1]
  if(isN1 == TRUE){
    index_empty <- index_empty+1
  }

  #Permits having the number of ABD (wich cause all the bugs for the rest)
  abd_nbr <- length(which(content == "Abd" | content == "Des" | content == "Dsq"))/2

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

  temp_matrix <- matrix(content, ncol = index_empty-1, byrow = TRUE)


  df_return <- as.data.frame(temp_matrix, stringsAsFactors = FALSE)
  colnames(df_return) <- c("Dossard", "Place", "Del", "Nom", "Club", "Cat", "Resultats", "Valeur", "Pts_course",
                         "Del", "Temps_pur", "Penalites")

  df_return <- df_return[, -ncol(df_return)]

  colnames(df_return)[is.na(colnames(df_return))] <- paste0("Unnamed_", which(is.na(colnames(df_return))))

  df_return <- df_return[, !grepl('Del', colnames(df_return))]

  na_cols <- grep('NA|Unnamed_', colnames(df_return), value = TRUE)

  df_return$Portes <- apply(df_return[, na_cols, drop = FALSE], 1, function(row) {
    paste(row[!is.na(row)], collapse = "-")
  })

  df_return <- df_return[, !grepl('NA|Unnamed_', colnames(df_return))]

  content_link <- read_html(link_course)%>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset('embarcations/voir')%>%
    str_extract(basename(.))


  df_return$ID_competition <- ID_competition
  df_return$ID_course <- ID_course
  df_return$annee <- year
  df_return$Type <- compet_info[1]
  df_return$NomCompet <- compet_info[2]
  df_return$NumCourse<- compet_info[3]
  df_return$Date <- compet_info[4]


  df_return$Embarcation <- ''

  content_emb <- read_html(link_course) %>%
    html_nodes("th") %>%
    html_text() %>%
    str_trim() %>%
    str_subset("^.{3}$") %>%
    str_subset("[A-Z]$")


  n <- length(df_return$Place)
  embarcation_count <- length(content_emb)
  embarcation_index <- 1

  # Boucle pour remplir la colonne Embarcation
  for (i in 1:n) {
    if (df_return$Place[i] == "1" && i != 1) {
      embarcation_index <- embarcation_index + 1

      if (embarcation_index > embarcation_count) {
        embarcation_index <- 1
      }
    }

    # Remplissage de la colonne Embarcation
    df_return$Embarcation[i] <- content_emb[embarcation_index]
  }


  return(df_return)


}

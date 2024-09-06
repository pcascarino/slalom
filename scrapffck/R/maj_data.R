#' Update Data Function
#'
#' This function retrieves a list of years using the `give_years` function and prints
#' the result. It is designed to be a part of a larger data management workflow where
#' updating or processing data for specific years is necessary. It checks for existing
#' courses in a database and scraps data for new courses if necessary.
#'
#' @param link_database_csv A character string containing the file path to the CSV
#'        file that contains the database of courses.
#' @param link_error_csv A character string containing the file path to the CSV
#'        file for logging errors encountered during data retrieval.
#'
#' @return A list of years printed to the console. The function does not return any
#'         value (invisible NULL).
#' @export
maj_data <- function(link_database_csv, link_error_csv){

  if (!file.exists(link_database_csv)) {
    #write.csv(data.frame(), file = link_database_csv)
    db_csv <- data.frame()
  }else{
    db_csv <- read.csv(link_database_csv)
  }

  if (!file.exists(link_error_csv)) {
    #write.csv(data.frame(), file = link_database_csv)
    error_csv <- data.frame()
  }else{
    error_csv <- read.csv(link_error_csv)
  }

  delete_course <- c(20120470, 20119847, 20119742, 20119645) # Chpt de France par équipe


  years <- give_years()
  years <- sort(years, decreasing = TRUE)


  for(year in years){
    # if our data countains all penalities and gates
    if(year > 2010){
      year_course_df <- give_id_race(year)

      for(i in 1:nrow(year_course_df)){

        if(year_course_df$ID_course[i] %in% unique(db_csv$ID_course)){
          print(paste0('(', i, '/', nrow(year_course_df), ') - ', year, ' : ', year_course_df$ID_course[i], ' [En base]'))
        }else if(year_course_df$ID_course[i] %in% delete_course){
            print(paste0('(', i, '/', nrow(year_course_df), ') - ', year, ' : ', year_course_df$ID_course[i], ' [PAS OK]'))
        }
        else{
          print(paste0('(', i, '/', nrow(year_course_df), ') - ', year, ' : ', year_course_df$ID_course[i]))


          df_temp <- scrap_course(year_course_df$ID_course[i], year_course_df$ID_competition[i], year, isN1=FALSE)

          if(verif_course(df_temp)){
            db_csv <- rbind(db_csv, df_temp)
          }else{
            print("Test du format N1 ...")
            df_temp <- scrap_course(year_course_df$ID_course[i], year_course_df$ID_competition[i], year, isN1=TRUE)
            if(verif_course(df_temp)){
              db_csv <- rbind(db_csv, df_temp)
            }else{
              print('Erreur')
              error_csv <- rbind(error_csv, df_temp)
            }
          }

        write.csv(db_csv, file=link_database_csv, row.names = FALSE)
        write.csv(error_csv, file=link_error_csv, row.names = FALSE)
      }


    }
  }

  }
}

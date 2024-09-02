#' Verify Course import
#'
#' This function checks if the proportion of course names consisting solely of numbers
#' exceeds a specified threshold (10% of the total number of entries).
#'
#' @param df_test A data frame containing course names in the column 'Nom'.
#'
#' @return A logical value indicating whether the number of course names
#'         that are purely numeric exceeds 10% of the total number of names.
#' @export

verif_course <- function(df_test){

  nombre_chiffres <- sum(grepl("^[0-9]+$", df_test$Nom))

  return(! nombre_chiffres > nrow(df_test)/10)
}




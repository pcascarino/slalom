#' Create a DataFrame of gate results from a race
#'
#' This function processes a column of gate results from the input data frame (`db`) and
#' creates a new data frame where each gate result is split into individual columns.
#' The columns are named "Porte_1", "Porte_2", etc., and contain numeric values.
#' The new data frame also includes the `Embarcation` column from the original data frame.
#'
#' @import dplyr
#'
#' @param db A data frame containing race data, including a column `Portes` with gate results as strings and a column `Embarcation`.
#'
#' @return A data frame with individual columns for each gate result, named "Porte_1", "Porte_2", etc., and an additional `Embarcation` column.
#' @export
create_gate_df <- function(db){
  porte_tab <- db$Portes

  # Split each string by "-" and convert to a list of vectors
  porte_list <- strsplit(porte_tab, "-")

  # Convert the list of vectors to a data frame
  porte_df <- as.data.frame(do.call(rbind, porte_list))

  # Rename columns to "Porte_1", "Porte_2", etc.
  colnames(porte_df) <- paste0("Porte_", 1:ncol(porte_df))

  # Convert columns to numeric type
  porte_df <- porte_df %>%
    mutate(across(everything(), as.numeric))

  porte_df$Embarcation <- db$Embarcation

  return(porte_df)
}

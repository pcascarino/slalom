#' Create a DataFrame with counts of specific gate results
#'
#' This function processes the gate results from the input data frame (`db`) by splitting
#' the gate results into individual columns, and then counts the occurrences of the values
#' 2 and 50 for each gate and each `Embarcation`. It returns a new data frame with these counts.
#'
#' @import dplyr
#'
#' @param db A data frame containing a column `Portes` with gate results as strings and a column `Embarcation`.
#'
#' @return A data frame with columns for each gate result, counts of 2 and 50, and the corresponding `Embarcation`.
#' @export
create_gate_df <- function(db){
  porte_tab <- db$Portes
  porte_list <- strsplit(porte_tab, "-")
  porte_df <- as.data.frame(do.call(rbind, porte_list))
  colnames(porte_df) <- 1:ncol(porte_df)
  porte_df <- porte_df %>%
    mutate(across(everything(), as.numeric))
  porte_df$Embarcation <- db$Embarcation

  result_df <- data.frame(Porte = character(), count_2 = integer(), count_50 = integer(), Embarcation = character())
  embarcations <- unique(porte_df$Embarcation)

  for (porte in colnames(porte_df)[1:(ncol(porte_df) - 1)]) {
    for (embarcation in embarcations) {
      porte_data <- porte_df[porte_df$Embarcation == embarcation, porte, drop = FALSE]
      count_2 <- sum(porte_data[[1]] == 2)
      count_50 <- sum(porte_data[[1]] == 50)
      result_df <- rbind(result_df, data.frame(Porte = porte, count_2 = count_2, count_50 = count_50, Embarcation = embarcation))
    }
  }

  return(result_df)
}

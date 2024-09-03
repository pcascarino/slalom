#' Plot the distribution of athletes based on race results
#'
#' This function visualizes the distribution of athletes based on their race results.
#' A histogram is generated to represent the number of athletes per second, with vertical lines
#' indicating the positions of the athletes specified in `athlete_name`. Athletes with results
#' exceeding a threshold are counted and optionally labeled on the plot.
#'
#' @import dplyr
#' @import ggplot2
#' @import ggrepel
#'
#' @param db A data frame containing race results, including columns like `Resultats`, `Nom`, `Embarcation`, `Temps_pur`, and `Penalites`.
#' @param athlete_name A vector of athlete names to highlight in the plot.
#' @param group_all A boolean indicating whether to group all athletes together or to facet by `Embarcation`.
#'
#' @return A ggplot object visualizing the distribution of athletes based on their race results.
#' @export
#'
plot_course_time <- function(db, athlete_name, group_all=FALSE){

  db$Resultats <- as.integer(db$Resultats)

  cut_time <- min(db$Resultats) +50

  db_reduce <- db %>%
    filter(Resultats < cut_time)




  db_bad <- db %>%
    filter(Resultats >cut_time)%>%
    group_by(Embarcation) %>%
    summarise(count=n())

  count_bad <- data.frame(cut_time = cut_time, counts = nrow(db_bad))



  athlete_df <- db %>%
    filter(Nom %in% athlete_name)



  fig <- ggplot(db_reduce, aes(x = Resultats)) +
    geom_histogram(aes(fill = Embarcation),
                   binwidth = 1,
                   position = "identity",
                   color='black') +
    geom_vline(data = athlete_df,
               aes(xintercept = Resultats),
               linewidth=1.5,
               #linetype = "dashed",
               color = "red") +
    xlab("Résultats finaux") +
    ylab("Nbr d'athlètes dans la seconde") +
    ggtitle("Répartition des athlètes en fonction des résultats d'une course") +
    scale_x_continuous(limits = c(min(db_reduce$Resultats), cut_time + 10)) +
    scale_y_continuous(limits = c(0, 10)) +
    geom_label_repel(data = athlete_df,
                     aes(x=Resultats, y=10, label=paste0(Nom," : ", Temps_pur, "(+", Penalites, ")")),
                     color='red',
                     fill='white',
                     alpha=0.7)

  if(! group_all){
    fig <- fig +
      geom_label(data = db_bad,
                 aes(x = cut_time + 5, y = 5,
                     label = paste(count, "athlètes \n(+", cut_time, "sec)")),
                 #vjust = -0.5,
                 alpha = 0.2) +
      facet_grid(Embarcation ~ .)
  }else{
    fig <- fig +
      geom_label(data = db_bad,
                 aes(x = cut_time + 5, y = 5,
                     label = paste(sum(db_bad$count), "athlètes \n(+", cut_time, "sec)")),
                 #vjust = -0.5,
                 alpha = 0.2)
  }
  return(fig)

}


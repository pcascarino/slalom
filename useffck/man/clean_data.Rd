% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_data.R
\name{clean_data}
\alias{clean_data}
\title{Clean and convert data columns to numeric}
\usage{
clean_data(db)
}
\arguments{
\item{db}{A data frame containing the dataset to be cleaned, including columns like \code{Dossard}, \code{Place}, \code{Resultats}, \code{Valeur}, \code{Pts_course}, \code{Temps_pur}, \code{Penalites}, \code{ID_competition}, \code{ID_course}, and \code{annee}.}
}
\value{
A data frame with the specified columns converted to numeric type.
}
\description{
This function cleans the input data frame by converting specific columns to numeric type.
It ensures that the selected columns are in the correct format for further analysis.
}

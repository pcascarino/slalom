library(tidyverse)
library(dplyr)

library(shiny)
library(bs4Dash)

library(shinyauthr)

# plot_colour <- "#8965CD"
# 
# theme <- create_theme(
#   bs4dash_color(
#     lime = "#52A1A5",
#     olive = "#4A9094",
#     purple = "#8965CD"
#   ),
#   bs4dash_status(
#     primary = "#E1EDED",
#     info = "#E4E4E4"
#   )
# )


# Load data --------------------------------------------------------------------

df_base <- read_csv('./Data/2024/df_2024.csv')


# Info box value ---------------------------------------------------------------

n_paddler <- length(unique(df_base$Nom))

ui <- dashboardPage(
  
  title = "Slalom App",
  
  dark = FALSE,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Slalom App",
      image = "./img/logo.png"
    )
  ),
  
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Accueil",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "Profils athlètes",
        tabName = "athletes",
        icon = icon("bar-chart")
      ),
      menuItem(
        "Profils courses",
        tabName = "courses",
        icon = icon("bar-chart")
      ),
      menuItem(
        "Classement",
        tabName = "classement",
        icon = icon("bar-chart")
      ),
      menuItem(
        "Prédictions",
        tabName = "predictions",
        icon = icon("bar-chart")
      )
    )
  ),
  
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(
    left = "Slalom App",
    right = "2024"
  ),
  
  
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        
        jumbotron(
          title = "Bienvenue !",
          status = "info",
          lead = "Slalom App est un application test pour le canoë-kayak slalom en France",
          btnName = "En savoir plus",
          href=""
        ),
        
        fluidRow(
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Paul CASCARINO",
              subtitle = "C1M",
              image = "",
              type = 1
            ),
            status = "purple",
            "Ajoute du texte"
          ),
          
          box(
            title = "Les Nouveautés",
            width = 6,
            collapsible = FALSE,
            blockQuote(
              "Une nouvelle box",
              color = "purple"
            )
          )
          
        )
        
        
      ),
      
      tabItem(
        tabName = "athletes",
        fluidRow(
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Paul CASCARINO",
              subtitle = "C1M",
              image = "",
              type = 1
            ),
            status = "purple",
            "Ajoute du texte"
          )
        )
        
      ),
      tabItem(
        tabName = "courses"
      ),
      tabItem(
        tabName = "classement"
      ),
      tabItem(
        tabName = "predictions"
      )
    )
  )
  
)

server <- function(input, output) {}

shinyApp(ui, server)
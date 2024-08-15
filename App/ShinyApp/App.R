library(tidyverse)
library(dplyr)
library(shiny)
library(shinythemes)
library(shinyWidgets)

load('../Data/df_final.RData')

df_tidy <- df_final %>%
  filter(ID_course %in% c(20120324, 20120402, 20120377, 20120378, 20120459, 20120456, 20120458))




athletes <- tabPanel(
  title = "Analyse des athlètes",
  

  sidebarLayout(
    
    ###
    ### Sidebar
    ###
    sidebarPanel(
      width = 3,
      h3("Filtrage :"),
      
      checkboxGroupInput(
        inputId = "athlete_emb",
        label = "Choix des embarcations :",
        choices = list(
          "C1" = "H",
          "K1" = "D",
          "C2" = "M"
        ),
        inline = TRUE,
        selected = c('H','D', 'M')
      ),
      
      
      checkboxGroupInput(
        inputId = "athlete_sexe",
        label = "Choix des sexes :",
        choices = list(
          "Homme" = "H",
          "Femme" = "D",
          "Mixte" = "M"
        ),
        inline = TRUE,
        selected = c('H','D', 'M')
      ),
      
      checkboxGroupInput(
        inputId = "athlete_cat",
        label = "Choix des catégories :",
        choices = list(
          "Minime : -15 ans" = "M",
          "Cadet : 15-16 ans" = "C",
          "Junior : 16-18 ans" = "J",
          "Senior : 18-35 ans" = "S",
          "Vétéran 1" = "V1",
          "Vétéran 2" = "V2",
          "Vétéran 3" = "V3",
          "Vétéran 4" = "V4",
          "Vétéran 5" = "V5"
        ),
        selected = c("M", "C", "J", "S", "V1", "V2", "V3", "V4", "V5" )
      ),
      
      sliderInput(
        inputId="athlete_date",
        label=HTML("Sélectionner les années"),
        min=2001,
        max=2024,
        value=c(2001,2024),
        step = 1
      )
      
    ),
    
    
    ###
    ### Main Panel
    ###
    mainPanel(widht=9,
              tabsetPanel(
              id = "athletes_choix",
              tabPanel("Tableau des classements",
                       hr(),
                       h3("En cours de développement"),
                         
                ),
                tabPanel("Analyse d'un athlète", 
                         HTML("<br>"),
                         sidebarPanel(width = 4,
                                      title = "",
                                      pickerInput(
                                        inputId = "athlete_athlete",
                                        label = "Sélectionner un athlète",
                                        choices = unique(df_tidy$Nom),
                                        multiple = FALSE,
                                        options = pickerOptions(
                                          liveSearch = TRUE
                                        )
                                      )
                         ),
                         hr(),
                      
                
              )
      )
    )
  )
)


competitions <- tabPanel(
  title = "Analyse des compétitions",
  
  sidebarLayout(
    ###
    ### Sidebar
    ###
    sidebarPanel(width = 3,
                 title = "",
    ),
    
    
    ###
    ### Main Panel
    ###
    mainPanel(widht=9,
              tabsetPanel(
                id = "compo_main_tabs",
                tabPanel("Compositions/équipes", 
                         hr(),
                         
                ),
                tabPanel("Joueurs/individuel", 
                         hr(),
                         
                         
                )
              )
    )
  )
)


predictions <- tabPanel(
  title = "Prédictions sur les futures courses",
  
  
  sidebarLayout(
    
    ###
    ### Sidebar
    ###
    sidebarPanel(width = 3,
                 title = "",
    ),
    
    
    ###
    ### Main Panel
    ###
    mainPanel(widht=9,
              tabsetPanel(
                id = "compo_main_tabs",
                tabPanel("Compositions/équipes", 
                         hr(),
                         
                ),
                tabPanel("Joueurs/individuel", 
                         hr(),
                         
                         
                )
              )
    )
  )
)








ui <- navbarPage(
  title = "Canoe stats France ",
  theme = shinytheme("cerulean"),
  athletes,
  competitions,
  predictions
)








server <- function(input, output) {
  
}


# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
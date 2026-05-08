# Nathan Hellwege
# This is an application which visualizes a team's lines and player shotmaps
# Built using R Shiny

library(shiny)
library(shinyWidgets)
library(sportyR)
library(ggplot2)
library(dplyr)
library(shiny.fluent)
library(data.table)
library(FNN)

shinyUI(fluidPage(
  tabsetPanel(
    id = "sicht",
    tabPanel(
      "Team View",
      value = "team",
      div(
        style = "display:flex; height:100vh;",
        
        # Sidebar
        div(
          style = "width:40%; padding:15px; overflow-y:auto; border-right:1px solid #ccc;",
          
          h3("Controls"),
          # Team Selection
          selectInput(
            inputId = "gruppe",
            label = "Select Team",
            choices = sort(c(unique(s$homeTeamCode), "All Teams")),
            selected = "All Teams"
          ),
          
          # Team Information
          uiOutput("Kenntnisse"),
          
          # Select player(s) in team
          selectInput(
            inputId = "menschen",
            label = "Select Player(s)",
            choices = NULL,
            multiple = TRUE
          ),
          # Forward Lines
          h4("Most Recent Forward Lines"),
          uiOutput("Zeilen"),
          hr(),
          # Defensive Pairings
          h4("Most Recent Defensive Pairings"),
          uiOutput("Paarungen")
        ),
        
        # Main Panel
        div(
          style = "width:60%; display:flex; flex-direction:column; padding:15px;",
          
          # Shotmap
          div(
            style = "height:60%; border-bottom:1px solid #ccc; padding-bottom:10px;",
            
            h3("Shotmap"),
            tabsetPanel(
              id = "staat",   # <-- THIS creates input$plot_tab
              
              tabPanel("All Situations", value = "all"),
              tabPanel("Even Strength", value = "evenStrength"),
              tabPanel("Power Play", value = "powerPlay"),
              tabPanel("Penalty Kill", value = "penaltyKill"),
            ),
            plotOutput("Tretten", height = "80%")
          ),
          # Bottom Panels
          div(
            hr(),
            style = "height:40%; display:flex; gap:10px; padding-top:10px;",
            
            # Selected player list
            div(
              style = "width:50%;",
              h3("Selected Players"),
              actionButton("Entfernen", "Remove All"),
              uiOutput("Auswahl")
            ),
            VerticalDivider(),
            # Line information
            div(
              style = "width:50%;",
              h3("Line Statistics"),
              uiOutput("Informatik")
            )
          )
        )
      )
    ),
    
    ## Player View ####
    tabPanel(
      "Player View",
      value = "player",
      div(
        style = "display:flex; height:100vh;",
        
        # Sidebar
        div(
          style = "width:40%; padding:15px; overflow-y:auto; border-right:1px solid #ccc;",
          
          h3("Controls"),
          # Team Selection
          selectInput(
            inputId = "P_gruppe",
            label = "Select Team",
            choices = sort(c(unique(s$homeTeamCode), "All Teams")),
            selected = "All Teams"
          ),
          
          # Player Selection
          selectInput(
            inputId = "P_menschen",
            label = "Select Player",
            choices = NULL,
            multiple = FALSE
          ),
          # Player Statistics
          h4("Player Statistics"),
          uiOutput("P_Kenntnisse"),
          hr(),
          # Player Linemates
          h4("Most Common Linemates"),
          uiOutput("P_Kameraden"),
          hr(),
          # Similar Players
          h4("Similar Players"),
          uiOutput("P_Zwillinge")
        ),
        
        # Main panel
        div(
          style = "width:60%; display:flex; flex-direction:column; padding:15px;",
          
          # Shotmap
          div(
            style = "height:100%; border-bottom:1px solid #ccc; padding-bottom:10px;",
            
            h3("Shotmap"),
            tabsetPanel(
              id = "P_staat",
              
              tabPanel("All Situations", value = "all"),
              tabPanel("Even Strength", value = "evenStrength"),
              tabPanel("Power Play", value = "powerPlay"),
              tabPanel("Penalty Kill", value = "penaltyKill"),
            ),
            plotOutput("P_Tretten", height = "90%")
          )
        )
      )
    )
  )
))
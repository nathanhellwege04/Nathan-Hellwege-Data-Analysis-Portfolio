library(shiny)
library(shinyWidgets)
library(sportyR)
library(ggplot2)
library(dplyr)
library(shiny.fluent)
library(data.table)
library(FNN)

function(input, output, session) {
  # Define global values
  all_teams <- sort(unique(s$shooterName))
  ids <- reactiveVal(NULL)
  top_lines <- reactiveVal(NULL)
  top_pairs <- reactiveVal(NULL)
  
  # Function to get line statistics given a list of player ids
  get_stats <- function(player_ids) {
    matches <- player_index[
      J(player_ids),
      .N,
      by = .(shift_id, team)
    ][N == length(player_ids)]
    
    rows <- linestats[
      matches,
      on = .(shift_id, team)
    ]
    result <- rows[, lapply(.SD, sum),
                   .SDcols = patterns("^for_|^against_")]
    
    result[, toi := sum(rows$duration)]
    
    result
  }
  
  # Team and player selection
  observeEvent(input$gruppe, {
    choices <- if (input$gruppe == "All Teams"){
      all_teams
    } else {
      sort(unique(s[which(s$shootingTeam == input$gruppe),]$shooterName))
    }
    updateSelectInput(
      session,
      inputId = "menschen",
      choices = choices,
      selected = NULL
    )
    gid <- max(lines$gameId[lines$playerTeam == input$gruppe & lines$gameId %in% unique(linestats$gameId)], na.rm = TRUE)
    top_lines(lines[which(lines$playerTeam == input$gruppe & lines$gameId == gid)[1:4],9:14])
    # print(top_lines())
    top_pairs(pairs[which(pairs$playerTeam == input$gruppe & pairs$gameId == gid)[1:3],9:12])
    # print(top_lines)
  })
  
  # Player selection
  observeEvent(input$menschen, {
    new_ids <- unique(c(ids(), pids$playerId[match(input$menschen, pids$name)]))
    ids(new_ids)
  })
  
  # Shotmap
  output$Tretten <- renderPlot({
    # print(ids())
    req(input$staat)
    # print(input$staat)
    players <- s %>% 
      filter(shooterPlayerId %in% ids())
    
    if (input$staat != "all"){
      players <- players[which(players$situation == input$staat),]
    }
    # print(unique(players$shooterPlayerId))
    geom_hockey(league="NHL", display_range = "offense", rotation = 270) + 
      geom_point(
        data = players,
        aes(x = yCordAdjusted, y = -1 * xCordAdjusted),
        color = "black",
        alpha = 0.6,
        size = 1.5
      )
  })
  
  # Get a team's most recent lines
  output$Zeilen <- renderUI({
    team_lines <- top_lines()
    # print(team_lines)
    tagList(
      lapply(seq_len(nrow(team_lines)), function(i) {
        
        line <- team_lines[i, ]
        
        div(
          style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:10px;",
          
          # List names
          div(
            strong(paste("Line", i, ": ")),
            paste(line$L1, line$L2, line$L3, sep = " - ")
          ),
          # print(i),
          # Buttons to add players
          actionButton(
            inputId = paste0("add_line_", i),
            label = "Add"
          )
        )
      })
    )
  })
  
  # Adds the line ids of players in the selected line to the global list
  add_line_ids <- function(i) {
    line <- top_lines()[i, ]
    # print(line)
    line_ids <- as.integer(unlist(line[, c("ID1", "ID2", "ID3")]))
    # print(line_ids)
    new_ids <- unique(c(ids(), line_ids))
    # print(new_ids)
    ids(new_ids)
    # print(ids())
    updateSelectizeInput(
      session,
      "players",
      selected = new_ids
    )
  }
  
  # Observers for the four "add line" buttons
  observeEvent(input$add_line_1, {
    add_line_ids(1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_line_2, {
    add_line_ids(2)
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_line_3, {
    add_line_ids(3)
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_line_4, {
    add_line_ids(4)
  }, ignoreInit = TRUE)
  
  
  # Defensive Pairings
  output$Paarungen <- renderUI({
    team_pairs <- top_pairs()
    
    tagList(
      lapply(seq_len(nrow(team_pairs)), function(i) {
        
        pair <- team_pairs[i, ]
        
        div(
          style = "display:flex; align-items:center; justify-content:space-between; margin-bottom:10px;",
          
          # List the names
          div(
            strong(paste("Pairing", i, ": ")),
            paste(pair$P1, pair$P2, sep = " - ")
          ),
          
          # Buttons to add the players
          actionButton(
            inputId = paste0("add_pairing_", i),
            label = "Add"
          )
        )
      })
    )
  })
  
  # Add the player ids to the global list
  add_pair_ids <- function(i) {
    pair <- top_pairs()[i, ]
    
    pair_ids <- as.integer(unlist(pair[, c("ID1", "ID2")]))
    
    new_ids <- unique(c(ids(), pair_ids))
    
    ids(new_ids)
    
    updateSelectizeInput(
      session,
      "players",
      selected = new_ids
    )
  }
  
  # Observers for the "add pairing" buttons
  observeEvent(input$add_pairing_1, {
    add_pair_ids(1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_pairing_2, {
    add_pair_ids(2)
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_pairing_3, {
    add_pair_ids(3)
  }, ignoreInit = TRUE)
  
  # Team information
  output$Kenntnisse <- renderUI({
    req(input$gruppe)
    #filename <- paste0("/home/nathan/RFolder/MU SPRT/COSC 5500/Final/Images/", input$gruppe, ".svg")
    #filename <- file.path("www/Images", paste0(input$gruppe, ".svg"))
    filename <- paste0(input$gruppe, ".svg")
    teamdata <- teams[which(teams$Abbrev == input$gruppe),]
    
    div(
      style = "border:1px solid #ccc; padding:10px; margin-bottom:15px; border-radius:8px;",
      
      div(
        style = "display:flex; align-items:center; gap:15px;",
        
        # Logo
        tags$img(
          src = filename,
          height = "60px",
          style = "object-fit:contain;"
        ),
        
        # Team name and record
        div(
          strong(teamdata$Name),
          br(),
          if (input$gruppe != "All Teams"){
            if (teamdata$Rank == 1){
              paste0(teamdata$W, "-", teamdata$L, "-", teamdata$OL, ": ", teamdata$Rank, "st in ", teamdata$Division, " Division")
            } else if (teamdata$Rank == 2){
              paste0(teamdata$W, "-", teamdata$L, "-", teamdata$OL, ": ", teamdata$Rank, "nd in ", teamdata$Division, " Division")
            } else if (teamdata$Rank == 3){
              paste0(teamdata$W, "-", teamdata$L, "-", teamdata$OL, ": ", teamdata$Rank, "rd in ", teamdata$Division, " Division")
            } else {
              paste0(teamdata$W, "-", teamdata$L, "-", teamdata$OL, ": ", teamdata$Rank, "th in ", teamdata$Division, " Division")
            }
          },
          br(),
          if ( input$gruppe != "All Teams"){
            if (teamdata$Playoffs == 1){
              qual <- "Clinched Playoffs"
            } else {
              qual <- "Eliminated from Playoffs"
            }
          }
        )
      ),
      
      # Team statistics
      div(
        style = "margin-top:10px;",
        
        strong("Team Statistics"),
        br(),
        if (input$gruppe != "All Teams"){
          paste("Goals For:", teamdata$GF)
        },
        br(),
        if (input$gruppe != "All Teams"){
          paste("Goals Against:", teamdata$GA)
        },
        br(),
        if (input$gruppe != "All Teams"){
          paste0("Power Play %: ", teamdata$PP., "%")
        },
        br(),
        if (input$gruppe != "All Teams"){
          paste0("Penalty Kill %: ", teamdata$PK., "%")
        }
      )
    )
  })
  
  # "Remove all" button logic
  observeEvent(input$Entfernen, {
    ids(NULL)
  })
  
  # Display all selected players
  output$Auswahl <- renderUI({
    
    sel_ids <- ids()
    req(length(sel_ids) > 0)
    
    # Map IDs to names
    players <- pids[pids$playerId %in% sel_ids, ]
    
    tagList(
      lapply(seq_len(nrow(players)), function(i) {
        
        id <- players$playerId[i]
        name <- players$name[i]
        
        div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
          
          span(name),
          
          # Give a button to remove players from the list
          actionButton(
            inputId = paste0("remove_", id),
            label = "X",
            class = "btn-danger btn-sm"
          )
        )
      })
    )
  })
  
  # Observer for whenever an "X" button is pressed
  observe({
    sel_ids <- ids()
    lapply(sel_ids, function(id) {
      
      observeEvent(input[[paste0("remove_", id)]], {
        
        new_ids <- setdiff(ids(), id)
        
        ids(new_ids)
        
        updateSelectizeInput(
          session,
          "players",
          selected = new_ids
        )
        
      }, ignoreInit = TRUE)
      
    })
  })
  
  # Display line information
  output$Informatik <- renderUI({
    style = "border:1px solid #ccc; padding:10px; margin-bottom:15px; border-radius:8px;"
    
    sel_ids <- na.omit(ids())
    
    if(length(sel_ids) == 0){
      "No players selected"
    } else {
      info <- get_stats(sel_ids)
      if(info$toi == 0){
        "These players have not been on the ice together this season"
      } else {
        tagList(
          paste0("Total TOI Together: ", floor(info$toi / 60), ":", info$toi %% 60),
          br(),
          # paste0("Goals For:", info$for_goal),
          # br(),
          # paste0("Goals Against: ", info$against_goal),
          # br(),
          paste0("Goals Percentage: ", round((info$for_goal / (info$for_goal + info$against_goal)) * 100, 2), "%"),
          br(),
          paste0("Corsi Percentage: ", round(((info$for_shot + info$for_miss + info$for_blck) / (info$for_shot + info$for_miss + info$for_blck + info$against_shot + info$against_miss + info$against_blck)) * 100, 2), "%"),
          br(),
          paste0("Fenwick Percentage: ", round(((info$for_shot + info$for_miss) / (info$for_shot + info$for_miss + info$against_shot + info$against_miss)) * 100, 2), "%"),
          br(),
          paste0("Turnover Percentage: ", round(((info$for_take + info$against_give) / (info$for_take + info$against_take + info$for_give + info$against_give)) * 100, 2), "%"),
          br(),
          paste0("Shooting Percentage For: ", round((info$for_goal / info$for_shot) * 100, 2), "%"),
          br(),
          paste0("Shooting Percentage Against: ", round((info$against_goal / info$against_shot) * 100, 2), "%")
        )
      }
    }
  })
  
  
  ## Player View ####
  # Single player shotmap
  output$P_Tretten <- renderPlot({
    # print(ids())
    req(input$P_staat)
    req(input$P_menschen)
    # print(input$staat)
    # players <- s %>% 
    #   filter(shooterPlayerId %in% ids())
    player <- s[which(s$shooterName == input$P_menschen),]
    
    if (input$P_staat != "all"){
      player <- player[which(player$situation == input$P_staat),]
    }
    # print(unique(players$shooterPlayerId))
    geom_hockey(league="NHL", display_range = "offense", rotation = 270) + 
      geom_point(
        data = player,
        aes(x = yCordAdjusted, y = -1 * xCordAdjusted),
        color = "black",
        alpha = 0.6,
        size = 1.5
      )
  })
  
  # Team and player selection
  observeEvent(input$P_gruppe, {
    req(input$P_gruppe)
    choices <- if (input$P_gruppe == "All Teams"){
      all_teams
    } else {
      sort(unique(s[which(s$shootingTeam == input$P_gruppe),]$shooterName))
    }
    updateSelectInput(
      session,
      inputId = "P_menschen",
      choices = choices,
      selected = NULL
    )
  })
  
  output$P_Kenntnisse <- renderUI({
    req(input$P_menschen)
    pid <- pids[which(pids$name == input$P_menschen),]$playerId
    # print(pid)
    playerdata <- skaters[which(skaters$playerId == pid & skaters$situation == "all"),]
    # print(playerdata)
    div(
      style = "border:1px solid #ccc; padding:10px; margin-bottom:15px; border-radius:8px;",
      if(nrow(playerdata) == 0){
        "No Player Selected"
      } else {
        tagList(
          paste0("Player: ", playerdata$name),
          br(),
          paste0("Position: ", playerdata$pos),
          br(),
          paste0("Games Played: ", playerdata$games_played),
          br(),
          paste0("Goals: ", playerdata$I_F_goals),
          br(),
          paste0("Assists: ", playerdata$I_F_points - playerdata$I_F_goals),
          br(),
          paste0("Points: ", playerdata$I_F_points),
          br(),
          paste0("Penalty Minutes: ", playerdata$I_F_penalityMinutes),
          br(),
          paste0("Hits: ", playerdata$I_F_hits),
          br(),
          paste0("Takeaways: ", playerdata$I_F_takeaways),
          br(),
          paste0("Giveaways: ", playerdata$I_F_giveaways)
        )
      }
    )
  })
  
  # Find a player's most common linemates given their id
  top_teammates <- function(playerId) {
    player_shifts <- player_index[
      player == playerId,
      .(shift_id, team)
    ]
    
    rows <- linestats[
      player_shifts,
      on = .(shift_id, team),
      nomatch = 0
    ]
    
    teammates <- data.table(
      player = unlist(rows$players),
      duration = rep(rows$duration, times = lengths(rows$players))
    )[player != playerId]
    
    totals <- teammates[
      ,
      .(total_time = sum(duration, na.rm = TRUE)),
      by = player
    ]
    
    totals[order(-total_time)][1:3]
  }
  
  # Obtain and display the player's most common linemates
  output$P_Kameraden <- renderUI({
    req(input$P_menschen)
    pid <- pids[which(pids$name == input$P_menschen),]$playerId
    buddies <- top_teammates(pid)
    div(
      style = "solid #ccc; padding:10px; margin-bottom:15px; border-radius:8px;",
      
      paste0(
        "1: ",
        pids[which(pids$playerId == buddies$player[1]),]$name,
        ", TOI Together: ",
        sprintf("%d:%02d",
                buddies$total_time[1] %/% 60,
                buddies$total_time[1] %% 60)
      ),
      br(),
      
      paste0(
        "2: ",
        pids[which(pids$playerId == buddies$player[2]),]$name,
        ", TOI Together: ",
        sprintf("%d:%02d",
                buddies$total_time[2] %/% 60,
                buddies$total_time[2] %% 60)
      ),
      br(),
      
      paste0(
        "3: ",
        pids[which(pids$playerId == buddies$player[3]),]$name,
        ", TOI Together: ",
        sprintf("%d:%02d",
                buddies$total_time[3] %/% 60,
                buddies$total_time[3] %% 60)
      )
    )
  })
  
  # Run knn to find three most similar players
  output$P_Zwillinge <- renderUI({
    req(input$P_menschen)
    pid <- pids[which(pids$name == input$P_menschen),]$playerId
    s.red <- na.omit(skaters[which(skaters$situation == "all"), c("playerId", "G_60", "PA_60", "SA_60", "ShotAt_60", "PIM_60", "H_60", "Give_60", "Take_60", "Reb_60", "Blck_60")])
    s.red[,-1] <- scale(s.red[,-1])
    guy <- s.red[which(s.red$playerId == pid), 2:11, drop = FALSE]
    s.knn <- s.red[which(s.red$playerId != pid),2:11]
    knn_result <- get.knnx(data = s.knn, query = guy, k=3)
    p1 <- pids[which(pids$playerId == s.red[knn_result$nn.index[1,1],]$playerId),]$name
    p2 <- pids[which(pids$playerId == s.red[knn_result$nn.index[1,2],]$playerId),]$name
    p3 <- pids[which(pids$playerId == s.red[knn_result$nn.index[1,3],]$playerId),]$name
    div(
      style = "solid #ccc; padding:10px; margin-bottom:15px; border-radius:8px;",
      paste0("1: ", p1),
      br(),
      paste("2:", p2),
      br(),
      paste("3:", p3)
    )
  })
}
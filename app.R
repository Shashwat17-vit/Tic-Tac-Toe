library(shiny)

# Helper Functions (same as before)
won = function(board, player, debug = FALSE) {
  stopifnot(player %in% c("X", "O"))
  stopifnot(board %in% c(" ", "X", "O"))
  n = dim(board)[1]
  stopifnot(n > 0, n == dim(board)[2])
  
  if (debug) {
    cat(sep = "", "player = ", player, ", board = ", "\n")
    print(board)
  }
  
  for (i in 1:n) {
    if (all(board[ , i] == player) | all(board[i,  ] == player)) {
      return(TRUE)
    }
  }
  
  return(all(diag(board) == player) | all(diag(board[n:1, ]) == player))
}

safe.sample <- function(x, ...) x[sample.int(length(x), ...)]

# UI
ui <- fluidPage(
  titlePanel("R n×n Tic-Tac-Toe"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("board_size", "Board Size (n×n):", value = 3, min = 3, max = 6),
      actionButton("reset", "New Game", class = "btn-primary"),
      hr(),
      h4(textOutput("status")),
      textOutput("turn_info")
    ),
    
    mainPanel(
      plotOutput("board", click = "plot_click", width = "500px", height = "500px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store game state
  game <- reactiveValues(
    board = matrix(" ", 3, 3),
    player = "X",
    winner = " ",
    game_over = FALSE,
    n = 3
  )
  
  # Initialize/reset game
  observeEvent(c(input$reset, input$board_size), {
    n <- input$board_size
    game$n <- n
    game$board <- matrix(" ", n, n)
    game$player <- "X"
    game$winner <- " "
    game$game_over <- FALSE
  })
  
  # Handle plot clicks
  observeEvent(input$plot_click, {
    req(!game$game_over)
    req(game$player == "X")  # Only allow human moves on click
    
    n <- game$n
    col <- round(input$plot_click$x)
    row <- round(input$plot_click$y)
    
    # Validate click is within bounds and square is empty
    if (col >= 1 && col <= n && row >= 1 && row <= n) {
      if (game$board[row, col] == " ") {
        # Human move
        game$board[row, col] <- "X"
        
        # Check for win
        if (won(game$board, "X")) {
          game$winner <- "X"
          game$game_over <- TRUE
        } else if (!any(game$board == " ")) {
          game$game_over <- TRUE  # Draw
        } else {
          # Computer move
          game$player <- "O"
          
          Sys.sleep(0.3)  # Brief pause for better UX
          
          empty_squares <- which(c(game$board) == " ")
          if (length(empty_squares) > 0) {
            index <- safe.sample(empty_squares, 1)
            
            # Convert index to row/col
            computer_col <- ((index - 1) %/% n) + 1
            computer_row <- ((index - 1) %% n) + 1
            
            game$board[computer_row, computer_col] <- "O"
            
            # Check for computer win
            if (won(game$board, "O")) {
              game$winner <- "O"
              game$game_over <- TRUE
            } else if (!any(game$board == " ")) {
              game$game_over <- TRUE
            }
          }
          
          game$player <- "X"
        }
      }
    }
  })
  
  # Render the board
  output$board <- renderPlot({
    n <- game$n
    x <- rep(1:n, each = n)
    y <- rep(1:n, times = n)
    
    par(bg = "#F5F5DC", mar = c(1, 1, 3, 1))
    
    plot(x, y, type = "n", xlim = c(0.5, n + 0.5), ylim = c(n + 0.5, 0.5),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         main = paste0("R ", n, "×", n, " Tic-Tac-Toe"), 
         cex.main = 2, col.main = "#2E4057", font.main = 2)
    
    # Draw grid
    for (i in 1:n) {
      for (j in 1:n) {
        rect(i - 0.45, j - 0.45, i + 0.45, j + 0.45, 
             col = "#FFFFFF", border = "#2E4057", lwd = 2)
      }
    }
    
    # Draw X's and O's
    for (i in 1:n) {
      for (j in 1:n) {
        if (game$board[j, i] == "X") {
          text(i, j, "X", cex = 4, col = "#1976D2", font = 2)
        } else if (game$board[j, i] == "O") {
          text(i, j, "O", cex = 4, col = "#C62828", font = 2)
        }
      }
    }
    
    # Show winner
    if (game$game_over) {
      announcement <- ifelse(game$winner == " ", "Draw!", paste(game$winner, "won!"))
      rect(0.5, 0.2, n + 0.5, 0.5, col = "#FFD700", border = "#FF8C00", lwd = 3)
      text((n + 1) / 2, 0.35, announcement, cex = 1.8, col = "#1B5E20", font = 2)
    }
  })
  
  # Status text
  output$status <- renderText({
    if (game$game_over) {
      if (game$winner == " ") {
        "Game Over - Draw!"
      } else {
        paste0(game$winner, " Wins!")
      }
    } else {
      "Game in Progress"
    }
  })
  
  output$turn_info <- renderText({
    if (!game$game_over) {
      if (game$player == "X") {
        "Your turn - Click to play!"
      } else {
        "Computer is thinking..."
      }
    } else {
      "Click 'New Game' to play again"
    }
  })
}

shinyApp(ui, server)

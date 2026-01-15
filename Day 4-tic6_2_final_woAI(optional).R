# Final version without AI

rm(list = ls())



won = function(board, player, debug = FALSE) {
  stopifnot(player %in% c("X", "O"))
  
  stopifnot(board %in% c(" ", "X", "O"))
  stopifnot(dim(board)[1] == dim(board)[2], dim(board)[1] == 3)
  
  if (debug) {
    cat(sep = "", "player = ", player, ", board = ", "\n")
    print(board)
  }
  
  return(
      all(        board[ , 1] == player) |
      all(        board[ , 2] == player) |
      all(        board[ , 3] == player) |
      all(        board[1,  ] == player) |
      all(        board[2,  ] == player) |
      all(        board[3,  ] == player) |
      all(        diag(board) == player) |
      all(diag(board[3:1,  ]) == player)
  )
}

safe.sample = function(x, ...) x[sample.int(length(x), ...)] # safer sampling, 
                                                             # safe.sample(3, 1) is always 3
                                                             # sample(3, 1) gives a random one out of 1, 2, and 3!



x = rep(1:3, each = 3)
y = rep(1:3, times = 3)

if (.Platform$OS.type == "windows") windows()

plot(x, y, type = "n", xlim = c(0, 4), ylim = c(4, 0), 
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
segments(x0 = c(0.5, 0.5, 1.5, 2.5), 
         y0 = c(1.5, 2.5, 0.5, 0.5),
         x1 = c(3.5, 3.5, 1.5, 2.5), 
         y1 = c(1.5, 2.5, 3.5, 3.5))

board = matrix(data = rep(" ", times = 9), nrow = 3, ncol = 3)

player = "X"
winner = " "

for (i in 1:9) {
  if (player == "X") {
    repeat {
      index = identify(x, y, n = 1, plot = FALSE, tolerance = 0.8)
      # or, input the index by hand below:
      # index = as.numeric(readline(prompt="Enter the square number where your stone is put: "))
      
      col = x[index]
      row = y[index]
      
      if (board[row, col] == " ") {
        rect(1, 3.55, 3, 4, col = par("bg"), border = par("bg")) # erase the existing warning
        break
      } else {
        text(x = 2, y = 3.7, labels = "Please click on empty squares.", col = "red")
      }
    }
    
  } else {
    index = safe.sample(x = which(c(board) == " "), size = 1)
    
    col = x[index]
    row = y[index]
  }
  
  board[row, col] = player
  text(x = col, y = row, labels = player)
  
  if (won(board, player, debug = FALSE)) {
    winner = player
    break
  }
  
  player = ifelse(test = (player == "X"), yes = "O", no = "X")
}

announcement = ifelse(winner == " ", "Draw!", paste(winner, " won!"))
text(x = 2, y = 1/3, labels = announcement, col = "red")

invisible(readline(prompt="Press [enter] to proceed")) # pause until you press enter
if (.Platform$OS.type == "windows") dev.off() # close the Windows graphics device
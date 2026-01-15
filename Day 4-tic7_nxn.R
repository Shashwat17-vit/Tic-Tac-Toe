# 1. Draw a tic-tac-toe board.
# 2. Add the playing loop and switch the players.
# 3. Add click-to-play.
# 4. Add check for a win.
# 5. Require play on empty square. (And change empty representation
#    from "E" to " " because it makes board easier to read.)
# 6. Add computer player without AI, just choosing an remaining empty square randomly
# 7. Expandability:
# 7-a. Generalize program to play n-by-n, not 3-by-3, tic-tac-toe.
# 7-b. Add computer player with a little AI or invinsible AI, ... challenging, coming soon ...
# 7-c. Minor improvement, ... check the comments below ...
# 7-d. Other chess game, ... coming soon ...

rm(list = ls())

won = function(board, player, debug = FALSE) {
  stopifnot(player %in% c("X", "O")) # test the validity of the parameter: player
  
  stopifnot(board %in% c(" ", "X", "O")) # test the validity of the parameter: board
  n = dim(board)[1] # get the dimension of the board
  stopifnot(n > 0, n == dim(board)[2]) # test if the board is square
  
  if (debug) {
    cat(sep = "", "player = ", player, ", board = ", "\n")
    print(board)
  }
  
  for (i in 1:n) { # for() to test the winning in nxn board
    if (all(board[ , i] == player) |      # check n columns
        all(board[i,  ] == player)) {     # check n rows
      return(TRUE)
    }
  }
  
  return(all(       diag(board) == player) | # check in diagonal
         all(diag(board[n:1, ]) == player))  # check in reverse diagonal
}

test.board = matrix(data = c("X", "O", " ", 
                             "O", "X", "O", 
                             "X", "O", "X"), nrow = 3, ncol = 3, byrow = TRUE)
print(test.board)
stopifnot( won(test.board, "X"))
stopifnot(!won(test.board, "O"))

test.board[2, 2] = "O"
print(test.board)
stopifnot(!won(test.board, "X", TRUE))
stopifnot( won(test.board, "O", TRUE))



safe.sample <- function(x, ...) x[sample.int(length(x), ...)] # safer sampling




n = 4 # 4x4 board
x = rep(1:n, each = n) # 3 -> n
y = rep(1:n, times = n) # 3 -> n

if (.Platform$OS.type == "windows") windows()

plot(x, y, type = "n", xlim = c(0, n+1), ylim = c(n+1, 0), # 3 -> n
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
segments(x0 = c(rep(0.5, n-1),      seq(1.5, n-0.5, 1)), # plotting squares for nXn board
         y0 = c(seq(1.5, n-0.5, 1), rep(0.5, n-1)),
         x1 = c(rep(n+0.5, n-1),    seq(1.5, n-0.5, 1)),
         y1 = c(seq(1.5, n-0.5, 1), rep(n+0.5, n-1)))

board = matrix(data = rep(" ", times = n*n), nrow = n, ncol = n) # empty nxn board

player = "X"
winner = " "

for (i in 1:(n*n)) { # total move <= nXn
  if (player == "X") {
    repeat {
      index = identify(x, y, n = 1, plot = FALSE, tolerance = 0.8)
      # or, input the index by hand below:
      # remember the indices are numbered column by column
      # index = as.numeric(readline(prompt="Enter the square number where your stone is put: "))
      
      col = x[index]
      row = y[index]
      
      if (board[row, col] == " ") {
        rect(n/2 - n/2, n + 0.6, n/2 + n/2, n + 1.1, col = par("bg"), border = par("bg")) # erase the existing warning
        break
      } else {
        text(x = n/2 + 0.5, y = n + 0.8, labels = "Please click on empty squares.", col = "red")
      }
    }
  } else {
    index = safe.sample(x = which(c(board) == " "), size = 1) # When n is even, using sample() directly might cause some bugs.
    
    col = x[index]
    row = y[index]
  }
  
  board[row, col] = player
  text(x = col, y = row, labels = player)
  
  cat(sep = "", "i=", i, ", player=", player, ", index=", index,
      ", row=", row, ", col=", col, ", board:", "\n")
  print(board)
  
  if (won(board, player, debug = FALSE)) {
    winner = player
    break
  }
  
  player = ifelse(test = (player == "X"), yes = "O", no = "X")
}

announcement = ifelse(winner == " ", "Draw!", paste(winner, " won!"))
text(x = n/2 + 0.5, y = 0.1, labels = announcement, col = "red")

invisible(readline(prompt="Press [enter] to proceed")) # pause until you press enter
if (.Platform$OS.type == "windows") dev.off() # close the Windows graphics device
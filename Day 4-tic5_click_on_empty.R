# 1. Draw a tic-tac-toe board.
# 2. Add the playing loop and switch the players.
# 3. Add click-to-play.
# 4. Add check for a win.
# 5. Require play on empty square. (And change empty representation
#    from "E" to " " because it makes board easier to read.)

rm(list = ls()) # initialization

won = function(board, player, debug = FALSE) {
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



# main program
x = rep(1:3, each = 3)
y = rep(1:3, times = 3)

if (.Platform$OS.type == "windows") windows(restoreConsole = T)

plot(x, y, type = "n", xlab = "", xlim = c(0, 4), ylim = c(4, 0), xaxt = "n"); 
axis(3); mtext(text = "x", side = 3, line = 2.5)
segments(x0 = c(0.5, 0.5, 1.5, 2.5), 
         y0 = c(1.5, 2.5, 0.5, 0.5),
         x1 = c(3.5, 3.5, 1.5, 2.5), 
         y1 = c(1.5, 2.5, 3.5, 3.5))

board = matrix(data = rep(" ", times = 9), nrow = 3, ncol = 3) # empty grid: "E" => " "

player = "X"
for (i in 1:9) {
  repeat { # must obey the rule: play on an empty square
    
    index = identify(x, y, n = 1, plot = FALSE, tolerance = 0.8)
    # or, input the index by hand below:
    # index = as.numeric(readline(prompt="Enter the square number where your stone is put: "))
    
    col = x[index]
    row = y[index]
    
    if (board[row, col] == " ") { # play on an empty square?
      break # OK, escape from the innermost loop, i.e., repeat loop here
    } else {
      text(x = 2, y = 3.7, labels = "Please click on empty squares.", col = "red") # show a warning
                                                                                   # cannot remove it easily!
    }
  }
  
  board[row, col] = player
  text(x = col, y = row, labels = player)
  
  cat(sep = "", "i = ", i, ", player = ", player, ", index = ", index,
      ", row = ", row, ", col = ", col, ", board:", "\n")
  print(board)
  
  if (won(board, player, debug = TRUE)) {
    text(x = 2, y = 1/3, labels = paste(player, " won!"), col = "red") # no draw announcement?
    break
  }
  
  player = ifelse(test = (player == "X"), yes = "O", no = "X")
}

invisible(readline(prompt="Press [enter] to proceed")) # pause until you press enter
if (.Platform$OS.type == "windows") dev.off() # close the Windows graphics device
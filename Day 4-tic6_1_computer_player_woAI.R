# 1. Draw a tic-tac-toe board.
# 2. Add the playing loop and switch the players.
# 3. Add click-to-play.
# 4. Add check for a win.
# 5. Require play on empty square. (And change empty representation
#    from "E" to " " because it makes board easier to read.)
# 6. Add computer player without AI, just choosing a remaining empty square randomly

rm(list = ls())

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

if (.Platform$OS.type == "windows") windows()

plot(x, y, type = "n", xlim = c(0, 4), ylim = c(4, 0), 
     xaxt = "n", yaxt = "n", xlab = "", ylab = "") # hide the axes and their labels (or by ann = FALSE)
segments(x0 = c(0.5, 0.5, 1.5, 2.5), 
         y0 = c(1.5, 2.5, 0.5, 0.5),
         x1 = c(3.5, 3.5, 1.5, 2.5), 
         y1 = c(1.5, 2.5, 3.5, 3.5))

board = matrix(data = rep(" ", times = 9), nrow = 3, ncol = 3)

player = "X"
winner = " "

for (i in 1:9) {
  if (player == "X") { # human player
    repeat {
      index = identify(x, y, n = 1, plot = FALSE, tolerance = 0.8)
      # or, input the index by hand below:
      # index = as.numeric(readline(prompt="Enter the square number where your stone is put: "))
      
      col = x[index]
      row = y[index]
      
      if (board[row, col] == " ") {
        break
      } else {
        text(x = 2, y = 3.7, labels = "Please click on empty squares.", col = "red")
      }
    }
    
  } else { # computer player, playing at random without intelligence
    safe.sample = function(x, ...) x[sample.int(length(x), ...)] # safe sampling
    # When x has length 1, sample(x, 1) has a different meaning. Check ?sample.
    # safe.sample(3, 1) # should always be 3
    # sample(3, 1) # random one of 1, 2, and 3
    
    index = safe.sample(x = which(c(board) == " "), size = 1) # choose an remaining empty square randomly
                                                              # by using sample() and which()
                                                              # cannot use sample() directly!
                                                              # check ?sample for details.
    
    col = x[index]
    row = y[index]
  }
  
  board[row, col] = player
  text(x = col, y = row, labels = player)
  
  # cat(sep = "", "i = ", i, ", player = ", player, ", index = ", index,
  #     ", row = ", row, ", col = ", col, ", board:", "\n")
  # print(board)
  
  if (won(board, player, debug = TRUE)) {
    winner = player
    break
  }
  
  player = ifelse(test = (player == "X"), yes = "O", no = "X")
}

announcement = ifelse(winner == " ", "Draw!", paste(winner, " won!")) # announce draw if so
text(x = 2, y = 1/3, labels = announcement, col = "blue")

invisible(readline(prompt="Press [enter] to proceed")) # pause until you press enter
if (.Platform$OS.type == "windows") dev.off() # close the Windows graphics device
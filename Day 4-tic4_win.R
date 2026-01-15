# 1. Draw a tic-tac-toe board.
# 2. Add the playing loop and switch the players.
# 3. Add click-to-play.
# 4. Add check for a win.

rm(list=ls()) #initialization

won = function(board, player, debug = FALSE) { # The current player wins?
                                               #  input:   board - the current board 
                                               #          player - the current player
                                               #           debug - debugging/normal mode 
                                               #                 - with/without printing the board
                                               # output:    TRUE - The current player wins and the game is over.
                                               #           FALSE - No winner and keep going.
  if (debug) { #d ebugging mode
    cat(sep = "", "player = ", player, ", board = ", "\n") # print all the parameters as input
    print(board)
  }
  
  return(
         all(        board[ , 1] == player) | # check columns 1-3
         all(        board[ , 2] == player) |
         all(        board[ , 3] == player) |
         all(        board[1,  ] == player) | # check rows 1-3
         all(        board[2,  ] == player) |
         all(        board[3,  ] == player) |
         all(        diag(board) == player) | # check diagonals
         all(diag(board[3:1,  ]) == player)   # reverse diagonal = diagonal of the reversed matrix
         )
}

test.board = matrix(data = c("X", "O", "E", 
                             "O", "X", "O", 
                             "X", "O", "X"), nrow = 3, ncol = 3, byrow = TRUE) # test win() for the debugging
print(test.board)
stopifnot( won(test.board, "X")) # test the above function with test.board and current player being "X"
stopifnot(!won(test.board, "O")) # test the above function with test.board and current player being "O"

test.board[2, 2] = "O" # test win () with another board for the debugging
print(test.board)
stopifnot(!won(test.board, "X"))
stopifnot( won(test.board, "O"))

# You can try more tests for sure in the same way.



# main program
x = rep(1:3, each = 3)
y = rep(1:3, times = 3)

if (.Platform$OS.type == "windows") windows()

plot(x, y, type = "n", xlab = "", xlim = c(0, 4), ylim = c(4, 0), xaxt = "n"); 
axis(3); mtext(text = "x", side = 3, line = 2.5)
segments(x0 = c(0.5, 0.5, 1.5, 2.5), 
         y0 = c(1.5, 2.5, 0.5, 0.5),
         x1 = c(3.5, 3.5, 1.5, 2.5), 
         y1 = c(1.5, 2.5, 3.5, 3.5))

board = matrix(data = rep("E", times = 9), nrow = 3, ncol = 3)

player = "X"
for (i in 1:9) {
  index = identify(x, y, n = 1, plot = FALSE, tolerance = 0.8) # plot = FALSE: hide the index
                                                               # enlarge the tolerance to remove annoying warnings

  col = x[index]
  row = y[index]
  
  board[row, col] = player
  text(x = col, y = row, labels = player)
  
  cat(sep = "", "i = ", i, ", player = ", player, ", index = ", index,
      ", row = ", row, ", col = ", col, ", board:", "\n")
  print(board)
  
  if (won(board, player, debug = TRUE)) { # already win?
    text(x = 2, y = 1/3, labels = paste(player, " won!"), col = "red") # announce the winner on the top
    break #quit
  }
  
  player = ifelse(test = (player == "X"), yes = "O", no = "X")
}

invisible(readline(prompt="Press [enter] to proceed")) # pause until you press enter
if (.Platform$OS.type == "windows") dev.off() # close the Windows graphics device
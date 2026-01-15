# 1. Draw a tic-tac-toe board.
# 2. Add the playing loop and switch the players.
# 3. Add click-to-play.

rm(list = ls()) # initialization

x = rep(1:3, each = 3)
y = rep(1:3, times = 3)

# Windows only. Open a Windows graphics device for the future plots.
# Otherwise, the function identify() cannot catch the cursor's position correctly 
# in the current version of RStudio.
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
  # Read the position of the cursor at click into index by identify().
  # index is the index of the closest base points indicated by x and y
  # For example, if you click in the middle of the central square of the chessboard,
  # the closest base point is the hidden central point (we already hid it by using type = "n") in the central square.
  # Its x- and y-coordinates are (2, 2), indicated by x and y, and the corresponding index is 5.
  
  # If you get the warning: no point within 0.25 inches,
  # move the cursor closer to the base points (the hidden central point of each square) and redo it.
  # Use the parameter tolerance to catch the position far from (x, y).
  # Press Esc to escape.
  
  # Showing the index on the board is not necessary. Check ?identify to hide it.  
  index = identify(x, y, n = 1, tolerance = 1)
  
  col = x[index] # convert index into col/row.
  row = y[index] 
  
  board[row, col] = player # Record the current move on the board in the matrix.

  text(x = col, y = row, labels = player) # plot the current move on the board

  cat(sep = "", "i = ", i, ", player = ", player, ", index = ", index,
      ", row = ", row, ", col = ", col, ", board:", "\n") # more print for debugging
                                                      # including the position of the move
  print(board)                                        # and the board

  player = ifelse(test = (player == "X"), yes = "O", no = "X") # flipping: "X" <==> "O"
}

invisible(readline(prompt="Press [enter] to proceed")) # pause until you press enter
if (.Platform$OS.type == "windows") dev.off() # close the Windows graphics device
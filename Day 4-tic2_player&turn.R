# 1. Draw a tic-tac-toe board.
# 2. Add the playing loop and switch the players.

rm(list = ls()) # initialization

x = rep(1:3, each = 3)
y = rep(1:3, times = 3)

plot(x, y, type = "n",                           # type = "n": hide the unnecessary circles of each point
     xlab = "",                                  # remove the default x labels
     xlim = c(0, 4), ylim = c(4, 0), xaxt = "n") # xaxt = "n": hide the default x axis at the bottom, 1st axis
axis(3)                                          # add another x axis at the top, 3rd axis 
mtext(text = "x", side = 3, line = 2.5)          # draw x label on the top


segments(x0 = c(0.5, 0.5, 1.5, 2.5), 
         y0 = c(1.5, 2.5, 0.5, 0.5),
         x1 = c(3.5, 3.5, 1.5, 2.5), 
         y1 = c(1.5, 2.5, 3.5, 3.5))

board = matrix(data = rep("E", times = 9), nrow = 3, ncol = 3) # make an empty matrix correpsonding to the original board
                                                               # "E": empty
                                                               # "X": player 1's move
                                                               # "O": player 2's move
board # printout for debugging, should be a matrix full of "E"

player = "X" # Player "X" plays first
for (i in 1:9) { # 9 moves at most
  cat(sep = "", "i = ", i, ", player = ", player, "\n") # print the current player for the debugging

  player = ifelse(test = (player == "X"), yes = "O", no = "X") # switch the players: "X" <==> "O"
}
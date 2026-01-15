# 1. Draw a tic-tac-toe board.

rm(list = ls()) # initialization

x = rep(1:3, each = 3)  # x-coordinates;    each: each element repeated 3 times
y = rep(1:3, times = 3) # y-coordinates;   times: whole vector repeated 3 times
                        # check the values of x, y on Environment panel, or print them out

plot(x, y, xlim = c(0, 4), ylim = c(4, 0)) # ylim: 4, 3, 2, 1, 0 in a reverse order.
                                           # The circles in the middle of the squares are 
                                           # the base points of each square,
                                           # good for the reference of identify() in the future code,
                                           # and should be hidden eventually.
                                           # Check ?plot or google the solution.
                                           # x axis should be drawn at the top eventually.
segments(x0 = c(0.5, 0.5, 1.5, 2.5), # drawing two horizontal lines from the first two columns
         y0 = c(1.5, 2.5, 0.5, 0.5),
         x1 = c(3.5, 3.5, 1.5, 2.5), # drawing two vertical lines from the last two columns
         y1 = c(1.5, 2.5, 3.5, 3.5))  
# you can try lines() to draw the same chessboard
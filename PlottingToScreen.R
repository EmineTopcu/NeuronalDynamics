library(scatterplot3d)
library(ggplot2)

# Plot3DToFile: Creates a 3D scatterplot using the scatterplot3D library
#   and saves it to a file
Plot3D <- function(x, y, z, filename, xlabel = "X", ylabel = "Y", zlabel = "Z", w = 1000, h = 500, plottitle = "")
{
  scatterplot3d(x, y, z, type = "l", box = FALSE, highlight.3d = TRUE, main = plottitle, xlab = xlabel, ylab = "", zlab = zlabel, cex.lab = 2, cex.axis = 2)
  
  # scatterplot3D does not plot the y axis label at the proper place. 
  # The following link is used as a guide for proper labeling
  # https://stackoverflow.com/questions/20637484/change-ylab-position-in-r-scatterplot3d
  
  # take the coordinates of the plot 
  #   (leftBottomX, RightBottomX, LeftBottomY, RightTopY)
  dims <- par("usr") 
  
  # the x and y coordinates for proper label position is calculated
  # dims[2] - dims[1] is the width of the plot. 
  xpos <- dims[1] + (dims[2] - dims[1]) * 0.9 
  # 0.9 multiplier is found by trial and error for the best aesthetics
  
  # dims[4] - dims[3] is the height of the plot. 
  ypos <- dims[3] + (dims[4] - dims[3]) * 0.1 
  # 0.1 multiplier is found by trial and error for the best aesthetics
  
  # angle of the y axis label is calculated
  angle <- atan(0.225 * h/w) * 180 
  
  # place the y axis label on the plot
  text(xpos, ypos, ylabel, srt = angle, cex = 2)
}

# Plot2DToFile: Creates a 2D plot using the ggplot library
#   and saves it to a file
Plot2D <- function(x, y, plottitle, xlabel = "X", ylabel = "Y", w = 2000, h = 1000)
{
    df<- data.frame(x, y)
    # Create the plot
    g <- ggplot(df, aes(x, y)) +
        xlab(xlabel) + ylab(ylabel) + 
        labs(title = plottitle) + 
        geom_point(shape = ".") + theme_classic() +
        # label fonts are set so that they are legible when resized
        theme(text = element_text(size = 20, family = "mono")) 
    print(g)
}


library(scatterplot3d)
library(ggplot2)

# Plot3DToFile: Creates a 3D scatterplot using the scatterplot3D library
#   and saves it to a file
Plot3DToFile <- function(x, y, z, filename, xlabel = "X", ylabel = "Y", zlabel = "Z", w = 1000, h = 500, plottitle = "")
{
  jpeg(paste(filename, ".jpg"), width = w, height = h)
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
  dev.off()
}

# Plot2DToFile: Creates a 2D plot using the ggplot library
#   and saves it to a file
Plot2DToFile <- function(x, y, filename, xlabel = "X", ylabel = "Y", w = 2000, h = 1000, plottitle = "")
{
  df<- data.frame(x, y)
  # Create the plot
  g <- ggplot(df, aes(x, y)) +
    xlab(xlabel) + ylab(ylabel) + 
    geom_point(shape = ".") + theme_classic() +
    # label fonts are set so that they are legible when resized
    theme(text = element_text(size = 20, family = "mono")) 

  # Save the plot to a file
  ggsave(filename = paste(filename, ".jpg"), plot = g, units = "px", width = w, height = h)
}

# Plots log(intervals) between spikes versus Vu
#   and saves it to a file
# df: the data frame with list of spike intervals for each Vu value
PlotIntervals_Vu <- function(df, filename, logbase = 10)
{
  # Create the plot
  g <- ggplot(df, aes(Vu, log(Intervals, logbase)))
  if (logbase == 2)
    g <- g + labs(x = bquote(V[u]), y = bquote(Intervals (log[2]))) 
  else
    g <- g + labs(x = bquote(V[u]), y = bquote(Intervals (log[10])))
  g <- g + geom_point(shape = ".") + theme_classic() +
    # label fonts are set so that they are legible when resized
    theme(text = element_text(size = 20, family = "mono")) +
    # reverse the x-axis to be consistent with the original article
    scale_x_reverse() 

  # Save the plot to a file
  ggsave(filename, width = 2000, height = 1000, units = "px")
}

# Plots log(intervals) between spikes versus gaut
#   and saves it to a file
# df: the data frame with the list of spike intervals for each gaut value
PlotIntervals_gaut <- function(df, filename, logbase = 10)
{
  # Create the plot
  g <- ggplot(df, aes(gaut, log(Intervals, logbase)))
  if (logbase == 2)
    g <- g + labs(x = bquote(g[aut]), y = bquote(Intervals (log[2]))) 
  else
    g <- g + labs(x = bquote(g[aut]), y = bquote(Intervals (log[10])))
  g <- g + geom_point(shape = ".") + theme_classic() +
    # label fonts are set so that they are legible when resized
    theme(text = element_text(size = 20, family = "mono"))
  
  # Save the plot to a file
  ggsave(filename, width = 2000, height = 1000, units = "px")
}

# Plots the maximum (Imax) and mean (Imean) currents across time
#   and saves it to a file
PlotCurrent <- function(df, filename)
{
  # Create the plot
  g<- ggplot(df) +
  #the left y-axis shows Imax, the right y-axis shows Imean
    geom_line(aes(x = gaut, y = maxI), color = "red") +
    geom_line(aes(x = gaut, y = 5*meanI), color = "black") +
    theme_classic() + 
    labs(x = bquote(g[aut])) +
    scale_y_continuous(name = bquote(I[max]), sec.axis = sec_axis(~./ 5, name = bquote(I[mean]))) +
    theme(axis.title.y.left = element_text(color = "red"), axis.text.y.left = element_text(color = "red")) +
    # label fonts are set so that they are legible when resized
    theme(text = element_text(size = 20, family = "mono"))
  
  # Save the plot to a file
  ggsave(filename, width = 2000, height = 1000, units = "px")
}

# Plots the spike frequency versus gaut
#   and saves it to a file
PlotFrequencies <- function(df, filename)
{
  # find the total number of intervals
  totalcount <- length(df$Intervals)
  dfFreq <- data.frame(row.names = c("gaut", "Frequency"))
  for (g in unique((df$gaut)))
  {
    # for each gaut value, the frequency is calculated as 
    #   the number of intervals for that gaut value
    #   divided by the total number of intervals
    newrow <- data.frame("gaut" = g, "Frequency" = nrow(df[df$gaut == g,]) / totalcount)
    dfFreq <- rbind(dfFreq, newrow)
  }
  # Create the plot
  g <- ggplot(dfFreq, aes(gaut, Frequency)) + 
    geom_point(shape = ".") + theme_classic() +
    labs(x = bquote(g[aut])) +
    # label fonts are set so that they are legible when resized
    theme(text = element_text(size = 20, family = "mono"))
  
  # Save the plot to a file
  ggsave(filename, width = 2000, height = 1000, units = "px")
}

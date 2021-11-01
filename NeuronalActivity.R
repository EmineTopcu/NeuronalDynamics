library(deSolve)

source("ConstMorrisLecar.R") #This line needs to be commented out for biological run
#source("ConstBiologic.R") #This line needs to be included for biological run
source("MorrisLecar.R")
source("Plotting.R")

# Vlist: the list of membrane potentials (V) for each time point 
# Returns: the list of time points when a spike occurs
# A membrane potential that is higher than the preceding and succeeding 
#   time points is considered a spike
# To eliminate noise and fluctuations, the spike is considered to reach a voltage
#   within the range of 0.01 mV smaller than the max voltage
findSpikes <- function (Vlist)
{
  maxVal <- max(Vlist) # get the maximum value of the membrane potential
  spikeList <- c()  # the data structure to keep the list of spikes
  
  # iterate through the list of membrane potentials
  for (i in which(V > maxVal - 0.01)) 
  {
    # a spike needs to be a peak: 
    #   have a greater value than the previous and next time point
    if ((i == 1 || (Vlist[i-1] < Vlist[i]))
          && (i == length(V) || Vlist[i] > Vlist[i+1]))
      spikeList <- c(spikeList, i)
  }
  return (spikeList)
}

# spikeList: the list of time points where a spike occurs
# timeList: the list of time points
# Returns: the list of intervals between spikes
findIntervals <- function(spikeList, timeList)
{
  intervals <-c() # the data structure to keep the list of intervals
  numofspikes <- length(spikeList)
  for (i in 1: numofspikes - 1)
  {
    # calculate the interval between two successive spikes
    interval <- timeList[spikeList[i + 1]] - timeList[spikeList[i]]
    intervals <- c(intervals, interval)
  }
  return (intervals)
}

# The main function that iterates through a list of possible Vu and gaut values
# Solve the differential equations of the Morris Lecar model 
#   for the given initial state. 
# Vulist: list of Vu values
# gautlist: list of gaut values
# state_init: initial V, u, and w values
# save_ODE: if TRUE, the solution of the model will be saved to a CSV file
#   the name of the file is automatically generated using the Vu and gaut values
# save_PLOT: if TRUE, the plots will be created and saved as jpg files
#   the name of the file is automatically generated using the Vu and gaut values
# fileIntervals: if given, the list of spike intervals are saved to a CSV file
# fileCurrent: if given, the list of current values are saved to a CSV file
SolveAndPlot<- function(Vulist, gautlist, state_init, saveODE = FALSE, savePlot = FALSE, 
                        fileIntervals = "", fileCurrent = "")
{
  dfInterval <- data.frame(row.names = c("Vu", "gaut", "Intervals"))
  dfCurrent <- data.frame(row.names = c("Vu", "gaut", "meanI", "maxI"))
  
  for (Vuiter in Vulist)
  {
    for (gautiter in gautlist)
    {
      desc <- sprintf("Vu = %.4f, gaut = %.3f", Vuiter, gautiter)
      pars <- c (Vu = Vuiter, gaut = gautiter)
      print(desc) # to be able to visualize the progress for long tasks
      
      plotwidth <- 1000
      
      inc <- 0.001
      tstart <- 2100 # the start time of the plots
      if (Vuiter >= 0.1415 & Vuiter < 0.145)
      {
        plotwidth <- 1500 #Increased to 10000 for chaos plots
        tstart <- 0
      }
      tend <- 3000 # the end time of the simulation - set to 5000 for chaos plots
      t <- seq(0, tend, inc)
      out <- ode (y = state_init, times = t, func = Morris_Lecar, parms = pars, method = rkMethod("rk4"))
      if (saveODE)
      {
        filename <- sprintf("ODE_Vu_%.4f_gaut_%.3f.csv", Vuiter, gautiter)
        write.csv(out, file = filename)
      }
      
      rngstart <- as.integer(tstart/inc)
      rngend <- as.integer(tend/inc)
      t_full = out[, 1]
      V_full = out[, 2]
      w_full = out[, 3]
      u_full = out[, 4]
      
      t <- t_full[rngstart:rngend]
      V <- V_full[rngstart:rngend]
      w <- w_full[rngstart:rngend]
      u <- u_full[rngstart:rngend]
      
      Iaut <- -gautiter * (as.matrix(V) - Vsyn)/(1+exp(-lambda*(V - thetas)))
      newrow <- data.frame("Vu" = Vuiter, "gaut" = gautiter, "meanI" = mean(Iaut), "maxI" = max(Iaut))
      dfCurrent <- rbind(dfCurrent, newrow)
      
      Intervals <- findIntervals(findSpikes(V), t)
      for (i in Intervals)
      {
        newrow <- data.frame("Vu" = Vuiter, "gaut" = gautiter, "Intervals" = i)
        dfInterval <- rbind(dfInterval, newrow)
      }
      
      if (savePlot)
      {
        plotdesc <- paste("Morris Lecar 3D Full Plot ", desc)
        Plot3DToFile(V_full, w_full, u_full, filename = plotdesc, h = 1000, xlabel = "V", ylabel = "w", zlabel = "u")
        
        plotdesc <- paste("Morris Lecar 3D Plot ", desc)
        Plot3DToFile(V, w, u, filename = plotdesc, h = 1000, xlabel = "V", ylabel = "w", zlabel = "u")
        
        plotdesc <- paste("Voltage vs t - ", desc)
        Plot2DToFile(t, V, filename = plotdesc, w = plotwidth, xlabel = "t", ylabel = "V")
        
        plotdesc <- paste("Voltage vs u - ", desc)
        Plot2DToFile(u, V, filename = plotdesc, w = plotwidth, xlabel = "u", ylabel = "V")
        
        Iaut <- -gautiter * (as.matrix(V) - Vsyn)/(1+exp(-lambda*(V - thetas)))
        plotdesc <- paste("Iaut vs t - ", desc)
        Plot2DToFile(t, Iaut, filename = plotdesc, w = plotwidth, xlabel = "t", ylabel = bquote(I[aut]))
      }
    }
  }
  if (fileIntervals != "")
  {
    write.csv(dfInterval, file = fileIntervals)
    PlotIntervals(dfInterval)
  }
  if (fileCurrent != "")
  {
    write.csv(dfCurrent, file = fileCurrent)
    PlotCurrent(dfCurrent)
  }
}





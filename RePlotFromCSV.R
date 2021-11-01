source("Plotting.R")
source("ConstMorrisLecar.R")

# To be able to generate plots with different aesthetics for the same data, 
#   the differential equation solutions are saved in CSV files.
# A specific naming convention is used to represent the files 
#   for a specific V<u> and g<aut>
# ReadAndPlotMainGraph reads a file that contain Morris Lecar model data
#   and plots V versus t, V versus u, I<aut> versus t, 
#   and the 3-D plot of V, w, and u.
# V<u>: reversal potential of the feedback current
# g<aut>: autapse conductance
# V: membrane potential
# u: feedback current
# I<aut>: autapse current
# w: the gating variable
# t: time
ReadAndPlotMainGraph <- function(filename, desc, gaut, rangestart = 2100, rangeend = 3000)
{
  out <- read.csv(filename)
  
  plotwidth <- 2000
  inc <- 0.001
  if (rangestart < 1000)
    plotwidth <- 4000
  rngstart <- as.integer(rangestart/inc)
  rngend <- as.integer(rangeend/inc)
  t_full = out[, 2]
  V_full = out[, 3]
  w_full = out[, 4]
  u_full = out[, 5]
  
  t = t_full[rngstart:rngend]
  V = V_full[rngstart:rngend]
  w = w_full[rngstart:rngend]
  u = u_full[rngstart:rngend]
  
  plotdesc <- paste("Morris Lecar 3D Full Plot ", desc)
  Plot3DToFile(V_full, w_full, u_full, filename = plotdesc, h = 1000, xlabel = "V", ylabel = "w", zlabel = "u")

  plotdesc <- paste("Morris Lecar 3D Plot ", desc)
  Plot3DToFile(V, w, u, filename = plotdesc, h = 1000, xlabel = "V", ylabel = "w", zlabel = "u")
  
  plotdesc <- paste("Voltage vs t - ", desc)
  Plot2DToFile(t, V, filename = plotdesc, w = plotwidth, xlabel = "t", ylabel = "V")
    
  plotdesc <- paste("Voltage vs u - ", desc)
  Plot2DToFile(u, V, filename = plotdesc, w = plotwidth, xlabel = "u", ylabel = "V")
    
  Iaut <- -gaut * (as.matrix(V) - Vsyn)/(1+exp(-lambda*(V - thetas)))
  plotdesc <- paste("Iaut vs t - ", desc)
  Plot2DToFile(t, Iaut, filename = plotdesc, w = plotwidth, xlabel = "t", ylabel = bquote(I[aut]))
}

# To be able to generate plots with different aesthetic for the same data, 
#   the differential equation solutions are saved in CSV files.
# A specific naming convention is used to represent the files for a specific V<u> and g<aut>
# ReadAndPlotMainGraphs iterates through a list of possible 
#   V<u> and g<aut> values, reads the files that contain the corresponding data,
#   and plots V versus t, V versus u, I<aut> versus t, 
#   and the 3-D plot of V, w, and u.
# V<u>: reversal potential of the feedback current
# g<aut>: autapse conductance
# V: membrane potential
# u: feedback current
# I<aut>: autapse current
# w: the gating variable
# t: time
# to be consistent with the figures displayed in Wang et al., 2020, 
#   the x-axis of the plots are limited to 2100-3000.
#   except the chaotic region, which uses the 700-3000 range.
ReadAndPlotMainGraphs <- function(Vulist, gautlist, rangestart = 2100, rangeend = 3000)
{
  for (Vuiter in Vulist)
  {
    for (gautiter in gautlist)
    {
      desc <- sprintf("Vu = %.4f, gaut = %.2f", Vuiter, gautiter)
      filename <- sprintf("CSV_Vu0.2_GautVar/ODE_Vu_%.4f_gaut_%.3f.csv", Vuiter, gautiter)
      if (gautiter==0)
        filename <- sprintf("CSV_VuVar_GautZero/ODE_Vu_%.4f_gaut_%.3f.csv", Vuiter, gautiter)
      rgstart <- rangestart
      if (Vuiter >= 0.1415 & Vuiter < 0.145)
      {
        rgstart <- 700
      }
      
      ReadAndPlotMainGraph(filename, desc, gaut = gautiter, rangestart = rgstart, rangeend = rangeend)

    }
  }
}

ReadAndPlotFigure0 <- function()
{
  ReadAndPlotMainGraph("CSV_VuVar_GautZero/ODE_Vu_0.2720_gaut_0.000_ZeroStart.csv", desc = "Zero Start Graphs", gaut = 0)

  ReadAndPlotMainGraph("CSV_VuVar_GautZero/ODE_Vu_0.2720_gaut_0.000.csv", desc = "Non-zero Start Graphs", gaut = 0)
}

ReadAndPlotFigure1 <- function()
{
  df <- read.csv("CSV_VuVar_GautZero/IntervalsZeroGaut.csv")
  PlotIntervals_Vu(df, "Figure1alog10.jpg", logbase = 10)
  PlotIntervals_Vu(df, "Figure1alog2.jpg", logbase = 2)

  Vulist <- c(0.272, 0.2, 0.145, 0.1415, 0.14, 0.13)
  gautlist <- c(0)
  ReadAndPlotMainGraphs(Vulist, gautlist)
  
  Vulist<- c(0.272)
  ReadAndPlotMainGraphs(Vulist, gautlist, rangestart = 2300, rangeend = 3200)
}

ReadAndPlotFigure2 <- function()
{
  df <- read.csv("CSV_Vu0.2_GautVar/IntervalsVarGaut.csv")
  PlotIntervals_gaut(df, "Figure2alog10.jpg", logbase = 10)
  PlotIntervals_gaut(df, "Figure2alog2.jpg", logbase = 2)
  PlotFrequencies(df, "Figure2c.jpg")
  
  df <- read.csv("CSV_Vu0.2_GautVar/IntervalsVarGautZoom.csv")
  PlotIntervals_gaut(df, "Figure2blog10.jpg", logbase = 10)
  PlotIntervals_gaut(df, "Figure2blog2.jpg", logbase = 2)
  
  df <- read.csv("CSV_Vu0.2_GautVar/CurrentsVarGaut.csv")
  PlotCurrent(df, "Figure2d.jpg")
}

ReadAndPlotFigure3_4 <- function()
{
  Vulist <- c(0.2)
  gautlist <- c(0, 0.4, 1.2, 1.73, 1.8, 1.9, 2.1)
  ReadAndPlotMainGraphs(Vulist, gautlist, rangestart = 2400)
}

ReadAndPlotFigure0()
ReadAndPlotFigure1()
ReadAndPlotFigure2()
ReadAndPlotFigure3_4()
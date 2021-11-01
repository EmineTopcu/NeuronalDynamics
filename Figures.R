source("NeuronalActivity.R")

# Create the figures of the paper

# Runs the simulation for two different initial states:
#   (V = 0, w = 0, u = 0) and (V = 0.1, w = 0.2, u = 0.43)
# Vu = 0.272 and gaut = 0
# Saves the ODE solution and plots the diagrams shown in the paper
Figure0 <- function()
{
  # Create
  Vulist <- c(0.272)
  gaut <- c(0)
  state_init <- c (V = 0, w = 0, u = 0)
  SolveAndPlot(Vulist, gaut, state_init, saveODE = TRUE, savePlot = TRUE)

  state_init <- c (V = 0.1, w = 0.2, u = 0.43)
  SolveAndPlot(Vulist, gaut, state_init, saveODE = TRUE, savePlot = TRUE)
}

# Runs the simulation for zero gaut and variable Vu values
#   with the initial state of (V = 0.1, w = 0.2, u = 0.43)
# Saves the ODE solution and plots the diagrams shown in the paper
Figure1 <- function()
{
  Vulist <- c(0.2, 0.145, 0.1415, 0.14, 0.13, 0.272)
  gaut <- c(0)
  state_init <- c (V = 0.1, w = 0.2, u = 0.43)
  SolveAndPlot(Vulist, gaut, state_init, saveODE = FALSE, savePlot = TRUE)
}

# Runs the simulation for zero gaut and a range of Vu values
#   with the initial state of (V = 0.1, w = 0.2, u = 0.43)
# Saves the ODE solution and plots the diagrams shown in the paper
# WARNING: this function takes a couple of days to run.
Figure1A <- function()
{
  Vulist <- seq(0.12, 0.27, 0.001)
  gautlist <- c(0)
  state_init <- c(V = 0.1, w = 0.2, u = 0.43)
  SolveAndPlot(Vulist, gautlist, state_init, saveODE = TRUE, savePlot = FALSE, fileIntervals = "IntervalsZeroGaut.csv")
}

# Runs the simulation for 0.2 Vu and a range of gaut values
#   with the initial state of (V = 0.1, w = 0.2, u = 0.43)
# Saves the ODE solution and plots the diagrams shown in the paper
# WARNING: this function takes a couple of days to run.
Figure2AB <- function()
{
  Vulist <- c(0.2)
  gautlist <- seq(0.0, 2.1, 0.01) 
  state_init <- c (V = 0.1, w = 0.2, u = 0.43)
  SolveAndPlot(Vulist, gautlist, state_init, saveODE = TRUE, savePlot = FALSE, fileIntervals = "IntervalsVarGaut.csv")
  gautlist <- seq(1.73, 2.1, 0.001) 
  SolveAndPlot(Vulist, gautlist, state_init, saveODE = FALSE, savePlot = FALSE, fileIntervals = "IntervalsVarGautZoom.csv")
}

# Runs the simulation for 0.2 Vu and variable gaut values
#   with the initial state of (V = 0.1, w = 0.2, u = 0.43)
# Saves the ODE solution and plots the diagrams shown in the paper
Figure3And4 <- function()
{
  Vulist <- c(0.2)
  gautlist <- c(0.4, 1.2, 1.73, 1.8, 1.9, 2.1)
  state_init <- c (V = 0.1, w = 0.2, u = 0.43)
  SolveAndPlot(Vulist, gautlist, state_init, saveODE = TRUE, savePlot = TRUE)
}

# Runs the simulation for Vu = 0.2 and gaut = 0
#   with the initial state of (V = 0.1, w = 0.2, u = 0.43)
# Saves the ODE solution and plots the diagrams shown in the paper
Figure6 <- function()
{
  Vulist <- c(0.2)
  gautlist <- c(0)
  state_init <- c (V = 0.1, w = 0.2, u = 0.43)
  SolveAndPlot(Vulist, gautlist, state_init, saveODE = FALSE, savePlot = TRUE)
}

Figure0()
Figure1()
Figure1A()
Figure2AB()
Figure3And4()
Figure6()

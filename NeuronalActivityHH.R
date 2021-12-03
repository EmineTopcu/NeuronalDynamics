library(deSolve)

source("Hodgkin-Huxley.R")
source("PlottingToScreen.R")

SolveAndPlotHH<- function(state_init, i_list)
{
    nmh_plotted <- FALSE
    for (i in i_list)
    {
        pars <- c (I = i) 
        
        plotwidth <- 1000
        
        inc <- 0.001
        tstart <-0 # the start time of the plots
        tend <- 1000 # the end time of the simulation 
        t <- seq(0, tend, inc)
        out <- ode (y = state_init, times = t, func = Hodgkin_Huxley, parms = pars, method = rkMethod("rk4"))
        
        t_full = out[, 1]
        V_full = out[, 2]
        n_full = out[, 3]
        m_full = out[, 4]
        h_full = out[, 5]
        
        rngstart <- as.integer(0/inc)
        rngend <- as.integer(10/inc)
        t <- t_full[rngstart:rngend]
        V <- V_full[rngstart:rngend]
        n <- n_full[rngstart:rngend]
        m <- m_full[rngstart:rngend]
        h <- h_full[rngstart:rngend]
        
        
        # Plot3D(h_full, n_full, m_full, filename = "hnm", xlabel = "h", ylabel = "n", zlabel = "m")
        
        Plot2D(t_full, V_full, plottitle = paste("Voltage vs t, I =", i), w = plotwidth, xlabel = "t", ylabel = "V")

        if (!nmh_plotted)
        {
            df <- data.frame(t, n, m, h)
            df <- melt(df, id.vars="t")
            g <- ggplot(df, aes(t, value, col = variable)) +     
                geom_point(size = 0.1) +
                labs(title = paste("I =", i)) 
            print(g)
            nmh_plotted <- TRUE
        }

    }
}

state_init <- c (V = 0, n = 0, m = 0, h = 1)
SolveAndPlotHH(state_init, 50)

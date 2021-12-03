EK  <-  -12 
ENa <-  120 
EL <- 10.6
gK_avg <- 36
gNa_avg <-  120 
gL <- 0.3
Cm <- 1 


IK <- expression(gK_avg * n**4 * (V - EK))

INa <- expression(gNa_avg * m**3 * h * (V - ENa))

IL <- expression(gL * (V - EL))

alpha_n <- function(V)
{
    0.01 * (10 - V) / (exp((10-V)/10) - 1)
}

beta_n <- function(V)
{
    0.125 *  exp(-V/80)
}

alpha_m <- function(V)
{
    0.1 * (25 - V) / (exp((25-V)/10) - 1)
}

beta_m <- function(V)
{
    4 * exp(-V/18)
}

alpha_h <- function(V)
{
    0.07 * exp(-V/20)
}

beta_h <- function(V)
{
    1/ (exp((30-V)/10) + 1)
}


Hodgkin_Huxley <- function (time, state_init, pars)
{
    with (as.list(c(state_init, pars)),{

        dV_dt <- (1/Cm) * (I - eval(IK) - eval(INa) - eval(IL))
        dn_dt <- alpha_n(V) * (1 - n) - beta_n(V) * n
        dm_dt <- alpha_m(V) * (1 - m) - beta_m(V) * m
        dh_dt <- alpha_h(V) * (1 - h) - beta_h(V) * h
        return (list(c(dV_dt, dn_dt, dm_dt, dh_dt)))
    })
}
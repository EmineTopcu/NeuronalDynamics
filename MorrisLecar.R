# The Morris-Lecar model as listed in:
# Wang, X., Gu, H., & Lu, B. (2020). Paradoxical reduction and the bifurcations 
#   of neuronal bursting activity modulated by positive self-feedback. 
#   Nonlinear Dynamics, 101(4), 2383-2399. 
#   https://doi.org/10.1007/s11071-020-05913-y

# m_inf: steady state of the m gating variable
m_inf <- function(V)
{
  return (0.5 * (1 + tanh((V - V1)/V2)))
}

# w_inf: steady state of the w gating variable
w_inf <- function(V)
{
  return (0.5 * (1 + tanh((V - V3)/V4)))
}

# gamma: relaxation time
gamma <- function(V)
{
  x <- (V-V3)/(2*V4)
  return (gamma_mult * cosh(x))
}

# IK: Potassium (K) current
# gK: maximum conductance of K channels
# w: gating variable
# EK: reversal potential of K
IK <- expression(-gK * w * (V - EK))

# ICa: Calcium (Ca) current
# gCa: maximum conductance of Ca channels
# m_inf: steady state of the m gating variable
# ECa: reversal potential of Ca
ICa <- expression(-gCa * m_inf(V) * (V - ECa))

# IL: Leak current
# gL: maximum conductance of leak channels
# EK: reversal potential of leak
IL <- expression(-gL * (V - EL))

# Iaut: Autapse current
# gaut: maximum autapse conductance
# Vpos: post-synaptic membrane potential
# Vpre: pre-synaptic membrane potential
# lambda: neurotransmitter release rate
# thetas: synaptic threshold
Iaut <- expression(-gaut * (Vpos - Vsyn)/(1 + exp(-lambda * (Vpre - thetas))))

#Morris Lecar equations as listed in the article
Morris_Lecar <- function (time, state_init, pars)
{
  with (as.list(c(state_init, pars)),{
    Vpre <- V
    Vpos <- V
  
    #1/Cm coeffcient is added to handle the run with biologically relevant V values
    dV_dt <- (1/Cm) * (-1 * u + eval(IK) + eval(ICa) + eval(IL) + eval(Iaut))
    dw_dt <- gamma(V) * (w_inf(V) - w)
    du_dt <- mu * (Vu + V)
    return (list(c(dV_dt, dw_dt, du_dt)))
  })
}

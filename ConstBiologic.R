## The constant values that are more biologically relevant

V1 <- -1      # the constant used in steady state calculations
V2 <- 15      # the constant used in steady state calculations
V3 <- 10      # the constant used in steady state calculations
V4 <- 5       # the constant used in steady state calculations
EL <- -50     # Reversal potential of leakage
EK <- -70     # Reversal potential of K ions
ECa <- 100    # Reversal potential Ca ions
lambda <- 30  # neurotransmitter release rate
gK <- 2       # maximum conductance of K channels
gL <- 0.5     # maximum conductance of leakage
gCa <- 1.2    # maximum conductance of Ca channels
mu <- 0.005   # feedback coefficient
thetas <- 60  # synaptic threshold - a value less than max membrane potential
Vsyn <- 100   # autapse reversal potential - a value larger than max membrane potential (ENa can be as high as 90)
gamma_mult <- 1/15 # the multiplier used in gamma(V) calculation
Cm <- 100     # membrane conductance - selected to the voltage increase

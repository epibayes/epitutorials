require(deSolve)

R0 <- 2.5
gamma <- 0.05
beta <- R0*gamma

params <- c(beta=beta,gamma=gamma)

closed.sir.model <- function (t, x, params) {
## first extract the state variables
S <- x[1]
I <- x[2]
R <- x[3]
## now extract the parameters
beta <- params["beta"]
gamma <- params["gamma"]
## now code the model equations
dSdt <- -beta*S*I
dIdt <- beta*S*I-gamma*I
dRdt <- gamma*I
## combine results into a single vector
dxdt <- c(dSdt,dIdt,dRdt)
## return result as a list!
list(dxdt)
}


times <- seq(from=0,to=365,by=1) # returns a sequence
xstart <- c(S=0.999,I=0.001,R=0.000) 

out <- as.data.frame(
ode(
func=closed.sir.model,
y=xstart,
times=times,
parms=params
))

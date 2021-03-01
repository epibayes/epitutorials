require(deSolve)
require(ggplot2)

two.group.sir.model <- function(t, x, params) {
S1 <- x[1]
I1 <- x[2]
R1 <- x[3]
p1 <- sum(x[1:3])
S2 <- x[4]
I2 <- x[5]
R2 <- x[6]
p2 <- 1-p1



## now extract the parameters
beta <- params["beta"]
gamma <- params["gamma"]
tau <- params["tau"]
rho <- params["rho"]

## Calculate proportion of within-between group contact
## for each group
g1_within <- tau
g1_between <- 1-tau
g2_between <- p1*g1_between/p2
g2_within <- (1-g2_between)

lambda_1 <- rho*beta*S1*(g1_within*I1 + g1_between*I2)
lambda_2 <- beta*S2*(g2_within*I2 + g1_between*I1)

## now code the model equations
dS1dt <- -lambda_1
dI1dt <- lambda_1-gamma*I1
dR1dt <- gamma*I1
## now code the model equatios
dS2dt <- -lambda_2
dI2dt <- lambda_2-gamma*I2
dR2dt <- gamma*I2
## combine results into a single vector
dxdt <- c(dS1dt,dI1dt,dR1dt,dS2dt,dI2dt,dR2dt)
## return result as a list!
list(dxdt)

}


beta <- 0.2
gamma <- 0.1

## Isolation of group 1 
tau <- 0.1
## Relative susceptibility of community 2 vs 1
rho <- 1.5

## Proportion in group two
p2 <- 0.8
p1 <- 1-p2

params <- c(beta=beta,gamma=gamma, tau=tau, rho=rho)

times <- seq(from=0,to=365,by=1) # returns a sequence
xstart <- c(S1=0.999*p1,I1=0.001*p2,R1=0.0,S2=0.999*p2,I2=0.001*p2,R2=0.000) 

out <- as.data.frame(
ode(
func=two.group.sir.model,
y=xstart,
times=times,
parms=params
))

g <- ggplot(out, aes(x=time)) + geom_line(aes(y = R1/p1)) + geom_line(aes(y=R2/p2), linetype = "dashed") + xlab("Day") + ylab("Cumulative proportion infected") + ylim(0,1s)

# generate autocorrelated data.
autocor_outcome <- function(n,a,b,corr) {
  
## Randomly scatter points on a 3 x 3 grid representing
## 9 neighborhoods
x <- runif(n,0,3)
y <- runif(n,0,3)

cases <- data.frame(id = 1:n, 
                    x = x, 
                    y = y)

case_dist <- as.matrix(dist(cbind(cases$x,cases$y)))

## Set up the neighborhood indices
neighborhood_loc <- expand.grid(1:3, 1:3)
names(neighborhood_loc) <- c("nx","ny")
neighborhood_loc$neighborhood <- 1:nrow(neighborhood_loc)
neighborhood_loc$walkability <- sample(1:9)

cases <- cases %>%
  mutate(nx = ceiling(x),
         ny = ceiling(y)) %>%
  inner_join(neighborhood_loc)

# fake, uncorrelated observations
X = rnorm(nrow(cases))

###############################################
# fake sigma... correlated decreases distance.
sigma = diag(nrow(cases))
sigma <- corr ^ case_dist

###############################################

# Y is autocorrelated...
Y <- t(X %*% chol(sigma))
cases$z <- a + b*cases$walkability + (Y - mean(Y))

return(cases)

}

neighborhood_ranef_plot <- function(zz) {
## Point plot of neighborhood random effects
g <- ggplot(zz,aes(x=x,y=y,colour=z)) + 
  geom_point() + 
  scale_colour_distiller(palette="Reds",direction=+1)

return(g)

}

neighborhood_mean_plot <- function(zz) {
  ## Point plot of neighborhood random effects
  
  nn <- zz %>%
    group_by(nx,ny) %>%
    summarize(z = mean(z)) %>% 
    inner_join(neighborhood_loc)
  
  g <- ggplot(nn,aes(x=nx,y=ny,fill=z)) + 
    geom_tile() + 
    scale_fill_distiller(palette="Reds",direction=+1)
  
  return(g)
  
}




## Neighborhood plot

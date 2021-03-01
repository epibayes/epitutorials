require(dplyr)
require(ggplot2)
require(MASS)

kde2ddf <- function(df, n = 50, lims = c(range(0:10),range(0:10))){
  ## This code turns the data into a df
  dd <- kde2d(df$x, df$y, n= 100, lims = lims)
  gr <- data.frame(with(dd, expand.grid(x,y)), as.vector(dd$z))
  colnames(gr) <- c("x","y","z")
  gr$p <- gr$z/sum(gr$z)
  return(gr)
}

## Generate points on a 10x10 square
num_bg_pts <- 1000
background_pts <- data.frame(x = runif(num_bg_pts, 0, 10), 
                             y = runif(num_bg_pts, 0, 10))
background_pts$case <- 0
hotspot_center <- 5
hotspot_sd <- 0.25


## Add in a hotspot centered in the middle
num_hs_pts <- 100
hotspot_pts <- data.frame(x = rnorm(num_hs_pts, hotspot_center, hotspot_sd),
                          y = rnorm(num_hs_pts, hotspot_center, hotspot_sd)) %>% filter(x > 0, x < 10, y > 0, y < 10) 

hotspot_pts$case <- 1

df <- rbind(background_pts, hotspot_pts)

g <- ggplot(df, aes(x=x,y=y, group = case)) + geom_point(aes(colour=as.factor(case)))



g <- ggplot(df, aes(x=x,y=y)) + geom_density2d_filled() + geom_point()

## Separate into case and control plots
g2 <- ggplot(df, aes(x=x,y=y)) + 
  geom_density2d_filled() + 
  geom_point() + facet_wrap(~ case)

## Randomize the locations of cases and controls and do it again
rdf <- df
rdf$case <- sample(df$case)
g3 <- ggplot(rdf, aes(x=x,y=y)) + 
  geom_density2d_filled() + 
  geom_point() + facet_wrap(~ case)


## Get differences between case and control
cs_smooth <- kde2ddf(hotspot_pts)
cl_smooth <- kde2ddf(background_pts)
cs_smooth$z<- cs_smooth$p - cl_smooth$p
g <- ggplot(cs_smooth, aes(x=x,y=y,z=z)) + geom_raster(aes(fill = z))

rdf <- df
maxvals <- rep(0, 100)
for (i in 1:100) {
rdf$case <- sample(df$case)
cs_smooth <- rdf %>% filter(case == 1) %>% kde2ddf()
cl_smooth <- rdf %>% filter(case == 0) %>% kde2ddf()
cs_smooth$z<- cs_smooth$p - cl_smooth$p
maxvals[i] <- max(cs_smooth$z)
}

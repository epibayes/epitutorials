require(dplyr)

multilevel_dataset <- function() {
  
  ## Make block motif 
  block <- data.frame(block = 1:4, high_risk_b = c(1,0,0,0), bx = c(1,1,2,2), by = c(1,2,1,2))
  
  neighborhood <- data.frame(neighborhood = 1:4, high_risk_n = c(1,0,0,0), nx = c(1,1,2,2), ny = c(1,2,1,2))
  
  quadrant <-data.frame(quadrant = 1:4, high_risk_q = c(1,0,0,0), qx = c(1,1,2,2), qy = c(1,2,1,2))
  
  ## Make neighborhood from 4 blocks
  neighborhoods <- list()
  for (i in 1:4) {
    ntmp <- block
    ntmp$neighborhood <- i
    neighborhoods[[i]] <- ntmp
  }
  
  neighborhoods <- dplyr::bind_rows(neighborhoods)
  
  quadrants <- list()
  for (i in 1:4) {
   qtmp <- neighborhoods
   qtmp$quadrant <- i
   quadrants[[i]] <- qtmp
  }
  
  quadrants <- dplyr::bind_rows(quadrants) %>% 
    inner_join(quadrant) %>%
    inner_join(neighborhood) %>%
    mutate(x = bx + 2*(nx-1) + 4*(qx-1), y = by + 2*(ny-1) + 4*(qy-1)) %>%
    select(-bx,-by,-qx,-qy,-nx,-ny)
    
  
  quadrants$high_risk_h <- 1
  quadrants$high_risk_h[quadrants$x > 4] <- 0
  
  return(quadrants)
}

risk_frame_fixed <- function(df, n, avg = 0.05, nor = 1 , bor = 1, qor = 1) {
  

  or <- exp(df$high_risk_n*log(nor) + df$high_risk_b*log(bor) + df$high_risk_q*log(qor))
  a <- (nrow(df)*avg)/sum(or)
  p <- a*or
  
  p[p > 1] <- 1
 
  return(p) 
}

risk_frame_logit <- function(df, alpha = 0.05, hor = 1, nor = 1 , bor = 1, qor = 1) {
  
  
  p <- plogis(qlogis(alpha) + df$high_risk_h*log(hor) + df$high_risk_n*log(nor) + df$high_risk_b*log(bor) + df$high_risk_q*log(qor))

  
  p[p > 1] <- 1
  
  return(p) 
}

draw_risk_frame <- function(alpha = 0.05, hor = 1, nor=1, bor=1, qor=1) {
  
  df <- multilevel_dataset()

  df$p <- risk_frame_logit(df, alpha, hor, nor, bor, qor)
  df$n <- rbinom(nrow(df), 1000, df$p)
  return(df)

}


long_risk_frame <- function(df) {
  
  d1 <- df
  d1$vaccinated <- 1
  
  
  d2 <- df
  d2$vaccinated <- 0
  d2$n <- 1000-d1$n
  
  dd <- rbind(d1, d2)
  
  return(dd)
}


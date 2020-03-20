require(ggplot2)
require(lhs)
require(lme4)
require(ggplot2)


group_indices <- function(N, J) {
    out_groups <- c()
    split_size <- N %/% J

    for (i in 1:(J - 1)) {
        out_groups <- c(out_groups, rep(i, split_size))
    }

    if ((N %% split_size) > 0) {
        last_indices <- rep(J, N %% split_size)
    } else {
        last_indices <- rep(J, split_size)
    }
    out_groups <- c(out_groups, last_indices)

    return(out_groups)
}


split_df <- function(data, n) {
    rownames(data) <- c(1:nrow(data))
    split_list <- list()
    split_size <- nrow(data) %/% n
    for (i in 1:(n - 1)) {
        split_list[[i]] <- data[(((i - 1) * split_size) + 1):(i * split_size), ]
    }
    split_list[[n]] <- data[(((n - 1) * split_size) + 1):nrow(data), ]
    split_list
}


J <- 5
N <- 1000

xvals <- 10 * sort(as.vector(randomLHS(N, 1)))
group_ids <- group_indices(N, J)
walkability <- (group_ids - 1) / (J - 1)

wealth_b <- -5
intercept <- 150
walkability_b <- 50

y_hat <- intercept + wealth_b * xvals + walkability_b * walkability

y_sim <- rnorm(length(y_hat), y_hat, 2)

df <- data.frame(x = xvals, walkability = walkability, neighborhood = as.factor(group_ids), y = y_sim)

g <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(colour = neighborhood)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(method = "lm", aes(group = neighborhood), se = FALSE) +
    xlab("Wealth") +
    ylab("SBP")
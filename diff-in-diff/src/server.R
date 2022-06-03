sim_full_data <- reactive({
  N <- input$N
  ids <- 1:input$N
  num_control <- ifelse((N %% 2) == 1, ((N + 1) / 2) - 1, N / 2)
  num_exp <- ifelse((N %% 2) == 1, ((N + 1) / 2), N / 2)
  
  treatment <- c(rep(0, num_control), rep(1, num_exp))
  
  p_high <-
    plogis(qlogis(input$p_high) + log(1 / input$or_hr) * treatment)
  
  high_risk <- rbinom(input$N, 1, p_high)
  
  df1 <- data.frame(id = ids,
                    treatment = treatment,
                    high_risk = high_risk)
  df1$time <- 0
  
  df2 <- df1
  df2$time <- 1
  
  df <- rbind(df1, df2)
  
  mu <-
    input$alpha + input$beta_time * df$time + input$beta_t_delta * (df$time *
                                                                      df$treatment) + input$beta_t * df$treatment * df$time + input$alpha_diff *
    df$high_risk
  
  df$y <- rnorm(nrow(df), mu, sd = input$sd)
  
  return(df)
  
})

sim_exp_data <- reactive({
  sim_full_data() %>% filter(time == 1) %>% select(-time)
})

exp_model_fit <- reactive({
  fit <- lm(y ~ treatment, data = sim_exp_data())
  return(fit)
})

exp_model_summary <- renderPrint({
  summary(exp_model_fit())
})

output$exp_model_summary <- exp_model_summary
output$exp_model_summary2 <- exp_model_summary

exp_model_plot <- renderPlotly({
  g <- ggplot(sim_exp_data()) +
    geom_jitter(aes(
      x = treatment,
      y = y,
      colour = as.factor(high_risk),
      shape = as.factor(treatment)
    )) +
    geom_smooth(
      aes(x = treatment, y = y),
      colour = "black",
      method = "lm",
      se = FALSE
    ) +
    theme(legend.position = "bottom")
  
  p <- ggplotly(g)
  return(p)
  
  
})

output$exp_model_plot <- exp_model_plot
output$exp_model_plot2 <- exp_model_plot

output$selectionError <- renderPlotly({
  conf_range <- confint(exp_model_fit(), "Treatment") - input$beta_t
  
  df <-
    data.frame(
      x = 0,
      ymin = conf_range[1],
      ymax = conf_range[2],
      tf = coef(exp_model_fit())[["treatment"]] - input$beta_t
    )
  
  g <- ggplot(df) +
    geom_errorbar(aes(x = x, ymin = ymin, ymax = ymax)) +
    geom_point(aes(x = x, y = tf)) +
    xlim(-0.5, .5) +
    xlab("") +
    ylim(-11, 2) +
    ylab ("Error in Estimated Treatment Effect") +
    geom_hline(yintercept = 0, linetype = "dashed")
  p <- ggplotly(g)
  return(p)
  
})

## Here are the basic inputs to the natural experiment model
conditions <- expand.grid(time = 0:1,
                          treatment = 0:1)

## Make 200 simulated observations
sim_input <- purrr::map_dfr(seq_len(50), ~ conditions)


sim_data <- reactive({
  time_eff <-
    input$beta_t + (sim_input$time * sim_input$treatment) * input$beta_t_delta
  y <-
    rnorm(
      nrow(sim_input),
      120 + input$alpha * sim_input$treatment + time_eff * sim_input$time + input$beta_treat *
        (sim_input$time * sim_input$treatment),
      input$sd
    )
  
  out_df <- sim_input
  out_df$y <- y
  
  return(out_df)
})



model_fit <- reactive({
  if (input$did == FALSE) {
    fit <- lm(y ~ time + treatment, data = sim_full_data())
  } else {
    fit <-
      lm(y ~ time + treatment + time * treatment, data = sim_full_data())
  }
  return(fit)
})


model_summary <- renderPrint({
  summary(model_fit())
})

output$model_summary <- model_summary
output$model_summary2 <- model_summary
output$model_summary3 <- model_summary

simDataFigure <- reactive({
  df <- sim_full_data()
  
  pred_d <- conditions
  
  
  # cfd <- data.frame(time = c(0,1), treatment = c(1,0))
  #cfd$y <- predict(model_fit(), newdata = cfd)
  pred_d$y <- predict(model_fit(), newdata = conditions)
  
  pred_d$offset <- 0
  if (input$did == TRUE) {
    pred_d$offset[4] <- coef(model_fit())[["time:treatment"]]
  }
  
  g <- ggplot() +
    geom_jitter(data = df, aes(
      x = time,
      y = y,
      colour = as.factor(high_risk),
      shape = as.factor(treatment)
    )) +
    geom_line(data = pred_d, aes(
      x = time,
      y = y,
      #group = as.factor(treatment),
      linetype = as.factor(treatment)
    ))
  
  if (input$did == TRUE) {
    did_d <-
      data.frame(
        x = 1,
        xend = 1,
        y = pred_d$y[4],
        yend =  pred_d$y[4] - pred_d$offset[4]
      )
    g <-
      g +  geom_line(data = pred_d[3:4,],
                     aes(x = time, y = y - offset),
                     linetype = "dashed") +
      geom_segment(
        data = did_d,
        aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend
        ),
        arrow = arrow(ends = "first"),
        colour = "red"
      )
  }
  g <- g + xlim(-1, 2) +
    theme(legend.position = "bottom")
  
  p <- ggplotly(g)
  return(p)
  
})

simDataPlot <- renderPlotly(simDataFigure())

output$simDataPlot <- simDataPlot
output$simDataPlot2 <- simDataPlot
output$simDataPlot3 <- simDataPlot

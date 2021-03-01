require(purrr)
require(faux)
require(shiny)
require(dplyr)
require(ggplot2)
require(lme4)

## Simulate from a multilevel model

## N = Number of individuals,
## G = Number of neighborhoods


generateNeighborhoodData <- function(r, beta_n, neighborhood_sd){
  
G <- 30
N <- 1000


alpha <- 120
#neighborhood_sd <- 5


## Sample individuals into neighborhoods w/equal probability
df <- data.frame(neighborhood = 1:G, 
                 N = rmultinom(1:G, N, rep(1/G, G)),
                 walkability = rnorm(G, 0, 1)) %>% 
                 mutate(mu_income = rnorm_pre(walkability, 0, 1, r = r),
                        n_alpha = rnorm(G, alpha, neighborhood_sd) + beta_n*walkability) %>%
  filter(N > 0)

return(df)
}

generateIndividualData <- function(df, beta_income) {
  
  ind_sd <- 1
  
  neighborhoods <- pmap_dfr(df, function(neighborhood,N, walkability, mu_income, n_alpha) { 

  data.frame(neighborhood = neighborhood, 
             income = rnorm(N,mu_income,1)) %>%
             mutate(y = rnorm(N, n_alpha + beta_income*income, ind_sd))
  }) %>% inner_join(df)


  return(neighborhoods)
}

histogramUI <- function(id) {
  sidebarLayout(
    sidebarPanel(
      h4("Generate neighborhood-level data"),
      p("Use the sliders to examine different relationships between neighborhood walkability, average income, and average blood pressure:"),
      sliderInput(NS(id, "nsd"), "Standard deviation of neighborhood-level errors", min = 0.5, max = 10, step = 0.1, value = 2),
    sliderInput(NS(id, "beta_w"), "Change in average SBP for 1-unit changed in walkability", min = -4, max = 0, step = 0.1, value = -1)),
    
    mainPanel(
      plotOutput(NS(id, "bp"))
  )
  )
}


histogramServer <- function(id) {
  moduleServer(id, function(input, output, session){
    
    ## Generate the model data in response to parameter inputs
    neighborhoodData <- reactive(generateNeighborhoodData(input$r, input$beta_w, input$nsd))
    individualData <- reactive(generateIndividualData(neighborhoodData(), input$incomebp))
    
    output$hist <- renderPlot({
      df <- neighborhoodData()
      g <- ggplot(df, aes(x=walkability, y = mu_income)) + 
        geom_point() +
        geom_smooth(method="lm", se=FALSE) + 
        xlab("Neighborhood Walkability") +
        ylab("Neighborhood Average Income") + 
        xlim(-2.5, 2.5) + 
        ylim(-2.5, 2.5)
      return(g)
    })
  
  output$bp <- renderPlot({
    df <- neighborhoodData()
    g <- ggplot(df, aes(x=walkability, y = n_alpha)) + 
      geom_point() + 
      geom_smooth(method="lm", se=FALSE) + 
      xlab("Neighborhood Walkability") +
      ylab("Neighborhood Expected SBP") + 
      xlim(-2.5, 2.5) 
    return(g)
  })

  
  output$income <- renderPlot({
    df <- neighborhoodData()
    g <- ggplot(df, aes(x=mu_income, y = n_alpha)) + 
      geom_point() +
      geom_smooth(method="lm", se=FALSE) + 
      xlab("Neighborhood Avg. Income") +
      ylab("Neighborhood Expected SBP") + 
      xlim(-2.5, 2.5) 
    return(g)
  })
  
  output$indbp <- renderPlot({
    df <- individualData()
    g <- ggplot(df, aes(x=income, y = y)) + 
      geom_point() +
      geom_point(aes(colour=walkability)) +
      geom_smooth(method="lm", se=FALSE) + 
      xlab("Individual Income") +
      ylab("Individual SBP") + 
      xlim(-2.5, 2.5) 
    return(g)
  })
  
  simpleModel <- reactive({
    if (input$adjust == TRUE){
      f <- y ~ income + walkability
    } else {
      f <- y ~ income
    }
      
    return(lm(f, data = individualData()))
  })
  
  multilevelModel <- reactive({
    if (input$adjust == TRUE){
      f <- y ~ income + walkability + (1 | neighborhood)
    } else {
      f <- y ~ income + (1 | neighborhood)
    }
    
    return(lmer(f, data = individualData()))
  })
  
  selectedModel <- reactive({
    if (input$hierarchical == TRUE) {
      return(multilevelModel()) 
    } else {
      return(simpleModel())
      }
  })
  
  output$modelOut <- renderPrint({
    return(summary(selectedModel()))
  })
  
  output$modelResid <- renderPlot({
    m <- selectedModel()
    df <- data.frame(y = resid(m))
    g <- ggplot(df, aes(sample=y)) + stat_qq() + stat_qq_line()
    return(g)
  
  })
  
  output$incomeResid <- renderPlot({
    df <- individualData()
    x <- df$income
    df$resid <- resid(selectedModel())
    
    g <- ggplot(df, aes(x=income, y=resid)) + geom_point(aes(colour=walkability)) + geom_smooth(method="lm", se=FALSE) 

    return(g)
  })
  
  output$beta_income_val <- renderText({
    return(paste0("Input value of income effect = ", as.character(input$incomebp)))
  })
  
  output$beta_walk_val <- renderText({
    return(paste0("Input value of walkability effect = ", as.character(input$beta_w)))
  })
})
  
}

individualUI <- function(id) {
  sidebarLayout(
      sidebarPanel(
        h4("Generate individual level data"),
      p("Manipulate the relationship between individual-level income and individual SBP."),
      sliderInput(NS(id, "incomebp"), "Change in SBP for each 1-unit change in income", 
                  min = -4, max = 0, step = 0.1, value = -1)),      
    mainPanel(
  plotOutput(NS(id,"indbp")),
    ))
}

modelUI <- function(id) {
  sidebarLayout(
    sidebarPanel(
      h4("Analyze individual level data"),
      checkboxInput(NS(id, "adjust"), "Adjust for walkability", value = FALSE),
      checkboxInput(NS(id, "hierarchical"), "Use hierarchical model", value = FALSE),
      h4("Input parameter values"),
      textOutput(NS(id, "beta_income_val")),
      textOutput(NS(id, "beta_walk_val"))
      
      
      
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Model", verbatimTextOutput(NS(id, "modelOut"))),
      tabPanel("Normality", plotOutput(NS(id, "modelResid"))),
      tabPanel("Residuals",plotOutput(NS(id, "incomeResid")))
      )
    )
  )
  
}

correlationUI <- function(id) {
  sidebarLayout(
    sidebarPanel(
      sliderInput(NS(id, "r"), "Correlation between neighborhood walkability and average income", min = 0, max = 1, step = 0.02, value = 0.0),
    ),
    mainPanel(
      plotOutput(NS(id, "hist"))
      )

    )
  
}


histogramApp <- function() {
  ui <- fluidPage(
    histogramUI("hist1"),
    individualUI("hist1"),
    modelUI("hist1"),
    correlationUI("hist1")
  )
  server <- function(input, output, session) {
    histogramServer("hist1")
  }
  shinyApp(ui, server)  
}


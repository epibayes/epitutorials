require(shiny)
require(flexdashboard)
#source("src/segregation.R")

## Generate grid
neighborhood_df <- reactive({
  draw_risk_frame(alpha = input$punvax, hor = input$hor, qor = input$qor, nor = input$nor, bor = input$bor)
})

neighborhood_df_long <- reactive({
  long_risk_frame(neighborhood_df())
})

output$initialPlot <- renderPlot({
  grid_plot(neighborhood_df())
})

output$proportionNonVax <- renderPlot({
  proportion_hist(neighborhood_df())
})


isoval <- reactive({
  isolation(neighborhood_df()$p*1000, 1000)
})

interactval <- reactive({
  interaction(neighborhood_df()$p*1000, 1000)
})

theilval <- reactive({
  ldf <- neighborhood_df_long()
  x <- 100*(theil(ldf, "quadrant") + theil_within(ldf, "quadrant", "block"))
  print(as.numeric(x))
  return(as.numeric(x))
})

dissimval <- reactive({
  dissimilarity(neighborhood_df()$p*1000, 1000)
})

moranval <- reactive({
  morans_grid(d, input$ntype, neighborhood_df()$p)$I
})

unvaxval <- reactive({
  round(mean(100*neighborhood_df()$p),0)
})

output$avgp <- renderText(
  paste0("Percent unvaccinated = ", round(mean(100*neighborhood_df()$p),2),"%")
)

output$iso <- renderText(
  paste0("Isolation = ", round(isoval(),2))
)

output$dissim <- renderText(
  paste0("Dissimilarity = ", round(dissimval(),2))
)

output$morans <- renderText(
  paste0("Moran's I = ", round(moranval(),2))
  
)

output$blocktheil <- renderText({
  ldf <- neighborhood_df_long()
  block_h <- theil(ldf, "quadrant") + theil_within(ldf, "quadrant", "block")
  paste0("Total:", round(block_h,2))
}
)

output$neightheil <- renderText({
  ldf <- neighborhood_df_long()
  block_h <- theil(ldf, "neighborhood")
  paste0("Between Neighborhood:", round(block_h,2))
}
)

output$quadtheil <- renderText({
  ldf <- neighborhood_df_long()
  block_h <- 100*theil(ldf, "quadrant") / (theil(ldf, "quadrant") + theil_within(ldf, "quadrant", "block"))
  paste0("Between Quadrant:", round(block_h,0),"%")
}
)

output$withinquadtheil <- renderText({
  ldf <- neighborhood_df_long()
  block_h <- 100*theil_within(ldf, "quadrant", "block") / (theil(ldf, "quadrant") + theil_within(ldf, "quadrant", "block"))
  paste0("Within quadrant:", round(block_h,0),"%")
}
)

output$halftheil <- renderText({
  ldf <- neighborhood_df_long()
  block_h <- theil(ldf, "high_risk_h")
  paste0("Between Halves:", round(block_h,2))
}
)

output$theilgauge <- renderGauge({
  gauge(round(theilval(),2),
        min = 0,
        max = 100)
})

output$dissimgauge <- renderGauge({

  
  gauge(round(dissimval(),2),
        min = 0,
        max = 1)
  
})

output$isogauge <- renderGauge({
  
  gauge(round(isoval(),2),
        min = 0,
        max = 1)
  
})

output$intergauge <- renderGauge({
  
  gauge(round(interactval(),2),
        min = 0,
        max = 1)
  
})



output$morangauge <- renderGauge({
  
  gauge(round(moranval(),2),
        min = -1,
        max = 1)
  
})

output$unvaxgauge <- renderGauge({
  gauge(unvaxval(),
        min = 0, 
        max = 100)
})



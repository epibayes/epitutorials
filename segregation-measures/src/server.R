require(shiny)
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



dissimval <- reactive({
  dissimilarity(neighborhood_df()$p*1000, 1000)
})

moranval <- reactive({
  morans_grid(d, neighborhood_df()$p)$I
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
  block_h <- theil(ldf, "block")
  paste0("Block:", round(block_h,2))
}
)

output$neightheil <- renderText({
  ldf <- neighborhood_df_long()
  block_h <- theil(ldf, "neighborhood")
  paste0("Neighborhood:", round(block_h,2))
}
)

output$quadtheil <- renderText({
  ldf <- neighborhood_df_long()
  block_h <- theil(ldf, "quadrant")
  paste0("Quadrant:", round(block_h,2))
}
)

output$halftheil <- renderText({
  ldf <- neighborhood_df_long()
  block_h <- theil(ldf, "high_risk_h")
  paste0("Half:", round(block_h,2))
}
)



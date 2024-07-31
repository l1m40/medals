print(getwd())
source("R/shiny_brazilian_medals.R",local=T)
shinyApp(ui = ui, server = server)

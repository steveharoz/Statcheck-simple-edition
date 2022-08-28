#
# Little shiny app to redirect to another page. Useful if the URL changes.
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$meta(`http-equiv` = "refresh", content="0; URL=http://statcheck.steveharoz.com")
  ),
  p("redirecting to ", a("statcheck.steveharoz.com", href="http://statcheck.steveharoz.com"))
)   

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)

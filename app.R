library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

df <- read.csv("rockets.csv")

companies <- unique(df$Company.Name)
countries <- unique(df$Country)
minYear <- min(df$Year)
maxYear <- max(df$Year)

ui <- fluidPage(
  titlePanel("Rocket Launches"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country", c(countries)),
      selectInput("mfr", "Manufacturer", c(companies)),
      sliderInput("years", "Years", minYear, maxYear,
        c(minYear, maxYear), step = 1, sep = "")
    ),
    
    
    mainPanel(plotOutput("distPlot", 
                         brush = brushOpts("plot_brush", 
                                           resetOnNew = T, 
                                           direction = "x")),
              dataTableOutput("table")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$plot_brush, {
    info_plot <- brushedPoints(df, input$plot_brush)
    output$table <- renderDataTable(info_plot)
  })
  
  output$distPlot <- renderPlot({
    ggplot(
      df %>% filter(
        Year >= input$years[1] &
          Year <= input$years[2] &
          Company.Name == input$mfr &
          Country == input$country
      ),
      aes(
        x = Year,
        fill = Status.Mission,
        text = paste("Country:", Country)
      )
    ) +
      geom_dotplot(binwidth = .25) +
      geom_rug() +
      scale_y_continuous(breaks = NULL) +
      theme(axis.title.y = element_blank())
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

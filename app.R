library(shiny)
library(dplyr)
library(ggplot2)
#library(plotly)

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
      sliderInput("years", "Years", minYear, maxYear,
        c(minYear, maxYear), step = 1, sep = ""),
      hr(),
      selectInput("mfr", "Manufacturer", c(companies))
    ),
    
    
    mainPanel(plotOutput("distPlot"),
              dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  observe({
    df.filtered <- df %>% filter(
        Year >= input$years[1] &
        Year <= input$years[2] &
        Country == input$country
    )
    
    updateSelectInput(session, "mfr",
                      choices = c(unique(df.filtered$Company.Name)))
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
        fill = Status.Mission
      )
    ) +
      geom_dotplot(binwidth = .25, position=position_dodge(0.8)) +
      geom_rug() +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(labels=c(df$Year), breaks=c(df$Year)) +
      theme(axis.title.y = element_blank())
  })
  
  output$table <- renderDataTable(df %>% filter(
    Year >= input$years[1] &
      Year <= input$years[2] &
      Company.Name == input$mfr &
      Country == input$country
  ))
  
}

# Run the application
shinyApp(ui = ui, server = server)

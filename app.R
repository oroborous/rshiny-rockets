library(shiny)
library(dplyr)
library(ggplot2)
library(shinyWidgets)

df <- read.csv("rockets.csv")
# Convert string 'Wed Sep 30, 2015 20:30 UTC' to date
df$Date <- as.Date(df$Datum, "%a %b %d, %Y")

# Populate selection boxes
companies <- unique(df$Company.Name)
countries <- sort(unique(df$Country))
# Endpoints for slider
minYear <- min(df$Year)
maxYear <- max(df$Year)

ui <- fluidPage(
  titlePanel("Rocket Launches"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country", c("ALL", countries)),
      sliderInput("years", "Years", minYear, maxYear,
        c(minYear, maxYear), step = 1, sep = ""),
      hr(),
      pickerInput("mfr", "Manufacturer", c(companies),
                  options = list('actions-box' = TRUE),
                  multiple = TRUE),
      verbatimTextOutput("temp")
    ),
    
    
    mainPanel(plotOutput("distPlot"),
              dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  # View value of pickerInput for debug
  output$temp <- renderPrint(input$mfr)
  
  # Filter dataset on country and years
  df.filter1 <- reactive({
    df %>% filter(
        Year >= input$years[1] &
        Year <= input$years[2] &
        if (input$country == "ALL") TRUE else Country == input$country
    )
  })
  

  # Use filtered dataset to repopulate the Manufacturer dropdown
  # with companies that meet the country/year criteria
  observeEvent(input$country, {
    updatePickerInput(session = session, inputId = "mfr",
                      choices = sort(unique(df.filter1()$Company.Name)),
                      selected = sort(unique(df.filter1()$Company.Name)))
  })
  
  # Apply the manufacturer filter and create a dotplot showing each
  # launch as a dot, color-coded to indicate the mission outcome
  output$distPlot <- renderPlot({
    ggplot(
      df.filter1() %>% filter(Company.Name %in% input$mfr),
      aes(x = Year, fill = Status.Mission)
    ) +
      geom_dotplot(binwidth = .25, position=position_dodge(0.8)) +
      geom_rug() +
      scale_y_continuous(breaks = NULL) +
      scale_x_continuous(labels=c(df$Year), breaks=c(df$Year)) +
      theme(axis.title.y = element_blank())
  })
  
  # Populate the datatable with the filtered data set and sort by mission
  # date (earliest to latest). Hide columns with extraneous information.
  output$table <- renderDataTable({
    df.filter1() %>% filter(Company.Name %in% input$mfr)
    }, options=list(order = list(10, 'asc'),
                    columnDefs = list(list(visible=FALSE, targets=c(2, 4, 5, 6, 8, 9)))))
  
}

# Run the application
shinyApp(ui = ui, server = server)

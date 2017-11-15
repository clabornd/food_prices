source("requirements.R")

food_data <- read_csv("Data/food_data_clean.csv")

countries <- as.list(unique(food_data$adm0_name))
foods <- as.list(unique(food_data$staples))

ggthemr("dust")

ui <- fluidPage(
  
  sidebarLayout(position = "left",
    
    sidebarPanel(
      selectInput("countries", label = "Select Countries", choices = countries, multiple = TRUE),
      
      selectInput("foods", label = "Choose Foods", choices = foods),
      
      actionButton("updateplot", "Update Plot!")
    ),
    
    mainPanel(
      
      plotOutput("mainplot")
    )
    
  )

)


server <- function(input, output, session){
  
  observeEvent(input$countries,{  
    updateSelectInput(session, "foods", label = "Choose Commodity",
                      choices = as.list(unique((food_data %>% filter(adm0_name %in% input$countries))$staples))
    )
  })
  
  
  
  output$mainplot <- renderPlot({
    
    input$updateplot
    
    isolate({
      p <- food_data %>% filter(adm0_name %in% input$countries, staples == input$foods) %>%
      ggplot(aes(Date, mean_price, color = adm0_name)) + geom_line() + 
      labs(y ="Average Price", color = "Country")
    print(p)
    })
  })

  
}

shinyApp(ui = ui, server = server)

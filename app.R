source("requirements.R")

food_data <- read_csv("Data/WFPVAM_FoodPrices_24-7-2017.csv")

food_data <- food_data %>% mutate(Date = as.Date(paste(as.character(mp_year), as.character(mp_month), "1", sep = "-"))) %>%
    group_by(Date, cm_id) %>%
    mutate(mean_price = mean(mp_price))

countries <- as.list(unique(food_data$adm0_name))
foods <- as.list(unique(food_data$cm_name))

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
    updateSelectInput(session, "foods", label = "Choose Foods",
                      choices = as.list(unique((food_data %>% filter(adm0_name %in% input$countries))$cm_name))
    )
  })
  
  
  
  output$mainplot <- renderPlot({
    
    input$updateplot
    
    isolate({
      p <- food_data %>% filter(adm0_name %in% input$countries, cm_name == input$foods) %>%
      ggplot(aes(Date, mean_price, color = adm0_name)) + geom_line() + 
      labs(y ="Average Price", color = "Country")
    print(p)
    })
  })

  
}

shinyApp(ui = ui, server = server)

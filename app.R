source("requirements.R")

food_data <- read_csv("Data/food_data_2015-2018.csv")
countries <- as.list(unique(food_data$adm0_name))
divisions <- colnames(food_data)[-which(colnames(food_data) %in% c("mean_price", "Date", "mp_price"))]
foods <- as.list(unique(food_data$staples))

ggthemr("dust")

ui <- fluidPage(
  
  fluidRow(
    div(style = "left:10",
    splitLayout(
      selectInput("blahg", label = "Select Countries", choices = countries),
      actionButton("load", "Load...")
     )
    )
    
  ),
  
  fluidRow(id = "basic_plot_fluidRow",
    
    column(width = 4,
           
      tags$br(),
      tags$br(),
      
      selectInput("countries", label = "Select Countries", choices = countries),
      
      selectInput("foods", label = "Choose Commodity", choices = foods),
      
      actionButton("updateplot", "Generate Plot")
      
      
    ),
    
    column(8,
      
      trelliscopeOutput("trelliscope_out")
    )
    
  ),
  
  fluidRow(id = "trelliscope_fluidRow",
    
    column(width = 4,
           
           tags$br(),
           tags$br(),
           
           selectInput("division_1", label = "Choose Division 1", choices = divisions),
           
           selectInput("division_2", label = "Choose Division 2", choices = divisions),
           
           actionButton("make_trelliscope", "Create Trelliscope Display")
           
           
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
      food_data %>% filter(adm0_name %in% input$countries, staples == input$foods) %>%
        ggplot(aes(x = Date, y = mp_price, group = Date)) + geom_boxplot() +
        labs(y = "Price", x = "Date")
    
    })
  })

    output$trelliscope_out <- renderTrelliscope({
      input$make_trelliscope
      print("we here")
      isolate(
        display <-  food_data %>% 
            group_by(!!rlang::sym(input$division_1), !!rlang::sym(ifelse(is.null(input$division_2), input$division_1, input$division_2))) %>%
            nest() %>%
            mutate(plot = map_plot(data, price_v_time)) %>%
            trelliscope("display1", path = tempdir())
      ) 
      print(class(display))
      return(display)
    })
  
  
}

shinyApp(ui = ui, server = server)

suppressPackageStartupMessages({
  source("requirements.R", local = TRUE)
})

source("plot_fns.R")

food_data <- read_csv("Data/food_data_2015-2018.csv") %>%
  select(-contains("_id")) 
countries <- as.list(unique(food_data$adm0_name))
divisions <- colnames(food_data)[-which(colnames(food_data) %in% c("mean_price", "Date", "mp_price"))]
foods <- as.list(unique(food_data$staples))

ui <- navbarPage("Test Some Food Prices.....", id = "top_page",
  
      tabPanel("Plotsss",
        fluidRow(id = "basic_plot_fluidRow",
          column(width = 4,
                 
            tags$br(),
            tags$br(),
            
            selectInput("countries", label = "Select Countries", choices = countries),
            
            selectInput("foods", label = "Choose Commodity", choices = foods),
            
            actionButton("updateplot", "Generate Plot")
            
          ),
          
          column(8,
            plotOutput("mainplot")
          )
        ),
        
        fluidRow(id = "trelliscope_fluidRow",
          column(width = 4,
                 
                 tags$br(),
                 tags$br(),
                 
                 selectInput("division_1", label = "Choose Division 1", choices = c(divisions, "oh another input")),
                 
                 actionButton("make_trelliscope", "Create Trelliscope Display")
          ),
          column(width = 8,
                 trelliscopeOutput("trelliscope_out")
                 )
          )
        ),
      tabPanel("Button Priority",
        actionButton("addup", "The great button", style = "margin-left:50%"),
        fluidRow(
          column(4,div(htmlOutput("output1"), style = "margin-left:40%")),
          column(4,div(htmlOutput("output2"), style = "margin-left:40%")),
          column(4,div(htmlOutput("output3"), style = "margin-left:40%"))
        ),
        fluidRow(
          column(4,plotOutput("numplot1")),
          column(4,plotOutput("numplot2")),
          column(4,plotOutput("numplot3"))
        )
    )
  )


server <- function(input, output, session){
  revals <- reactiveValues("N1" = 1, "N2" = 1, "N3" = 1, 
                           reac_df1 = data.frame(x = 0, y = 1), reac_df2 = data.frame(x = 0, y = 1), reac_df3 = data.frame(x = 0, y = 1))
  
  observeEvent(input$countries,{  
    updateSelectInput(session, "foods", label = "Choose Commodity",
                      choices = as.list(unique((food_data %>% filter(adm0_name %in% input$countries))$staples))
    )
  })
  
  output$mainplot <- renderPlot({
    
    input$updateplot
    
    isolate({
      food_data %>% filter(adm0_name %in% input$countries, staples == input$foods) %>%
        mean_price_v_time()
    
    })
  })

  output$trelliscope_out <- renderTrelliscope({
    if(input$make_trelliscope == 0){
      NULL
    }
    else{
      isolate(
        food_data %>% 
          group_by(!!rlang::sym(input$division_1), Date) %>%
          slice(1) %>% ungroup() %>%
          group_by(!!rlang::sym(input$division_1)) %>%
          nest() %>%
          mutate(plot = map_plot(data, mean_price_v_time)) %>%
          trelliscope("display1", self_contained = TRUE, width = 800)
      ) 
    }
  })
  
  lapply(1:3, function(i){
    output[[paste0("output", i)]] <- renderText(paste0("<div style = color:blue>This is a number:  ",revals[[paste0("N",as.character(i))]],"</div>"))
    output[[paste0("numplot", i)]] <- renderPlot({
      ggplot(revals[[paste0("reac_df", i)]], aes(x = x, y = y)) + geom_line() + ylim(c(0, max(revals[[paste0("N",1)]], revals[[paste0("N",2)]],revals[[paste0("N",3)]])))
    })
  })
  
  # observers which add the values of the other two reactive values to the current value
  # priority controls who the leader is
  observeEvent(input$addup,{
    revals$N1 <- revals$N2 + revals$N3
  }, ignoreInit = TRUE, priority = 5)
  
  observeEvent(input$addup,{
    revals$N2 <- revals$N1 + revals$N3
  }, ignoreInit = TRUE, priority = 4)
  
  observeEvent(input$addup,{
    revals$N3 <- revals$N2 + revals$N1
  },ignoreInit = TRUE, priority = 3)
  
  # reactively creates the dataframes that are passed to the plot UI's
  observeEvent(input$addup,{
    revals$reac_df1 <- rbind(revals$reac_df1, c(input$addup, revals$N1)) 
  }, ignoreInit = TRUE, priority = 1)
  
  observeEvent(input$addup,{
    revals$reac_df2 <- rbind(revals$reac_df2, c(input$addup, revals$N2)) 
  }, ignoreInit = TRUE, priority = 1)
  
  observeEvent(input$addup,{
    revals$reac_df3 <- rbind(revals$reac_df3, c(input$addup, revals$N3)) 
  },ignoreInit = TRUE, priority = 1)
}

shinyApp(ui = ui, server = server)

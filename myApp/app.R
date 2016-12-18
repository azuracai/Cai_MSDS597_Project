library(shiny)
library(tidyverse)
library(choroplethr)
library(choroplethrMaps)
education_tall <- read_csv("data/education_tall.csv")
ued <- read_csv("data/uedjoin.csv")

ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("State Unemployment Rates / Educational Attainment / Race"),
  

  sidebarLayout(position = "right",
                
    sidebarPanel(
      # select an educational level
      radioButtons(inputId="LevelId", label="Education Level", 
                   choices=c("High school graduate","Bachelor's degree", "Advanced degree")),
      # education map
      plotOutput("eduPlot"),
      
      # select two states to compare
      selectInput(inputId="StateInput1", label="Choose a state:", 
                  choices=ued$State, selected="new jersey"),
      selectInput(inputId="StateInput2", label="Choose another state:", 
                  choices=ued$State, selected="texas"),
      # tables for two selected state
      tableOutput("eduTable"),
      tableOutput("popTable"),
      width = 6
    ),
    
    mainPanel(
      # tab panel for population maps
      tabsetPanel(
        tabPanel("All", plotOutput("allplot")),
        tabPanel("White", plotOutput("wplot")),
        tabPanel("Black", plotOutput("bplot")),
        tabPanel("Asian", plotOutput("aplot")),
        tabPanel("Hispanic", plotOutput("hplot"))
      ),
      # unemployment rate map
      plotOutput("unempPlot"),
      width = 6
    )
    
  )
))


server <- shinyServer(function(input, output) {
  
  # education map
  reduced_level.df <- reactive({
    filter(
      education_tall,
      Level == input$LevelId
    )
  })
  output$eduPlot <- renderPlot({
    reduced_level.df() %>% dplyr::rename(region=State, value=Percentage) %>% 
      state_choropleth(title = input$LevelId, legend="Percentage")
  })
  
  # state table
  reduced_state.df <- reactive({
    filter(
      ued, 
      State %in% c(input$StateInput1, input$StateInput2)
    )
  })
  output$eduTable <- renderTable({
    reduced_state.df() %>% 
      dplyr::select(State, Rate, `High school graduate`, `Bachelor's degree`, `Advanced degree`)
  })
  output$popTable <- renderTable({
    reduced_state.df() %>%
      dplyr::select(State, Rate, percent_white, percent_black, percent_asian, percent_hispanic)
  })
  
  # population map
  output$allplot <- renderPlot({
    ued %>% dplyr::rename(region=State, value=total_population) %>% 
      state_choropleth(legend="total population")
  })
  output$wplot <- renderPlot({
    ued %>% dplyr::rename(region=State, value=percent_white) %>% 
      state_choropleth(legend="percent white")
  })
  output$bplot <- renderPlot({
    ued %>% dplyr::rename(region=State, value=percent_black) %>%
      state_choropleth(legend="percent black")
  })
  output$aplot <- renderPlot({
    ued %>% dplyr::rename(region=State, value=percent_asian) %>%
      state_choropleth(legend="percent asian")
  })
  output$hplot <- renderPlot({
    ued %>% dplyr::rename(region=State, value=percent_hispanic) %>%
      state_choropleth(legend="percent hispanic")
  })
  
  # unemployment rate map
  output$unempPlot <- renderPlot({
    ued %>% dplyr::rename(region=State ,value=Rate) %>%
      state_choropleth(title="October 2016 State Unemployment Rates in the U.S.",
                       legend="Unemployment Rates(%)")
  })
  
})


# Run the application 
shinyApp(ui = ui, server = server)


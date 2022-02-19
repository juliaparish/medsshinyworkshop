# load packages ----
library(shiny)
library(palmerpenguins)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(bslib)

# load data -----
temp_summary <- readRDS("data/temp_month_summary.rds")

# create UI ----
ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "minty"),
  
  navbarPage(
    "Palmer Station Antarctica Penguins: Data Exploration",
    tabPanel("Background",
             em("PENGUIN PHOTOS HERE")),
    tabPanel("Penguin Visualizations",
             tabsetPanel(
               tabPanel("Scatterplot",
                        h2("Here are plots..."),
                  
                        # body mass slider input ----
                        sliderInput(inputId = "body_mass",    # Id 'I' has to be capitalized
                                    label = "Select a range of body masses (g):",
                                    min = 2700, max = 6300, value = c(3000, 4000)), # can select to columns/rows vs hard code
                        
                        # body mass output ----
                        plotOutput(outputId = "bodyMass_scatterPlot")),
               
               tabPanel("Histogram",
                        h2("Here is a histogram"),
                        
                        # island input ----
                        pickerInput(inputId = "island", label = "Select an island:",
                                    choices = c("Torgersen", "Dream", "Biscoe"),
                                    selected = c("Torgersen", "Dream", "Biscoe"),
                                    multiple = TRUE,
                                    options = pickerOptions(actionsBox = TRUE)),
                        
                        # flipper length plot output ----
                        plotOutput(outputId = "flipperLength_hist")))),
               
    tabPanel("Palmer Station Weather",
             em("some widget to explore weather data here")),
    tabPanel("Data exploration",
             tabsetPanel(
               tabPanel("Penguin Data",
                        em("penguin DT here")),
               tabPanel("Palmer Station Weather Data",
                        em("weather DT here"))))
  # # app title ----
  # tags$h1("Penguin App"), # alt format: h1("text here")
  # 
  # # app subtitle ----
  # p(strong("This is an app to explore Palmer Penguin data.")), # use html code
  # 
  # penguin data table output ----
 # DT::dataTableOutput(outputId = "penguin_data")

)) # function to adjust screen size, etc. 



# break -----




# server instructions ----
server <- function(input, output){
  
  # filter body masses ----
  body_mass_df <- reactive({ # create reactive df
    penguins %>% 
      filter(body_mass_g %in% input$body_mass[1]:input$body_mass[2]) # call UI silder input value
  })
  
  # filter island data ----
  island_df <- reactive({
    
    validate(
      need(length(input$island) > 0, "Please choose at least one island to visualize.")
    )
    
    penguins %>% 
      filter(island == input$island)
  })
  
  # render scatterplot ----
  output$bodyMass_scatterPlot <- renderPlot({ 
    
  # code to generate scatterplot goes here
  ggplot(na.omit(body_mass_df()),  # have to call df with ()
           aes(x = flipper_length_mm, y = bill_length_mm, 
               color = species, shape = species)) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Adelie" = "#FEA346", 
                                    "Chinstrap" = "#B251F1", 
                                    "Gentoo" = "#4BA4A4")) +
      scale_shape_manual(values = c("Adelie" = 19, "Chinstrap" = 17, "Gentoo" = 15)) +
      labs(x = "Flipper length (mm)", y = "Bill length (mm)", 
           color = "Penguin species", shape = "Penguin species") +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.2),
            legend.background = element_rect(color = "white"))
    })
  
  # render the flipper length histogram ----
  output$flipperLength_hist <- renderPlot({
    ggplot(na.omit(island_df()), aes(x = flipper_length_mm, fill = species)) +
      geom_histogram(alpha = 0.7) +
      scale_fill_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo" = "#4BA4A4")) +
      labs(x = "Flipper length (mm)", y = "Frequency", 
           fill = "Penguin species") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.background = element_rect(color = "white"))
  })
  
  # render the penguins data table ----
  output$penguin_data <- DT::renderDataTable({
    DT::datatable(penguins,
                  options = list(pageLength = 5),
                  caption = tags$caption(
                    style = 'caption-side: top; text-align: left;',
                    'Table 1: ', tags$em('Size measurements for adult foraging penguins near Palmer Station, Antarctica'
                    )))
    
  })
    
}

# combine UI and server into an app ----
shinyApp(ui = ui, server = server)

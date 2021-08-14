library(shiny)
library(tidyverse)
library(shiny)
library(here)
library(janitor)
library(tmap)
library(png)
library(bslib)
library(jpeg)
library(leaflet)

ui <- fluidPage(includeCSS(here("www", "theme.css")),
    
    navbarPage("TMMC Marine Mammal Strandings",
               tabPanel("About This App",
                        sidebarLayout(
                            mainPanel(p("Welcome to My Marine Mammal App!", style = "font-size:24px"),
                                      p("This app helps visualize The Marine Mammal Center's stranding data for Cetaceans, California Sea Lions, Guadalupe Fur Seals, Northern Fur Seals, Stellar Sea Lions, Elephant Seals, and Harbor Seals."),
                                      p("The data was downloaded from the Axiom Data Portal: California Marine Mammal Health.")),
                            sidebarPanel(img(src = "hamachi_csl.jpg", height = 200, width = 230))
                        )),
               tabPanel("Stranding Causes Yearly",
                        sidebarLayout(
                            sidebarPanel("Stranding Causes Yearly",
                                         selectInput(inputId = "pick_issue",
                                                     label = "Choose Cause",
                                                     selected = "Malnutrition",
                                                     multiple = TRUE,
                                                     choices = unique(mm_tmmc$health_category)),
                                         p("Click above to select a stranding cause to be reflected in the graph. You can always click on a selected cause and press backspace to remove it from consideration.", style = "color:black"),
                                         checkboxGroupInput(inputId = "pick_species",
                                                            label = "Pick Species",
                                                            selected = "California Sea Lion",
                                                            choices = unique(mm_tmmc$species)),
                                         p("Check and uncheck boxes above to add and remove marine mammal species from the graph.", style = "color:black")
                            ),
                            mainPanel(plotOutput("issue_plot"),
                                      p("Figure 1: The total counts of stranded marine mammals over time due to a specific cause, as determined by The Marine Mammal Center."))
                        )),
               tabPanel("Stranding Causes Monthly",
                        sidebarLayout(
                            sidebarPanel("Stranding Causes Monthly",
                                         selectInput(inputId = "pick_issue_2",
                                                     label = "Choose Cause",
                                                     selected = "Malnutrition",
                                                     multiple = TRUE,
                                                     choices = unique(mm_tmmc$health_category)),
                                         p("Click above to select a stranding cause to be reflected in the graph. You can always click on a selected cause and press backspace to remove it from consideration.", style = "color:black"),
                                         checkboxGroupInput(inputId = "pick_species_2",
                                                            label = "Pick Species",
                                                            selected = "California Sea Lion",
                                                            choices = unique(mm_tmmc$species)),
                                         p("Check and uncheck boxes above to add and remove marine mammal species from the graph.", style = "color:black")
                                     ),
                        mainPanel(plotOutput("issue_plot_2"))
                        )),
               tabPanel("Map",
                        sidebarLayout(
                            sidebarPanel("Map of TMMC Strandings",
                                         selectInput(inputId = "pick_year",
                                                     label = "Pick Year",
                                                     selected = "2005",
                                                     multiple = TRUE,
                                                     choices = map_mm$year),
                                         p("Click above to select a year to be reflected on the map. You can always click on a selected cause and press backspace to remove it from consideration.", style = "color:black"),
                                         selectInput(inputId = "pick_reason",
                                                     label = "Select Stranding Cause",
                                                     selected = "Biotoxin",
                                                     multiple = TRUE,
                                                     choices = unique(map_mm$health_cat)),
                                         p("Click above to select a stranding cause to be reflected in the graph. You can always click on a selected cause and press backspace to remove it from consideration.", style = "color:black"),
                                         checkboxGroupInput(inputId = "choose_species",
                                                            label = "Choose Species",
                                                            selected = "California Sea Lion",
                                                            choices = unique(map_mm$species)),
                                         p("Check and uncheck boxes above to add and remove marine mammal species from the map.", style = "color:black")
                            ),
                            mainPanel(leafletOutput("map"))
                            )),
               tabPanel("Averages",
                        sidebarLayout(
                            sidebarPanel("Stranding Monthly Averages",
                                         checkboxGroupInput(inputId = "choose_species_2",
                                                            label = "Pick Species",
                                                            selected = c("California Sea Lion", "Cetacean"),
                                                            choices = unique(averages$species))
                            ),
                            mainPanel(plotOutput("average_plot"))
                        ))
               ))



server <- function(input, output) {
    
    issue_reactive <- reactive({
        
        mm_tmmc %>% 
            filter(health_category %in% input$pick_issue) %>% 
            filter(species %in% input$pick_species) %>% 
            group_by(year) %>% 
            count()
    })
    
    output$issue_plot <- renderPlot({
        ggplot(data = issue_reactive(), aes(x = year, 
                                            y = n)) +
            geom_col(fill = "lightblue") +
            theme_minimal() +
            labs(x = "Year",
                 y = "Count",
                 title = "Count of Marine Mammals by a Specific Cause of Stranding")
    })
    
    issue_reactive_2 <- reactive({
        
        mm_tmmc %>% 
            filter(health_category %in% input$pick_issue_2) %>% 
            filter(species %in% input$pick_species_2) %>% 
            group_by(year, month) %>% 
            count() %>% 
            mutate(month = case_when(
                month == "01" ~ "Jan",
                month == "02" ~ "Feb",
                month == "03" ~ "Mar",
                month == "04" ~ "Apr",
                month == "05" ~ "May",
                month == "06" ~ "Jun",
                month == "07" ~ "Jul",
                month == "08" ~ "Aug",
                month == "09" ~ "Sep",
                month == "10" ~ "Oct",
                month == "11" ~ "Nov",
                month == "12" ~ "Dec"
            ))
    })
    
    output$issue_plot_2 <- renderPlot({
        ggplot(data = issue_reactive_2(), aes(x = factor(month, level = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), 
                                            y = n)) +
            geom_col(fill = "lightblue") +
            theme_minimal() +
            labs(x = "Month",
                 y = "Count",
                 title = "Count of Marine Mammals by a Specific Cause of Stranding")
        })
    
    map_reactive <- reactive({
        
        map_mm %>% 
            filter(year %in% input$pick_year) %>% 
            filter(species %in% input$choose_species) %>% 
            filter(health_cat %in% input$pick_reason)
        
    })
    
    x <- colorFactor(c("blue", "green", "orange", "black", "purple", "red", "yellow", "deeppink"), domain = c("California Sea Lion", "Harbor Seal", "Elephant Seal", "Northern Fur Seal", "Stellar Sea Lion", "Guadalupe Furl Seal", "Cetacean", "NA"))
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles("Esri.OceanBasemap") %>%
            addCircleMarkers(data = map_reactive(), color = ~x(species))
    })
    
    average_reactive <- reactive({
        
        averages %>% 
            filter(species %in% input$choose_species_2)
        
    })
    
    output$average_plot <- renderPlot({
        
        ggplot(data = average_reactive(),aes(x = month,
                                           y = average)) +
            geom_col(aes(fill = species)) +
            theme_minimal() +
            labs(x = "Month",
                 y = "Average Count")
        
    })
    
}

shinyApp(ui = ui, server = server)

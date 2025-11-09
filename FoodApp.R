library(shiny)
library(bslib)
library(bsicons)

library(ggplot2)
library(tidycensus)
library(tigris)
library(leaflet)

# Define UI
ui <- navbarPage(
  "Food Insecurity in SFL",
  
  tabPanel("Race Data",
           sidebarLayout(
             sidebarPanel(
               
               selectInput(
                 "race1",
                 "Choose a race:",
                 choices = c("All", "White", "Black", "Hispanic", "Asian"),
                 selected = "All"
               ),
               
               selectInput(
                 "metric1",
                 "Choose a metric:",
                 choices = c("Low Access"),
                 selected = "Low Access"
               )
             ),
             
             mainPanel(
               textOutput("selected_race_1"),
               textOutput("selected_metric_1"),
               plotOutput("plot_1")
             )
           )
  ),
  
  ###---PANEL 2---###
  
  tabPanel("Mapping",
           sidebarLayout(
             sidebarPanel(
               
               selectInput(
                 "dist2",
                 "Select Distance:",
                 choices = c("1/2", "1", "10"),
                 selected = "1"
               ),
               
               sliderInput(
                 inputId = "car",
                 label = "Shade by % of Households with Cars:",
                 min = 0,
                 max = 50,
                 value = 0
               )
               
             ),
             mainPanel(
               textOutput("selected_dist_2"),
               tableOutput("values"),
               leafletOutput("map2")
             )
           )
  ),
  tabPanel("Logistic/Loglinear Modeling")
)


# Define server logic ----
server <- function(input, output) {
  
  output$selected_race_1 <- renderText({
    paste("Race selected:", input$race1)
  })
  
  output$selected_metric_1 <- renderText({
    paste("Metric selected:", input$metric1)
  })

  ###------------------------------------------------------------------------###
  
  output$map2 <- renderLeaflet({
    
    dict <- data.frame(
      name = c("1/2", "1", "10"),
      var = c("LATracts_half", "LATracts1", "LATracts10")
    )
    distance <- dict$var[dict$name == input$dist2]
    
    # MEDIAN HOME VALUE with Geometry
    flMedvG <- get_acs(geography = "tract", year=year,
                       state = "FL", 
                       county = c("Miami-Dade", "Broward", "Palm Beach"),
                       variables = "B25077_001E", 
                       geometry = TRUE)%>%
      mutate(GEOID=as.numeric(GEOID))
    
    # Join Spatial with DF
    joinFood<-geo_join(spatial_data=flMedvG , data_frame=food_sfl, 
                       by_sp='GEOID', by_df='GEOID')
    
    ## CREATE A POPUP MESSAGE
    popup<-paste("Tract: ", as.character(substring(joinFood$GEOID, 6, 11)), "<br>",
                 "Proportion on SNAP", as.character(joinFood$Prop_SNAP), "<br>",
                 "Population: ", as.character(joinFood$OHU2010))
    
    ### QUANTILE COLORS
    qpal <- colorFactor(
      palette = c("grey90", "darkolivegreen3"),  # or any two colors you want
      domain = joinFood[[distance]]
    )    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = joinFood,
                  fillColor = ~qpal(get(distance)),
                  fillOpacity = ~ifelse(as.numeric(TractHUNV) / OHU2010 > (input$car/100), 1, 0.2),,
                  color = "grey",
                  opacity = 0.5,
                  weight = 0.4,
                  smoothFactor = 0.2,
                  popup = popup) %>%
      addLegend("bottomright",
                pal = qpal,
                values = joinFood[[distance]],
                opacity = 0.7,
                title = paste("Low Access (", input$dist2, " mi)", sep = ""))
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)


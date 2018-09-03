library(shiny)
library(leaflet)
library(dplyr)
library(maps)

mapCountries <- map("world", fill = TRUE, plot = FALSE)

# read earthquake data
data <- read.csv("earthquake_all.csv")

# convert date into factor(year)
data$time <- as.Date(as.character(data$time))
data$year <- as.factor(format(data$time, format="%Y"))

data <- data[!is.na(data$mag) & data$mag > 0 & data$mag < 8,]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          sliderInput("intensity", "Select EarthQuake Intensity",
                      min = 0, max = 8, value = c(0, 8)),
          checkboxGroupInput("years", "Select at least one year", c("2017", "2018"),c("2017", "2018")),
          plotOutput("histPlot")
          

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("mymap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        selectedData <- reactive({
        val <- input$intensity
        data <- data[data$mag > val[1] & data$mag < val[2], ]
        data[data$year %in% input$years,]
            })
    
    # main map
    output$mymap <- renderLeaflet({
        mybins=seq(input$intensity[1], input$intensity[2], by=1)
        mypalette = colorBin(palette="RdYlGn", domain=selectedData()$mag, na.color="transparent", bins=mybins, reverse=TRUE)
        
        # Prepar the text for the tooltip:
        mytext=paste("Depth: ", selectedData()$depth, "<br/>", "Stations: ", selectedData()$stations, "<br/>", "Magnitude: ", selectedData()$mag, sep="") %>%
            lapply(htmltools::HTML) 

        leaflet(data = mapCountries) %>% 
            addTiles() %>%
            setView(lat=-0, lng=0 , zoom=2) %>%
            addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE, label=paste(mapCountries$names)) %>%
            addCircleMarkers(selectedData()$longitude, selectedData()$latitude, 
                             fillColor = ~mypalette(selectedData()$mag), 
                             fillOpacity = 0.7, 
                             color="white", radius=selectedData()$mag^1.3, stroke=FALSE,
                             label = mytext,
                             labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
            ) %>%
            addLegend( pal=mypalette, values=~selectedData()$mag, opacity=0.9, title = "Magnitude", position = "bottomright" )

    })
    
    output$histPlot <- renderPlot({
        ggplot(selectedData(), aes(mag, fill = year)) + geom_histogram(binwidth = 0.25)
    })
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Beer Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            fileInput("file_1", "Upload Beer Data"),
            radioButtons("graph_type", "ABV & IBU Graph Type:",
                         c("Histogram" = "histo",
                           "Box Plot" = "box_plot")),
            radioButtons("scatter_type", "ABV vs IBU Scatter Plot:",
                         c("No Regression" = "no_reg",
                           "Regression" = "reg"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ibuPlot"),
           plotOutput("abvPlot"),
           plotOutput("scatter_plot"),
           plotOutput("pie_chart")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    values <- reactiveValues(beer_data=NULL)
    
    queried_beer_data <- reactive(
        values$beer_data <- read_csv(input$file_1$datapath)
    )
    
    output$ibuPlot <- renderPlot({
        beer_data <- queried_beer_data()
        if(is.null(beer_data))
            return(NULL)
        # generate bins based on input$bins from ui.R
        ibu_data <- beer_data$IBU
        
        # Use a switch to set the values for the radio buttons
        graph_style <- switch(input$graph_type,
                              histo = hist(ibu_data,col = 'darkgray', border = 'white'),
                              box_plot = boxplot(ibu_data),
                              hist)
        # draw the histogram with the specified number of bins
        graph_style

    })
    
    output$abvPlot <- renderPlot({
        beer_data <- queried_beer_data() 
        if(is.null(beer_data))
            return(NULL)
        

        # generate bins based on input$bins from ui.R
        abv_data <- beer_data$ABV
        abv_min <- 0
        abv_max <- 50
        bins <- seq(abv_min, abv_max, length.out = input$bins + 1)
        
        # Use a switch to set the values for the radio buttons
        graph_style <- switch(input$graph_type,
                              histo = hist(abv_data,col = 'darkgray', border = 'white'),
                              box_plot = boxplot(abv_data),
                              hist)
        # draw the histogram with the specified number of bins
        graph_style
            
    })
    
    output$scatter_plot <- renderPlot({
        beer_data <- queried_beer_data()
        if(is.null(beer_data))
            return(NULL)
        
        abv_data <- beer_data$ABV
        ibu_data <- beer_data$IBU
        
        # Use a switch to set the values for the radio buttons
        graph_style <- switch(input$scatter_type,
                              no_reg = plot(abv_data, ibu_data),
                              reg = plot(abv_data, ibu_data, abline(lm(IBU ~ ABV, data = beer_data), col = "blue")),
                              no_reg)
        
        graph_style
        
    })
    
    output$pie_chart <- renderPlot({
        
        beer_data <- queried_beer_data()
        if(is.null(beer_data))
            return(NULL)
        
        abv_data <- beer_data$ABV
        ibu_data <- beer_data$IBU
        
        pie(table(beer_data$Style))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

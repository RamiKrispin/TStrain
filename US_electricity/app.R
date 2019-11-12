library(shiny)
library(USgrid)
library(plotly)
library(RColorBrewer)
data("Cal_elec")

palette <- rownames(RColorBrewer::brewer.pal.info)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("California Independent System Operator "),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("palette",
                        "Select the Color Palette:",
                        choices = palette,
                        selected = palette[11]),
            sliderInput("width",
                        "Select the Line Width",
                        min = 0,
                        max = 3,
                        step = 0.1,
                        value = 0.5)
        ),

        
        mainPanel(
           plotlyOutput("plot")
        )
    )
)

server <- function(input, output) {

    output$plot <- renderPlotly({
        plot_ly(data = US_source,
                x = ~ date_time,
                y = ~ series,
                color = ~ source,
                colors = input$palette,
                line = list(width = input$width),
                type = "scatter",
                mode = "lines") %>%
            layout(title = "US Electricity Generation by Source",
                   yaxis = list(title = "Mwh"),
                   xaxis = list(title = "Source: US Energy Information Administration (Nov 2019)"))
        
    })
}

shinyApp(ui = ui, server = server)

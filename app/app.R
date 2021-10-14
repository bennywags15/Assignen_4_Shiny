#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
# If you need to install my gardenR library
library(devtools)
library(tidyverse)
library(DT) # for table output
library(rsconnect)


mls_team <- read_csv(file = 'mls_team.csv')
teams <- mls_team %>% 
    distinct(Team) %>% 
    arrange(Team) %>% 
    pull(Team)

stats <- as.data.frame(t(t(colnames(mls_team[5:15]))))
stats <- stats %>% 
    rename("Stats:" = "V1")

ui <- fluidPage(theme = bs_theme(primary = "#0e507d", 
                                 secondary = "#D44420", 
                                 base_font = list(font_google("Raleway"), "-apple-system", 
                                                  "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                                  "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                                  "Segoe UI Symbol"), 
                                 bootswatch = "sandstone"),

    # Application title
    titlePanel("MLS Team Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "stat", # to use in code
                        label = "Stat:", # how it looks in UI
                        choices = stats, 
                        selected = "ShtF"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "team_stat"),
            dataTableOutput(outputId = "team_stat_tbl")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    team_smry <- reactive(mls_team %>% 
        #filter(Stat == input$stat) %>% 
        group_by(Team))
    # Now use that function, with no arguments.
    output$team_stat <- renderPlot({
        team_smry() %>% 
            ggplot(aes(x = Team, y = .data[[input$stat]])) +
            geom_col(fill= "light blue")+
            labs(title = paste(input$stat, "for each Team"),
                 x = "",
                 y = "") +
            theme_minimal()
    })
    
    output$team_stat_tbl <- renderDataTable(team_smry())
}


# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Let's check some exam clashes!"),
    
    # Let's us set the layout; we will have a sidebar panel and a main panel
    sidebarLayout(
        
    sidebarPanel(fileInput(inputId = "file", label = "Choose exam namelist file"),
                 textInput(inputId = "subject1", label = "Indicate the first subject"),
                 textInput(inputId = "subject2", label = "Indicate the second subject")),
    
    # This is where we place our output (i.e., the exam clash table)
    mainPanel(tableOutput(outputId = "joined_table")
              
        )
    
    )
    
)
# Define server logic to receive file
server <- function(input, output) {
    mydata <- reactive({
        inFile <- input$file
        if(is.null(inFile)) return(NULL)
        dat <- read_excel(inFile$datapath, col_names = FALSE) %>% 
            select("id" = 2, "name" = 3, "subject" = 4) %>% 
            mutate(id = toupper(id),
                   name = toupper(name)) %>% 
            # Let's get rid of some unnecesary rows
            # We retain only rows with legitimate student IDs (i.e., they start with B/C/E)
            filter(str_detect(string = id, pattern = "^[B|C|E|b|c|e]"))
        
        df1 <- dat %>% 
            filter(subject == input$subject1)
        df2 <- dat %>% 
            filter(subject == input$subject2)
        df_joined <- inner_join(df1, df2, by = c("id", "name"))
        
        return(df_joined)
})
    

    output$joined_table <- renderTable({
        print(mydata())

    })

}


# Run the application 
shinyApp(ui = ui, server = server)


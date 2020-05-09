#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(highcharter)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Yearly Developer Salary"),

 

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        dfdata %>% filter(Employment %in% 'Employed full-time') %>% 
            filter(Gender %in% c('Male','Female')) %>% 
            group_by(Age) %>% 
            mutate(n = n()) %>% 
            summarise(med_sal = median(ConvertedSalary, na.rm = T)) %>% 
            arrange(med_sal) %>% 
            select(Age) %>% mutate(Age = factor(Age)) -> avg_salary
        options(scipen=999)
        dfdata %>% filter(Employment %in% 'Employed full-time') %>% 
            filter(Gender %in% c('Male','Female')) %>% 
            group_by(Age) %>% 
            mutate(n = n()) %>% 
            #arrange(desc(ConvertedSalary)) %>% 
            ungroup(Age) %>% 
            ggplot() +
            geom_boxplot(aes(Age,ConvertedSalary, fill = Gender))  +
            scale_x_discrete(limits = avg_salary$Age) +
            coord_flip() +
            #facet_grid('Gender') +
            scale_y_log10() + 
            labs(x = "Age", 
                 y = "Log of Annual Salary in USD",
                 title = "Annual Salary in USD - Male vs Female - by Age")
    })
}



# Run the application 
shinyApp(ui = ui, server = server)

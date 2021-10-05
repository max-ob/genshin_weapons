
library(shiny)
library(shiny)
library(ggplot2)
library(dplyr)
weapons <- read.csv("weapons.csv", header = T)

# UI
ui <- fluidPage(
    fluidRow(htmlOutput("text"),
             selectInput("weapon_type", "", choices = unique(weapons$Type)), selected = "Sword"),
    plotOutput("plot"),
    
    fluidRow(htmlOutput("text2")),
    fluidRow(column(6, selectInput("weapon_1", "", choices = weapons$Name)),
             column(6, selectInput("weapon_2", "", choices = weapons$Name, selected = "Lost Prayer to the Sacred Winds"))),
    
    selectInput("comparing", "What do you want to see?", choices = c("L1_ATK", "L90_ATK", "L1_2nd_stat", "L90_2nd_stat")),
    fluidRow(column(6, plotOutput("compare_plot")),
             column(6, htmlOutput("Comparing_descrip")))
)

# Server
server <- function(input, output, session) {
    output$text <- renderText({paste0("<b>Compare All Weapons:</b>")})
    
    output$plot <- renderPlot(ggplot(subset(weapons, Type == input$weapon_type), 
                                     mapping = aes(x=Name, y=L90_ATK, fill = Sub.Stat))+ 
                                  geom_bar(stat = "identity") + 
                                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10), 
                                        plot.title = element_text(face='bold', hjust = 0.5)) +
                                  xlab(" ") + ylab("Base Attack (Level 90)") + 
                                  ggtitle(input$weapon_type)+ labs(fill = "Sub stat"))
    
    output$text2 <- renderText({paste0("<b>Compare Two Weapons:</b>")})
    
    
    output$compare_plot <- renderPlot(ggplot(filter(weapons, Name %in% c(input$weapon_1, input$weapon_2)), 
                                             mapping = aes(x=Name, fill = as.factor(Rarity)))+
                                          geom_bar(stat = "identity", aes_string(y= input$comparing))+
                                          theme(axis.text=element_text(size=12)) +
                                          xlab("Weapons") + ylab(" ") + labs(fill = "Rarity")) 
    
    
    w1_details <- reactive(as.character(weapons %>% filter(Name==input$weapon_1)))
    w2_details <- reactive(as.character(weapons %>% filter(Name==input$weapon_2)))
    
    output$Comparing_descrip <- renderText({paste0("<i><b>", w1_details()[1], "</b></i><br>",
                                                   "<b>Sub stat:</b> ", w1_details()[6], "<br>",
                                                   "<b>Passive Ability:</b> ", w1_details()[9], "<br><br>",
                                                   
                                                   "<i><b>", w2_details()[1], "</b></i><br>",
                                                   "<b>Sub stat:</b> ", w2_details()[6], "<br>",
                                                   "<b>Passive Ability:</b> ", w2_details()[9])})
}

# Run the application 
shinyApp(ui = ui, server = server)



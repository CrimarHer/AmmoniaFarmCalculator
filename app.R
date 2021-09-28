#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)

#read the data

df <- read.csv("hackathon.csv")
# nodes <- data.frame(id = 1:12, 
#                shape = "image",
#                image = paste0(df[,c("images")]),
#                label = df[,c("Farm.System")],
#                amm_Kg = df[,c("Kg..farm.of.ammonia..as.NH3.")]
# )


edges <- data.frame(from = c(1), to = c(1))

# Define UI for application that draws a histogram
ui <-  dashboardPage(
  
  

  dashboardHeader(title = "VisApp"),
   
  
   
   # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    accordion(
      id = "accordion_plus",
      accordionItem(
        title = "Add Items",
        
        collapsed = TRUE,
        actionButton(inputId = "plus_combinable_crops", 
                     
                     label = img (src="plus_wheat.png", width="150", height="150"))
        
      )
    ),
    accordion(
      id = "accordion_minus",
      accordionItem(
        title = "Remove Items",
        
        collapsed = TRUE,
        
        actionButton(inputId = "minus_combinable_crops",
                     
                     label = img (src="minus_wheat.png", width="150", height="150"))
      )
    )#,
     # actionButton(inputId = "plus_combinable_crops", 
     #               
     #              label = img (src="plus_wheat.png", width="150", height="150")),
     # actionButton(inputId = "minus_combinable_crops",
     # 
     #              label = img (src="minus_wheat.png", width="150", height="150"))
  ),
      # Show a plot of the generated distribution
  dashboardBody(
    tags$head(tags$style(HTML("#maindivnetwork{
                    width: 100%;
                    height: 500px;

                    background-size: 100% 100%;
                    border: 2px solid #e9385a;
                    background-image: url('farm-background_white.jpg');}"))
    ),
        visNetworkOutput("network", width = "100%", height = "500px"),
        textOutput("calculation_amm")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  formula <- reactiveValues(nodes=data.frame(id = 1:12,
                                             shape =  "image",
                                             image =  paste0(df[,c("images")]),
                                             label = df[,c("Farm.System")],
                                             amm_Kg =  df[,c("Kg..farm.of.ammonia..as.NH3.")]))
  # formula <- reactiveValues({ nodes = data.frame(
  #   id = 1:12,
  #   shape =  "image",
  #   image =  paste0(df[,c("images")]),
  #   label = df[,c("Farm.System")],
  #   amm_Kg =  df[,c("Kg..farm.of.ammonia..as.NH3.")]
  # )
    
 # })
  
  observe({
  
  output$network <- renderVisNetwork({
    
   
    visNetwork(nodes = formula$nodes, edges)  #%>% visInteraction(selectConnectedEdges = FALSE, selectable = TRUE)%>% visIgraphLayout (smooth = T,layout = "layout_in_circle") %>% #https://igraph.org/r/doc/layout_.html
      
      #visNodes(size = 30, font = list(size = 25)) %>% visEdges(width = 3, selectionWidth = 6) #%>%  visOptions(highlightNearest = list(enabled = T, degree = list(from = 1, to = 1), hover = T, algorithm= "hierarchical")) 
})
  #Add a combinable crops item
  observeEvent(input$plus_combinable_crops,{
     newnode <- data.frame(id = nrow(formula$nodes) +1,
                           shape = "image",
                         image = "wheat.png",
                         label = "combinable crops",
                         amm_Kg = "1408.571429"
    )
    formula$nodes <- rbind(formula$nodes, newnode)
   
    visNetworkProxy("network", session = session) %>% 
      visUpdateNodes(formula$nodes)
  })
  
  observeEvent(input$minus_combinable_crops,{
    # newnode <- data.frame(id = nrow(nodes) +1,
    #                       shape = "image",
    #                       image = "wheat.png",
    #                       label = "combinable_crops"
    # )
    removed <- formula$nodes %>% filter((label == "combinable crops"))#nodes %>% filter((label == "combinable crops" & row_number() == 1))
    removed <- removed[,"id" ][1]
    formula$nodes <- formula$nodes %>% filter(!id == removed)
    visNetworkProxy("network", session = session) %>% 
      visRemoveNodes(removed)
  })
  
  observeEvent(formula$nodes,{
  output$calculation_amm <- renderText({paste("The result is =", sum(as.numeric(formula$nodes$amm_Kg)))})
  })

  })#end of observe  
}


# Run the application 
shinyApp(ui = ui, server = server)


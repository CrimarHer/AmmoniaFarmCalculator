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
library(stringr)

#read the data
#C:/Users/crimar/Documents/Repositories/Proposal/TrialVis/
df <- read.csv("hackathon.csv")
# nodes <- data.frame(id = 1:12, 
#                shape = "image",
#                image = paste0(df[,c("images")]),
#                label = df[,c("Farm.System")],
#                amm_Kg = df[,c("Kg..farm.of.ammonia..as.NH3.")]
# )
mitigation <- read.csv("mitigation.csv")

edges <- data.frame(from = c(13), to = c(13))
edgesmitigation <- data.frame(from = c(6), to = c(6))
#create a transparent node that is always there and the edges go to it.

# Define UI for application that draws a histogram
ui <-  fluidPage(
  
  # App title ----
  titlePanel("Ammonia Calculator Tool"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel (),
     
    #,
     # actionButton(inputId = "plus_combinable_crops", 
     #               
     #              label = img (src="plus_wheat.png", width="150", height="150")),
     # actionButton(inputId = "minus_combinable_crops",
     # 
     #              label = img (src="minus_wheat.png", width="150", height="150"))
 
      # Show a plot of the generated distribution
  mainPanel(width = 12, #background-size: 100% 100%;
    # tags$head(tags$style(HTML("#maindivnetwork{
    #                 width: 100%;
    #                 height: 50px;
    #                 background-size: 100% 100%;
    #                 
    #                 border: 2px solid black;
    #                 background-image: url('background_white_50.png');}"))
    # ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    htmlOutput("calculation_amm"),  
    tabsetPanel(type = "tabs",
                tabPanel("Your Farm",
                         column(2, 
                                
                                tags$div(class = "buttons_div",
                                         tags$h3("Combinable Crops"), 
                         actionButton(inputId = "plus_combinable_crops", 
                                      
                                      label = img (src="CombinableCropsPLUS.png", width="70", height="70")),
                         
                         
                         
                         actionButton(inputId = "minus_combinable_crops",
                                      
                                      label = img (src="CombinableCropsMINUS.png", width="70", height="70"))
            
                                ), 
            tags$div(class = "buttons_div",
                     tags$h3("combinable + pig manure"), 
                     actionButton(inputId = "plus_Combinable_Crops_PigManure", 
                                  
                                  label = img (src="CombinableCropsPigManurePLUS.png", width="70", height="70")),
                     
                     
                     
                     actionButton(inputId = "minus_Combinable_Crops_PigManure",
                                  
                                  label = img (src="CombinableCropsPigManureMINUS.png", width="70", height="70"))
            ), 
            tags$div(class = "buttons_div",
                     tags$h3("Roots/combinable crops"), 
                     actionButton(inputId = "plus_RootsCombinableCrops", 
                                  
                                  label = img (src="RootsCombinableCropsPLUS.png", width="70", height="70")),
                     
                     
                     
                     actionButton(inputId = "minus_RootsCombinableCrops",
                                  
                                  label = img (src="RootsCombinableCropsMINUS.png", width="70", height="70"))
            )),#column,
            column(2, 
                   
                   tags$div(class = "buttons_div",
                            tags$h3("Dairy"), 
                            actionButton(inputId = "plus_Dairy", 
                                         
                                         label = img (src="DairyPLUS.png", width="70", height="70")),
                            
                            
                            
                            actionButton(inputId = "minus_Dairy",
                                         
                                         label = img (src="DairyMINUS.png", width="70", height="70"))
                            
                   ), 
                   tags$div(class = "buttons_div",
                            tags$h3("Grazing - lowland"), 
                            actionButton(inputId = "plus_GrazingLowland", 
                                         
                                         label = img (src="GrazingLowlandPLUS.png", width="70", height="70")),
                            
                            
                            
                            actionButton(inputId = "minus_GrazingLowland",
                                         
                                         label = img (src="GrazingLowlandMINUS.png", width="70", height="70"))
                   ), 
                   tags$div(class = "buttons_div",
                            tags$h3("Grazing - LFA"), 
                            actionButton(inputId = "plus_GrazingLFA", 
                                         
                                         label = img (src="GrazingLFAPLUS.png", width="70", height="70")),
                            
                            
                            
                            actionButton(inputId = "minus_GrazingLFA",
                                         
                                         label = img (src="GrazingLFAMINUS.png", width="70", height="70"))
                   )),#column,
                  column(8,   visNetworkOutput("network", width = "100%", height = "700px"))
            
                     
            
      ),
      tabPanel(
         "Mitigation Measures", 
         p(), 
         p(), 
         column(2, 
         
         tags$div(class = "buttons_div",
                  tags$h3("New Chicken House"), 
         actionButton(inputId = "minus_NewChickenHouse",
                      
                      label = img (src="NewChickenHouseMINUS.png", width="70", height="70")),
         
         actionButton(inputId = "plus_NewChickenHouse",
                      
                      label = img (src="NewChickenHousePLUS.png", width="70", height="70"))
         ), 
         
         tags$div(class = "buttons_div",
                  tags$h3("New Cattle House"), 
         actionButton(inputId = "minus_NewCattleHouse",
                      
                      label = img (src="NewCattleHouseMINUS.png", width="70", height="70")),
         
         actionButton(inputId = "plus_NewCattleHouse",
                      
                      label = img (src="NewCattleHousePLUS.png", width="70", height="70"))
         ), 
         
         tags$div(class = "buttons_div",
                  tags$h3("New Pig House"), 
         actionButton(inputId = "minus_NewPigHouse",
                      
                      label = img (src="NewPigHouseMINUS.png", width="70", height="70")),
         
         actionButton(inputId = "plus_NewPigHouse",
                      
                      label = img (src="NewPigHousePLUS.png", width="70", height="70"))
         )), 
         column(2, 
         
         tags$div(class = "buttons_div",
                  tags$h3("Slurry Injection"), 
         actionButton(inputId = "plus_SlurryInjection",
                      
                      label = img (src="SlurryInjectionPLUS.png", width="70", height="70")),
         actionButton(inputId = "minus_SlurryInjection",
                      
                      label = img (src="SlurryInjectionMINUS.png", width="70", height="70"))
         
      ),
      tags$div(class = "buttons_div",
               tags$h3("Trailing Shoe"), 
               actionButton(inputId = "plus_TrailingShoe",
                            
                            label = img (src="TrailingShoePLUS.png", width="70", height="70")),
               actionButton(inputId = "minus_TrailingShoe",
                            
                            label = img (src="TrailingShoeMINUS.png", width="70", height="70"))
               
      )),
      column(8, 
        visNetworkOutput("mitigation", width = "100%", height = "700px")
      ))
      )))
        
      
   )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  formula <- reactiveValues(nodes=data.frame(id = 1:13,
                                             shape =  "image",
                                             image =  paste0(df[,c("images")]),
                                             label = df[,c("Farm.System")],
                                             amm_Kg =  df[,c("Kg..farm.of.ammonia..as.NH3.")]))
  
  mitig<- reactiveValues(nodes=data.frame(id = 1:6,
                                             shape =  "image",
                                             image =  paste0(mitigation[,c("images")]),
                                             label = mitigation[,c("Farm.System")],
                                             amm_Kg =  mitigation[,c("Kg..farm.of.ammonia..as.NH3.")]))
  
  
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
    
   
    visNetwork(nodes = formula$nodes, edges) %>% visNodes(size = 45, font = "30px", shadow = list(enabled = TRUE, size = 10)) %>%
      visPhysics(solver = "repulsion") %>% visIgraphLayout (randomSeed = 123) %>% visEdges(color = "transparent")#%>% visInteraction(selectConnectedEdges = FALSE, selectable = TRUE)%>% visIgraphLayout (smooth = T,layout = "layout_in_circle") %>% #https://igraph.org/r/doc/layout_.html
      
      #visNodes(size = 30, font = list(size = 25)) %>% visEdges(width = 3, selectionWidth = 6) #%>%  visOptions(highlightNearest = list(enabled = T, degree = list(from = 1, to = 1), hover = T, algorithm= "hierarchical")) 
})
  #Add a combinable crops item
  observeEvent(input$plus_combinable_crops,{
    if (exists("id_combinable_crops")){
    id_combinable_crops <<- paste0("combinable", as.numeric(str_remove(id_combinable_crops, "combinable")) + 1 )
    } else {
      id_combinable_crops <<- paste0("combinable",nrow(formula$nodes) +1)
    }
     newnode <- data.frame(id = id_combinable_crops,
                           shape = "image",
                         image = "CombinableCrops.png",
                         label = "combinable crops",
                         amm_Kg = 1408.571429
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
    if (is.na(removed)){
      return()
    } else{
    formula$nodes <- formula$nodes %>% filter(!id == removed)
    visNetworkProxy("network", session = session) %>% 
      visRemoveNodes(removed)
    }
  })
  
  observeEvent(input$minus_Combinable_Crops_PigManure,{
    
    removed <- formula$nodes %>% filter((label == "combinable \n+ pig manure"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      formula$nodes <- formula$nodes %>% filter(!id == removed)
      visNetworkProxy("network", session = session) %>% 
        visRemoveNodes(removed)
    }
  })
  
  observeEvent(input$minus_RootsCombinableCrops,{
    
    removed <- formula$nodes %>% filter((label == "Roots/ \ncombinable crops"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      formula$nodes <- formula$nodes %>% filter(!id == removed)
      visNetworkProxy("network", session = session) %>% 
        visRemoveNodes(removed)
    }
  })
  
  observeEvent(input$minus_Dairy,{
    
    removed <- formula$nodes %>% filter((label == "Dairy"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      formula$nodes <- formula$nodes %>% filter(!id == removed)
      visNetworkProxy("network", session = session) %>% 
        visRemoveNodes(removed)
    }
  })
  
  observeEvent(input$minus_GrazingLowland,{
    
    removed <- formula$nodes %>% filter((label == "Grazing \nLowland"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      formula$nodes <- formula$nodes %>% filter(!id == removed)
      visNetworkProxy("network", session = session) %>% 
        visRemoveNodes(removed)
    }
  })
  
  observeEvent(input$minus_GrazingLFA,{
    
    removed <- formula$nodes %>% filter((label == "Grazing - \nLFA"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      formula$nodes <- formula$nodes %>% filter(!id == removed)
      visNetworkProxy("network", session = session) %>% 
        visRemoveNodes(removed)
    }
  })
  
  observeEvent(input$minus_NewChickenHouse,{
    
    removed <- mitig$nodes %>% filter((label == "New \nchicken \nhouse"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      mitig$nodes <- mitig$nodes %>% filter(!id == removed)
      visNetworkProxy("mitigation", session = session) %>% 
        visRemoveNodes(removed)
    }
  })
  
  observeEvent(input$minus_NewCattleHouse,{
    
    removed <- mitig$nodes %>% filter((label == "New \ncattle \nhouse"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      mitig$nodes <- mitig$nodes %>% filter(!id == removed)
      visNetworkProxy("mitigation", session = session) %>% 
        visRemoveNodes(removed)
    }
  })

  
  observeEvent(input$minus_NewPigHouse,{
    
    removed <- mitig$nodes %>% filter((label == "New \npig \nhouse"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      mitig$nodes <- mitig$nodes %>% filter(!id == removed)
      visNetworkProxy("mitigation", session = session) %>% 
        visRemoveNodes(removed)
    }
  })
  
  observeEvent(input$minus_SlurryInjection,{
    
    removed <- mitig$nodes %>% filter((label == "Slurry \ninjection"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      mitig$nodes <- mitig$nodes %>% filter(!id == removed)
      visNetworkProxy("mitigation", session = session) %>% 
        visRemoveNodes(removed)
    }
  })
  
  observeEvent(input$minus_TrailingShoe,{
    
    removed <- mitig$nodes %>% filter((label == "Trailing \nshoe"))
    removed <- removed[,"id" ][1]
    if (is.na(removed)){
      return()
    } else{
      mitig$nodes <- mitig$nodes %>% filter(!id == removed)
      visNetworkProxy("mitigation", session = session) %>% 
        visRemoveNodes(removed)
    }
  })
  
  observeEvent(c(formula$nodes,mitig$nodes),{
  output$calculation_amm <- renderUI({HTML(paste0("<p style = 'font-size: 30px; font-weight:bold;'>The Ammonia emmited is ", round(sum(as.numeric(formula$nodes$amm_Kg), as.numeric(mitig$nodes$amm_Kg)), 2), " kg/year</p>"))})
  })
  
  
  observeEvent(input$plus_NewCattleHouse,{
    
    if (exists("id_new_cattle_house")){
      id_new_cattle_house <<- paste0("new_cattle_house", as.numeric(str_remove(id_new_cattle_house, "new_cattle_house")) + 1 )
    } else {
      id_new_cattle_house <<- paste0("new_cattle_house",nrow(mitig$nodes) +1)
    }
    newnode <- data.frame(id = id_new_cattle_house,
                          shape = "image",
                          image = "NewCattleHouse.png",
                          label = "New \ncattle \nhouse",
                          amm_Kg = -1827.5
    )
    mitig$nodes <- rbind(mitig$nodes, newnode)
    
    visNetworkProxy("mitigation", session = session) %>% 
      visUpdateNodes(mitig$nodes)
  })
  
  observeEvent(input$plus_NewChickenHouse,{
    
    if (exists("id_new_chicken_house")){
      id_new_chicken_house <<- paste0("new_chicken_house", as.numeric(str_remove(id_new_chicken_house, "new_chicken_house")) + 1 )
    } else {
      id_new_chicken_house <<- paste0("new_chicken_house",nrow(mitig$nodes) +1)
    }
    newnode <- data.frame(id = id_new_chicken_house,
                          shape = "image",
                          image = "NewChickenHouse.png",
                          label = "New \nchicken \nhouse",
                          amm_Kg = -10752.5

    )
    mitig$nodes <- rbind(mitig$nodes, newnode)
    
    visNetworkProxy("mitigation", session = session) %>% 
      visUpdateNodes(mitig$nodes)
  })
  
  observeEvent(input$plus_NewPigHouse,{
    
    if (exists("id_new_pig_house")){
      id_new_pig_house <<- paste0("new_pig_house", as.numeric(str_remove(id_new_pig_house, "new_pig_house")) + 1 )
    } else {
      id_new_pig_house <<- paste0("new_pig_house",nrow(mitig$nodes) +1)
    }
    newnode <- data.frame(id = id_new_pig_house,
                          shape = "image",
                          image = "NewPigHouse.png",
                          label = "New \npig \nhouse",
                          amm_Kg = -10485.35714

    )
    mitig$nodes <- rbind(mitig$nodes, newnode)
    
    visNetworkProxy("mitigation", session = session) %>% 
      visUpdateNodes(mitig$nodes)
  })
  
  observeEvent(input$plus_SlurryInjection,{
    
    if (exists("id_slurry_injection")){
      id_slurry_injection <<- paste0("slurry_injection", as.numeric(str_remove(id_slurry_injection, "slurry_injection")) + 1 )
    } else {
      id_slurry_injection <<- paste0("slurry_injection",nrow(mitig$nodes) +1)
    }
    newnode <- data.frame(id = id_slurry_injection,
                          shape = "image",
                          image = "SlurryInjection.png",
                          label = "Slurry \ninjection",
                          amm_Kg = -845.1428574
    )
    mitig$nodes <- rbind(mitig$nodes, newnode)
    
    visNetworkProxy("mitigation", session = session) %>% 
      visUpdateNodes(mitig$nodes)
  })
  
  observeEvent(input$plus_TrailingShoe,{
    
    if (exists("id_trailing_shoe")){
      id_trailing_shoe <<- paste0("trailing_shoe", as.numeric(str_remove(id_trailing_shoe, "trailing_shoe")) + 1 )
    } else {
      id_trailing_shoe <<- paste0("trailing_shoe",nrow(mitig$nodes) +1)
    }
    newnode <- data.frame(id = id_trailing_shoe,
                          shape = "image",
                          image = "TrailingShoe.png",
                          label = "Trailing \nshoe",
                          amm_Kg = -422.5714287
                          
    )
    mitig$nodes <- rbind(mitig$nodes, newnode)
    
    visNetworkProxy("mitigation", session = session) %>% 
      visUpdateNodes(mitig$nodes)
  })
  
  observeEvent(input$plus_Combinable_Crops_PigManure,{
    if (exists("id_Combinable_Crops_PigManure")){
      id_Combinable_Crops_PigManure <<- paste0("Combinable_Crops_PigManure", as.numeric(str_remove(id_Combinable_Crops_PigManure, "Combinable_Crops_PigManure")) + 1 )
    } else {
      id_Combinable_Crops_PigManure <<- paste0("Combinable_Crops_PigManure",nrow(formula$nodes) +1)
    }
    newnode <- data.frame(id = id_Combinable_Crops_PigManure,
                          shape = "image",
                          image = "CombinableCropsPigManure.png",
                          label = "combinable \n+ pig manure",
                          amm_Kg = 7164.285714
    )
    formula$nodes <- rbind(formula$nodes, newnode)
    
    visNetworkProxy("network", session = session) %>% 
      visUpdateNodes(formula$nodes)
  })
  
  
  observeEvent(input$plus_RootsCombinableCrops,{
    if (exists("id_RootsCombinableCrops")){
      id_RootsCombinableCrops <<- paste0("RootsCombinableCrops", as.numeric(str_remove(id_RootsCombinableCrops, "RootsCombinableCrops")) + 1 )
    } else {
      id_RootsCombinableCrops <<- paste0("RootsCombinableCrops",nrow(formula$nodes) +1)
    }
    newnode <- data.frame(id = id_RootsCombinableCrops,
                          shape = "image",
                          image = "RootsCombinableCrops.png",
                          label = "Roots/ \ncombinable crops",
                          amm_Kg = 1044.285714
                          
    )
    formula$nodes <- rbind(formula$nodes, newnode)
    
    visNetworkProxy("network", session = session) %>% 
      visUpdateNodes(formula$nodes)
  })
  
  
  observeEvent(input$plus_Dairy,{
    if (exists("id_Dairy")){
      id_Dairy <<- paste0("Dairy", as.numeric(str_remove(id_Dairy, "Dairy")) + 1 )
    } else {
      id_Dairy <<- paste0("Dairy",nrow(formula$nodes) +1)
    }
    newnode <- data.frame(id = id_Dairy,
                          shape = "image",
                          image = "Dairy.png",
                          label = "Dairy",
                          amm_Kg = 5221.428571
                          
    )
    formula$nodes <- rbind(formula$nodes, newnode)
    
    visNetworkProxy("network", session = session) %>% 
      visUpdateNodes(formula$nodes)
  })
  
  
  
  observeEvent(input$plus_GrazingLowland,{
    if (exists("id_GrazingLowland")){
      id_GrazingLowland <<- paste0("GrazingLowland", as.numeric(str_remove(id_GrazingLowland, "GrazingLowland")) + 1 )
    } else {
      id_GrazingLowland <<- paste0("GrazingLowland",nrow(formula$nodes) +1)
    }
    newnode <- data.frame(id = id_GrazingLowland,
                          shape = "image",
                          image = "GrazingLowland.png",
                          label = "Grazing \nLowland",
                          amm_Kg = 1396.428571
                          
    )
    formula$nodes <- rbind(formula$nodes, newnode)
    
    visNetworkProxy("network", session = session) %>% 
      visUpdateNodes(formula$nodes)
  })
  
  observeEvent(input$plus_GrazingLFA,{
    if (exists("id_GrazingLFA")){
      id_GrazingLFA <<- paste0("GrazingLFA", as.numeric(str_remove(id_GrazingLFA, "GrazingLFA")) + 1 )
    } else {
      id_GrazingLFA <<- paste0("GrazingLFA",nrow(formula$nodes) +1)
    }
    newnode <- data.frame(id = id_GrazingLFA,
                          shape = "image",
                          image = "GrazingLFA.png",
                          label = "Grazing \nLowland",
                          amm_Kg = 874.2857143
                          
    )
    formula$nodes <- rbind(formula$nodes, newnode)
    
    visNetworkProxy("network", session = session) %>% 
      visUpdateNodes(formula$nodes)
  })
  

  
  output$mitigation<- renderVisNetwork({
    
    
    visNetwork(nodes = mitig$nodes, edgesmitigation) %>% visNodes(size = 45, font = "30px", shadow = list(enabled = TRUE, size = 10), margin = 20) %>%
   visIgraphLayout (randomSeed = 123, "layout_as_star") %>% visEdges(color = "transparent")#%>% visInteraction(selectConnectedEdges = FALSE, selectable = TRUE)%>% visIgraphLayout (smooth = T,layout = "layout_in_circle") %>% #https://igraph.org/r/doc/layout_.html
    
    #visNodes(size = 30, font = list(size = 25)) %>% visEdges(width = 3, selectionWidth = 6) #%>%  visOptions(highlightNearest = list(enabled = T, degree = list(from = 1, to = 1), hover = T, algorithm= "hierarchical")) 
  })
  
  
  
  })#end of observe  
  

  
}


# Run the application 
shinyApp(ui = ui, server = server)


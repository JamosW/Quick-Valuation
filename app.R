#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
source("modules.R")
source("functions.R")
library(profvis)
library(shinyWidgets)
library(excelR)


# Define UI for application that draws a histogram
ui <- function(title){
  
  fluidPage(
    
    # Application title
    uiOutput("title"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioUI("mdl", "Valuation Type: ", c("DDM", "FCFE", "FCFF")),
        radioUI("stage", "Stages:", c("One", "Two", "Three")),
        fluidRow(
          width = 4,
          textOutput("allo"),
          uiOutput("capm")),
        fluidRow(
          width = 4,
          uiOutput("first")),
        fluidRow(
          width = 4,
          uiOutput("second")),
        fluidRow(
          width = 4,
          uiOutput("third"))),
      mainPanel(
        div(dropdownButton(uiOutput("extras"), circle = TRUE, icon = icon("gear"), width = "300px",
                       tooltip = tooltipOptions(title = "Extra Parameters", placement = "left")), align = "right"),
        tabsetPanel(
          id = "tabset",
          tabPanel("Table", materialSwitch(
            inputId = "tableFormat",
            label = "Excel Format", 
            status = "primary",
            right = TRUE
          ), uiOutput("table"),
          DTOutput("tabl")),
          tabPanel("Present Values", echarts4rOutput("plot")),
          tabPanel("Terminal", echarts4rOutput("terminal")),
          tabPanel("Hypothetical", echarts4rOutput("hypothetical"))
        ))
    ))
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
        
        #based on button, we get the type of Valuation model in str format
        
  
        type <- radioBServer("mdl")
        
        stage <- radioBServer("stage")
        
        
        lapply(list(getVars("mainL"), getVars("extraL"), getVars("intermediateL"), getVars("terminalL"), getVars("CAPM")), \(x){
          reactive({
            switch(type(),
                   "DDM" = get("DDM", as.environment(x)),
                   "FCFE" =  get("FCFE", as.environment(x)),
                   "FCFF" =  get("FCFF", as.environment(x)))
            
          })
        }) |> 
          setNames(c("mainValues", "extraValues",  "intermediateValues","terminalValues", "CAPMvalues")) |> 
          list2env(envir = sys.frame())
        
        #isolated values that don't need to change frequently
        isoValues <- reactive(c(CAPMvalues(), mainValues(), extraValues())) |> isolate()
        
        # calcValues <- reactive({get(type(), as.environment(calcVals))})
        
        output$title <- renderUI({
          div(titlePanel(type()), align = "center")
        })
        

#all components that are react to a change in stage       
        observe({
          
          costs <- reactive({
  
              list("CAPM", CAPMvalues()) |> 
              setNames(list("title", "capmV"))
          })
          
          #values for first stage
          firstStage <- reactive({
            main = mainValues()
            (if(stage() != "One") { 
              list("Stage One", main)
            }  else {
              
              #get rid of time span from mainValues
              list("Inputs", main[-length(main)])
            }) |> 
              setNames(list("title", "mainV"))
          })
          
          secondStage <- reactive({
            (if(stage() == "Two") { 
              list("Terminal Stage", terminalValues())
            }  else if(stage() == "Three") {
              list("Declining Stage", intermediateValues())
            } else {
              list(NULL, NULL)
            }) |> 
              setNames(list("title", "secondV"))
          })
          
          thirdStage <- reactive({
            (if(stage() == "Three") { 
              list("Terminal Stage", terminalValues())
            } else {
              list(NULL, NULL)
            }) |> 
              setNames(list("title", "thirdV"))
          })
          
          #coe/coc title
          output$capm <- renderUI({
            div(titlePanel(costs()$title), align = "center",
                Map(\(x,y) {column(4, numericVInput(x,y))}, costs()$capmV, names(costs()$capmV))
            )})
          
          output$first <- renderUI({
            div(titlePanel(firstStage()$title), align = "center",
              Map(\(x,y) {column(4, numericVInput(x,y))}, firstStage()$mainV, names(firstStage()$mainV))
            )})
            
          output$second <- renderUI({
            div(titlePanel(secondStage()$title), align = "center",
              Map(\(x,y) {column(4, numericVInput(x,y))}, secondStage()$secondV, names(secondStage()$secondV))
              )})
          
          output$third<- renderUI({
            div(titlePanel(thirdStage()$title), align = "center",
                Map(\(x,y) {column(4, numericVInput(x,y))}, thirdStage()$thirdV, names(thirdStage()$thirdV))
            )})
          
          #numeric input for the dropdown (extra panel)
          output$extras <- renderUI({
            Map(\(x,y) {column(4, numericVInput(x,y))}, extraValues(), names(extraValues()))
          })
          
        })

        
        #call the numeric Output functions in server
        firstList <- reactive(Map(numericVServer, isoValues(), names(isoValues()), 0) |> 
                            setNames(paste0(isoValues(), "_nInput")))
        
        secondList <- reactive(Map(numericVServer, terminalValues(), names(terminalValues()), 0) |> 
                                 setNames(paste0(terminalValues(), "_nInput")))
        
        thirdList <- reactive(Map(numericVServer, intermediateValues(), names(intermediateValues()), 0) |> 
                                 setNames(paste0(intermediateValues(), "_nInput")))
        
        #Numbers of first,second and third stage reactive values (they are called in the lapply)
        firstNumbers <- reactive({lapply(firstList(), \(x) x())}) 
        secondNumbers <- reactive({lapply(secondList(), \(x) x())})
        thirdNumbers <- reactive({lapply(thirdList(), \(x) x())})
        
        allNumbers <- reactive({c(firstNumbers(), secondNumbers(), thirdNumbers())})
        
        #main data all graphs and functions use and display
        myData = reactive(
          rlang::inject(transitionTable(tabl =rlang::inject(pvTable(name = names(isoValues()), stage = stage(), !!!firstNumbers())),
                          stage  = stage(),
                          !!!allNumbers()))
          )
        
        format <- reactive({input$tableFormat})
        
#----------------------------------------Table--------------------------------------------------------------
        
        output$table <- renderUI({if(format()) {
          renderExcel({
            data =  cbind("input" = rownames(myData()), myData())
            excelTable(data =  data, title = fncol(data), colHeaders = LETTERS[seq_col(data)], autoColTypes = F, pagination = 10)
          })
        } else {
          renderDT(myData())
        }
          }) 

#---------------------------------------Plots----------------------------------------------------------------        
        
        output$plot <- renderEcharts4r({
          if(input$tabset == "Present Values"){
            pvPlot(myData())
          }
        }) |> 
          bindCache(allNumbers()) |> 
          bindEvent(allNumbers())
########################################      Terminal Plot       ###############################################################        
        output$terminal <- renderEcharts4r({
          lcol <- fncol(myData())
          lrow <- fncol(myData())
          
          if(input$tabset == "Terminal"){
            plotData <- rlang::inject(perpValue(names(isoValues()), myData(), stage = stage(), !!!c(firstNumbers(), secondNumbers(), thirdNumbers())))
            
            terminalPlot(plotData$firstStage, plotData$secondStage, stage = stage(), total = plotData$total)
          }
        }) |> 
          bindCache(allNumbers()) |> 
          bindEvent(allNumbers())
        
######################################    Hypothetical Graph    #######################################################################        
       
         output$hypothetical <- renderEcharts4r({
          if(input$tabset == "Hypothetical"){
            rlang::inject(hypoGraph(myData(), stage(), !!!c(firstNumbers(), secondNumbers())))
          }
        }) |> 
          bindCache(allNumbers()) |> 
          bindEvent(allNumbers())
        
        
############################################################################################################
        
} 
# Run the application 
shinyApp(ui , server)



####Immediate#######
#fix bug with transitioning from ddm to fcfe and error occuring
#make terminal stage greyed out in three stage model





#add a value of growth chart pg(338)
#plots working for both tables
#perhaps change to material page
#perhaps tabset within a tabset(growth stage and terminal stage each having plot and table)
#mathematical forumala shown (latex)
#tooltips for users or perhaps a glossary
#source material and links like damodaran ERP site
#update plots axis and names
#max timeSpan 100
#CSS styling
#manual mode (first one) (assissted mode, company lookup)
#package button. To package the data in csv format, and images in a folder

#change the numeric input to percentage for certain fields


#slow down when switching between ddm and fcfe and terminal plot panel
#add the functionality to download the graph
#module for the amount of tab panels

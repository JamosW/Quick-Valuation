source("future.R")
source("modules.R")
source("functions.R")
library(shiny)
library(DT)
library(profvis)
library(shinyWidgets)
library(excelR)
library(shinymaterial)


# Define UI for application that draws a histogram
ui <- function(title){
  
  fluidPage(
    
    # Application title
    uiOutput("title"),
    verbatimTextOutput("text"),
    fluidRow(column(2, textInput("ticker", "Enter Ticker", width = "200px")),
    column(1, actionButton("ticker_btn", "Run"), style = "bottom: -25px;")),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioUI("mdl", "Valuation Type: ", c("DDM", "FCFE", "FCFF")),
        radioUI("stage", "Stages:", c("One", "Two", "Three")),
        fluidRow(
          width = 4,
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
          tabPanel("Hypothetical", echarts4rOutput("hypothetical")),
        ))
    ))
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # my_future_function()
  source_python('yahooquery_function.py')
  
  results <- reactive({get_financials(input$ticker)})
        
  #based on button, we get the type of Valuation model in str format
  type <- radioBServer("mdl")
        
  stage <- radioBServer("stage")
        
        #get names as reactive
  lapply(lapply(c("mainL", "intermediateL", "terminalL", "CAPM"), getVars), \(x){
    reactive({
      switch(type(),
             "DDM" = get("DDM", as.environment(x)),
             "FCFE" =  get("FCFE", as.environment(x)),
             "FCFF" =  get("FCFF", as.environment(x)))
            
          })
    }) |> 
    setNames(c("mainNames", "intermediateNames","terminalNames", "capmNames")) |> 
    list2env(envir = sys.frame())
        
  isoValues <- reactive(c(capmNames(), mainNames()))
        
  # calcValues <- reactive({get(type(), as.environment(calcVals))})
        
  output$title <- renderUI({
    div(titlePanel(type()), align = "center")})
        
  #all components that are react to a change in stage       
  observe({
    costs <- reactive({
      list("CAPM", capmNames()) |> 
        setNames(list("title", "capmV"))})
    
    firstStage <- reactive({
      main = mainNames()
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
        
        list("Terminal Stage", terminalNames())
        
        }  else if(stage() == "Three") {
          
          list("Declining Stage", intermediateNames())
          
          } else {
            
            list(NULL, NULL)
            
            }) |> 
        setNames(list("title", "secondV"))
      })
    
    thirdStage <- reactive({
      (if(stage() == "Three") {
        
        list("Terminal Stage", terminalNames())
        
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
          Map(\(x,y) {column(4, numericVInput(x,y))}, firstStage()$mainV, names(firstStage()$mainV)))
      })
    
    output$second <- renderUI({
      div(titlePanel(secondStage()$title), align = "center",
          Map(\(x,y) {column(4, numericVInput(x,y))}, secondStage()$secondV, names(secondStage()$secondV))
          )})
    
    output$third<- renderUI({
      div(titlePanel(thirdStage()$title), align = "center",
          Map(\(x,y) {column(4, numericVInput(x,y))}, thirdStage()$thirdV, names(thirdStage()$thirdV))
          )})
          
        })
  
  #call the numeric Output functions in server
  firstList <- reactive(Map(numericVServer, isoValues(), names(isoValues()), 0) |> 
                            setNames(paste0(isoValues(), "_nInput")))
  
  secondList <- reactive(Map(numericVServer, terminalNames(), names(terminalNames()), 0) |> 
                           setNames(paste0(terminalNames(), "_nInput")))
  
  thirdList <- reactive(Map(numericVServer, intermediateNames(), names(intermediateNames()), 0) |> 
                          setNames(paste0(intermediateNames(), "_nInput")))
  
  #Numbers of first,second and third stage reactive values (they are called in the lapply)
  firstNumbers <- reactive({lapply(firstList(), \(x) x())})
  secondNumbers <- reactive({lapply(secondList(), \(x) x())})
  thirdNumbers <- reactive({lapply(thirdList(), \(x) x())})
  
  allNumbers <- reactive({c(firstNumbers(), secondNumbers(), thirdNumbers())})
  
  first_table <- reactive({
    rlang::inject(pvTable(name = names(isoValues()), stage = stage(), !!!firstNumbers()))
    })
  

  #main data all graphs and functions use and display
  merged_table <- reactive({
    rlang::inject(transitionTable(tabl =first_table(),stage  = stage(),!!!allNumbers()))
    })

  format <- reactive({input$tableFormat})
  
  #update numeric inputs when run is clicked
  observe({
    Map(\(x,y) updateNumericInput(session, x, value = y), lookup[match(names(results()), names(lookup))], results())
    
    }) |>
    bindEvent(input$ticker_btn)
  
  #
  observe({
    updateNumericInput(session, "d_payout-nInput", value = input[["dividends-nInput"]]/input[["net_inc-nInput"]])
    Map(\(x,y) updateNumericInput(session, x, value = input[[y]]), 
        list("t_coe-nInput", "t_payout", "t_growth"), 
        list("d_coe-nInput", "d_payout", "d_growth"))

  })
  
  #update/invalidate COE only when capm variables change manually by user and 
  observe({
    updateNumericInput(session, "d_coe-nInput", value = input[["rf-nInput"]] + (input[["beta-nInput"]] * input[["erp-nInput"]]))
  }) |> 
    bindEvent(input[["rf-nInput"]], input[["beta-nInput"]], input[["erp-nInput"]])


#----------------------------------------Table--------------------------------------------------------------
  output$table <- renderUI({if(format()) {
    
    renderExcel({
      
      data =  cbind("input" = rownames(merged_table()), merged_table())
      excelTable(data =  data, title = fncol(data), colHeaders = LETTERS[seq_col(data)], autoColTypes = F, pagination = 10)
      
      })
    
    } else {
      
      renderDT(merged_table())
      }
    
    
    }) 

#---------------------------------------Plots----------------------------------------------------------------
  output$plot <- renderEcharts4r({
    if(input$tabset == "Present Values"){
      pvPlot(merged_table())
      }
    }) |> 
    bindEvent(allNumbers(), input$ticker_btn)
########################################      Terminal Plot       ###############################################################        
  output$terminal <- renderEcharts4r({
    if(input$tabset == "Terminal"){
      plotData <- rlang::inject(perpValue(names(isoValues()), merged_table(), stage = stage(), !!!c(firstNumbers(), secondNumbers(), thirdNumbers())))
      terminalPlot(plotData$firstStage, plotData$secondStage, stage = stage(), total = plotData$total)
      }
    }) |>
    bindEvent(allNumbers(), input$ticker_btn)
        
######################################    Hypothetical Graph    #######################################################################        
       
  output$hypothetical <- renderEcharts4r({
    if(input$tabset == "Hypothetical"){
      rlang::inject(hypoGraph(merged_table(), stage(), !!!c(firstNumbers(), secondNumbers())))
      }
    }) |> 
    bindEvent(allNumbers())
        
        
        
############################################################################################################
  
  }
# Run the application 
shinyApp(ui , server)



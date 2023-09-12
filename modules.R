


### numeric input module
numericVInput <- function(id, label, value = 0) {
  ns <- NS(id)
  tagList(
    #numericInput usually paired with a text Output
    numericInput(inputId = ns("nInput"), label = list(icon("info-circle") |> 
                                                        bsplus::bs_embed_tooltip(title = tooltipValues(label)), title = label), value = value, width = "100px", max = 100, min = 1)
  )
}

numericVServer <- function(id, lbl, val) {
  moduleServer(
    id,
    function(input, output, session) {
      bins = reactive(input$nInput)
      
      return(bins)
      
    }
  )
}



radioUI <- function(id, label, choices) {
  ns <- NS(id)
  
  tagList(
    radioGroupButtons(
      inputId = ns("radioB"),
      label = label,
      choices = choices,
      justified = TRUE
    )
  )
}

radioBServer <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      type = reactive(input$radioB)
      
      return(type)
    }
  )
}







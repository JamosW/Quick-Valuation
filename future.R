library(reticulate)
library(future)
library(promises)
plan(multicore)

# Define any Python packages needed for the app here:
PYTHON_DEPENDENCIES = 'yahooquery'


my_future_function <- function(){
  
  fut <- future({
    expr <- quote({
      use_virtualenv("tester", required = TRUE)
    })
    
    if(virtualenv_exists(envname = "tester")){
      eval(expr)
    } else {
      virtualenv_install("tester", packages = c(PYTHON_DEPENDENCIES))
      eval(expr)
    }
  }) %...>% (function(result) {
    return(result)
  })

  return(fut)
}

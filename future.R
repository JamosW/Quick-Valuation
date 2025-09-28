library(reticulate)


# Define any Python packages needed for the app here:
PYTHON_DEPENDENCIES = 'yahooquery'

virtualenv_create(envname = 'tester', 
                              python = '/usr/bin/python3')
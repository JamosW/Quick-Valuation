library(reticulate)


# Define any Python packages needed for the app here:
PYTHON_DEPENDENCIES = 'yahooquery'

virtualenv_create(envname = 'tester')
virtualenv_install('tester', packages = c(PYTHON_DEPENDENCIES))
use_virtualenv("tester", required = T)

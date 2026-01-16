# Shiny for checking the Sequencing Plate Plan excel

/!\ You need to have access to the INSERM network. Please use the VPN if necessary.

## Set-up

In R :
```
install.packages(c("shiny","shinydashboard", "DT", "DBI", "RPostgres", "readxl", "shinyjs", "ssh", "remotes"))
remotes::install_github('ropensci/ssh')
```

## Launch

In R :
```
   shiny::runGitHub('ShinyCheckSequencingPlatePlan', 'GeNeHetX', subdir='CheckSeq' ,ref='main')
```

Wait for the page to open or open the link printed by the console in a browser if it doesn't.
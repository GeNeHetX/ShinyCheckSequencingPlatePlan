# Shiny for checking the Sequencing Plate Plan excel

/!\ Please check the procedure at the following location before filling the excel file : [RNASeqProcedureICM](https://insermfrance-my.sharepoint.com/:w:/g/personal/diana_mendes_inserm_fr/IQCp1XhWSPg9RbJAnDkLJ1sZAaRnWzzg4NutQvwCjbgw32Y?rtime=Wb4USN1Y3kg)


/!\ You need to have access to the INSERM network. Please use the VPN if necessary.

## Set-up

In R :
```
install.packages(c("shiny","shinydashboard", "DT", "DBI", "RPostgres", "readxl", "shinyjs", "ssh", "remotes", "httr"))
remotes::install_github('ropensci/ssh')
```

## Launch

In R :
```
   shiny::runGitHub('ShinyCheckSequencingPlatePlan', 'GeNeHetX', subdir='CheckSeq' ,ref='main')
```

Wait for the page to open or open the link printed by the console in a browser if it doesn't.
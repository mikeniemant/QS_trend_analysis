# QS trend analysis
## Description
R Shiny app for QS trending analysis of the positive control. (QS system output)

Interactive Shiny interface to plot Ct values measured by the QS machine over time for trending analysis. User can import one or multiple files (txt format) that are added to the locally built database to analyze the trending of positive control Ct values over time. 

## Installation
Install the latest R version, the necessary libraries and fork repo from GitHub.

1. Install R: https://cran.r-project.org

2. Start R console and install the necessary R libraries by running the following commands

```
install.packages("tidyverse")
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyFiles")
install.packages("shinyjs")
install.packages("DT")
install.packages("plotly")
```

3. Fork repository from GitHub, either by using the Git console commands or by downloading the complete repo (zip) by clicking on the green 'Code' in the top right. Open the zip file and extract all files. Make sure you have the following folder structure in your system: "C:/QS_trend_analysis/app/app.R", and not "C:/QS_trend_analysis/QS_trend_analysis-master/app/app.R", in order to make the app work.

## Usage
Usage of the app depends on the operator system.

### MacOS

- Launch the app in terminal with the following command: `R -e "shiny::runApp('DIR_APP', port = 8888)"`
- Open the following link in your browser: http://127.0.0.1:8888

### Windows

Multiple options to start the Shiny app in Windows. 

- Start the R console with one of the two following options:
  - `C:\"Program Files"\R\R-[VERSION]\bin\Rscript` OR
  - RStudio
- Type the following command in the console `shiny::runApp([insert the Shiny app dir here], port = 8888)`. Example: `shiny::runApp("C:/QS_trend_analysis/app/app.R", port = 8888)`. Don't forget the citation quotes and parentheses!

The app should start now, however we can automate the above tasks by creating a `bat` file. For this, we need the following directories:

- App directory: should look something like this `C:/Shiny/QS_shiny-master/app/app.R`
- Rscript directory: should look something like this: `C:/"Program Files"/R/R-3.6.1/bin/Rscript`

Now we have to paste these two directories together:
  
  - `C:\"Program Files"\R\R-3.6.1\bin\Rscript -e "shiny::runApp('C:/Shiny/QS_shiny-master/app.R', port = 1111, launch.browser =  T)"`. Make sure to edit all directories!

Open a text editor program and paste the above line in a new file. Save the file as 'QS_trend_analysis.bat' file.

## Update app
When a new version is available of the app we can a similar setup as the `bat` file. More on the 'QS_trend_analyis_update.bat' file later..

## To do:
A lot ..

## Known bugs
- Looking at trending from data that originates from outside the US or EU may not be valid as the code does not take these time-zones into account. Therefore, all data generated on a timezone other than PSA, PTA, CEST or CST will likely not be processed correctly! 

### Windows
When using R for the first time, the LC_CTYPE global parameter may either not be defined or set to another language. To resolve this problem, change this global parameter to "English_United States.1250". In R, this can be done with the following commands:

`user_renviron = path.expand(file.path("~", ".Renviron"))`
`file.edit(user_renviron)`

Paste the following lines in the script that just opened:
  
`LC_COLLATE  = "English_United States.1250"`
`LC_CTYPE    = "English_United States.1250"`
`LC_MONETARY = "English_United States.1250"`
`LC_NUMERIC  = "English_United States.1250"`
`LC_TIME     = "English_United States.1250"`

Save file and restart R / RStudio.

The next time we start R, R knows what language to use for understanding sequences of bytes of text data characters.

## License
QS_trend_analysis app is under the APACHE-II license (2021).

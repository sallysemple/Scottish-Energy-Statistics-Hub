require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



GlossaryOutput <- function(id) {
  ns <- NS(id)
  tagList(
    
    
    fluidRow(
      column(10,h3("Glossary", style = "color: #1A5D38;  font-weight:bold")),
      ),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    
  )
}




###### Server ######
Glossary <- function(input, output, session, parent_session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("Glossary.R")
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/8 - Target Tracker/Glossary.txt")[2])
                     
                   )))
  })
  
  }


# ui.R

shinyUI(fluidPage(
  titlePanel("censusVis"),
  ##########################################################################################
  sidebarLayout(
    ########################################################################################
    sidebarPanel(
      helpText("Create demographic maps with information from the 2010 US Census."),
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("A", "B", "C", "D"),
                  selected = "Percent White"
                ),
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100)
                ),
      actionButton("testButton","TESTING",width="100%")
    ),
    ########################################################################################
    mainPanel(
      textOutput("text1"),
      textOutput("text2")
    )
    ########################################################################################
  )
  ##########################################################################################
))
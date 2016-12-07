library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Particle Degradation"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("max.time", "Max time", value=100),
      numericInput("init.rad", "Initial particle radius", value=100),
      sliderInput("alpha","Attachment rate (primary) [log10]",
                  min=-10,max=0,value=-7,step=0.1),
      sliderInput("beta", "Growth rate (primary) [log10]",
                  min=-4,max=0,value=-2.1,step=0.1),
      sliderInput("alpha2","Attachment rate (secondary) [log10]",
                  min=-10,max=0,value=-10,step=0.1),
      sliderInput("beta2", "Growth rate (secondary) [log10]",
                  min=-4,max=0,value=-4,step=0.1),
      sliderInput("gamma", "Uptake rate [log10]",
                  min=-8,max=-3,value=-6,step=0.1),
      sliderInput("mu",   "Death rate [log10]",
                  min=-10,max=0,value=-3.5,step=0.1),
      sliderInput("K",    "Saturation constant [log10]",
                  min=-10,max=0,value=-7,step=0.1),
      sliderInput("prod", "Production rate [log10]",
                  min=0,  max=5,value=3,step=0.1),
      sliderInput("rho",  "Particle density [log10]",
                  min=-2,  max=2,value=0,step=0.1),
      sliderInput("nu",  "Monomer loss [log10]",
                  min=-2,  max=2,value=0,step=0.1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("degrade.plot")
    )
  )
))

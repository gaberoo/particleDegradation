library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Particle Degradation"),

  withMathJax(),

  h2("Equations"),

  tabsetPanel(id="eqTabs",
    tabPanel(title="Parasite",value=0,
      div("$$\\dot B_1 = \\left( a_1 + b_1 g M \\right) \\left( 1 - \\frac{B_1+B_2}{K \\cdot 4 \\pi r^2} \\right) - m \\, B_1$$"),
      div("$$\\dot B_2 = \\left( a_2 + b_2^\\ast B_1 B_2 \\right) \\left( 1 - \\frac{B_1+B_2}{K \\cdot 4 \\pi r^2} \\right) - m \\, B_2, \\quad b_2^\\ast = 10000 b_2$$"),
      div("$$\\dot r   = -p B_1 \\frac{q(r)}{4 \\pi r^2 \\cdot \\rho}, \\quad q(r) \\propto 4 \\pi r^2 $$"),
      div("$$\\dot M   = p B_1 q(r) - g M \\left( 1 - \\frac{B_1+B_2}{K \\cdot 4 \\pi r^2} \\right) - n \\, M$$")
    ),
    tabPanel(title="Cheater",value=1,
      div("$$\\dot B_1 = a_1 + b_1 g M \\frac{B_1}{B_1+B_2} \\left( 1 - \\frac{B_1+B_2}{K \\cdot 4 \\pi r^2} \\right) - m \\, B_1$$"),
      div("$$\\dot B_2 = a_2 + b_2 g M \\frac{B_2}{B_1+B_2} \\left( 1 - \\frac{B_1+B_2}{K \\cdot 4 \\pi r^2} \\right) - m \\,B_2$$"),
      div("$$\\dot r   = -p B_1 \\frac{q(r)}{4 \\pi r^2 \\cdot \\rho}, \\quad q(r) \\propto 4 \\pi r^2 $$"),
      div("$$\\dot M   = p B_1 q(r) - g M \\left( 1 - \\frac{B_1+B_2}{K \\cdot 4 \\pi r^2} \\right) - n \\, M$$")
    )
  ),

  # Sidebar with a slider input for the number of bins
  fluidRow(
    column(3,
      numericInput("max.time", "Max time", value=100),

      numericInput("init.rad", "Initial particle radius", value=100),

      sliderInput("alpha","Attachment rate, a1 [log10]",
                  min=-10,max=0,value=-7,step=0.1),
      sliderInput("beta", "Growth rate, b1 [log10]",
                  min=-4,max=0,value=-2.1,step=0.1),
      sliderInput("alpha2","Attachment rate, a2 [log10]",
                  min=-10,max=0,value=-10,step=0.1),
      sliderInput("beta2", "Growth rate, b2 [log10]",
                  min=-4,max=0,value=-4,step=0.1)
    ),
    column(3,
      sliderInput("gamma", "Uptake rate, g [log10]",
                  min=-8,max=-3,value=-6,step=0.1),
      sliderInput("mu",   "Death rate, m [log10]",
                  min=-10,max=0,value=-3.5,step=0.1),
      sliderInput("K",    "Saturation constant, K [log10]",
                  min=-10,max=0,value=-7,step=0.1),
      sliderInput("prod", "Production rate, p [log10]",
                  min=0,  max=5,value=3,step=0.1),
      sliderInput("rho",  "Particle density, rho [log10]",
                  min=-2,  max=2,value=0,step=0.1),
      sliderInput("nu",  "Monomer loss, n [log10]",
                  min=-2,  max=2,value=0,step=0.1)
    ),
    column(6,
      plotOutput("degrade.plot")
    )
  )
))

# clear workspace
rm(list = ls())

# load Shiny library
library(shiny)

# THIS VERSION OF THE APP RUNS USING OUTPUT FROM "input_sulfparam4.R"

# If running files from local directory on disk, run "input_sulfparam.R" first to generate
# "const_input.rds" and "var_input.rds". If you wish to change the default values for any
# of the parameters, this can be done in "input_sulfparam.R", whereupon you will have to 
# run this file again to generate two updated parameter input files.

# load saved inputs and functions
const <- readRDS("const_input.rds")
var <- readRDS("var_input.rds")
param_ana <- readRDS("char_input.rds")
param_names <- names(const[1:length(var)])
source("funct_env.R")
source("funct_metab.R")
source("funct_frac.R")

ychoices <- c("net.frac", "SO4_in", "APS", "SO3")


# UI: generate space for dynamic UI: contains 20 sliders for varying metabolic rate (csSRR),
# Michaelis constants (K_m) and a given reaction's maximum rate (V_m)
ui <- shinyUI(fluidPage(
  titlePanel("Variability of biogenic sulfur fractionation
             with microbial metabolic expression"),
  p(HTML("Fractionation is in permil; metabolite concentrations and K<sub>m</sub> are in moles.
         V<sub>max</sub> and csSRR (also referred to as J0) are in femtomoles per cell per day.")),
  fluidPage(
    column(width = 2, 
           fluidRow(uiOutput("controlsvar")),
           fluidRow(sliderInput("SO4_out", "External sulfate",
                                1e-5, 1e-1, const$SO4_out)),
           fluidRow(sliderInput("H2S", "External sulfide",
                                1e-5, 1e-1, const$H2S)),
           fluidRow(plotOutput("fracPlot", width = "650")
                    )
           ),
    fluidPage(
      column(width = 2, uiOutput("controlsJ0"), uiOutput("controlsmb")
      ),
      column(width = 2, uiOutput("controlsA")
      ),
      column(width = 2, uiOutput("controlsB")
      ),
      column(width = 2, uiOutput("controlsC")
      ),
      column(width = 2, uiOutput("controlsD")
      )
    )
  )
  )
  )


# SERVER: define server logic required to i) generate dynamic UI, ii) calculate fractionation 
# for plot, and iii) generate plot
server <- shinyServer(function(input, output, session) {
  # define reactive value that changes according to user input selected through drop-down list;
  # this is necessary to trigger reloading of sliders to ensure that input fed to function
  # that calculates fractionation is provided with only one slider range (the one that
  # comprises the x-axis)
  rvx <- reactiveValues(selected = param_names[1])
  rvy <- reactiveValues(selected = ychoices[1])
  
  # generate panel with drop-down list and space for sliders
  output$controlsvar <- renderUI({
    div(
      fluidRow(
        column(12,
               selectInput("var_param", "Varied parameter (x-axis)",
                           param_names, selected = param_names[1])
               )
        ),
      fluidRow(
        column(12,
               selectInput("yaxis", "Choose y-axis variable",
                           choices = ychoices, selected = ychoices[1])
               )
        )
      )
  })
  
  output$controlsJ0 <- renderUI({
    div(
      fluidRow(
        column(12, uiOutput("varyingJ0"))
      )
    )
  })
  
  output$controlsmb <- renderUI({
    div(
      fluidRow(
        column(12, uiOutput("varyingmb"))
      )
    )
  })
  
  output$controlsA <- renderUI({
    div(
      fluidRow(
        column(12, uiOutput("varyingA"))
      )
    )
  })
  
  output$controlsB <- renderUI({
    div(
      fluidRow(
        column(12, uiOutput("varyingB"))
      )
    )
  })
  
  output$controlsC <- renderUI({
    div(
      fluidRow(
        column(12, uiOutput("varyingC"))
      )
    )
  })
  
  output$controlsD <- renderUI({
    div(
      fluidRow(
        column(12, uiOutput("varyingD"))
      )
    )
  })
  
  # generate 20 sliders, each corresponding to the possible inputs of the drop-down list;
  # the single parameter selected through the drop-down list will cause the corresponding
  # slider to be a range slider instead of a single-value slider, creating a range of values
  # within the slider endpoint values of that parameter that will be passed to the fractionation-
  # calculating function as well as the x-axis of the plot
  output$varyingJ0 <- renderUI({
    lapply(1, function(i) {
      # the default endpoints for the range slider are simply the endpoints of the slider itself
      lims <- c(var[1, i], var[nrow(var), i])
      fluidRow(
        column(12,
               if (rvx$selected == param_names[i]) {
                 sliderInput(param_names[i], param_ana[i],
                             lims[1], lims[2], lims)
               } else {
                 sliderInput(param_names[i], param_ana[i],
                             lims[1], lims[2], as.numeric(const[i]))
               }
        )
      )
    })
  })
  
  output$varyingmb <- renderUI({
    lapply(2:3, function(i) {
      # the default endpoints for the range slider are simply the endpoints of the slider itself
      lims <- c(var[1, i], var[nrow(var), i])
      fluidRow(
        column(12,
               if (rvx$selected == param_names[i]) {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], lims)
               } else {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], as.numeric(const[i]))
               }
        )
      )
    })
  })
  
  output$varyingA <- renderUI({
    lapply(4:6, function(i) {
      # the default endpoints for the range slider are simply the endpoints of the slider itself
      lims <- c(var[1, i], var[nrow(var), i])
      fluidRow(
        column(12,
               if (rvx$selected == param_names[i]) {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], lims)
               } else {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], as.numeric(const[i]))
               }
        )
      )
    })
  })
  
  output$varyingB <- renderUI({
    lapply(7:11, function(i) {
      # the default endpoints for the range slider are simply the endpoints of the slider itself
      lims <- c(var[1, i], var[nrow(var), i])
      fluidRow(
        column(12,
               if (rvx$selected == param_names[i]) {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], lims)
               } else {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], as.numeric(const[i]))
               }
        )
      )
    })
  })
  
  output$varyingC <- renderUI({
    lapply(12:17, function(i) {
      # the default endpoints for the range slider are simply the endpoints of the slider itself
      lims <- c(var[1, i], var[nrow(var), i])
      fluidRow(
        column(12,
               if (rvx$selected == param_names[i]) {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], lims)
               } else {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], as.numeric(const[i]))
               }
        )
      )
    })
  })
  
  output$varyingD <- renderUI({
    lapply(18:22, function(i) {
      # the default endpoints for the range slider are simply the endpoints of the slider itself
      lims <- c(var[1, i], var[nrow(var), i])
      fluidRow(
        column(12,
               if (rvx$selected == param_names[i]) {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], lims)
               } else {
                 sliderInput(param_names[i], HTML(param_ana[i]),
                             lims[1], lims[2], as.numeric(const[i]))
               }
        )
      )
    })
  })
  
  
  # this simply changes the reactive value to that of the name of the parameter chosen through
  # the drop-down list such that the above routine is rerun, updated with the user's choice
  observeEvent(input$var_param, {
    rvx$selected <- input$var_param
  })
  observeEvent(input$yaxis, {
    rvy$selected <- input$yaxis
  })
  
  
  # calculate fractionation and generate plot
  output$fracPlot <- renderPlot({
    
    # assigning the sliders to a set of parameter input names recognized by the function; this is
    # necessary to ensure that the inputs can easily change from the single-value defaults to a
    # multi-value range as defined by the range slider.
    J0 <- input$J0
    m <- input$m
    b <- input$b
    SO4_out <- input$SO4_out
    H2S <- input$H2S
    
    K_As1 <- input$K_As1
    K_Ap1 <- input$K_Ap1
    K_Bs1 <- input$K_Bs1
    K_Bs2 <- input$K_Bs2
    K_Bp1 <- input$K_Bp1
    K_Bp2 <- input$K_Bp2
    K_Cs1 <- input$K_Cs1
    K_Cs2 <- input$K_Cs2
    K_Cp1 <- input$K_Cp1
    K_Cp2 <- input$K_Cp2
    K_Cp3 <- input$K_Cp3
    K_Ds1 <- input$K_Ds1
    K_Ds2 <- input$K_Ds2
    K_Dp1 <- input$K_Dp1
    K_Dp2 <- input$K_Dp2
    V_A <- input$V_A
    V_B <- input$V_B
    V_C <- input$V_C
    V_D <- input$V_D
    
    # this somewhat convoluted-but-necessary process takes the range slider endpoint values and
    # generates a range of values between them with which fractionation should be calculated;
    # this range is then given named such that "funct_metab" (below) is able to find it 
    xval <- input[[rvx$selected]]
    res <- 50
    xmin <- min(xval)
    xmax <- max(xval)
    xrange <- seq(xmin, xmax, length.out = res)
    assign(paste(rvx$selected), xrange)
    
    # calculate thermodynamic values; necessary to re-do with every input change to external sulfate
    env <- funct_env(SO4_out, const$PMF, const$z, const$Temp, const$R)

    nCalc <- unlist(env[1])
    H_in_out <- unlist(env[2])
    G_A0 <- unlist(env[3])
    
    
    # calculate metabolite concentrations, now that inputs and parameter names have been established
    metab <- funct_metab(J0, K_As1, K_Ap1, K_Bs1, K_Bs2,K_Bp1, K_Bp2, K_Cs1, K_Cs2, 
                         K_Cp1, K_Cp2, K_Cp3, K_Ds1, K_Ds2, K_Dp1, K_Dp2, V_A, V_B, V_C, V_D,
                         SO4_out, H2S, m, b, const$MK_red, const$MK_ox, const$AMP, const$ATP,
                         H_in_out, nCalc,
                         G_A0, const$G_B0, const$G_C0, const$G_D0, const$R, const$Temp)
    
    # "metab" contains four output lists, each corresponding to concentrations of a specific
    # metabolite; unlisting these four outputs is done to convert these to numeric values that are 
    # then used to calculate the net fractionation of the entire sulfate reduction process. Each list
    # constitutes of a number of objects equal to that of "res", seen in the routine for generating
    # a range of values between the range slider endpoints.
    SO3 <- unlist(metab[1])
    SO4_in <- unlist(metab[2])
    APS <- unlist(metab[3])
    PPi <- unlist(metab[4])
    
    # calculate net fractionation; "funct_frac" is actually a recursive function where fractionation
    # associated with each of the four reactions that comprise sulfate reduction is also calculated
    net.frac <- funct_frac(SO4_in, APS, SO3, PPi, SO4_out, H2S,
                           H_in_out, nCalc, const$ATP, const$AMP, const$MK_ox, const$MK_red,
                           G_A0, const$G_B0, const$G_C0, const$G_D0,
                           const$a34eqA, const$a34eqB, const$a34eqC, const$a34eqD,
                           const$a34kinA, const$a34kinB, const$a34kinC, const$a34kinD,
                           const$R, const$Temp)
    
    if (rvy$selected == ychoices[1]) {
      yrange <- net.frac
    }
    if (rvy$selected == ychoices[2]) {
      yrange <- SO4_in
    }
    if (rvy$selected == ychoices[3]) {
      yrange <- APS
    }
    if (rvy$selected == ychoices[4]) {
      yrange <- SO3
    }
    
    # plot the varied parameter (controlled by the range slider) and the resulting net fractionation
    plot(xrange, yrange, xlab = rvx$selected, ylab = rvy$selected)
  })
})



# run the application 
shinyApp(ui = ui, server = server)


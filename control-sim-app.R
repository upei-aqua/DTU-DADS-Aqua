library("shiny")
library("shinythemes")
library("tidyverse")
library("markdown")

# ---- Load simulation functions -----------------------------------------------

source("sourceASF.R")
sourceASF()
# User has to decide what to do with index farm choice
# For now, first cage of each farm is used 10 times
indexHerdFile <- rep(seq(1,780, 20), each = 10)

# ---- User interface ----------------------------------------------------------

ui <- navbarPage("DTU-DADS-Aqua",
  theme = shinytheme("flatly"),
  tabPanel("Introduction",
    fluidRow(
      column(
        10,
        includeMarkdown("www/intro.md")
      )
    )
  ),

  tabPanel("Example",
    sidebarLayout(

      # ---- side panel ----------------------------------------------------------

      sidebarPanel(

        helpText("Simulation control"),

        textInput("runID",
                  label = "Scenario ID",
                  value = "default",
                  placeholder = "Enter a name for the scenario"),

        br(),

        sliderInput("n",
                    "Number of iterations:",
                    value = 10,
                    min = 1,
                    max = 1000),

        sliderInput("maxTime",
                    "Simulation days:",
                    value = 120,
                    min = 1,
                    max = 365*3),
        
        sliderInput("seed",
                    "Random seed:",
                    value = -10,
                    min = -10,
                    max = 1000),

        helpText("Scenario-specific parameters"),
        
        sliderInput("BetCageProb",
                    "Cage-specific transmission parameter:",
                    value = 5,
                    min = 0,
                    max = 10,
                    step = .05),

        sliderInput("ScalingInf",
                    "Distance-based scaling parameter:",
                    value = .4,
                    min = 0,
                    max = 3,
                    step = .05),

        sliderInput("Capacity",
                    "Culling capacity (thousands fish):",
                    value = 10,
                    min = 1,
                    max = 300,
                    step = .5),

        br(),

        actionButton("update", "Run Simulation")

      ),

      # ---- Main panel ----------------------------------------------------------

      mainPanel(

        tabsetPanel(type = "tabs",

          tabPanel("Parameters",

            fluidRow(
              column(10,

                div(includeMarkdown("www/parameters.md"), 
                    style = "font-size:80%")

              )
            )
          ),

          tabPanel("Summary",

            fluidRow(
              column(10,

                br(),
                div(tableOutput("iter_table"), 
                    style = "font-size:80%")
              )
            )
          ),

          tabPanel("Infection mode",

            fluidRow(
              column(10,

                br(),
                plotOutput("plot")
              )
            )
          )
        ) # tabset ends
      ) # mainpanel ends
    )
  ),

  tabPanel("References",
           fluidRow(
             column(
               10,
               includeMarkdown("www/references.md")
             )
           )
  )
)


# ---- Server functions --------------------------------------------------------

server <- function(input, output) {

  # ---- Run simulation -------------------------------------------------------
  
  # run simulation only after parameters have been selected
  asf_options <- eventReactive(input$update, {

    ASFoptions(
      maxTime         = input$maxTime,
      Capacity        = input$Capacity * 1000,
      BetCageProb     = input$BetCageProb,
      ScalingInf      = input$ScalingInf,
      runID           = as.character(input$runID),
      seed            = input$seed,
      n               = input$n,
      indexHerdSelect = list(ID = indexHerdFile)
    )

  })


  observe({
  
    if(input$update > 0) {
      
      isolate(file.remove(paste0(input$runID, "-ISA.txt")))
      isolate(file.remove(paste0(input$runID, "-AllInfCages.txt")))
      
      #newRun <- isolate(asf_options())
      isolate(ASF(asf_options()))
    
    }
  })

  # ---- Outputs ---------------------------------------------------------------

  # Summary tab

  output$iter_table <- renderTable({
  
    if(input$update > 0) {
      
      tryCatch({

        x <- read_delim(
          paste0(input$runID, "-ISA.txt"),
          delim = " ",
          skip = 1L,
          col_names = c(
             "First_Det",
             "Outbr_Duration",
             "Last_Inf_Cage",
             "Num_Inf_Cage",
             "Num_Inf_Farm",
             "Num_Det_Cage",
             "Num_Cull_Cage",
             "Recovered",
             "Num_Cull_Fish",
             "VisCount")
        
        )
      }, error = function (e) {
      
        a <- print("File not found")
        return(a)
      
      })  

      x %>%
       select(-Recovered, -VisCount)
    
    }
  }, # table options 
    hover = TRUE,
    striped = TRUE,
    digits = 0
  )

  # Infection mode tab
  
  output$plot <- renderPlot({
    
    if(input$update > 0) {
      
      tryCatch({

        y <- read_delim(
          paste0(input$runID, "-AllInfCages.txt"),
          delim = " ",
          skip = 1L,
          col_names = c(
             "iteration",
             "diagnosisTime",
             "immuneTime",
             "IndexAIH",
             "timeInfected",
             "infMode",
             "FarmID",
             "FarmProdTime")
        
        )
      }, error = function (e) {
      
        b <- print("File not found")
        return(b)
      
      })  

      y %>%
        filter(infMode != 0) %>%
        group_by(iteration, infMode, timeInfected) %>%
        arrange(timeInfected) %>%
        tally() %>%
        mutate(nn = cumsum(n)) %>%
        ggplot(aes(x     = timeInfected,
                   y     = nn,
                   color = factor(infMode, levels = 1:2, labels = c("Between farms", "Within farm")), # c("External", "Internal")
                   group = paste(iteration, infMode))) +
          geom_step() +
          scale_color_discrete(name = "Infection Mode") +
          labs(x = "Time (days)",
               y = "Number of infected net-pens",
               title = "Infection dynamics",
               caption = "") +
          theme_light() +
          theme(panel.grid = element_line(linetype = "dashed"))

    }

  })


}

shinyApp(ui = ui, server = server)
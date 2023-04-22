##########
# HPAI results app 
##########

library(plyr)
library(dplyr)
library(ggplot2)
library(shinyWidgets)

# Load datasets
fixed <- readRDS("fixed_results.rds")
low_trans <- readRDS("low_trans.rds")
med_trans <- readRDS("med_trans.rds")
high_trans <- readRDS("high_trans.rds")
med_popn <- readRDS("med_popn.rds") 
high_popn <- readRDS("high_popn.rds")
multi <- readRDS("multiphase.rds")
multi_trans <- readRDS("transmission_multiphase.rds")
multi_popn <- readRDS("population_multiphase.rds")


ui <- navbarPage("HPAI Bangladesh Results",

  tabPanel("Single control",
           
    tabsetPanel(
    tabPanel("Boxplot", plotOutput("boxplot")),
    tabPanel("Log transformed boxplot", plotOutput("boxplot_log"))
    ),
    
    hr(),
    
    fluidRow(
      column(3,
             #Scenario selector
             selectInput("scenario", label = h4("Select scenario:"),
                         choices = list("Real-world" = 1, "Increased transmissibility" = 2, "Farming intensification" = 3),
                         selected = 1),
             p("Three different outbreak scenarios were investigated: (1) Outbreak parameters match those of real-world epidemics 
             in Bangladesh (2) Disease transmissibility increases and (3) Poultry populations increase because of increased farming 
             intensification."),
             p("These increases can be at baseline (no increase), 50% above baseline (medium increase) or 100% above baseline (high increase)")
      ),
      
      column(4, offset = 1,
             
             # Transmission selector
             conditionalPanel(condition = "input.scenario == 2", uiOutput("transmission")),
             
             # Population selector
             conditionalPanel(condition = "input.scenario == 3", uiOutput("population")),
             
             # Control selector
             pickerInput(
               inputId = "control",
               label = h4("Select controls:"),
               choices = list("IP cull", "1km cull", "2km cull", "3km cull", "4km cull", 
                              "5km cull", "6km cull", "7km cull", "8km cull", "9km cull", 
                              "10km cull", "1km vacc", "2km vacc", "3km vacc", "4km vacc", 
                              "5km vacc", "6km vacc", "7km vacc", "8km vacc", "9km vacc", 
                              "10km vacc", "Reactive-distance", "Reactive-popn", 
                              "Proactive-popn", "Proactive-density"
               ),
               selected = "IP cull",
               options = list(
                 `actions-box` = TRUE,
                 size = 12,
                 `selected-text-format` = "count > 3"
               ),
               multiple = TRUE
             ),
             
             p("Choose at least one control scenario from: infected premises culling, ring culling and ring vaccination
               rings of between 1 and 10km radius and a selection of active surveillance approaches"),
             
      ),
      column(4,

             # Objective selector
             selectInput("outcome", label = h4("Select management objective:"),
                         choices = list("Outbreak size", "Outbreak duration (days)", "Culled premises",
                                        "Culled chickens", "Vaccinated premises", "Vaccinated chickens"),
                         selected = 1),
             
             p("Choose the management objective that you are interested in. This can be minimising: the premises outbreak size, outbreak duration, 
               amount of culling or amount of vaccination")
      )
    )
  ),
  
  tabPanel("Multiphase control",
           
    plotOutput("heatmap"),
    p("Results given to 3 s.f."),
    p("Numbers of chickens culled are given in thousands."),

    hr(),
    
    fluidRow(
      column(3,
             #Scenario selector
             selectInput("scenario_multi", label = h4("Select scenario:"),
                         choices = list("Real-world" = 1, "Increased transmissibility" = 2, "Farming intensification" = 3),
                         selected = 1),
             
             p("Three different outbreak scenarios were investigated: (1) Outbreak parameters match those of real-world epidemics 
             in Bangladesh (2) Disease transmissibility increases and (3) Poultry populations increase because of increased farming 
             intensification."),
             p("These increases can be at baseline (no increase), 50% above baseline (medium increase) or 100% above baseline (high increase)")
      ),
      
      
      column(4, offset = 1, 
             
        # Transmission selector     
        conditionalPanel(condition = "input.scenario_multi == 2", uiOutput("transmission_multi")),
         
        # Population selector
        conditionalPanel(condition = "input.scenario_multi == 3", uiOutput("population_multi")),
        
        # Trigger selector
        uiOutput("measure"),
        # selectInput("measure", label = h4("Select multiphase control trigger:"), 
        #             choices = list("Outbreak duration (days)" = 1, "Outbreak size" = 2, "Time since last IP" = 3), 
        #             selected = 1), 
        
        p("The control strategy can be changed when the outbreak reaches a set number of days, size (number of infected premises)
          or when 14 days have passed since the last infected premises (real-world scenarios only)")
             
      ),
      column(4,
             
        # Wave selector
        selectInput("wave", label = h4("Select wave:"), choices = list("Wave 2", "Wave 5"), selected = "Wave 2"),
        
        p("Outbreaks were simulated using parameterisation from two differenct real world outbreaks, referred to as wave 2 and wave 5"),
         
        # Outcome metric selector
        selectInput("outcome_multi", label = h4("Select management objective:"),
                    choices = list("Mean outbreak size" = 1, "Mean outbreak duration" = 2, "Mean culled chickens (thousands)" = 3, 
                                   "Outbreak size (95th percentile)" = 4, "Outbreak duration (95th percentile)" = 5, "Culled chickens (95th percentile, thousands)" = 6),
                    selected = 1),
        
        p("Choose the management objective that you are interested in. This can be minimising: the premises outbreak size, outbreak duration, 
               or the amount of chickens culled. Results can be displayed as means or 95th percentiles.")

      )
    )
  ),
  
  tabPanel("User Guide",
           a("User Guide", href="https://github.com/nabury/HPAI/wiki/HPAI-App-User-Guide", target="_blank")

  # tabPanel(title=HTML("</a></li><li><a href='http://google.com' target='_blank'>test")
  # tabPanel(id = "guide", "User Guide",
  #          tags$iframe(style="height:1500px; width:100%; scrolling=yes",
  #                      src="User guide.pdf")
  ),
)

server <- function(input, output, session) {
  
  # Use different data set when outbreak selection changes
  Current_dataset <- reactive({
    req(input$scenario)
    if (input$scenario == 1) {Current_dataset <- fixed}
    else if (input$scenario == 2 & input$transmission == 0) {Current_dataset <- low_trans}
    else if (input$scenario == 2 & input$transmission == 50) {Current_dataset <- med_trans}
    else if (input$scenario == 2 & input$transmission == 100) {Current_dataset <- high_trans}
    else if (input$scenario == 3 & input$population == 0) {Current_dataset <- low_trans}
    else if (input$scenario == 3 & input$population == 50) {Current_dataset <- med_popn}
    else if (input$scenario == 3 & input$population == 100) {Current_dataset <- high_popn}
  })
  
  # Select current outcome of interest
  Current_outcome <- reactive({
    req(input$outcome)
    if (input$outcome == "Outbreak size") {Current_outcome <- "outbreak_size"}
    else if (input$outcome == "Outbreak duration (days)") {Current_outcome <- "outbreak_duration"}
    else if (input$outcome == "Culled premises") {Current_outcome <- "culled_premises"}
    else if (input$outcome == "Culled chickens") {Current_outcome <- "culled_chickens"}
    else if (input$outcome == "Vaccinated premises") {Current_outcome <- "vaccinated_premises"}
    else if (input$outcome == "Vaccinated chickens") {Current_outcome <- "vaccinated_chickens"}
  })

  # Code for box plot
  output$boxplot <- renderPlot({
    
    Current_dataset <- Current_dataset()
    Current_outcome <- Current_outcome()
    Current_controls <- input$control
    
    plot_data <- filter(Current_dataset, control_text_long %in% Current_controls)

    ggplot(plot_data, aes_string(y = Current_outcome)) +
      geom_boxplot(aes(x = control_text_long), width = 0.5, fill = "#e28743") +
      facet_grid(capacity ~ wave) +
      labs(
        x = "Control strategy",
        y = input$outcome) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle=45, hjust = 1))
    
  })
  
  # Code for log transformed box plot
  output$boxplot_log <- renderPlot({
    Current_dataset <- Current_dataset()
    Current_outcome <- Current_outcome()
    Current_controls <- c(input$control)
    
    ylab <- paste(input$outcome, "(log transformed)")
    
    plot_data <- filter(Current_dataset, control_text_long %in% Current_controls)
    
    ggplot(plot_data, aes_string(y = Current_outcome)) +
      geom_boxplot(aes(x = control_text_long), width = 0.5, fill = "#e28743") +
      facet_grid(capacity ~ wave) +
      labs(
        x = "Control strategy", 
        y = ylab) + 
      scale_y_log10() +
      theme_bw() +
      theme(axis.text.x = element_text(angle=45, hjust = 1))
    
  })
  
  # Code for heatmap
  
  # Use different data set when outbreak selection changes
  Current_results <- reactive({
    req(input$scenario_multi)
    if (input$scenario_multi == 1) {Current_results <- multi}
    else if (input$scenario_multi == 2) {Current_results <- multi_trans}
    else if (input$scenario_multi == 3) {Current_results <- multi_popn}
  })
  
  output$heatmap <- renderPlot({

    Current_results <- Current_results()
    Current_wave <- input$wave
    Current_trigger <- input$measure
    Current_transmission <- input$transmission_multi
    Current_population <- input$population_multi

    if (input$scenario_multi == 1) {
      plot_data <- filter(Current_results, wave %in% Current_wave & trigger_type %in% Current_trigger)
    } else if (input$scenario_multi == 2) {
      plot_data <- filter(Current_results, wave %in% Current_wave & trigger_type %in% Current_trigger & increase %in% Current_transmission)
    } else if (input$scenario_multi == 3) {
      plot_data <- filter(Current_results, wave %in% Current_wave & trigger_type %in% Current_trigger & increase %in% Current_population)
    }
    
    if (input$outcome_multi == 1) {output = plot_data$premises_mean}
    else if (input$outcome_multi == 2) {output = plot_data$duration_mean}
    else if (input$outcome_multi == 3) {output = plot_data$culled_mean/1000}
    else if (input$outcome_multi == 4) {output = plot_data$premises_95per}
    else if (input$outcome_multi == 5) {output = plot_data$duration_95per}
    else if (input$outcome_multi == 6) {output = plot_data$culled_95per/1000}
    
    ggplot(plot_data, aes(strategy.1, strategy.2, fill = output)) +
      geom_tile() +
      facet_grid(capacity ~ trigger) +
      geom_text(aes(label = signif(output, digits = 3))) +
      scale_fill_gradient(low="white", high="red") +
      labs(x = "First control", y = "Second control") +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(angle=50, hjust=1))
  })
  
  # Transmission sliders
  # Single phase
  output$transmission <- renderUI({
    sliderInput("transmission", "Increase in transmission rate from baseline:",
                min = 0, max = 100, value = 0, step = 50, post = "%")
  })
  # Multiphase
  output$transmission_multi <- renderUI({
    sliderInput("transmission_multi", "Increase in transmission rate from baseline:",
                min = 0, max = 100, value = 0, step = 50, post = "%")
  })
  
  # Population sliders
  # Single phase
  output$population <- renderUI({
    sliderInput("population", "Increase in chicken population from baseline:",
                min = 0, max = 100, value = 0, step = 50, post = "%")
  })
  # Multiphase
  output$population_multi <- renderUI({
    sliderInput("population_multi", "Increase in chicken population from baseline:",
                min = 0, max = 100, value = 0, step = 50, post = "%")
  })
  
  # Dynamic trigger selector
  output$measure <- renderUI({
    if (input$scenario_multi == 1) {
      selectInput("measure", label = h4("Select multiphase control trigger:"),
                  choices = list("Outbreak duration (days)" = 1, "Outbreak size" = 2, "Time since last IP" = 3),
                  selected = 1)
    } else {
      selectInput("measure", label = h4("Select multiphase control trigger:"),
                  choices = list("Outbreak duration (days)" = 1, "Outbreak size" = 2),
                  selected = 1)
    }
  })

}

shinyApp(ui = ui, server = server)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(drc)
library(DT)
library(lmtest)
library(sandwich)
library(geomtextpath)
source("functions/plot_functions.R")
source("functions/model_functions.R")
source("functions/data_functions.R")



ui <- dashboardPage(
  dashboardHeader(title="Dose-Response Analysis"),
  dashboardSidebar(
    
    # Set Up Sidebar ----
    sidebarMenu(id = "sidebar",
      
      ## Menu 1: Home/Start Here ----
      menuItem("Start Here", tabName = "home", icon = icon("house")),
      
      ## Menu 2: Select data  ----
      menuItem("Select Data", tabName = "data", icon = icon("database")),
      conditionalPanel(condition="input.sidebar == 'data'",
         radioButtons("data_source", "Choose Data Source:",
                      choices = c("Upload CSV", "Simulate Data"),
                      selected = "Upload CSV"),
         
         ### Set upload options ----
         conditionalPanel(
           condition = "input.data_source == 'Upload CSV'",
          fileInput("file", "Choose CSV File", accept = c(".csv")),
          "Column Selection:",
          pickerInput(
            inputId = "dose_column",
            label = "Select Dose Column",
            choices = NULL,
            options = list(`actions-box` = TRUE),
            multiple = FALSE
          ),
          pickerInput(
            inputId = "response_column",
            label = "Select Response Column",
            choices = NULL,
            options = list(`actions-box` = TRUE),
            multiple = FALSE
          ),
          pickerInput(
            inputId = "group_column",
            label = "Select Group Column (optional)",
            choices = NULL,
            selected = "none",
            options = list(`actions-box` = TRUE),
            multiple = FALSE
          ),
          actionButton("upload_button", "Upload Data")),
         
         ### Set simulation options ----
         conditionalPanel(
           condition = "input.data_source == 'Simulate Data'",
           selectInput("data_type", "Data Type",
                       choices = c("binomial", "continuous", "poisson")),
           numericInput("n_obs", "Number of Observations:", 10, min = 1),
           textInput("dose_column_sim", "Dose Label", "Dose"),
           numericRangeInput("dose_range", "Dose Range:", value = c(0, 10)),
           textInput("response_column_sim", "Response Label", "Response"),
           numericRangeInput("response_range", "Response Range:", value = c(0, 100)),
           selectInput("expected_response_design", "Expected Response",
                       choices = c("inhibition", "stimulation")),
           numericInput("seed", "Random Seed", 1983, min = 1),
           actionButton("simulate_button", "Simulate Data")
         ),
        ),
      
      ## Menu links for additional pages ----
      menuItem("Compare Models", tabName = "compare", icon = icon("code-compare")),
      menuItem("Select / Analyze Model", tabName = "select", icon = icon("magnifying-glass-chart"))
    )
  ),
  
  # Set up body ----
  dashboardBody(
    includeCSS("www/custom.css"),
    tabItems(
      
      ## Main page ----
      
      tabItem(tabName="home",
        titlePanel("Dose-Response Analysis"),
        h3("Purpose of App"),
        p("The purpose of this app is to allow you to upload or simulate data appropriate for response-dose analysis. You then have the ability to basic analysis of the selected data's response curve, including estimating parameters, interpreting parameters, inspecting visuals, and calculating effective dose (ED) levels."),
        h3("About Dose Response Analysis"),
        HTML("
<p>Dose-response analysis is a statistical method used to assess the relationship between the dose (or concentration) of a substance and its biological effect. This analysis often involves fitting a mathematical model to experimental data to describe the <b>dose-response curve</b>. Non-linear regression is commonly employed for this task as it allows for the identification of complex relationships that mayinadequately captured by linear models.</p>

<p>A common function to fit these curves is known as the 4-parameter log-logistic fitting function, also known as the sigmoidal curve or Hill equation. This equation includes parameters representing the minimum and maximum response levels, the slope of the curve at its midpoint, and the concentration at which half of the maximum response is achieved (EC50 or IC50). </p>

<p>Dose-response analysis is very flextible. Other functions can be fit to the curve (2-parameters, Weibull, asymptotic regression) and parameters can also be constrained in various ways. Group comparisons (e.g., treatment vs control) can also be conducted. The resulting information provides valuable insights into the substance's effect, aiding in the interpretation of experimental findings and guiding further investigations.</p>"),
        h3("Example of Curve with Interpretation"),
        HTML('
<p>A typical dose-response curve fit with the common 4-parameter log-logistic model can be interpreted as such:</p>

<ul>
	<li><strong>E0 </strong>represents the response in the absence of the dose. This is the lowest part of the curve.</li>
	<li><strong>Einf </strong>represents the response with the maximum amount of the dose. This is the highest part of the curve.</li>
	<li><strong>H or -b </strong>is the slope, which indicates the steepness of your curve. The slope models how the response is affected by increasing (or decreasing) dosage. This is also known as a Hill slope.</li>
	<li><strong>EC50 (or IC50)</strong> indicates dosage amount where 50% of the maximum response is seen. This is also the inflection point (on the x/dose axis) halfway up the curve. Various values can be derived beyond the 50% point, which is known as EC<sub>anything</sub> (or IC<sub>anything</sub>) or Effective Dose analysis.</li>
</ul>
<p>The following plot illustrates these values:</p>

<p><img alt="" src="https://miro.medium.com/v2/resize:fit:1400/format:webp/1*Fx95C7jQZPssFE_5hy4kVw.png" style="width:50%" /></p>

<i>Image from <a href="https://towardsdatascience.com/drug-dose-response-data-analysis-5d7d336ad8e9">Towards Data Science</a></i>

             '),
        h3("Resources"),
        HTML('
<p>This application uses the drc package in R to estimate dose-response curves and for simulation (<a href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146021" target="_blank">Ritz, Baty,  & Gerhard, 2015</a>). Please see their article for technical specification and implementation. For more information, please consult the following resources:</p>

<p> </p>

<p><span style="font-size:10px">Amemiya, T. (1983). Non-linear regression models. <em>Handbook of econometrics</em>, <em>1</em>, 333-389.</span></p>

<p><span style="font-size:10px">Ritz, C. (2010). Toward a unified approach to dose&ndash;response modeling in ecotoxicology. <em>Environmental Toxicology and Chemistry</em>, <em>29</em>(1), 220-229.</span></p>

<p><span style="font-size:10px">Ritz, C., Baty, F., Streibig, J. C.,  & Gerhard, D. (2015). Dose-response analysis using R. <em>PloS one</em>, <em>10</em>(12), e0146021. <a href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146021" target="_blank">http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146021</a></span></p>

<p><span style="font-size:10px">Ruberg, S. J. (1995). Dose response studies II. Analysis and interpretation. <em>Journal of biopharmaceutical statistics</em>, <em>5</em>(1), 15-42.</span></p>
             ')
      ),
      
      ## Select Data page ----
      tabItem(tabName = "data",
        titlePanel("Select Data"),
        
        ### upload CSV section ----
        conditionalPanel( condition = "input.data_source == 'Upload CSV'",
          h3("Upload CSV"),
          p("You can upload a CSV of data. This data should be in 'long' format. Each row represents an observation and each column a unique variable. At minimum, you must have a response column and a dose column. You may also have a group column to model different groups.")
        ),
        
        ### simulate data section ----
        conditionalPanel( condition = "input.data_source == 'Simulate Data'",
                          h3("Simulate Data"),
        HTML('
             <p>You can simulate data that is useful for experimenting with dose response analysis. You may specify the following arguments:</p>

<ul>
	<li><strong>Data Type</strong>. You may select continuous, binomial (such as a count divided by a total), or Poisson (count data)</li>
	<li><strong>Number of Observations</strong>. If you include groups, this will be the number per group.</li>
	<li><strong>Dose Label</strong>. What do you want to call your dose?</li>
	<li><strong>Dose Range</strong>. What is the numeric range of your dose? Random values will be generated within this range.</li>
	<li><strong>Response Label</strong>. What do you want to call your response (outcome)?</li>
	<li><strong>Response Range</strong>. What is the numeric range of your response? "Random values will be generated within this range.</li>
	<li><strong>Expected Response</strong>. "inhibition" if you expect the dose to decrease the response. "stimulation" for the opposite effect.</li>
	<li><strong>Random Seed</strong>. A value sed to initialize the <em>random</em> number generator. If you want to simulate different data with the same arguments, change the seed.</li>
</ul>
             ')
        ),
        # shared outputs
        uiOutput("table_output"),
        uiOutput("eda_plot")
        ),
      
      ## Compare models page ----
      tabItem(tabName="compare",
              titlePanel("Compare Models"),
             HTML("<p>Part of the modeling process is testing different specifications to see which model most closely represents your data.</p>

<p>Models can be compared by examining the AIC/BIC values, the model summaries, and plots of the models' predictions against the raw data.</p>

<p>Select the model types below. For descriptions of the parameters, please revisit 'Start Here'.</p>
"),
              checkboxGroupInput("models_for_comparison", "Select Models to Compare:",
                                 choices=model_names$choice_name),
              selectInput("model_type_compare", "Model Type",
                         choices = c("binomial", "continuous")),
              actionButton("compare_button", "Compare Models"),
              uiOutput("fit_comparison_table"),
              uiOutput("comparison_summary"),
              ),
      
      ## final model page ----
      tabItem(tabName = "select",
              titlePanel("Select a Final Model"),
              HTML('<p>You can use these options to fit your final model. A model summary, visual, and interpretation guide will be provided. In addition, you will be able to estimate Effective Doses and EC50, EC<sub>any</sub> values.</p>
'),
              selectInput("model_selector", 
                          "Select a Model Type",
                          choices = model_names$choice_name),
              selectInput("model_type", "Model Type",
                          choices = c("continuous", "binomial")),
              actionButton("fit_button", "Fit Data"),
              uiOutput("drm_summary"),
              uiOutput("model_interpretation"),
              uiOutput("ed_input"),
              uiOutput("ed_summary"),
              uiOutput("model_plot")
              
      )
      )
    )
)


server <- function(input, output, session) {
  
# SELECT DATA ----

  ## Read uploaded CSV file OR simulate data ----
  data <- reactive({
    if (input$data_source == "Upload CSV") {
      req(input$file)
      read.csv(input$file$datapath)
    } else {
      req(input$simulate_button)
      simulate_data(type = input$data_type,
                    nobs = input$n_obs,
                    dose_label = input$dose_column_sim,
                    response_label = input$response_column_sim,
                    dose_range = input$dose_range,
                    response_range = input$response_range,
                    seed = input$seed,
                    expected_response_design = input$expected_response_design)
    }
  })
  
  ## Update column choices for upload CSV dropdowns ----
  observe({
    col_choices <- colnames(data())
    updatePickerInput(session, "dose_column", choices = col_choices)
    updatePickerInput(session, "response_column", choices = col_choices)
    updatePickerInput(session, "group_column", choices = c("none", col_choices))
  })
  
  
  ## Preview data uploaded/simulated ----
  output$table_output <- renderUI({
    req(data())
    req(input$upload_button | input$simulate_button)
    tagList(
    h3("Preview of Data"),
    DTOutput("preview_table", width = "50%")
    )
  })

  output$preview_table <- renderDT(
    data(),
    options = list(searching = FALSE, lengthChange = FALSE, pageLength = 10)
  )
  
  ## Show eda plot ----
  output$eda_plot <- renderUI({
    req(data())
    req(input$upload_button | input$simulate_button)
    tagList(
      h3("Plotted Data"),
      plotOutput("rendered_eda_plot")
    )
  })

  output$rendered_eda_plot <- renderPlot({
    if(input$data_source == "Upload CSV"){
    eda_plot(data=data(), x=input$dose_column, y=input$response_column)
    } else {
    eda_plot(data=data(), x=input$dose_column_sim, y=input$response_column_sim)
    }
  })
  
# COMPARE MODELS ----
  
  ## Show AIC/BIC comparison ----
  output$fit_comparison_table <- renderUI({
    req(input$compare_button)
    tagList(
    h3("Compare AIC and BIC"),
    tableOutput("rendered_fit_comparison_table")
    )
  })
  
  output$rendered_fit_comparison_table <- renderTable({
    if(input$data_source == "Upload CSV"){
      compare_model_fit(models_to_compare = input$models_for_comparison,
                                 y=input$response_column, x=input$dose_column, data=data(),
                        type=input$model_type_compare)
    } else {
      compare_model_fit(models_to_compare = input$models_for_comparison,
                                 y=input$response_column_sim, x=input$dose_column_sim, data=data(),
                        type=input$model_type_compare)
    }
    
  })
  
  ## Print model estimates ----
  output$comparison_summary <- renderUI({
    req(input$compare_button)
    tagList(
      h3("Compare Model Estimates"),
      verbatimTextOutput("rendered_comparison_summary")
    )
  })
  
  output$rendered_comparison_summary <- renderPrint({
    if(input$data_source == "Upload CSV"){
    cat(compare_models_summary(models_to_compare = input$models_for_comparison,
                    y=input$response_column, x=input$dose_column, data=data(),
                    type=input$model_type_compare))
    } else {
      cat(compare_models_summary(models_to_compare = input$models_for_comparison,
                     y=input$response_column_sim, x=input$dose_column_sim, data=data(),
                     type=input$model_type_compare))
    }
    
  })
  
  

# FINAL MODEL ----
  
  ## fit data ----
  model_data <- reactive({
    req(input$fit_button)
    # select model type
    selected_model <- model_names[model_names$choice_name == input$model_selector, ]$argument
    if(input$data_source == "Upload CSV"){
    fit_drm(y = input$response_column,
            x = input$dose_column, 
            data = data(), 
            selected_fct=selected_model,
            type=input$model_type)
  } else {
    fit_drm(y = input$response_column_sim,
            x = input$dose_column_sim, 
            data = data(), 
            selected_fct=selected_model,
            type=input$model_type)
  }
  })
  
  
  ## Show parameter summary ----
  output$drm_summary <- renderUI({
    req(input$fit_button)
    tagList(
      h3("Model Summary"),
      checkboxInput("summary_robust", "Robust SE?", FALSE),
      verbatimTextOutput("rendered_drm_summary")
    )
  })
  
  output$rendered_drm_summary <- renderPrint({
    a_summary <- summarize_drm(model_data(), robust=input$summary_robust)
    summary_output <- capture.output(a_summary)
    cat(paste(summary_output, collapse = "\n"))
    
  })
  
  ## Give interpreted model parameters ----
  output$model_interpretation <- renderUI({
    req(input$fit_button)
    interpretation <- model_interpret(model_data())
    tagList(
      h3("Model Interpretation"),
      HTML(interpretation)
    )
  })
  
  
  ## Show ED estimation ----
  output$ed_input <- renderUI({
    req(model_data())
    req(input$fit_button)
    tagList(
      h3("Effective Dose Analyses"),
      p("Specify one or more values, separated by a comma, to estimate the expected dose for a given response level"),
      textInput("ed", label="Comma-separated levels"),
      actionButton("ed_go", "Evaluate!")
    )
  })
  
  
  output$ed_summary <- renderUI({
    req(input$ed_go)
    verbatimTextOutput("rendered_ed_summary")
  })
  

  output$rendered_ed_summary <- renderPrint({
    values <- unlist(lapply(strsplit(input$ed, ","), 
                            function(x) as.numeric(x)))
    a_summary <- ED(model_data(), values)
    summary_output <- capture.output(a_summary)
    
  })
  
  ## Observed vs predicted ----
  output$model_plot <- renderUI({
    req(input$fit_button)
    tagList(
      h3("Visual Inspection"),
      plotOutput("rendered_model_plot")
    )
  })
  

  output$rendered_model_plot <- renderPlot({
    req(model_data(), input$fit_button)
    req(input$fit_button)
    if(input$data_source == "Upload CSV"){
      plot_model(data=data(), model=model_data(), x=input$dose_column, y=input$response_column)
    } else {
      plot_model(data=data(), model=model_data(), x=input$dose_column_sim, 
                 y=input$response_column_sim)
    }
  })
}

shinyApp(ui, server)

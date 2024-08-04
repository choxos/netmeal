library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(netmeta)
library(colourpicker)
library(dmetar)
library(robvis)
library(gridExtra)

ui = dashboardPage(
        dashboardHeader(title = "NetMetal"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Data Input", tabName = "dataInput", icon = icon("table")),
                        menuItem("Model Specifications", tabName = "modelSpec", icon = icon("cogs")),
                        menuItem("Network Plot", tabName = "networkPlot", icon = icon("project-diagram")),
                        menuItem("Forest Plots", tabName = "forestPlots", icon = icon("tree")),
                        menuItem("Ranking", tabName = "ranking", icon = icon("sort-amount-down")),
                        menuItem("League Table", tabName = "leagueTable", icon = icon("th")),
                        menuItem("Net Heat Plot", tabName = "heatPlot", icon = icon("fire")),
                        menuItem("Direct Evidence Proportion", tabName = "evidenceProp", icon = icon("chart-pie")),
                        menuItem("Risk of Bias", tabName = "rob", icon = icon("exclamation-triangle")),
                        menuItem("GRADE Assessment", tabName = "grade", icon = icon("balance-scale")),
                        menuItem("Imputation", tabName = "imputation", icon = icon("calculator")),
                        menuItem("About", tabName = "about", icon = icon("info-circle"))
                )
        ),
        dashboardBody(
                tabItems(
                        # Tab 1: Data Input
                        tabItem(tabName = "dataInput",
                                tabsetPanel(
                                        # Subtab 1: Manual Data Entry
                                        tabPanel("Manual Data Entry",
                                                 fluidRow(
                                                         column(3, numericInput("nStudies", "Number of studies:", 2, min = 2)),
                                                         column(3, numericInput("nArms", "Maximum number of arms:", 2, min = 2)),
                                                         column(3, selectInput("dataType", "Data type:", choices = c("Continuous", "Binary"))),
                                                         column(3, actionButton("generateRandom", "Generate Random Data", class = "btn-info"))
                                                 ),
                                                 uiOutput("dataInputs"),
                                                 fluidRow(
                                                         column(12, 
                                                                actionButton("saveManualData", "Save Manual Data", class = "btn-success")
                                                         )
                                                 )
                                        ),
                                        # Subtab 2: File Upload
                                        tabPanel("File Upload",
                                                 fileInput("csvFile", "Choose CSV File",
                                                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                                 checkboxInput("header", "Header", TRUE)
                                        )
                                )
                        ),
                        
                        # Tab 2: Model Specifications
                        tabItem(tabName = "modelSpec",
                                fluidRow(
                                        column(6,
                                               selectInput("sm", "Summary measure:", 
                                                           choices = c("MD", "SMD", "OR", "RR", "RD")),
                                               checkboxInput("common", "Common effects model", value = TRUE),
                                               checkboxInput("random", "Random effects model", value = TRUE),
                                               selectInput("tau", "Between-study variance estimator:", 
                                                           choices = c("DL", "PM", "REML", "ML", "EB"))
                                        ),
                                        column(6,
                                               selectInput("reference", "Reference treatment:", choices = NULL),
                                               selectInput("small", "Small values are:", 
                                                           choices = c("good", "bad")),
                                               numericInput("tol", "Tolerance for convergence:", 1e-4, min = 0, step = 1e-5)
                                        )
                                ),
                                fluidRow(
                                        column(12, 
                                               actionButton("analyzeBtn", "Analyze Data", icon("play"), 
                                                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                        )
                                )
                        ),
                        
                        # Tab 3: Network Plot
                        tabItem(tabName = "networkPlot",
                                fluidRow(
                                        box(
                                                title = "Network Plot",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                width = 12,
                                                plotOutput("networkPlot", height = "600px")
                                                
                                        )
                                )
                        ),
                        
                        # Tab 4: Forest Plots
                        tabItem(tabName = "forestPlots",
                                tabsetPanel(
                                        tabPanel("Fixed Effects", 
                                                 plotOutput("forestFixedPlot", height = "800px"),
                                                 downloadButton("downloadForestFixed", "Download Plot")
                                        ),
                                        tabPanel("Random Effects", 
                                                 plotOutput("forestRandomPlot", height = "800px"),
                                                 downloadButton("downloadForestRandom", "Download Plot")
                                        ),
                                        tabPanel("Summary", 
                                                 verbatimTextOutput("forestSummary")
                                        ),
                                        tabPanel("Data Table", 
                                                 DTOutput("forestDataTable")
                                        )
                                )
                        ),
                        
                        # Tab 5: Ranking
                        tabItem(tabName = "ranking",
                                tabsetPanel(
                                        tabPanel("Rankogram (Fixed)", 
                                                 plotOutput("rankogramFixed", height = "600px"),
                                                 downloadButton("downloadRankogramFixed", "Download Fixed Effects Rankogram")
                                        ),
                                        tabPanel("Rankogram (Random)", 
                                                 plotOutput("rankogramRandom", height = "600px"),
                                                 downloadButton("downloadRankogramRandom", "Download Random Effects Rankogram")
                                        )
                                )
                        ),
                        
                        # Tab 6: League Table & Net Heat Plot
                        tabItem(tabName = "leagueTable",
                                tabsetPanel(
                                        tabPanel("League Table (Fixed)", 
                                                 DTOutput("leagueTableFixed"),
                                                 downloadButton("downloadLeagueTable", "Download Table")
                                        ),
                                        tabPanel("League Table (Random)", 
                                                 DTOutput("leagueTableRandom"),
                                                 downloadButton("downloadLeagueTable", "Download Table")
                                        )
                                )
                        ),
                        
                        # Tab 7: Net Heat Plot
                        tabItem(tabName = "heatPlot",
                                tabsetPanel(
                                        tabPanel("Net Heat Plot (Fixed)", 
                                                 plotOutput("netHeatFixed", height = "600px"),
                                                 downloadButton("downloadNetHeat", "Download Plot")
                                                 ),
                                        tabPanel("Net Heat Plot (Random)", 
                                                 plotOutput("netHeatRandom", height = "600px"),
                                                 downloadButton("downloadNetHeat", "Download Plot")
                                        )
                                )
                        ),
                        
                        # Tab 8: Direct Evidence Proportion Plot
                        tabItem(tabName = "evidenceProp",
                                fluidRow(
                                        box(
                                                title = "Direct Evidence Proportion",
                                                width = 12,
                                                plotOutput("evidenceProp", height = "600px"),
                                                downloadButton("downloadEvidenceProp", "Download Plot")
                                        )
                                )
                        ),
                        
                        # Tab 9: Risk of Bias
                        tabItem(tabName = "rob",
                                fluidRow(
                                        box(
                                                title = "Risk of Bias Assessment (RoB v2)",
                                                width = 12,
                                                uiOutput("rob_input"),
                                                actionButton("save_rob", "Save RoB Assessment"),
                                                actionButton("generate_rob_plot", "Generate RoB Plot")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                title = "Risk of Bias Plot",
                                                width = 12,
                                                plotOutput("rob_plot", height = "600px"),
                                                downloadButton("download_rob_plot", "Download RoB Plot")
                                        )
                                )
                        ),
                                
                                # Tab 10: GRADE
                        tabItem(tabName = "grade",
                                fluidRow(
                                        box(
                                                title = "GRADE Assessment",
                                                width = 12,
                                                numericInput("num_studies", "Number of studies", value = 1, min = 1),
                                                selectInput("risk_of_bias", "Risk of bias", 
                                                            choices = c("Not serious" = "Not serious", 
                                                                        "Serious" = "Serious", 
                                                                        "Very serious" = "Very serious")),
                                                selectInput("inconsistency", "Inconsistency", 
                                                            choices = c("Not serious" = "Not serious", 
                                                                        "Serious" = "Serious", 
                                                                        "Very serious" = "Very serious")),
                                                selectInput("indirectness", "Indirectness", 
                                                            choices = c("Not serious" = "Not serious", 
                                                                        "Serious" = "Serious", 
                                                                        "Very serious" = "Very serious")),
                                                selectInput("imprecision", "Imprecision", 
                                                            choices = c("Not serious" = "Not serious", 
                                                                        "Serious" = "Serious", 
                                                                        "Very serious" = "Very serious")),
                                                selectInput("publication_bias", "Publication bias", 
                                                            choices = c("Undetected" = "Undetected", 
                                                                        "Strongly suspected" = "Strongly suspected")),
                                                numericInput("n_exp", "Number of patients in experimental group", value = 0, min = 0),
                                                numericInput("n_ctrl", "Number of patients in control group", value = 0, min = 0),
                                                textInput("relative_effect", "Relative effect (95% CI)"),
                                                textInput("absolute_effect", "Absolute effect (95% CI)"),
                                                actionButton("calculate_grade", "Calculate GRADE"),
                                                textOutput("certainty_output"),
                                                hr(),
                                                textAreaInput("rob_explanation", "Risk of bias explanation", rows = 3),
                                                textAreaInput("inconsistency_explanation", "Inconsistency explanation", rows = 3),
                                                textAreaInput("indirectness_explanation", "Indirectness explanation", rows = 3),
                                                textAreaInput("imprecision_explanation", "Imprecision explanation", rows = 3),
                                                textAreaInput("publication_bias_explanation", "Publication bias explanation", rows = 3),
                                                downloadButton("download_grade", "Download GRADE Assessment")
                                        )
                                ),
                                fluidRow(
                                        box(
                                                title = "Explanations",
                                                width = 12,
                                                verbatimTextOutput("explanations_output")
                                        )
                                )
                        ),
                                
                                # Tab 11: Imputation
                        tabItem(tabName = "imputation",
                                tabBox(
                                        title = "Imputation Circumstances",
                                        id = "imputation_tabs",
                                        width = 12,
                                        tabPanel("1. Standard Error",
                                                 numericInput("se_n", "Sample Size", value = 100),
                                                 numericInput("se_se", "Standard Error", value = 1),
                                                 actionButton("se_calc", "Calculate"),
                                                 verbatimTextOutput("se_result")
                                        ),
                                        tabPanel("2. Confidence Interval",
                                                 numericInput("ci_n", "Sample Size", value = 100),
                                                 numericInput("ci_lower", "Lower CI", value = 0),
                                                 numericInput("ci_upper", "Upper CI", value = 2),
                                                 numericInput("ci_conf", "Confidence Level (e.g., 0.95)", value = 0.95),
                                                 actionButton("ci_calc", "Calculate"),
                                                 verbatimTextOutput("ci_result")
                                        ),
                                        tabPanel("3. Descriptive Statistics",
                                                 selectInput("desc_type", "Available Data",
                                                             choices = c("Median, Min, Max", "Median, Q1, Q3", "Min, Q1, Median, Q3, Max")),
                                                 numericInput("desc_n", "Sample Size", value = 100),
                                                 numericInput("desc_min", "Minimum", value = 0),
                                                 numericInput("desc_q1", "Q1", value = 25),
                                                 numericInput("desc_median", "Median", value = 50),
                                                 numericInput("desc_q3", "Q3", value = 75),
                                                 numericInput("desc_max", "Maximum", value = 100),
                                                 actionButton("desc_calc", "Calculate"),
                                                 verbatimTextOutput("desc_result")
                                        ),
                                        tabPanel("4. Pooled SD",
                                                 numericInput("pool_n1", "Sample Size 1", value = 50),
                                                 numericInput("pool_sd1", "SD 1", value = 10),
                                                 numericInput("pool_n2", "Sample Size 2", value = 50),
                                                 numericInput("pool_sd2", "SD 2", value = 12),
                                                 numericInput("pool_mean1", "Mean 1", value = 100),
                                                 numericInput("pool_mean2", "Mean 2", value = 105),
                                                 actionButton("pool_calc", "Calculate"),
                                                 verbatimTextOutput("pool_result")
                                        ),
                                        tabPanel("5. SD for Change Score",
                                                 numericInput("change_n", "Sample Size", value = 100),
                                                 numericInput("change_sd_baseline", "SD Baseline", value = 10),
                                                 numericInput("change_sd_post", "SD Post", value = 12),
                                                 numericInput("change_corr", "Correlation", value = 0.5),
                                                 actionButton("change_calc", "Calculate"),
                                                 verbatimTextOutput("change_result")
                                        ),
                                        tabPanel("6. SE of Difference",
                                                 numericInput("diff_n1", "Sample Size 1", value = 50),
                                                 numericInput("diff_n2", "Sample Size 2", value = 50),
                                                 numericInput("diff_se", "SE of Difference", value = 2),
                                                 actionButton("diff_calc", "Calculate"),
                                                 verbatimTextOutput("diff_result")
                                        ),
                                        tabPanel("7. Effect Estimate with CI",
                                                 numericInput("effect_n1", "Sample Size 1", value = 50),
                                                 numericInput("effect_n2", "Sample Size 2", value = 50),
                                                 numericInput("effect_est", "Effect Estimate", value = 5),
                                                 numericInput("effect_lower", "Lower CI", value = 2),
                                                 numericInput("effect_upper", "Upper CI", value = 8),
                                                 numericInput("effect_conf", "Confidence Level (e.g., 0.95)", value = 0.95),
                                                 actionButton("effect_calc", "Calculate"),
                                                 verbatimTextOutput("effect_result")
                                        ),
                                        tabPanel("8. Effect Estimate with Z-score",
                                                 numericInput("z_n1", "Sample Size 1", value = 50),
                                                 numericInput("z_n2", "Sample Size 2", value = 50),
                                                 numericInput("z_est", "Effect Estimate", value = 5),
                                                 numericInput("z_score", "Z-score", value = 1.96),
                                                 actionButton("z_calc", "Calculate"),
                                                 verbatimTextOutput("z_result")
                                        ),
                                        tabPanel("9. Effect Estimate with t-value",
                                                 numericInput("t_n1", "Sample Size 1", value = 50),
                                                 numericInput("t_n2", "Sample Size 2", value = 50),
                                                 numericInput("t_est", "Effect Estimate", value = 5),
                                                 numericInput("t_value", "t-value", value = 2),
                                                 actionButton("t_calc", "Calculate"),
                                                 verbatimTextOutput("t_result")
                                        ),
                                        tabPanel("10. Effect Estimate with p-value",
                                                 numericInput("p_n1", "Sample Size 1", value = 50),
                                                 numericInput("p_n2", "Sample Size 2", value = 50),
                                                 numericInput("p_est", "Effect Estimate", value = 5),
                                                 numericInput("p_value", "p-value", value = 0.05),
                                                 radioButtons("p_dist", "Distribution", choices = c("z", "t"), selected = "z"),
                                                 actionButton("p_calc", "Calculate"),
                                                 verbatimTextOutput("p_result")
                                        )
                                )
                        ),
                        
                        # Tab 10: About
                        tabItem(tabName = "about",
                                fluidRow(
                                        box(
                                                title = "About the Creator",
                                                width = 12,
                                                p("Creator: Ahmad Sofi-Mahmudi"),
                                                br(),
                                                p("Social Media:"),
                                                tags$div(
                                                        tags$a("Blog", href = "https://choxos.com", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("GitHub", href = "https://github.com/choxos", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("Twitter", href = "https://twitter.com/ASofiMahmudi", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("LinkedIn", href = "https://www.linkedin.com/in/asofimahmudi/", target = "_blank")
                                                )
                                        )
                                )
                        )
                )
        )
)

server = function(input, output, session) {
        
        rv = reactiveValues(
                data = NULL,
                p_data = NULL,
                nma_results = NULL,
                rob_data = NULL
        )
        
        # Manual Data Entry
        output$dataInputs = renderUI({
                req(input$nStudies, input$nArms, input$dataType)
                lapply(1:input$nStudies, function(i) {
                        box(
                                title = NULL,
                                width = 12,
                                fluidRow(
                                        column(3, textInput(paste0("studyLabel_", i), "Study Label:", value = paste("Study", i)))
                                ),
                                lapply(1:input$nArms, function(j) {
                                        fluidRow(
                                                column(2, textInput(paste0("arm", i, "_", j), paste("Arm", j, "name:"))),
                                                column(2, numericInput(paste0("n", i, "_", j), "n:", value = NA)),
                                                if(input$dataType == "Binary") {
                                                        column(2, numericInput(paste0("event", i, "_", j), "Events:", value = NA))
                                                } else {
                                                        list(
                                                                column(2, numericInput(paste0("mean", i, "_", j), "Mean:", value = NA)),
                                                                column(2, numericInput(paste0("sd", i, "_", j), "SD:", value = NA))
                                                        )
                                                }
                                        )
                                })
                        )
                })
        })
        
        observeEvent(input$generateRandom, {
                set.seed(as.numeric(Sys.time()))  # Set a seed for reproducibility
                
                for(i in 1:input$nStudies) {
                        # Randomly select 50% to 100% of arms for this study
                        num_arms_to_fill = sample(ceiling(input$nArms/3):input$nArms, 1)
                        arms_to_fill = sample(1:input$nArms, num_arms_to_fill)
                        
                        for(j in 1:input$nArms) {
                                updateTextInput(session, paste0("arm", i, "_", j), value = paste("Arm", j))
                                
                                if(j %in% arms_to_fill) {
                                        updateNumericInput(session, paste0("n", i, "_", j), value = round(runif(1, 50, 200)))
                                        
                                        if(input$dataType == "Binary") {
                                                n = as.numeric(input[[paste0("n", i, "_", j)]])
                                                updateNumericInput(session, paste0("event", i, "_", j), value = round(runif(1, 0, n)))
                                        } else {
                                                updateNumericInput(session, paste0("mean", i, "_", j), value = round(rnorm(1, 50, 10), 1))
                                                updateNumericInput(session, paste0("sd", i, "_", j), value = round(runif(1, 5, 15), 1))
                                        }
                                } else {
                                        # Clear the inputs for arms not selected
                                        updateNumericInput(session, paste0("n", i, "_", j), value = NA)
                                        if(input$dataType == "Binary") {
                                                updateNumericInput(session, paste0("event", i, "_", j), value = NA)
                                        } else {
                                                updateNumericInput(session, paste0("mean", i, "_", j), value = NA)
                                                updateNumericInput(session, paste0("sd", i, "_", j), value = NA)
                                        }
                                }
                        }
                }
        })
        
        observeEvent(input$saveManualData, {
                req(input$nStudies, input$nArms, input$dataType)
                
                data = data.frame()
                
                for(i in 1:input$nStudies) {
                        studyLabel = input[[paste0("studyLabel_", i)]]
                        for(j in 1:input$nArms) {
                                row = data.frame(
                                        study = studyLabel,
                                        t = input[[paste0("arm", i, "_", j)]],
                                        n = as.numeric(input[[paste0("n", i, "_", j)]])
                                )
                                
                                if (input$dataType == "Binary") {
                                        row$event = as.numeric(input[[paste0("event", i, "_", j)]])
                                } else {
                                        row$y = as.numeric(input[[paste0("mean", i, "_", j)]])
                                        row$sd = as.numeric(input[[paste0("sd", i, "_", j)]])
                                }
                                
                                data = rbind(data, row)
                        }
                }
                
                # Remove any rows with NA values
                data = na.omit(data)
                
                rv$data = data
                
                # Prepare pairwise data
                if (input$dataType == "Binary") {
                        rv$p_data = pairwise(treat = t, event = event, n = n, studlab = study, 
                                              data = data, sm = input$sm)
                } else {
                        rv$p_data = pairwise(treat = t, mean = y, sd = sd, n = n, studlab = study, 
                                              data = data, sm = input$sm)
                }
                
                showNotification("Manual data saved successfully!", type = "message")
        })
        
        # Read CSV file
        observeEvent(input$csvFile, {
                req(input$csvFile)
                rv$data = read.csv(input$csvFile$datapath, header = input$header)
        })
        
        availableSummaryMeasures = reactive({
                if(input$dataType == "Binary") {
                        c("OR", "RR", "RD")
                } else {
                        c("MD", "SMD")
                }
        })
        
        # Update the summary measure select input
        observe({
                updateSelectInput(session, "sm", choices = availableSummaryMeasures())
        })
        
        # Update reference treatment choices
        observe({
                req(rv$data)
                choices = if ("t" %in% names(rv$data)) {
                        unique(rv$data$t)
                } else {
                        unique(c(rv$data$t1, rv$data$t2, rv$data$t3))
                }
                updateSelectInput(session, "reference", choices = choices)
        })
        
        # After analysis, update again with the actual treatments used in the model
        observeEvent(rv$nma_results, {
                req(rv$nma_results)
                updateSelectInput(session, "reference", choices = rv$nma_results$trts)
        })
        
        # Analyze Data
        observeEvent(input$analyzeBtn, {
                req(rv$p_data)
                
                withProgress(message = 'Analyzing data...', value = 0, {
                        tryCatch({
                                
                                # Ensure logical arguments are not NULL
                                common = ifelse(is.null(input$common), FALSE, as.logical(input$common))
                                random = ifelse(is.null(input$random), TRUE, as.logical(input$random))
                                small.values = ifelse(is.null(input$small), "bad", input$small)
                                
                                # Perform network meta-analysis
                                # Random
                                rv$nma_results_random = netmeta(rv$p_data,
                                                          sm = input$sm,
                                                          common = F,
                                                          random = T,
                                                          reference.group = input$reference,
                                                          small.values = small.values,
                                                          method.tau = input$tau,
                                                          tol.multiarm = as.numeric(input$tol))
                                
                                # Fixed
                                rv$nma_results_fixed = netmeta(rv$p_data,
                                                                 sm = input$sm,
                                                                 common = T,
                                                                 random = F,
                                                                 reference.group = input$reference,
                                                                 small.values = small.values,
                                                                 tol.multiarm = as.numeric(input$tol))
                                
                                incProgress(1)
                                
                                # Update reference treatment choices
                                updateSelectInput(session, "reference", choices = rv$nma_results_random$trts)
                                
                                # Show success message
                                showNotification("Network meta-analysis completed successfully!", 
                                                 type = "message", duration = 5)
                                
                        }, error = function(e) {
                                # If an error occurs, show it to the user
                                print(paste("Error occurred:", e$message))
                                print("Error traceback:")
                                print(traceback())
                                showNotification(paste("Error in analysis:", e$message), 
                                                 type = "error", duration = NULL)
                        })
                })
        })
        
        
        # Network Plot
        output$networkPlot = renderPlot({
                req(rv$nma_results_random)
                tryCatch({
                        netgraph(rv$nma_results_random)
                }, error = function(e) {
                        print(paste("Error in netgraph:", e$message))
                })
        })
        
        # Forest Plots
        output$forestFixedPlot = renderPlot({
                req(rv$nma_results_fixed)
                tryCatch({
                        forest(rv$nma_results_fixed, 
                               reference.group = input$reference,
                               drop.reference.group = TRUE,
                               sortvar = TE)
                }, error = function(e) {
                        print(paste("Error in forest plot (fixed):", e$message))
                })
        }, res = 120)
        
        output$forestRandomPlot = renderPlot({
                req(rv$nma_results_random)
                tryCatch({
                        forest(rv$nma_results_random, 
                               reference.group = input$reference,
                               drop.reference.group = TRUE,
                               sortvar = TE)
                }, error = function(e) {
                        print(paste("Error in forest plot (random):", e$message))
                })
        }, res = 120)
        
        output$forestSummary = renderPrint({
                req(rv$nma_results_random)
                summary(rv$nma_results_random)
        })
        
        output$forestDataTable = renderDT({
                req(rv$nma_results_random)
                as.data.frame(rv$nma_results_random)
        })
        
        # Rankogram
        # Fixed
        output$rankogramFixed = renderPlot({
                req(rv$nma_results_fixed)
                tryCatch({
                        rankingF = rankogram(rv$nma_results_fixed, nsim = 1000)
                        plot(rankingF, type = "l")
                }, error = function(e) {
                        print(paste("Error in rankogram:", e$message))
                })
        })
        
        # Random
        set.seed(1280)
        output$rankogramRandom = renderPlot({
                req(rv$nma_results_random)
                tryCatch({
                        rankingR = rankogram(rv$nma_results_random, nsim = 1000)
                        plot(rankingR, type = "l")
                }, error = function(e) {
                        print(paste("Error in rankogram:", e$message))
                })
        })
        
        # League Table
        # Fixed
        output$leagueTableFixed = renderDT({
                req(rv$nma_results_fixed)
                tryCatch({
                        league_tableF = netleague(rv$nma_results_fixed)
                        datatable(as.data.frame(league_tableF$fixed))
                }, error = function(e) {
                        print(paste("Error in league table:", e$message))
                })
        })
        
        # Random
        output$leagueTableRandom = renderDT({
                req(rv$nma_results_random)
                tryCatch({
                        league_tableR = netleague(rv$nma_results_random)
                        datatable(as.data.frame(league_tableR$random))
                }, error = function(e) {
                        print(paste("Error in league table:", e$message))
                })
        })
        
        # Net Heat Plot
        # Fixed
        output$netHeatFixed = renderPlot({
                req(rv$nma_results_fixed)
                tryCatch({
                        netheat(rv$nma_results_fixed)
                }, error = function(e) {
                        print(paste("Error in net heat plot:", e$message))
                })
        })
        
        # Random
        output$netHeatRandom = renderPlot({
                req(rv$nma_results_random)
                tryCatch({
                        netheat(rv$nma_results_random, random = T)
                }, error = function(e) {
                        print(paste("Error in net heat plot:", e$message))
                })
        })
        
        # Evidence Proportion Plot
        # Fixed
        output$evidenceProp = renderPlot({
                req(rv$nma_results_fixed)
                tryCatch({
                        d.evidence = dmetar::direct.evidence.plot(rv$nma_results_fixed)
                        plot(d.evidence)
                }, error = function(e) {
                        print(paste("Error in evidence prop plot:", e$message))
                })
        })
        
        # RoB2
        output$rob_input = renderUI({
                req(rv$data)
                study_labels = unique(rv$data$study)
                
                tagList(
                        lapply(seq_along(study_labels), function(i) {
                                fluidRow(
                                        column(2, textInput(paste0("rob_studlab_", i), "Study label", value = study_labels[i])),
                                        column(1, selectInput(paste0("rob_D1_", i), "D1", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_D2_", i), "D2", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_D3_", i), "D3", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_D4_", i), "D4", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_D5_", i), "D5", choices = c("Low", "Some concerns", "High"))),
                                        column(1, selectInput(paste0("rob_Overall_", i), "Overall", choices = c("Low", "Some concerns", "High")))
                                )
                        }),
                        tags$script(HTML(paste0("$(document).ready(function() {",
                                                paste(sprintf("$('#rob_studlab_%d').prop('readonly', true);", seq_along(study_labels)), collapse = "\n"),
                                                "});")
                        ))
                )
        })
        
        observeEvent(input$save_rob, {
                req(rv$data)
                study_labels = unique(rv$data$study)
                
                rob_df = data.frame(
                        Study = study_labels,
                        D1 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D1_", i)]]),
                        D2 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D2_", i)]]),
                        D3 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D3_", i)]]),
                        D4 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D4_", i)]]),
                        D5 = sapply(seq_along(study_labels), function(i) input[[paste0("rob_D5_", i)]]),
                        Overall = sapply(seq_along(study_labels), function(i) input[[paste0("rob_Overall_", i)]])
                )
                
                rv$rob_data = rob_df
                
                
                showNotification("Risk of Bias assessment saved successfully", type = "message")
        })
        
        rob_plot = reactiveVal()
        
        observeEvent(input$generate_rob_plot, {
                req(rv$rob_data)
                
                plot = rob_traffic_light(rv$rob_data, tool = "ROB2")
                rob_plot(plot)
                
                output$rob_plot = renderPlot({
                        plot
                })
                
                showNotification("Risk of Bias plot generated successfully", type = "message")
        })
        
        output$download_rob_plot = downloadHandler(
                filename = function() {
                        paste("rob_plot_", Sys.Date(), ".png", sep = "")
                },
                content = function(file) {
                        req(rob_plot())
                        ggsave(file, plot = rob_plot(), device = "png", width = 10, height = 8, units = "in", dpi = 300)
                }
        )
        
        # GRADE
        grade_result = reactiveVal(NULL)
        
        observeEvent(input$calculate_grade, {
                # Convert descriptive terms to numeric values for calculation
                term_to_value = function(term) {
                        switch(term,
                               "Not serious" = 0,
                               "Serious" = -1,
                               "Very serious" = -2,
                               "Undetected" = 0,
                               "Strongly suspected" = -1,
                               0)  # default case
                }
                
                total_downgrade = term_to_value(input$risk_of_bias) + 
                        term_to_value(input$inconsistency) + 
                        term_to_value(input$indirectness) + 
                        term_to_value(input$imprecision) + 
                        term_to_value(input$publication_bias)
                
                certainty = case_when(
                        total_downgrade == 0 ~ "⨁⨁⨁⨁ High",
                        total_downgrade == -1 ~ "⨁⨁⨁◯ Moderate",
                        total_downgrade == -2 ~ "⨁⨁◯◯ Low",
                        total_downgrade <= -3 ~ "⨁◯◯◯ Very low"
                )
                
                grade_result(list(
                        num_studies = input$num_studies,
                        risk_of_bias = input$risk_of_bias,
                        inconsistency = input$inconsistency,
                        indirectness = input$indirectness,
                        imprecision = input$imprecision,
                        publication_bias = input$publication_bias,
                        n_exp = input$n_exp,
                        n_ctrl = input$n_ctrl,
                        relative_effect = input$relative_effect,
                        absolute_effect = input$absolute_effect,
                        certainty = certainty,
                        rob_explanation = input$rob_explanation,
                        inconsistency_explanation = input$inconsistency_explanation,
                        indirectness_explanation = input$indirectness_explanation,
                        imprecision_explanation = input$imprecision_explanation,
                        publication_bias_explanation = input$publication_bias_explanation
                ))
                
                output$certainty_output = renderText({
                        paste("Certainty of evidence:", certainty)
                })
                
                output$explanations_output = renderText({
                        result = grade_result()
                        explanations = c()
                        if (result$risk_of_bias != "Not serious") explanations = c(explanations, paste("Risk of bias:", result$rob_explanation))
                        if (result$inconsistency != "Not serious") explanations = c(explanations, paste("Inconsistency:", result$inconsistency_explanation))
                        if (result$indirectness != "Not serious") explanations = c(explanations, paste("Indirectness:", result$indirectness_explanation))
                        if (result$imprecision != "Not serious") explanations = c(explanations, paste("Imprecision:", result$imprecision_explanation))
                        if (result$publication_bias != "Undetected") explanations = c(explanations, paste("Publication bias:", result$publication_bias_explanation))
                        paste(explanations, collapse = "\n\n")
                        
                })
        })
        
        # Update the download handler to use these descriptive terms
        output$download_grade = downloadHandler(
                filename = function() {
                        paste("grade_assessment_", Sys.Date(), ".docx", sep = "")
                },
                content = function(file) {
                        result = grade_result()
                        
                        if (is.null(result) || length(result) == 0) {
                                stop("No GRADE assessment data available. Please calculate GRADE first.")
                        }
                        
                        tryCatch({
                                doc = read_docx()
                                doc = body_add_par(doc, "GRADE Assessment", style = "heading 1")
                                
                                # Create table data in wide format
                                tab_data = data.frame(
                                        `# studies` = result$num_studies,
                                        `Risk of bias` = result$risk_of_bias,
                                        Inconsistency = result$inconsistency,
                                        Indirectness = result$indirectness,
                                        Imprecision = result$imprecision,
                                        `Publication bias` = result$publication_bias,
                                        `# patients (exp)` = result$n_exp,
                                        `# patients (ctrl)` = result$n_ctrl,
                                        `Relative effect` = result$relative_effect,
                                        `Absolute effect` = result$absolute_effect,
                                        Certainty = result$certainty
                                )
                                
                                # Create flextable
                                ft = flextable(tab_data)
                                ft = autofit(ft)
                                ft = theme_box(ft)
                                ft = set_table_properties(ft, layout = "autofit")
                                ft = bold(ft, part = "header")
                                ft = width(ft, width = 1)  # Set table width to 100% of page width
                                ft = fontsize(ft, size = 6.5, part = "all")  # Reduce font size if needed
                                
                                # Adjust column widths if necessary
                                for (col in names(tab_data)) {
                                        ft = width(ft, j = col, width = 0.8)  # Adjust this value as needed
                                }
                                
                                # Add table to document
                                doc = body_add_flextable(doc, ft)
                                
                                doc = body_add_par(doc, "Explanations", style = "heading 2")
                                if (result$risk_of_bias != "Not serious") doc = body_add_par(doc, paste("Risk of bias:", result$rob_explanation))
                                if (result$inconsistency != "Not serious") doc = body_add_par(doc, paste("Inconsistency:", result$inconsistency_explanation))
                                if (result$indirectness != "Not serious") doc = body_add_par(doc, paste("Indirectness:", result$indirectness_explanation))
                                if (result$imprecision != "Not serious") doc = body_add_par(doc, paste("Imprecision:", result$imprecision_explanation))
                                if (result$publication_bias != "Undetected") doc = body_add_par(doc, paste("Publication bias:", result$publication_bias_explanation))
                                
                                print(doc, target = file)
                        }, error = function(e) {
                                stop(paste("Error generating GRADE assessment document:", e$message))
                        })
                }
        )
        
        # Imputation
        observeEvent(input$se_calc, {
                sd = sqrt(input$se_n) * input$se_se
                output$se_result = renderPrint({
                        cat("Estimated SD:", round(sd, 4))
                })
        })
        
        observeEvent(input$ci_calc, {
                if (input$ci_n >= 60) {
                        z = z_score((1 - input$ci_conf) / 2)
                        se = (input$ci_upper - input$ci_lower) / (2 * z)
                } else {
                        t = t_score((1 - input$ci_conf) / 2, input$ci_n - 1)
                        se = (input$ci_upper - input$ci_lower) / (2 * t)
                }
                sd = sqrt(input$ci_n) * se
                output$ci_result = renderPrint({
                        cat("Estimated SD:", round(sd, 4))
                })
        })
        
        observeEvent(input$desc_calc, {
                n = input$desc_n
                if (input$desc_type == "Median, Min, Max") {
                        a = input$desc_min
                        m = input$desc_median
                        b = input$desc_max
                        est_mean = if (n < 25) (a + 2*m + b) / 4 else m
                        est_sd = if (n <= 15) sqrt(((b-a)^2 + (a-2*m+b)^2) / 12)
                        else if (n <= 70) (b-a) / 4
                        else (b-a) / 6
                } else if (input$desc_type == "Median, Q1, Q3") {
                        q1 = input$desc_q1
                        m = input$desc_median
                        q3 = input$desc_q3
                        est_mean = (0.7 + 0.39/n) * (q1+q3)/2 + (0.3 - 0.39/n) * m
                        est_sd = (q3-q1) / (2 * qnorm((0.75*n - 0.125) / (n + 0.25)))
                } else {
                        a = input$desc_min
                        q1 = input$desc_q1
                        m = input$desc_median
                        q3 = input$desc_q3
                        b = input$desc_max
                        w = 2.2 / (2.2 + n^0.75)
                        est_mean = w * (a+b)/2 + (0.7 - 0.72/n^0.55) * (q1+q3)/2 + (0.3 + 0.72/n^0.55 - w) * m
                        est_sd = (b-a)/(4*qnorm((n-0.375)/(n+0.25))) + (q3-q1)/(4*qnorm((0.75*n-0.125)/(n+0.25)))
                }
                output$desc_result = renderPrint({
                        cat("Estimated Mean:", round(est_mean, 4), "\n")
                        cat("Estimated SD:", round(est_sd, 4))
                })
        })
        
        observeEvent(input$pool_calc, {
                n1 = input$pool_n1
                n2 = input$pool_n2
                sd1 = input$pool_sd1
                sd2 = input$pool_sd2
                m1 = input$pool_mean1
                m2 = input$pool_mean2
                pooled_sd = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2 + n1*n2/(n1+n2)*(m1^2+m2^2-2*m1*m2)) / (n1+n2-1))
                output$pool_result = renderPrint({
                        cat("Pooled SD:", round(pooled_sd, 4))
                })
        })
        
        observeEvent(input$change_calc, {
                sd_change = sqrt(input$change_sd_baseline^2 + input$change_sd_post^2 - 
                                         2 * input$change_corr * input$change_sd_baseline * input$change_sd_post)
                output$change_result = renderPrint({
                        cat("SD for Change Score:", round(sd_change, 4))
                })
        })
        
        observeEvent(input$diff_calc, {
                sd_avg = input$diff_se / sqrt(1/input$diff_n1 + 1/input$diff_n2)
                output$diff_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        observeEvent(input$effect_calc, {
                if (input$effect_n1 + input$effect_n2 >= 60) {
                        z = z_score((1 - input$effect_conf) / 2)
                        se = (input$effect_upper - input$effect_lower) / (2 * z)
                } else {
                        t = t_score((1 - input$effect_conf) / 2, input$effect_n1 + input$effect_n2 - 2)
                        se = (input$effect_upper - input$effect_lower) / (2 * t)
                }
                sd_avg = se / sqrt(1/input$effect_n1 + 1/input$effect_n2)
                output$effect_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        observeEvent(input$z_calc, {
                se = abs(input$z_est / input$z_score)
                sd_avg = se / sqrt(1/input$z_n1 + 1/input$z_n2)
                output$z_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        observeEvent(input$t_calc, {
                se = abs(input$t_est / input$t_value)
                sd_avg = se / sqrt(1/input$t_n1 + 1/input$t_n2)
                output$t_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        observeEvent(input$p_calc, {
                if (input$p_dist == "z") {
                        score = z_score(input$p_value)
                } else {
                        score = t_score(input$p_value, input$p_n1 + input$p_n2 - 2)
                }
                se = abs(input$p_est / score)
                sd_avg = se / sqrt(1/input$p_n1 + 1/input$p_n2)
                output$p_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        # Download handlers
        output$downloadForestFixed = downloadHandler(
                filename = function() paste("forest_fixed_", Sys.Date(), ".png", sep = ""),
                content = function(file) {
                        req(rv$nma_results_fixed)
                        png(file)
                        forest(rv$nma_results_fixed)
                        dev.off()
                }
        )
        
        output$downloadForestRandom = downloadHandler(
                filename = function() paste("forest_random_", Sys.Date(), ".png", sep = ""),
                content = function(file) {
                        req(rv$nma_results_random)
                        png(file)
                        forest(rv$nma_results_random)
                        dev.off()
                }
        )
        
        output$downloadRankogramFixed = downloadHandler(
                filename = function() paste("rankogram_fixed_", Sys.Date(), ".png", sep = ""),
                content = function(file) {
                        req(rv$nma_results_fixed)
                        png(file, res = 120)
                        rankingF = rankogram(rv$nma_results_fixed, nsim = 1000)
                        plot(rankingF, type = "l")
                        dev.off()
                }
        )
        
        output$downloadRankogramRandom = downloadHandler(
                filename = function() paste("rankogram_random_", Sys.Date(), ".png", sep = ""),
                content = function(file) {
                        req(rv$nma_results_random)
                        png(file, res = 120)
                        rankingR = rankogram(rv$nma_results_random, nsim = 1000)
                        plot(rankingR, type = "l")
                        dev.off()
                }
        )
        
        output$downloadLeagueTable = downloadHandler(
                filename = function() paste("league_table_", Sys.Date(), ".csv", sep = ""),
                content = function(file) {
                        req(rv$nma_results_random)  # or rv$nma_results_fixed, depending on your preference
                        write.csv(netleague(rv$nma_results_random), file, row.names = FALSE)
                }
        )
        
        output$downloadNetHeat = downloadHandler(
                filename = function() paste("net_heat_", Sys.Date(), ".png", sep = ""),
                content = function(file) {
                        req(rv$nma_results_random)  # or rv$nma_results_fixed, depending on your preference
                        png(file)
                        netheat(rv$nma_results_random)
                        dev.off()
                }
        )
}

shinyApp(ui = ui, server = server)
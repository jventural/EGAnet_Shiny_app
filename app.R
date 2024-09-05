library(shiny)
library(shinydashboard)
library(readxl)
library(EGAnet)
library(ggplot2)
library(dplyr)
library(tibble)
library(openxlsx)
library(parallel)
library(shinyjs)

# Función para aplicar colores y estilos
apply_colors <- function(top_bar, side_bar, button_bg, button_border, tab_active_border, tab_active_top, upload_complete_bg, toggle_button, notif_font_size, notif_border_color, notif_bg_color, notif_text_color) {
  css <- "
    .main-header .navbar, .main-header .logo {
      background-color: %s !important;
    }
    .main-sidebar {
      background-color: %s !important;
    }
    .btn, .plot-download-container {
      background-color: %s !important;
      border-color: %s !important;
      color: #FFFFFF !important;
    }
    .progress-bar {
      background-color: %s !important;
    }
    .skin-blue .main-header .navbar .nav > li > a {
      border-bottom: 3px solid %s !important;
    }
    .skin-blue .main-header .navbar .nav > li.active > a {
      border-bottom-color: %s !important;
    }
    .shiny-file-input-progress .progress-bar-success {
      background-color: %s !important;
    }
    .nav-tabs-custom > .nav-tabs > li.active > a, .nav-tabs-custom > .nav-tabs > li.active:hover > a {
      border-color: %s !important;
    }
    .nav-tabs-custom > .nav-tabs > li.active {
      border-top-color: %s !important;
    }
    .shiny-file-input-progress .progress-bar {
      background-color: %s !important;
    }
    .main-header .sidebar-toggle {
      background-color: %s !important;
    }
    .shiny-notification {
      font-size: %s !important;
      border: 2px solid %s !important;
      background-color: %s !important;
      color: %s !important;
    }
  "
  
  args <- list(top_bar, side_bar, button_bg, button_border, button_bg, tab_active_border, tab_active_border, upload_complete_bg, tab_active_border, tab_active_top, upload_complete_bg, toggle_button, notif_font_size, notif_border_color, notif_bg_color, notif_text_color)
  args <- lapply(args, function(x) if (is.null(x) || x == "none") "transparent" else x)
  
  css <- do.call(sprintf, c(css, args))
  
  return(css)
}

css <- apply_colors(
  top_bar = "#899DA4",
  side_bar = "#899DA4",
  button_bg = "#DC863B",
  button_border = "#DC863B",
  tab_active_border = NULL,
  tab_active_top = "#DC863B",
  upload_complete_bg = "#5D6D7E",
  toggle_button = "#899DA4",
  notif_font_size = "16px",
  notif_border_color = NULL,
  notif_bg_color = "#DC863B",
  notif_text_color = "#F9F9F9"
)

ui <- dashboardPage(
  dashboardHeader(title = "EGAnet Shiny"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&family=Roboto:wght@400;700&display=swap"),
      tags$style(HTML(css))
    ),
    fileInput("file", "Upload Excel file with test items", accept = c(".xlsx")),
    uiOutput("groupColumnUI"),
    selectizeInput("removeItemsManual", "Select items to remove:", choices = NULL, multiple = TRUE),
    selectInput("corr", "Correlation type:", choices = c("cor_auto", "spearman", "pearson")),
    selectInput("model", "Model:", choices = c("glasso", "TMFG")),
    selectInput("algorithm", "Algorithm:", choices = c("louvain", "walktrap", "leiden", "fast_greedy")),
    numericInput("resolution_parameter", "Resolution Parameter (Leiden):", value = 0.05, min = 0, step = 0.01),
    selectInput("objective_function", "Objective Function (Leiden):", choices = c("CPM", "modularity")),
    numericInput("iter", "Number of iterations for bootstrapping:", value = 100, min = 1),
    numericInput("seed", "Seed for reproducibility:", value = 2024),
    selectInput("type", "Bootstrapping type:", choices = c("resampling", "parametric")),
    selectInput("p_type", "Type of p:", choices = c("p_BH", "p")),
    numericInput("p_value", "p-value:", value = 0.05, min = 0, step = 0.01),
    numericInput("configural_threshold", "Configural threshold:", value = 0.70, min = 0, step = 0.01),
    actionButton("runBootEGA", "Run Reliability", class = "btn btn-primary btn-calcular"),
    actionButton("runInvariance", "Run Invariance", class = "btn btn-primary btn-calcular")
  ),
  dashboardBody(
    useShinyjs(),
    tabBox(
      id = "tabset1", height = "600px", width = 10,
      tabPanel("Validation of Internal Structure", 
               fluidRow(
                 column(8,
                        div(class = "plot-table-container",
                            plotOutput("plotEGA", width = "800px", height = "600px"),
                            fluidRow(
                              column(6, 
                                     h4("Network Loadings"),  # Título encima de la tabla de cargas de red
                                     tableOutput("networkLoads")),    
                              column(6, 
                                     h4("Informative Table"), # Título encima de la tabla informativa
                                     tableOutput("informativeTable")) 
                            )
                        )
                 ),
                 column(4,
                        actionButton("toggleSettings", "Show/Hide Plot Options"),
                        div(id = "plotSettingsEGA", style = "display: none;", 
                            numericInput("widthEGA", "Plot width (inches):", value = 8, min = 1, step = 1),
                            numericInput("heightEGA", "Plot height (inches):", value = 8, min = 1, step = 1),
                            numericInput("dpiEGA", "Plot resolution (dpi):", value = 600, min = 100, step = 100)
                        ),
                        downloadButton("downloadEGAPlot", "Download EGA Plot", class = "plot-download-container"),
                        downloadButton("downloadNetworkLoads", "Download Network Loads", class = "plot-download-container"),
                        downloadButton("downloadInformativeTable", "Download informative table", class = "plot-download-container") # Botón con nuevo nombre
                 )
               )
      ),
      tabPanel("Reliability", 
               fluidRow(
                 column(8,
                        div(class = "plot-table-container",
                            plotOutput("plotBootEGA", width = "800px", height = "600px"),
                            tableOutput("structuralConsistency")
                        )
                 ),
                 column(4,
                        actionButton("toggleSettingsBootEGA", "Show/Hide Plot Options"),
                        div(id = "plotSettingsBootEGA", style = "display: none;", 
                            numericInput("widthBootEGA", "Plot width (inches):", value = 8, min = 1, step = 1),
                            numericInput("heightBootEGA", "Plot height (inches):", value = 8, min = 1, step = 1),
                            numericInput("dpiBootEGA", "Plot resolution (dpi):", value = 600, min = 100, step = 100)
                        ),
                        downloadButton("downloadBootEGAPlot", "Download Item Stability Plot", class = "plot-download-container"),
                        downloadButton("downloadStructuralConsistency", "Download Structural Consistency Table", class = "plot-download-container")
                 )
               )
      ),
      tabPanel("Measurement Invariance", 
               fluidRow(
                 column(8,
                        plotOutput("invarianceResults")
                 ),
                 column(4,
                        actionButton("toggleSettingsInvariance", "Show/Hide Plot Options"),
                        div(id = "plotSettingsInvariance", style = "display: none;", 
                            numericInput("widthInvariance", "Plot width (inches):", value = 8, min = 1, step = 1),
                            numericInput("heightInvariance", "Plot height (inches):", value = 8, min = 1, step = 1),
                            numericInput("dpiInvariance", "Plot resolution (dpi):", value = 600, min = 100, step = 100)
                        ),
                        downloadButton("downloadInvariancePlot", "Download Invariance Plot")
                 )
               ),
               tableOutput("invarianceTable"),
               downloadButton("downloadInvarianceTable", "Download Invariance Table")
      ),
      tabPanel("Hierarchical Model", 
               fluidRow(
                 column(8,
                        div(id = "hierarchicalPlotContainer",
                            conditionalPanel(
                              condition = "output.hierEGAError !== null",
                              textOutput("hierEGAError")
                            ),
                            plotOutput("plotHierEGA", width = "800px", height = "600px")
                        )
                 ),
                 column(4,
                        actionButton("toggleSettingsHierEGA", "Show/Hide Plot Options"),
                        div(id = "plotSettingsHierEGA", style = "display: none;", 
                            numericInput("widthHierEGA", "Plot width (inches):", value = 8, min = 1, step = 1),
                            numericInput("heightHierEGA", "Plot height (inches):", value = 8, min = 1, step = 1),
                            numericInput("dpiHierEGA", "Plot resolution (dpi):", value = 600, min = 100, step = 100)
                        ),
                        downloadButton("downloadHierEGAPlot", "Download Hierarchical Plot")
                 )
               )
      ),
      tabPanel("Wording effects",
               fluidRow(
                 column(8,
                        div(id = "wordingEffectsPlotContainer",
                            conditionalPanel(
                              condition = "output.wordingEffectsError !== null",
                              textOutput("wordingEffectsError")
                            ),
                            plotOutput("plotWordingEffects", width = "800px", height = "600px")
                        )
                 ),
                 column(4,
                        actionButton("toggleSettingsWordingEffects", "Show/Hide Plot Options"),
                        div(id = "plotSettingsWordingEffects", style = "display: none;", 
                            numericInput("widthWordingEffects", "Plot width (inches):", value = 8, min = 1, step = 1),
                            numericInput("heightWordingEffects", "Plot height (inches):", value = 8, min = 1, step = 1),
                            numericInput("dpiWordingEffects", "Plot resolution (dpi):", value = 600, min = 100, step = 100)
                        ),
                        actionButton("runWordingEffects", "Run Wording Effects", class = "btn btn-primary btn-calcular"),
                        downloadButton("downloadWordingEffectsPlot", "Download Wording Effects Plot")
                 )
               )
      ),
      tabPanel("Redundancy Analysis: UVA", 
               textOutput("uvaSummary")
      ),
      # tabPanel("Information", 
      #          tags$div(
      #            tags$p("This Shiny app was created by Dr. José Ventura-León and his research team."),
      #            tags$p("For more information about the author, visit: ", 
      #                   tags$a(href="http://joseventuraleon.com", "joseventuraleon.com")),
      #            tags$p("For questions, inquiries, or errors in the calculator, write to ", 
      #                   tags$a(href="mailto:info@joseventuraleon.com", "info@joseventuraleon.com")),
      #            tags$p(
      #              tags$strong("Reference"),
      #              br(),
      #              "Ventura-León, J., Lino-Cruz, C., Tocto-Muñoz, S., & Sanchez-Villena, A. (2024). ",
      #              tags$em("EGAnet Shiny: An Interactive Shiny App for Exploratory Graph Analysis"), 
      #              " [Shiny Application]. Retrieved from ",
      #              tags$a(href="https://jventural.shinyapps.io/EGAnet_Shiny/", "https://jventural.shinyapps.io/EGAnet_Shiny/")
      #            )
      #          )
      # )
    )
  )
)

server <- function(input, output, session) {
  
  data_bfi <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  output$groupColumnUI <- renderUI({
    req(data_bfi())
    selectInput("groupColumn", "Select group column:", choices = names(data_bfi()), selected = NULL)
  })
  
  observe({
    req(data_bfi())
    updateSelectizeInput(session, "removeItemsManual", choices = setdiff(names(data_bfi()), input$groupColumn), server = TRUE)
  })
  
  observeEvent(input$file, {
    updateSelectizeInput(session, "removeItemsManual", choices = NULL)
    updateSelectInput(session, "groupColumn", choices = NULL, selected = NULL)
    updateSelectInput(session, "corr", selected = "cor_auto")
    updateSelectInput(session, "model", selected = "glasso")
    updateSelectInput(session, "algorithm", selected = "louvain")
    updateNumericInput(session, "resolution_parameter", value = 0.05)
    updateSelectInput(session, "objective_function", selected = "CPM")
    updateNumericInput(session, "iter", value = 100)
    updateNumericInput(session, "seed", value = 2024)
    updateSelectInput(session, "type", selected = "resampling")
    updateSelectInput(session, "p_type", selected = "p_BH")
    updateNumericInput(session, "p_value", value = 0.05)
    updateNumericInput(session, "configural_threshold", value = 0.70)
    
    # Clear the plots and error messages
    output$plotWordingEffects <- renderPlot(NULL)
    output$wordingEffectsError <- renderText(NULL)
    output$plotBootEGA <- renderPlot(NULL)
    output$structuralConsistency <- renderTable(NULL)
    output$invarianceResults <- renderPlot(NULL)
    output$invarianceTable <- renderTable(NULL)
  })
  
  filtered_data <- reactive({
    req(data_bfi())
    if (!is.null(input$groupColumn) && input$groupColumn %in% names(data_bfi())) {
      data <- data_bfi() %>% select(-all_of(input$groupColumn))
      if (!is.null(input$removeItemsManual)) {
        data <- data %>% select(-all_of(input$removeItemsManual))
      }
      data
    } else {
      data_bfi()
    }
  })
  
  ega_result <- reactive({
    req(filtered_data())
    ncores <- min(parallel::detectCores(logical = FALSE), 2)
    
    tryCatch({
      if (input$algorithm == "leiden") {
        EGA(
          data = filtered_data(),
          corr = input$corr,
          model = input$model,
          algorithm = input$algorithm,
          objective_function = input$objective_function,
          resolution_parameter = input$resolution_parameter,
          plot.EGA = TRUE,
          seed = input$seed,
          ncores = ncores
        )
      } else {
        EGA(
          data = filtered_data(),
          corr = input$corr,
          model = input$model,
          algorithm = input$algorithm,
          plot.EGA = TRUE,
          seed = input$seed,
          ncores = ncores
        )
      }
    }, error = function(e) {
      showNotification(paste("Error in running EGA:", e$message), type = "error")
      NULL
    })
  })
  
  output$plotEGA <- renderPlot({
    result <- ega_result()
    validate(need(!is.null(result), "Error in EGA result"))
    plot <- result$plot.EGA + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
      annotate("text", x = Inf, y = -Inf, label = paste("TEFI:", round(result$TEFI, 3)), hjust = 1, vjust = -1)
    print(plot + ggtitle("EGA Plot"))
  }, width = 600, height = 600, res = 100)
  
  output$networkLoads <- renderTable({
    result <- ega_result()
    validate(need(!is.null(result), "Error in EGA result"))
    net.loads(result)$std %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "Item")
  })
  
  # Función para convertir resultados EGA en data.frame y redondear decimales
  convert_EGA_to_df <- function(ega_result) {
    network_matrix <- ega_result$network
    methods <- attr(network_matrix, "methods")
    
    # Crear lista de métricas asegurando que los valores son convertibles a caracteres o números según corresponda
    metrics <- list(
      Model = if (!is.null(methods$model)) toupper(methods$model) else NA,
      Correlations = if (!is.null(methods$corr)) methods$corr else NA,
      Lambda = if (!is.null(methods$lambda)) formatC(methods$lambda, format = "f", digits = 3) else NA,
      `Number of nodes` = if (!is.null(nrow(network_matrix))) as.character(nrow(network_matrix)) else NA,
      `Number of edges` = if (!is.null(sum(network_matrix != 0, na.rm = TRUE))) as.character(sum(network_matrix != 0, na.rm = TRUE) / 2) else NA, # Dividido por 2 para evitar duplicados
      `Edge density` = if (!is.null(mean(network_matrix != 0, na.rm = TRUE))) formatC(mean(network_matrix != 0, na.rm = TRUE), format = "f", digits = 3) else NA,
      M = if (!is.null(mean(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(mean(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
      SD = if (!is.null(sd(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(sd(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
      Min = if (!is.null(min(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(min(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
      Max = if (!is.null(max(network_matrix[network_matrix != 0], na.rm = TRUE))) formatC(max(network_matrix[network_matrix != 0], na.rm = TRUE), format = "f", digits = 3) else NA,
      `Number of communities` = if (!is.null(ega_result$n.dim)) as.character(ega_result$n.dim) else NA,
      TEFI = if (!is.null(ega_result$TEFI)) formatC(ega_result$TEFI, format = "f", digits = 3) else NA
    )
    
    # Convertir la lista en data.frame
    result_df <- data.frame(Index = names(metrics), Value = unlist(metrics), stringsAsFactors = FALSE, row.names = NULL)
    
    return(result_df)
  }
  
  output$informativeTable <- renderTable({
    result <- ega_result()
    validate(need(!is.null(result), "Error in EGA result"))
    convert_EGA_to_df(result)
  })
  
  output$downloadInformativeTable <- downloadHandler(
    filename = function() { "informative_table.xlsx" },
    content = function(file) {
      result <- ega_result()
      validate(need(!is.null(result), "Error in EGA result"))
      informative_table <- convert_EGA_to_df(result)
      write.xlsx(informative_table, file)
    }
  )
  
  output$downloadEGAPlot <- downloadHandler(
    filename = function() { "EGA_plot.png" },
    content = function(file) {
      result <- ega_result()
      validate(need(!is.null(result), "Error in EGA result"))
      plot <- result$plot.EGA + 
        theme(plot.margin = unit(c(1,1,1,1), "cm")) +
        annotate("text", x = Inf, y = -Inf, label = paste("TEFI:", round(result$TEFI, 3)), hjust = 1, vjust = -1)
      ggsave(file, plot = plot, width = input$widthEGA, height = input$heightEGA, dpi = input$dpiEGA)
    }
  )
  
  output$downloadNetworkLoads <- downloadHandler(
    filename = function() { "network_loads.xlsx" },
    content = function(file) {
      result <- ega_result()
      validate(need(!is.null(result), "Error in EGA result"))
      write.xlsx(net.loads(result)$std %>% 
                   as.data.frame() %>% 
                   rownames_to_column(var = "Item"), file)
    }
  )
  
  observeEvent(input$runBootEGA, {
    showNotification("Running bootEGA...", type = "message", duration = NULL, id = "bootEGA")
    on.exit(removeNotification("bootEGA"))
    
    validate(
      need(input$iter > 0, "Number of iterations must be greater than 0")
    )
    
    ncores <- min(parallel::detectCores(logical = FALSE), 2)
    
    tryCatch({
      boot.wmt <- bootEGA(
        data = filtered_data(),
        iter = input$iter,
        model = input$model,
        corr = input$corr,
        algorithm = input$algorithm,
        objective_function = if (input$algorithm == "leiden") input$objective_function else NULL,
        resolution_parameter = if (input$algorithm == "leiden") input$resolution_parameter else NULL,
        seed = as.numeric(input$seed),
        type = input$type,
        ncores = ncores
      )
      
      sc <- EGAnet::dimensionStability(boot.wmt)
      
      output$structuralConsistency <- renderTable({
        data.frame(
          Dimension = names(sc$dimension.stability$structural.consistency),
          Consistency = sc$dimension.stability$structural.consistency
        )
      })
      
      output$plotBootEGA <- renderPlot({
        plot <- boot.wmt$stability$item.stability$plot + theme(plot.margin = unit(c(1,1,1,1), "cm"))
        print(plot + ggtitle("Item Stability"))
      }, width = 600, height = 600, res = 100)
      
      output$downloadBootEGAPlot <- downloadHandler(
        filename = function() { "item_stability_plot.png" },
        content = function(file) {
          plot <- boot.wmt$stability$item.stability$plot + 
            theme(plot.margin = unit(c(1,1,1,1), "cm"))
          ggsave(file, plot = plot, width = input$widthBootEGA, height = input$heightBootEGA, dpi = input$dpiBootEGA)
        }
      )
      
      output$downloadStructuralConsistency <- downloadHandler(
        filename = function() { "structural_consistency.xlsx" },
        content = function(file) {
          write.xlsx(data.frame(
            Dimension = names(sc$dimension.stability$structural.consistency),
            Consistency = sc$dimension.stability$structural.consistency
          ), file)
        }
      )
    }, error = function(e) {
      showNotification(paste("Error in running bootEGA:", e$message), type = "error")
    })
  })
  
  observeEvent(input$runInvariance, {
    showNotification("Running invariance analysis...", type = "message", duration = NULL, id = "invariance")
    on.exit(removeNotification("invariance"))
    
    data_for_invariance <- data_bfi()
    if (!is.null(input$groupColumn) && input$groupColumn %in% names(data_bfi())) {
      data_for_invariance <- data_for_invariance %>% select(-all_of(input$groupColumn))
      if (!is.null(input$removeItemsManual)) {
        data_for_invariance <- data_for_invariance %>% select(-all_of(input$removeItemsManual))
      }
      group_column <- data_bfi()[[input$groupColumn]]
      
      ncores <- min(parallel::detectCores(logical = FALSE), 2)
      
      tryCatch({
        invariance.result <- invariance(
          data = data_for_invariance,
          group = group_column,
          corr = input$corr,
          model = input$model,
          algorithm = input$algorithm,
          iter = input$iter,
          p_type = input$p_type,
          p_value = input$p_value,
          seed = as.numeric(input$seed),
          configural.threshold = as.numeric(input$configural_threshold),
          ncores = ncores
        )
        
        output$invarianceResults <- renderPlot({
          plot(invariance.result, p_type = input$p_type, p_value = input$p_value)
        })
        
        output$downloadInvariancePlot <- downloadHandler(
          filename = function() { "invariance_plot.png" },
          content = function(file) {
            plot <- plot(invariance.result, p_type = input$p_type, p_value = input$p_value)
            ggsave(file, plot = plot, width = input$widthInvariance, height = input$heightInvariance, dpi = input$dpiInvariance)
          }
        )
        
        output$invarianceTable <- renderTable({
          invariance.result$results %>% 
            as.data.frame() %>% 
            rownames_to_column(var = "Item")
        })
        
        output$downloadInvarianceTable <- downloadHandler(
          filename = function() { "invariance_results.xlsx" },
          content = function(file) {
            write.xlsx(invariance.result$results, file)
          }
        )
      }, error = function(e) {
        showNotification(paste("Error in running invariance analysis:", e$message), type = "error")
      })
    }
  })
  
  observe({
    req(data_bfi(), input$groupColumn)
    if (!is.null(input$groupColumn) && input$groupColumn %in% names(data_bfi())) {
      data_for_uva <- data_bfi() %>% select(-all_of(input$groupColumn))
      if (!is.null(input$removeItemsManual)) {
        data_for_uva <- data_for_uva %>% select(-all_of(input$removeItemsManual))
      }
      uva.wmt <- UVA(data_for_uva)
      
      output$uvaSummary <- renderText({
        if (is.null(uva.wmt$keep_remove$remove)) {
          "There are no redundant items, so there is nothing to remove."
        } else {
          paste("The item that should be removed is", paste(uva.wmt$keep_remove$remove, collapse = ", "))
        }
      })
    }
  })
  
  hierarchical_result <- reactive({
    req(data_bfi())
    data <- data_bfi()
    if (!is.null(input$groupColumn) && input$groupColumn %in% names(data)) {
      data <- data %>% select(-all_of(input$groupColumn))
    }
    if (!is.null(input$removeItemsManual)) {
      data <- data %>% select(-all_of(input$removeItemsManual))
    }
    EGAnet::hierEGA(data, scores = "network", plot.EGA = TRUE)
  })
  
  output$plotHierEGA <- renderPlot({
    result <- hierarchical_result()
    if (is.null(result$plot.hierEGA)) {
      output$hierEGAError <- renderText({
        "It is not possible to generate a hierarchical model based on the provided dataset."
      })
      return(NULL)
    }
    output$hierEGAError <- renderText({ NULL })
    plot <- result$plot.hierEGA + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
      annotate("text", x = Inf, y = -Inf, label = paste("GenTEFI:", round(result$TEFI, 3)), hjust = 1, vjust = 0)
    print(plot + ggtitle("Hierarchical EGA Plot"))
  }, width = 600, height = 600, res = 100)
  
  output$downloadHierEGAPlot <- downloadHandler(
    filename = function() { "hierEGA_plot.png" },
    content = function(file) {
      result <- hierarchical_result()
      if (is.null(result$plot.hierEGA)) {
        stop("It is not possible to generate a hierarchical model based on the provided dataset.")
      }
      plot <- result$plot.hierEGA + 
        theme(plot.margin = unit(c(1,1,1,1), "cm")) +
        annotate("text", x = Inf, y = -Inf, label = paste("GenTEFI:", round(result$TEFI, 3)), hjust = 1, vjust = -1)
      ggsave(file, plot = plot, width = input$widthHierEGA, height = input$heightHierEGA, dpi = input$dpiHierEGA)
    }
  )
  
  wording_effects_result <- reactive({
    req(filtered_data())
    ncores <- min(parallel::detectCores(logical = FALSE), 2)
    
    tryCatch({
      riEGA(
        data = filtered_data(),
        corr = input$corr,
        model = input$model,
        algorithm = input$algorithm,
        plot.EGA = TRUE,
        seed = input$seed,
        ncores = ncores
      )
    }, error = function(e) {
      showNotification(paste("Error in running riEGA:", e$message), type = "error")
      NULL
    })
  })
  
  observeEvent(input$runWordingEffects, {
    result <- wording_effects_result()
    if (is.null(result$Plot.EGA)) {
      output$wordingEffectsError <- renderText({
        "It is not possible to generate a wording effects model based on the provided dataset."
      })
      return(NULL)
    }
    output$wordingEffectsError <- renderText({ NULL })
    output$plotWordingEffects <- renderPlot({
      plot <- result$Plot.EGA + theme(plot.margin = unit(c(1,1,1,1), "cm")) +
        annotate("text", x = Inf, y = -Inf, label = paste("TEFI:", round(result$TEFI, 3)), hjust = 1, vjust = -1)
      print(plot + ggtitle("Wording Effects EGA Plot"))
    }, width = 600, height = 600, res = 100)
    
    output$downloadWordingEffectsPlot <- downloadHandler(
      filename = function() { "wording_effects_plot.png" },
      content = function(file) {
        plot <- result$Plot.EGA + 
          theme(plot.margin = unit(c(1,1,1,1), "cm")) +
          annotate("text", x = Inf, y = -Inf, label = paste("TEFI:", round(result$TEFI, 3)), hjust = 1, vjust = -1)
        ggsave(file, plot = plot, width = input$widthWordingEffects, height = input$heightWordingEffects, dpi = input$dpiWordingEffects)
      }
    )
  })
  
  observeEvent(input$toggleSettingsWordingEffects, {
    toggle("plotSettingsWordingEffects")
  })
  
  observeEvent(input$toggleSettings, {
    toggle("plotSettingsEGA")
  })
  
  observeEvent(input$toggleSettingsBootEGA, {
    toggle("plotSettingsBootEGA")
  })
  
  observeEvent(input$toggleSettingsInvariance, {
    toggle("plotSettingsInvariance")
  })
  
  observeEvent(input$toggleSettingsHierEGA, {
    toggle("plotSettingsHierEGA")
  })
}

shinyApp(ui = ui, server = server)

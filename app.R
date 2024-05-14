#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
source("functions/ELISA.R")
# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- bslib::page_sidebar(
  title = "ShinyElisa",
  sidebar = bslib::sidebar(
    title = "Load Data",
    open = T,
    shinyWidgets::radioGroupButtons(
      inputId = "wells",
      justified = T,
      direction = "horizontal",
      label = "plate type",
      choices = c("96 wells", "384 wells"),
      individual = TRUE,
      checkIcon = list(
        yes = tags$i(class = "fa fa-circle", 
                     style = "color: steelblue"),
        no = tags$i(class = "fa fa-circle-o", 
                    style = "color: steelblue"))
    ),
    fileInput('xlsxFile', 'Choose xlsx file',
              accept = c(".xlsx")
              )
    #datamods::import_file_ui(
    #  "xlsxFile",
    #  title = NULL,
    #  preview_data = F,
    #  file_extensions = c(".xls", ".xlsx")
    #  )
    ),

## sc panel ----------------------------------------------------------------

  bslib::card(
    bslib::card_header("Standard Curve"),
    bslib::card_body(
      bslib::layout_column_wrap(
        width = 1/2,
        DT::dataTableOutput("DT_curve"),
        plotOutput("SC")
      )
    )
  ),

## Conc panel --------------------------------------------------------------

  bslib::navset_card_tab(
    title = "samples",
    bslib::nav_panel(
      "Plotly",
      bslib::card_title("First Results"),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1/2,
          DT::dataTableOutput("DT_samples"),
          plotOutput("sample_plot")
        )
      )
    ),
    bslib::nav_panel(
      "Wide",
      bslib::card_title("Results Wide"),
      DT::dataTableOutput("DT_samplesWide")
    )
  )
)



# server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
  importedData <- reactive({
    file <- input$xlsxFile
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    
    data <- readxl::read_xlsx(file$datapath, sheet = 1)
  })
  #importedData <- datamods::import_file_server(
  #  "xlsxFile",
  #  btn_show_data = TRUE,
  #  show_data_in = c("popup"),
  #  trigger_return = c("button"),
  #  return_class = c("data.frame"),
  #  reset = reactive(NULL)
  #)
  
  results_plate <- reactive({
    res <- importedData() |>
      head(n = 9) |> 
      janitor::row_to_names(1) |> 
      dplyr::mutate(dplyr::across(2:13, as.numeric)) |> 
      tidyr::pivot_longer(!1, names_to = "Column")
    
    dilution <- importedData()[11:19,] |> 
      janitor::row_to_names(1) |> 
      dplyr::mutate(dplyr::across(2:13, as.numeric)) |> 
      tidyr::pivot_longer(!1, names_to = "Column", values_to = "dilution")
    
    samples_id <- importedData()[21:29,] |>
      janitor::row_to_names(1) |> 
      tidyr::pivot_longer(!1, names_to = "Column", values_to = "sample_id")
    
    Final_table <- res |> 
      dplyr::left_join(y = dilution, by = c("Row", "Column")) |> 
      dplyr::left_join(y = samples_id, by = c("Row", "Column")) |> 
      dplyr::mutate(sample_id = tolower(sample_id))
    
  })
  
# standard curve ----------------------------------------------------------

  standard <- reactive({
    standard_curve <- results_plate() |> 
      dplyr::filter(sample_id %in% c("standards", "standard")) |> 
      dplyr::rename(concentration = dilution) |> 
      dplyr::arrange(dplyr::desc(concentration))
  })
  
  output$DT_curve <- DT::renderDataTable({
    standard()
  },editable = "cell")
  
  standardModel <- reactive({
    ElisaScurve(standard())
  })
  
  output$SC <- renderPlot({standardModel()$plot})

# Samples -----------------------------------------------------------------

  samples <- reactive({
    samples <- results_plate() |> 
      dplyr::filter(!sample_id %in% c("standards", "standard", "blank", "blanks"))
  })
 
  concentrations <- reactive({
    concentrations <- ElisaConcPrediction(curveModel = standardModel(), ODcolumn = "value", newData = samples())
  })
  
  concentration_wide <- reactive({
    concentrations()$predictions |> 
      tidyr::pivot_wider(names_from = c("Column"), values_from = "Estimate_Conc", id_cols = "Row")
  })
  
  output$DT_samples <- DT::renderDataTable({concentrations()$predictions})
  output$sample_plot <- renderPlot({concentrations()$plot})
  
  output$DT_samplesWide <- DT::renderDataTable({concentration_wide()})
}

# Run the application 
shinyApp(ui = ui, server = server)

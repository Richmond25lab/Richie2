####################  Project Vision: AgroRepro  ##################
#####   Reproducible Agricultural Experiment Management Toolkit ####

# Combined Shiny App with 3 Modules: 
# 1. Experimental Design
# 2. Inventory Management
# 3. Field Data Collection

library(shiny)
library(shinyMobile)
library(tidyverse)
library(DT)
library(agricolae)
library(fs)
library(googlesheets4)
library(googledrive)
library(lubridate)

# Install the shinyMobile package from GitHub (recommended)
if (!require("shinyMobile")) {
  install.packages("remotes")
  remotes::install_github("RinteRface/shinyMobile")
}

library(shinyMobile)

packageVersion("shinyMobile")

# -----------------------------
# Authentication for Google Sheets (Module 2)
# -----------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)
gs4_auth()

sheet_id <- "https://docs.google.com/spreadsheets/d/1FcxawPrMIcN5-KHuPiFxk42nh4l9o8qmlFcvtoWv9To/edit?usp=sharing"
input_fields <- list(
  Seed      = c("Genotype","Species","Weight_g","Unit","Location","Date_Stored","Date_Harvested","Source","Notes"),
  Reagent   = c("Reagent_Name","Quantity","Unit","Location","Date_Received","Expiry_Date","Supplier","Notes",""),
  Equipment = c("Equipment_Name","Model","Location","Date_Added","Condition","Added_By","Notes","",""),
  `DNA Sample` = c("Sample_ID","Species","Concentration","Volume","Location","Date_Extracted","Extracted_By","Notes",""),
  Fertilizer= c("Product_Name","N_P_K","Quantity","Unit","Location","Date_Received","Expiry_Date","Notes",""),
  Other     = rep("",9)
)

# -----------------------------
# UI
# -----------------------------
ui <- navbarPage(
  title = "Agro_Repro Toolkit",
  
  tabPanel("Generate Experimental Design",
           fluidPage(
             titlePanel("Experimental Design Generator"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("design", "Design Type", choices = c("RCBD", "CRD", "Latin Square", "Split Plot", "Alpha Lattice", "Factorial")),
                 textInput("treatments", "Treatments (comma-separated)", value = "A,B,C"),
                 numericInput("replications", "Replications", value = 3, min = 2),
                 selectInput("layout_type", "Layout Type", choices = c("Cartesian", "Serpentine")),
                 conditionalPanel("input.design == 'Split Plot'",
                                  textInput("main_plot", "Main Plot Treatments", "Fert1,Fert2"),
                                  textInput("sub_plot", "Sub Plot Treatments", "Var1,Var2")),
                 conditionalPanel("input.design == 'Factorial'",
                                  textInput("factor1", "Factor 1 Levels", "F1a,F1b"),
                                  textInput("factor2", "Factor 2 Levels", "F2a,F2b")),
                 actionButton("generate", "Generate Design"),
                 downloadButton("download_csv", "Download CSV"),
                 downloadButton("download_plot", "Download Plot")
               ),
               mainPanel(
                 DTOutput("design_table"),
                 plotOutput("design_plot")
               )
             )
           )
  ),
  
  tabPanel("Inventory Management",
           fluidPage(
             titlePanel("Inventory Management"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("input_type", "Input Type", choices = names(input_fields)),
                 uiOutput("dynamic_fields"),
                 actionButton("add_input", "Add Entry"),
                 actionButton("delete_input", "Delete Selected")
               ),
               mainPanel(
                 DTOutput("input_table"),
                 downloadButton("download_inputs", "Download Inventory CSV")
               )
             )
           )
  ),
  
  tabPanel("Field Data Collection",
           fluidPage(
             titlePanel("Field Data Entry"),
             sidebarLayout(
               sidebarPanel(
                 textInput("trial_name", "Trial Name"),
                 dateInput("trial_date", "Date", value = Sys.Date()),
                 textInput("local_dir", "Directory", "C:/MyTrials/"),
                 textInput("plot_id", "Plot ID"),
                 numericInput("yield", "Yield (kg/plot)", value = NA),
                 numericInput("height", "Plant Height (cm)", value = NA),
                 textInput("notes", "Notes"),
                 actionButton("add_entry", "Add Entry")
               ),
               mainPanel(
                 DTOutput("entry_table"),
                 downloadButton("download_data", "Download CSV")
               )
             )
           )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  # ---- Module 1
  current_design <- reactiveVal()
  
  design_data <- eventReactive(input$generate, {
    trt <- unlist(strsplit(input$treatments, ","))
    if (input$design == "RCBD") df <- design.rcbd(trt, input$replications, seed = 123)$book
    else if (input$design == "CRD") df <- design.crd(trt, input$replications, seed = 123)$book
    else if (input$design == "Latin Square") df <- design.lsd(trt, seed = 123)$book
    else if (input$design == "Split Plot") df <- design.split(main = unlist(strsplit(input$main_plot, ",")),
                                                              sub = unlist(strsplit(input$sub_plot, ",")),
                                                              r = input$replications, seed = 123)$book
    else if (input$design == "Alpha Lattice") df <- design.alpha(trt, input$replications, k = 2, seed = 123)$book
    else if (input$design == "Factorial") {
      f1 <- unlist(strsplit(input$factor1, ","))
      f2 <- unlist(strsplit(input$factor2, ","))
      combo <- expand.grid(Factor1 = f1, Factor2 = f2) %>% unite("trt", everything(), sep = ":")
      df <- design.crd(trt = combo$trt, r = input$replications, seed = 123)$book
    }
    current_design(df)
    df
  })
  
  output$design_table <- renderDT({ req(design_data()); datatable(design_data()) })
  
  output$design_plot <- renderPlot({
    df <- current_design(); req(df)
    n <- nrow(df); nr <- ceiling(sqrt(n)); nc <- ceiling(n / nr)
    df <- df %>% mutate(Row = rep(1:nr, each = nc)[1:n],
                        Column = if (input$layout_type == "Serpentine") {
                          r <- rep(1:nc, times = nr)[1:n]; i <- rep(1:nr, each = nc)[1:n]; ifelse(i %% 2 == 0, rev(r), r)
                        } else rep(1:nc, times = nr)[1:n])
    ggplot(df, aes(x = Column, y = Row, fill = trt)) + geom_tile() + geom_text(aes(label = trt)) +
      scale_y_reverse() + coord_fixed() + theme_minimal()
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("experimental_design_", Sys.Date(), ".csv"),
    content = function(file) write_csv(current_design(), file)
  )
  
  output$download_plot <- downloadHandler(
    filename = function() paste0("field_layout_plot_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 800, height = 600)
      print(output$design_plot())
      dev.off()
    }
  )
  
  # ---- Module 2
  output$dynamic_fields <- renderUI({
    labels <- input_fields[[input$input_type]]
    tagList(seq_along(labels) %>% map(~{
      lbl <- labels[.x]; if (lbl == "") return(NULL)
      id <- paste0("field", .x)
      if (grepl("Date", lbl)) dateInput(id, lbl)
      else if (grepl("Weight|Quantity|Concentration|Volume", lbl)) numericInput(id, lbl, value = NA)
      else textInput(id, lbl)
    }))
  })
  
  load_data <- reactive({ read_sheet(sheet_id, sheet = input$input_type, col_types = "c", na = "") })
  
  observeEvent(input$add_input, {
    labels <- input_fields[[input$input_type]]
    values <- map_chr(seq_along(labels), ~ as.character(input[[paste0("field", .x)]]))
    df_new <- tibble(!!!set_names(as.list(values), labels))
    existing <- tryCatch(load_data(), error = function(e) NULL)
    if (is.null(existing) || nrow(existing)==0) sheet_write(df_new, ss = sheet_id, sheet = input$input_type)
    else sheet_append(df_new, ss = sheet_id, sheet = input$input_type)
  })
  
  selected_row <- reactiveVal(NULL)
  observeEvent(input$input_table_rows_selected, selected_row(input$input_table_rows_selected))
  observeEvent(input$delete_input, {
    req(selected_row()); df <- load_data(); df <- df[-selected_row(), ]
    sheet_write(df, ss = sheet_id, sheet = input$input_type); selected_row(NULL)
  })
  output$input_table <- renderDT({ datatable(load_data(), selection = "single", options = list(pageLength = 10)) })
  
  output$download_inputs <- downloadHandler(
    filename = function() paste0(input$input_type, "_inputs_", Sys.Date(), ".csv"),
    content = function(file) write_csv(load_data(), file)
  )
  
  # ---- Module 3
  entry_data <- reactiveVal(tibble())
  observeEvent(input$add_entry, {
    req(input$plot_id, input$yield, input$height)
    new_row <- tibble(
      Trial_Name = input$trial_name,
      Date = as.character(input$trial_date),
      Plot_ID = input$plot_id,
      Yield = input$yield,
      Height = input$height,
      Notes = input$notes
    )
    updated_data <- bind_rows(entry_data(), new_row)
    entry_data(updated_data)
    if (nzchar(input$local_dir)) {
      folder_path <- file.path(input$local_dir, paste0(input$trial_date, "_", input$trial_name))
      dir_create(folder_path)
      write_csv(updated_data, file.path(folder_path, "field_data.csv"))
    }
  })
  output$entry_table <- renderDT({ datatable(entry_data()) })
  output$download_data <- downloadHandler(
    filename = function() paste0("field_data_", Sys.Date(), ".csv"),
    content = function(file) write_csv(entry_data(), file)
  )
}

shinyApp(ui, server)

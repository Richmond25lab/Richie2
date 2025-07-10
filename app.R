#________________________________Lab_Repository________________________________
#___________________ To store, track and manage samples _______________________
#__________                                                         ___________
get
# Load necessary packages 
library(shiny)
library(DT)
library(tidyverse)
getwd()
setwd("C:/Users/rnein/Desktop/Lab_Repository")

# File paths for CSV storage
seed_data_file <- "seed_repository.csv"      #file path for seed_data
tissue_data_file <- "tissue_repository.csv"  #file path for tissue_data
reagent_data_file <- "reagent_repository.csv" #file path for reagent_data
equipment_data_file <- "equipment_repository.csv"  #file path for equipment_data
dna_data_file <- "dna_repository.csv"    #file path for dna_data
primer_data_file <- "primer_repository.csv"   #file path for primer_data

# Initialize CSV files if they don't exist
init_repository <- function(file, columns) {
  if (!file.exists(file)) {
    empty_data <- tibble(!!!columns)
    write_csv(empty_data, file)
  }
}

init_repository(seed_data_file, list(
  Genotype = character(),
  Species = character(),
  Storage_Location = character(),
  Date_Stored = as.Date(character()),
  Date_Harvested = as.Date(character()),
  Stored_By = character(),
  Weight_g = numeric(),
  Source = character(),
  Notes = character()
))

init_repository(tissue_data_file, list(
  Sample_ID = character(),
  Plant_Type = character(),
  Stage = character(),
  Storage_Location = character(),
  Date_Stored = as.Date(character()),
  Stored_By = character(),
  Notes = character()
))

init_repository(reagent_data_file, list(
  Reagent_Name = character(),
  Quantity = numeric(),
  Unit = character(),
  Storage_Location = character(),
  Expiry_Date = as.Date(character()),
  Date_Received = as.Date(character()),
  Supplier = character(),
  Notes = character()
))

init_repository(equipment_data_file, list(
  Equipment_Name = character(),
  Model = character(),
  Location = character(),
  Date_Added = as.Date(character()),
  Condition = character(),
  Added_By = character(),
  Notes = character()
))

init_repository(dna_data_file, list(
  Sample_ID = character(),
  Plant_Species = character(),
  Concentration_ng_ul = numeric(),
  Volume_ul = numeric(),
  Storage_Location = character(),
  Date_Extracted = as.Date(character()),
  Extracted_By = character(),
  Notes = character()
))

init_repository(primer_data_file, list(
  Primer_Name = character(),
  Sequence = character(),
  Target_Gene = character(),
  Storage_Location = character(),
  Date_Received = as.Date(character()),
  Supplier = character(),
  Notes = character()
))


# data <- function(vect_colmns , file_name){
#   dataframmm <- as.data.frame(matrix(,ncol = length(vect_colmns)))
#   colnames(dataframmm) <- vect_colmns
#   writexl::write_xlsx(dataframmm, paste0(file_name,'.xlsx'))
#   # return(dataframmm)
#   cat('done!')
# 
# }
# 
# vec <- c('Genotype' ,'Species' ,'Storage_Location' ,
#          'Date_Stored', 'Date_Harvested' ,'Stored_By' ,'Weight_g', 'Source','Notes' 
# )
# data(vect_colmns = vec,'first_file')
# 
# list.files()

load_data <- function(file) {
  if (file == reagent_data_file) {
    read_csv(file, col_types = cols(
      Reagent_Name = col_character(),
      Quantity = col_double(),
      Unit = col_character(),
      Storage_Location = col_character(),
      Expiry_Date = col_date(),
      Date_Received = col_date(),
      Supplier = col_character(),
      Notes = col_character()
    ))
  } else if (file == equipment_data_file) {
    read_csv(file, col_types = cols(
      Equipment_Name = col_character(),
      Model = col_character(),
      Location = col_character(),
      Date_Added = col_date(),
      Condition = col_character(),
      Added_By = col_character(),
      Notes = col_character()
    ))
  } else if (file == tissue_data_file) {
    read_csv(file, col_types = cols(
      Sample_ID = col_character(),
      Plant_Type = col_character(),
      Stage = col_character(),
      Storage_Location = col_character(),
      Date_Stored = col_date(),
      Stored_By = col_character(),
      Notes = col_character()
    ))
  } else if (file == seed_data_file) {
    read_csv(file, col_types = cols(
      Genotype = col_character(),
      Species = col_character(),
      Storage_Location = col_character(),
      Date_Stored = col_date(),
      Date_Harvested = col_date(),
      Stored_By = col_character(),
      Weight_g = col_double(),
      Source = col_character(),
      Notes = col_character()
    ))
  } else {
    read_csv(file, show_col_types = FALSE)  # fallback
  }
}


ui <- fluidPage(
  titlePanel("Lab Repository System"),
  tabsetPanel(
    tabPanel("Seeds",
             sidebarLayout(
               sidebarPanel(
                 textInput("genotype", "Genotype"),
                 textInput("species", "Species"),
                 textInput("location", "Storage Location"),
                 dateInput("date_stored", "Date Stored"),
                 dateInput("date_harvested", "Date Harvested"),
                 textInput("stored_by", "Stored By"),
                 numericInput("weight", "Weight (g)", value = 0, min = 0),
                 textInput("source", "Source"),
                 textAreaInput("notes", "Notes", "", rows = 3),
                 actionButton("add_seed", "Add Seed Entry")
               ),
               mainPanel(DTOutput("seed_table"))
             )
    ),
    tabPanel("Tissue Culture",
             sidebarLayout(
               sidebarPanel(
                 textInput("sample_id_tc", "Sample ID"),
                 textInput("plant_type_tc", "Plant Type"),
                 textInput("stage_tc", "Stage"),
                 textInput("location_tc", "Storage Location"),
                 dateInput("date_stored_tc", "Date Stored"),
                 textInput("stored_by_tc", "Stored By"),
                 textAreaInput("notes_tc", "Notes", "", rows = 3),
                 actionButton("add_tc", "Add Tissue Entry")
               ),
               mainPanel(DTOutput("tc_table"))
             )
    ),
    tabPanel("Reagents",
             sidebarLayout(
               sidebarPanel(
                 textInput("reagent_name", "Reagent Name"),
                 numericInput("quantity", "Quantity", value = 1),
                 textInput("unit", "Unit"),
                 textInput("location_re", "Storage Location"),
                 dateInput("expiry_date", "Expiry Date"),
                 dateInput("received_date", "Date Received"),
                 textInput("supplier", "Supplier"),
                 textAreaInput("notes_re", "Notes", "", rows = 3),
                 actionButton("add_re", "Add Reagent")
               ),
               mainPanel(DTOutput("re_table"))
             )
    ),
    tabPanel("Equipment",
             sidebarLayout(
               sidebarPanel(
                 textInput("equip_name", "Equipment Name"),
                 textInput("model", "Model"),
                 textInput("location_eq", "Location"),
                 dateInput("date_added", "Date Added"),
                 textInput("condition", "Condition"),
                 textInput("added_by", "Added By"),
                 textAreaInput("notes_eq", "Notes", "", rows = 3),
                 actionButton("add_eq", "Add Equipment")
               ),
               mainPanel(DTOutput("eq_table"))
             )
    )
  )
)

server <- function(input, output, session) {
  seed_data <- reactiveVal(load_data(seed_data_file))
  observeEvent(input$add_seed, {
    new_entry <- tibble(
      Genotype = input$genotype,
      Species = input$species,
      Storage_Location = input$location,
      Date_Stored = input$date_stored,
      Date_Harvested = input$date_harvested,
      Stored_By = input$stored_by,
      Weight_g = input$weight,
      Source = input$source,
      Notes = input$notes
    )
    updated_data <- bind_rows(seed_data(), new_entry)
    write_csv(updated_data, seed_data_file)
    seed_data(updated_data)
  })
  output$seed_table <- renderDT({
    datatable(seed_data(), options = list(pageLength = 10), filter = "top")
  })
  
  tc_data <- reactiveVal(load_data(tissue_data_file))
  observeEvent(input$add_tc, {
    new_entry <- tibble(
      Sample_ID = input$sample_id_tc,
      Plant_Type = input$plant_type_tc,
      Stage = input$stage_tc,
      Storage_Location = input$location_tc,
      Date_Stored = input$date_stored_tc,
      Stored_By = input$stored_by_tc,
      Notes = input$notes_tc
    )
    updated_data <- bind_rows(tc_data(), new_entry)
    write_csv(updated_data, tissue_data_file)
    tc_data(updated_data)
  })
  output$tc_table <- renderDT({
    datatable(tc_data(), options = list(pageLength = 10), filter = "top")
  })
  
  re_data <- reactiveVal(load_data(reagent_data_file))
  observeEvent(input$add_re, {
    new_entry <- tibble(
      Reagent_Name = input$reagent_name,
      Quantity = input$quantity,
      Unit = input$unit,
      Storage_Location = input$location_re,
      Expiry_Date = input$expiry_date,
      Date_Received = input$received_date,
      Supplier = input$supplier,
      Notes = input$notes_re
    )
    updated_data <- bind_rows(re_data(), new_entry)
    write_csv(updated_data, reagent_data_file)
    re_data(updated_data)
  })
  output$re_table <- renderDT({
    datatable(re_data(), options = list(pageLength = 10), filter = "top")
  })
  
  eq_data <- reactiveVal(load_data(equipment_data_file))
  observeEvent(input$add_eq, {
    new_entry <- tibble(
      Equipment_Name = input$equip_name,
      Model = input$model,
      Location = input$location_eq,
      Date_Added = input$date_added,
      Condition = input$condition,
      Added_By = input$added_by,
      Notes = input$notes_eq
    )
    updated_data <- bind_rows(eq_data(), new_entry)
    write_csv(updated_data, equipment_data_file)
    eq_data(updated_data)
  })
  output$eq_table <- renderDT({
    datatable(eq_data(), options = list(pageLength = 10), filter = "top")
  })
}

shinyApp(ui = ui, server = server)





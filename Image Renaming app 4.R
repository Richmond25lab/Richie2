install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(magick)
library(tools)

ui <- dashboardPage(
  dashboardHeader(title = "Photo Renaming App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Photos", tabName = "upload", icon = icon("upload")),
      menuItem("Rename Photos", tabName = "rename", icon = icon("edit"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload Photos", width = 12, 
                    fileInput("photos", "Choose Photos", 
                              accept = c('image/png', 'image/jpeg', 'image/heic'),
                              multiple = TRUE),
                    actionButton("uploadPhotos", "Upload Photos")
                )
              )
      ),
      tabItem(tabName = "rename",
              fluidRow(
                box(title = "Rename Photos", width = 12,
                    textInput("newName", "New Base Name", value = "photo"),
                    actionButton("renamePhotos", "Rename Photos"),
                    downloadButton("downloadRenamed", "Download Renamed Photos")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 1000 * 1024^2)  # Set maximum file upload size to 1GB
  
  observeEvent(input$uploadPhotos, {
    req(input$photos)
    upload_dir <- "uploads"
    dir.create(upload_dir, showWarnings = FALSE)
    
    for (i in 1:length(input$photos$name)) {
      file_ext <- tolower(file_ext(input$photos$name[i]))
      file_path <- input$photos$datapath[i]
      output_file <- file.path(upload_dir, paste0(file_path_sans_ext(input$photos$name[i]), ".jpeg"))
      
      if (file_ext == "heic") {
        img <- image_read(file_path)
        image_write(img, path = output_file, format = "jpeg")
      } else {
        file.copy(from = file_path, to = file.path(upload_dir, input$photos$name[i]), overwrite = TRUE)
      }
    }
    
    showNotification("Photos uploaded successfully", type = "message")
  })
  
  observeEvent(input$renamePhotos, {
    req(input$photos)
    new_base_name <- input$newName
    upload_dir <- "uploads"
    renamed_dir <- file.path(upload_dir, new_base_name)
    
    dir.create(renamed_dir, showWarnings = FALSE)
    
    for (i in 1:length(input$photos$name)) {
      file_ext <- file_ext(input$photos$name[i])
      old_path <- file.path(upload_dir, ifelse(tolower(file_ext) == "heic", 
                                               paste0(file_path_sans_ext(input$photos$name[i]), ".jpeg"), 
                                               input$photos$name[i]))
      new_name <- paste0(new_base_name, "_", i, ".", ifelse(tolower(file_ext) == "heic", "jpeg", file_ext))
      new_path <- file.path(renamed_dir, new_name)
      
      if (!file.exists(old_path)) {
        showNotification(paste("File not found:", old_path), type = "error")
      } else {
        file.rename(old_path, new_path)
      }
    }
    
    showNotification("Photos renamed successfully", type = "message")
  })
  
  output$downloadRenamed <- downloadHandler(
    filename = function() {
      paste(input$newName, ".zip", sep = "")
    },
    content = function(file) {
      upload_dir <- "uploads"
      renamed_dir <- file.path(upload_dir, input$newName)
      
      if (!dir.exists(renamed_dir)) {
        showNotification("No files to download", type = "error")
        return(NULL)
      }
      
      files <- list.files(renamed_dir, full.names = TRUE)
      if (length(files) == 0) {
        showNotification("No files to download", type = "error")
        return(NULL)
      }
      
      zip(zipfile = file, files = files)
    }
  )
}

shinyApp(ui = ui, server = server)




library(shiny)
library(tibble)
library(tidyr)
library(stringr)
library(shinyTime)
library(magrittr)
library(dplyr)
#library(tidyverse)
library(readr)
library(shinydashboard)
library(htmlwidgets)
library(shinyBS)
library(shinyjs)
library(shinycssloaders) #for loading spinners
library(DT)
library(shinybusy)
library(shinyalert)
library(leaflet)
library(shinyBS)
library(shinybusy)
library(shinyFiles)
library(shinythemes)
source('global.R')

ui <- fluidPage(
  list(tags$head(HTML('<link rel="icon", href="Rplot.png", 
                       type="image/png" />'),

                 tags$style(HTML("
                    .navbar .navbar-nav {float: right; 
                      color: black; 
                      font-size: 20px; } 
                    .navbar.navbar-default.navbar-static-top{ color: white; 
                      font-size: 25px;}
                        .navbar .navbar-header {float: center; } 
                      .navbar-default .navbar-brand { color: gray; 
                        font-size: 30px; 
                       } 
                      ")))),
  tags$head(tags$style(".modal-body {padding: 10px}
                        .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
                      
                       .modal-header {background-color: #F8BA14; border-top-left-radius: 6px; border-top-right-radius: 6px}
                       
                       .close { font-size: 16px}")),
  fluidPage(style = 'background-color:#45698D;',
            fluidRow(
              column(6,
                     h2('NABat File Renamer', align = 'right', style = 'font-weight: 600; color: white;')),
              column(6, 
                     br(),
                     actionButton('help_renamer', 'How to Use the File Renamer', style = 'background-color:#F8BA14;'), align = 'left'),
            ),
          
            sidebarLayout(
              sidebarPanel(width = 3, style = 'background-color: #C8DBE8;height:840px;',
                           
                           p(h4('1. Select files')),
                           # p(h6('It is strongly recommended that a working copy of the audio file folder is created before using this tool.')),
                           
                           br(),
                           fluidRow(
                             column(6, shinyFilesButton('audio_folder', 'Select folder', 'Please select a folder', multiple = TRUE))
                             ),
                           # p(h5('NOT AVAILABLE ONLINE. Request a downloadable file from jhcox@illinois.edu.')),
                           
                           br(),
                           p(h4('2. Modify filenames')),
                           p(h6('Replace text by entering a pattern and replacement.')),
                           fluidRow(
                             column(6, textInput(inputId = 'text_pattern', label = 'Replace text...')),
                             column(6, textInput(inputId = 'text_replacement', label = '..with this text.')),
                           ),
                           
                           p(h6('Add GRTS ID and/or Location Name prefix to filenames.')),
                           fluidRow(
                             column(6, textInput(inputId = 'GRTS_ID', label = 'GRTS ID')),
                             column(6, textInput(inputId = 'location_name', label = 'Location Name')),  
                           ),
                           br(),
                           p(h4('3. Rename files')),
                           p(h6('Original filenames will be overwritten with new filenames.')),
                           fluidRow(column(7, actionButton('rename_files', 'Rename Files'), align = 'center')),

              ),
              
              mainPanel(width = 9, style = 'height:840px',
                        fluidPage(
                          fluidRow(
                            column(11, offset = '10px', align = 'center', style = 'background-color:white;',
                                   br(),
                                   tableOutput(outputId = 'audio_file_folder'), style='height:840px;overflow-y:scroll;'),
                          )))
            ))
  )

server<-function(input, output, session) {
  # #shiny files client side script
  audioFiles<-reactiveValues() #rv for file renamer
  volumes=getVolumes()()
  shinyFileChoose(input, 'audio_folder', roots=volumes, updateFreq = 0)
  
  observeEvent(input$rename_files, {
    showModal(modalDialog(title = "Are you sure you want to rename your files? Don't worry your original files won't be renamed. A new folder will be
                          created that contains your renamed files.",
                          footer = tagList(actionButton("confirm_rename_files", "Yes", class = "btn-custom btn-block")),
                          easyClose = TRUE
    ))
  })

  # Function to perform the renaming operation
  renameFiles <- function(filenames.og, filenames.new, new_full_file_extension, root.dir) {
    files <- paste0(root.dir, '/', filenames.new, new_full_file_extension)
    files.og <- paste0(root.dir, '/', filenames.og, new_full_file_extension)
    files.og <- files.og %>% str_replace(pattern = '.xlsx', '')
    file.rename(files.og, files)
  }
  
  # Function to create a copy of files in a new folder with renamed files
  copyFilesToRenamedFolder <- function(root.dir, filenames.new, new_full_file_extension) {
    renamed_folder <- paste0(root.dir, "/renamed_", basename(root.dir))
    dir.create(renamed_folder, showWarnings = FALSE)
    files <- list.files(path = root.dir, full.names = TRUE)
    for (i in seq_along(files)) {
      file.copy(files[i], paste0(renamed_folder, "/", filenames.new[i], new_full_file_extension))
    }
  }
  
  # Event observer for updating filenames
  observe({
    #get the file names, types and paths and stores file_selected as DF
    file_selected <- parseFilePaths(volumes, input$audio_folder)
    audio_file_full_filepath <- file_selected$datapath
    audio_file_filepath <- dirname(file_selected$datapath[1])
    audioFiles$root.dir <- audio_file_filepath
    
    #get original filenames from path set by user input, then create a RV for use in the file renamer event observer
    filenames.og <- list.files(audio_file_filepath)
    filenames.og <- tools::file_path_sans_ext(filenames.og) #removes extension and returns filename
    
    file_extensions <- tools::file_ext(audio_file_full_filepath) #removes file name and returns extension, but is missing the '.'
    full_file_extension <- paste0('.', file_extensions) #adds the '.' back in front of the file extension
    
    #this IF statement checks for .## files (anabat) which apparently appear as '.'  if '.' = full_file_extension, create a new file extension
    ## that is .zc.  this could probably be placed in the if statement for .zc files instead?
    # if (full_file_extension == '.' & input$`8.3 Filenames` == TRUE) {
    #   new_full_file_extension <- '.zc'
    #   audioFiles$new_full_file_extension <- new_full_file_extension
    # } else {
    #   audioFiles$full_file_extension <- full_file_extension #creates a RV for the file extension (e.g., '.zc', '.wav')
    #   audioFiles$new_full_file_extension <- full_file_extension
    # } 
    
    audioFiles$filenames.og <- filenames.og #RV is stored with only the filename and no extension
    
    #set new filenames and create a RV for use in the file renamer event observer
    filenames.new <- paste0(filenames.og)
    audioFiles$filenames.new <- filenames.new
    
    #this if statement checks for an input pattern and if not null, will change the new filenames based on pattern and replacement inputs
    if (input$text_pattern != '') {
      patternAF <- input$text_pattern
      try(filenames.new <- filenames.new %>% sub(pattern = patternAF, replacement = input$text_replacement, fixed = TRUE))
      #this IF statement is for the 8.3 filenames fx (anabat_file_converter() ) checkbox input. if checked, replace the period with an underscore
      ## and replace the # symbol with nothing.  a '.zc' extension is added in another IF statement that checks for .## file extensions
      # if (input$`8.3 Filenames` == TRUE) {
      #   try(filenames.new <- filenames.new %>% str_replace('\\.', '_'))
      #   try(filenames.new <- filenames.new %>% str_replace('#', ''))
      # }
    }
    
    if(input$location_name != '') {
      try(filenames.new <- paste0(input$location_name, '_', filenames.new))
    }
    if(input$GRTS_ID != '') {
      try(filenames.new <- paste0(input$GRTS_ID, '_', filenames.new))
    }
    
    #store this for display in table for og filenames
    Extension <- tools::file_ext(audio_file_full_filepath)
    
    #create a DF to hold the new and old filenames, then create a RV to store the values as they update and display in the DT output
    compiled_audioData <- data.frame(filenames.og, Extension, filenames.new)
    compiled_audioData <- compiled_audioData %>% rename('Original Filename' = filenames.og, 'New Filename' = filenames.new)
    try(compiled_audioData$Extension <- paste0('.', compiled_audioData$Extension))
    compiled_audioData <- compiled_audioData %>% add_column('New Extension' = audioFiles$new_full_file_extension)
    # if (full_file_extension == '.' & input$`8.3 Filenames` == FALSE) {
    #   try(compiled_audioData$Extension <- '.##')
    #   try(compiled_audioData$`New Extension` <- '.##')
    # } else if (input$`8.3 Filenames` == TRUE) {
    #   try(compiled_audioData$Extension <- '.##')
    #   try(compiled_audioData$`New Extension` <- '.zc')
    # }
    audioFiles$audioData <- compiled_audioData
  })
  
  # Event observer for renaming files and showing the confirmation modal
  observeEvent(input$rename_files, {
    filenames.og <- audioFiles$audioData$`Original Filename`
    filenames.new <- audioFiles$audioData$`New Filename`
    root.dir <- audioFiles$root.dir
    new_full_file_extension <- audioFiles$new_full_file_extension
    copyFilesToRenamedFolder(root.dir, filenames.new, new_full_file_extension)
    renameFiles(filenames.og, filenames.new, new_full_file_extension, root.dir)
    # Update RV for new filenames to replace original filenames
    audioFiles$audioData$`Original Filename` <- filenames.new
    showModal(
      modalDialog(title = h3('Files have been renamed successfully!', align = 'center'), 
                  footer = NULL, easyClose = TRUE, 
      ))
  })
  
  # displays the active old and new filenames (file renamer)
  output$audio_file_folder<-renderTable({audioFiles$audioData})
  output$new_audio_file_folder<-renderTable({audioFiles$audioData$`New Filename`})

  ### HELP window for the renamer
  observeEvent(input$help_renamer, {
    showModal(
      modalDialog(
        title = h3('How to Use The File Renamer', align = 'center', style = 'color:black;background-color:#F8BA14;'), 
        size = 'm', 
        footer = NULL, 
        easyClose = TRUE, 
        p(
          h4("1. Use '1. Select folder' to select a folder of files that you wish to rename"),
          h5("    a. Click 'Select folder' and navigate to the folder of files you wish to rename"),
          "        i. Any files in the folder will be renamed based on the criteria you enter, so be sure you want to rename all files in this folder",
          h5("    b. Select a file within the folder. The entire folder of files will load into the viewer window"),
          br(),
          h4("2. Use the tools in '2. Modify filenames' to rename the files"),
          h5("    a. 'Replace text...'"),
          "        i. Enter a value for the renamer to locate in each filename (example: SN91025). The app will identify any matching values and will replace them with your replacement in the next box",
          "        ii. You can enter any value you like",
          h5("    b. '...with this text.'"),
          "        i. Enter a value for the renamer to replace the values in the previous selection (example: Site_A_). In the previous example, for any filenames containing 'SN91025', the 'SN91025' text will be replaced with 'Site_A_'.",
          "        ii. You can enter any value you like, except commas (,)",
          "        iii. If you entered a value in 'Replace text...', but leave the 'with this text.' box blank, the 'Replace text...' values will be removed from the filename.",
          h5("    c. 'GRTS ID'"),
          "        i. Enter the GRTS ID (NABat sample cell) value where the file was recorded. This will be added as a prefix to the filename, followed by an underscore (_) separator.",
          "        ii. Enter the Location name of the site where the file was recorded. This will be added as a prefix to the filename (after the 'GRTS ID' prefix, if you entered a GRTS ID), followed by an underscore (_) separator.",
          br(),
          h4("3. Click 'Rename Files'"),
          h5("    a. The files will be renamed."),
          "        i. If you entered a value in 'Replace text...', any values in filenames that match that value will be replaced.",
          "        ii. If you entered a value in 'GRTS ID' or 'Location Name', those values will be prepended to the filename",
          h5("    b. Your renamed files will be saved in the parent folder under a folder called renamed_{parent_folder_name}")
        )
      )
    )
  })
  
  # STOP APPLICATION
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



# rv_upload_console<-reactiveValues(message= "Log in using your NABat Partner Portal credentials, then select a project.") #for upload window textoutput

# # #displays an upload window with login questions #for autoID renamer
# observeEvent(input$upload_action, {
#   useShinyjs()
#   output$user_doc_name<- NULL
#   #next 2 lines remove .csv from the original doc extension. .csv extension is added in before uploading
#   # accounting for user input where user does not include .csv
#   raw_output_no_ext<-paste0(input$raw_output$name)
#   raw_output_no_ext<-gsub('.csv', '', raw_output_no_ext)
#   
#   #load some text into the outputs of this window
#   rv_upload_console$message = paste0("Log in using your NABat Partner Portal credentials, then select a project.")
#   #output$user_doc_name<- renderText(paste0('Upload will appear as: ', input$csvName, '.csv'))
#   project_choices = c('login to get list')
#   
#   showModal(
#     modalDialog(title = tags$b('Upload Data to Partner Portal Project'), size = 'l', footer = NULL, easyClose = TRUE,
#                 #box(height = 'auto', width = 'auto',
#                 fluidRow(
#                   column(6, align = 'center',
#                          textInput('username', 'NABat Username', value = ''),
#                          passwordInput('password', 'NABat Password', value = ""),
#                          actionButton('Login', 'Log in to the Partner Portal'),
#                   ),
#                   column(6, align = 'center',
#                          selectInput(
#                            inputId = 'projectId',
#                            label = 'Partner Portal Projects',
#                            choices = project_choices,
#                            selected = NULL
#                          ),
#                          textInput('csvName', '.csv Name', value = paste0('Toolbox_', Sys.time(),"_", raw_output_no_ext)),
#                          actionButton('uploadData', 'Upload to Project'),
#                   )
#                 ),
#                 fluidRow(
#                   column(12, align = 'center',
#                          HTML(
#                            paste(
#                              h5(' '), '<br/>',
#                              h5(" "), '<br/>'
#                              
#                            )
#                          ),
#                          htmlOutput("upload_console"),
#                          textOutput('user_doc_name')
#                   )
#                   
#                 )
#     ))
# })
# 
# observe(output$upload_console <- renderText(HTML(rv_upload_console$message)))
# 
# observeEvent(input$Login,{
#   rv_upload_console$message = paste0("Logging in...")
# })
# 
# observeEvent(input$Login,{
#   delay(500,
#         
#         withProgress(message = 'Acquiring login token...', {
#           req(input$username)
#           req(input$password)
#           
#           token = NULL
#           token = get_nabat_gql_token(username = input$username, password = input$password)
#           
#           if (is.null(token)){
#             print('token is null in LoginEvent')
#             rv_upload_console$message = paste0(tags$b('Error logging in, check username and password and try again.'))
#             output$user_doc_name<-NULL
#             
#           } else {
#             rv_upload_console$message = paste0('Token acquired. Attempting login...')
#             print('token is NOT null in LoginEvent')
#             rv_upload_console$message = paste0("Login complete! Select a project, verify the .csv name and press 'Upload to Project.'")
#             output$user_doc_name<- renderText(paste0('Document will be uploaded as: ', input$csvName, '.csv'))
#             user_id = get_user_id_by_email(input$username, token)
#             projects_by_user=get_projects(token)
#             project_choices<-projects_by_user %>% select('project_id','project_name')
#             project_choices$project_label<- paste(project_choices$project_id, project_choices$project_name, sep = ' - ')
#             project_choices<-project_choices %>% select('project_label','project_id')
#             rv_project<-project_choices$project_id
#             
#             updateSelectInput(
#               session = getDefaultReactiveDomain(),
#               inputId = 'projectId',
#               label = 'Your Partner Portal Projects',
#               choices = project_choices
#             )
#             return(input$projectId)
#           }
#         })
#   )})


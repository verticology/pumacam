##############################################################################################
####Run this one: Shiny ap to copy, rename, and append meta data for Puma Trail Cam Study####
############################################################################################
####Z. Warren
####zachary_warren@nps.gov
####7-17-2024
####V.1.1.3

# This shiny app is used to process raw trail camera files for a Zion Puma trail camera study
# It assumes that images are initially dumped into a raw file directory. That directory structure is
# "~/Photos/raw/4 letter abbreviation of deployment zone (i.e. grot)/deployement ID (grot_d1a)/deployment dates (050124_061224)/imgs"
# The code does the following:
#   1) Creates a mirrored folder structure located in ~/Photos/working
#   2) Copies raw files from the raw directory, to the new working directory with an exact mirror of the folder structure
#   3) corrects time errors where the time on the camera is inaccurate
#   4) Renames files from IMG0001 to a standard format zone_deployment_feature_ser.num_date_time (i.e. watc_d2a_cor_a010_05082024_161050.JPG)
#   5) Appends a metadata bar on top of the image containing metadata
#   6) Creates a csv that summarizes that deployment period
#   7) Creates a hand-vetting file of all images. This file is later updated in the post-analysis app. 

# Prior to running this file, a computer must have R installed
# occasionally, it has not automatically installed needed libraries and these had to be completed manually
# a text file is then opened with the following text included:
# 
# "C:\\Program Files\\R\\R-4.3.3\\bin\\Rscript.exe" -e "shiny::runApp('F:/WILDLIFE DRIVE/MAMMALS/Mountain_Lion/Research_Projects/2025_2028_Peyton_Movement_Diet_HabitatSelection/RCode/CamBatchFxn/ProcessPumaPics', launch.browser = TRUE)"
# for /f %%i in (F:/WILDLIFE DRIVE/MAMMALS/Mountain_Lion/Research_Projects/2025_2028_Peyton_Movement_Diet_HabitatSelection/RCode/AppFile/port.txt) do set PORT=%%i
# start http://127.0.0.1:%PORT%

#This txt file is then saved as a .bat file. The end user can then simply open the .bat file and the Shiny app will load. 

####Edit log:
#08/14/24: ZW: Made it so that all files <3bytes were removed from raw.file.paths to deal with corrupt files
########## Added a tryCatch, in the UI if it cannot read the file.mtime, it will output the identical camera.time for warning msg
#8/15/24: ZW: Fixed the pattern in first.ten.files to ignore prefixes to be more flexible across cameras
########## If a camera doesn't start with image 000, it will then take first 10 but it is slower;

#Code below:
###############################
#####Force it to set a port so it doesn't change randomly
#This allows the Port to be called in the .BAT file and be opened in an HTML window
port <- sample(3000:8000, 1)

#write the randomly sampled port number to a .txt file which is read in the .bat file
writeLines(as.character(port),
           "F:/WILDLIFE DRIVE/MAMMALS/Mountain_Lion/Research_Projects/2025_2028_Peyton_Movement_Diet_HabitatSelection/RCode/AppFile/port.txt")

#set the shiny port to this randomly sampled port number
options(shiny.port = port)

#####1. Install and load libraries#####
#Check if package is installed, if not, quietly install the package
######a. Load needed packages fxn######
lib.fxn <- function(pkg) {
  if(!requireNamespace(pkg, quietly = F)) {
    install.packages(pkg) }
}

######b. List of packages needed######
lib.list <- c(
  "lubridate",
  "magick",
  "shiny",
  "shinyFiles",
  "shinyWidgets",
  "fs",
  "shinyjs",
  "pbapply",
  "parallel",
  "withr")

######c. For loop to install packages if needed######
for(i in lib.list) {
  lib.fxn(i)
}

# Load the packages
# suppressWarnings(
#   suppressPackageStartupMessages({
#     library(lubridate)
#     library(magick)
#     library(shiny)
#     library(shinyFiles)
#     library(shinyWidgets)
#     library(fs)
#     library(shinyjs)
#     library(pbapply)
#     library(parallel)
#   }))

# suppressWarnings(
#   suppressPackageStartupMessages({
    require(lubridate)
    require(magick)
    require(shiny)
    require(shinyFiles)
    require(shinyWidgets)
    require(fs)
    require(shinyjs)
    require(pbapply)
    require(parallel)
    require(withr)
  # }))

#####2. User interface#####


ui <- fluidPage(
  useShinyjs(),
  #Add overall panel title
  titlePanel("Batch Process Raw Trail Camera Files v.1.1.3"),
  #add a subheader below title
  tags$h4("Please enlarge window to full screen"),
  
  #Create a sidebar layout so that options are on the left panel, and results are shown in the main panel
  sidebarLayout(
    
    sidebarPanel(
      #Change well color and font color within the sidebar panel
      #Background-color is panel background, color is the font color, btn color is the text color of buttons
      tags$style(".well {background-color: #3b3b48; color: #ffffff;}
                 .btn {color: black !important;}"),
      
      
      ######A. Directory Button######
      #Select raw file directory with raw image files
      #Add in text above the Directory button
      tags$p("1 - Select folder containing raw images to be processed"),
      
      #Add directory select button
      shinyDirButton(id = "raw",
                     label = "Browse Folders",
                     title = "Choose a folder containing the raw image files",
                     FALSE),
      #Output for nfile text to display number of image files in folder
      textOutput("fil_sel"), #output for selected Directory
      
      #add in a horizontal break
      tags$hr(),
      
      
      
      ######B. Date visit selector######
      #Add a date and time selector to select when the camera was actually visited,
      #This is the true time as recorded on datasheet in the field
      
      #Section header
      tags$p("2 - Select date and time when camera was visited"),
      
      #Add a date and time when the camera was visited
      airDatepickerInput(inputId = "visit.dt.time",
                         #label = "Select date and time when camera was visited",
                         placeholder = "Click Here",
                         timepicker = T,
                         addon = "none"),
      #Display chosen time
      textOutput("vis_time"),
      #Display a warning if it is an unrealistic time
      htmlOutput("vis_warn"),
      #Break
      tags$hr(),
      
      ######C. Date image selector######
      #Add a date and time selector the camera reported that visit image above was taken. 
      #If you visited a camera at 18:00 but the time on that image was 06:00, you would put 1800 in (B),
      #and 0600 here in (C).
      tags$p("3 - Select time reported on camera image"),
      
      #Add a date and time for when the image recorded this visit to correct dif.
      airDatepickerInput(inputId = "camera.dt.time",
                         #label = "Select time from visit image",
                         placeholder = "Click Here",
                         timepicker = T,
                         addon = "none"),
      
      #Display the time from the image (incorrect time)
      textOutput("old_time"),
      #Display the corrected time
      textOutput("new_time"),
      #Warnings
      htmlOutput("cam_warn"),
      htmlOutput("ent_warn"),
      #Break
      tags$hr(),
      
      ######D. Puma Sign Radio Button selector#######
      #Radio button to select the feature that camera is focused upon
      tags$p("4 - Select Puma Sign that was focus of camera deployment"),
      #Add in sign for why it was deployed
      radioButtons(inputId = "dep.feat", 
                   label = NULL,
                   choices = c("Scrape" = "scr", "Kill Site" = "kil", "Corridor" = "cor",
                               "Scat" = "sca", "Print" = "prt", "Trap" = "trp", "Other" = "otr")),
      tags$hr(),
      
      ######E. Camera ID Number####
      tags$p("5 - Enter Camera ID Number"),
      #Text input to enter serial number of camera
      textInput(inputId = "ser.num",
                label = NULL, 
                placeholder = "e.g. b001, a003"),
      tags$hr(),
      
      ######F. Cuddeback files?######
      tags$p(HTML('6 - Cuddeback Images? <span style="color:red;">*NEW*</span>')),
      #Yes/No as the whether or not images are from cuddeback cameras. These images are smaller in size so metadata must be resized.
      #Modified from overwrite on 1/17/25
      #Default is YES
      radioButtons(inputId = "cud.back", 
                   label = NULL,
                   choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE, inline = T),
      
      tags$hr(),
      
      ######G. Select processing cores
      tags$p("7 - How many processing cores do you wish to use?"),
      tags$i("Select a low number if you need to use your computer for other tasks, a high number processes images faster but your computer will be slow."),
      radioButtons(inputId = "core_select",label = NULL, c("1", "2", "3", "4", "5"), inline=T),
      
      
      ######G. Add in process button#######
      tags$p("8 - Click to process images "),
      #Action button to process images once clicked
      actionButton(inputId = "run",
                   label = "Process Images"),
      
      # uiOutput("run_button")
      
    ),
    
    ######H. Main panel display
    #What will be displayed on the right side once process is done
    #Mostly irrelevant as all progress bars are pop-ups but is a needed placeholder for
    #future releases
    mainPanel(
      textOutput("pro_time"),
      # Placeholder for the progress bar
      uiOutput("progressBar"),
      # Placeholder for status messages
      # TextOutput("status")
      
    )
  )
)


#####3. Server Fxn#####
server = function(input, output, session) {
  

  #set process button as unclicked initially*****
  # global <- reactiveValues(clicked = FALSE)
  
  ######A. Directory selector with raw images######
  
  #Set roots, the folder path that serves as the default for the directory selector
  #RAW is a user defined option and can be renamed to anything relevant i.e. "OffloadedImages"
  #Note: the naming of things in roots and shinyDirChoose is tempermental. It may not take decimals, underscores, or camel case.
  #If no files are seen when code is run, try renaming directors or "Raw" to not have special characters
  roots = c(Raw = "F:/WILDLIFE DRIVE/MAMMALS/Mountain_Lion/Research_Projects/2025_2028_Peyton_Movement_Diet_HabitatSelection/Photos/raw")
  
  #Insert a directory selector. 
  #roots = roots should equal above path
  #'raw' should match shinyDirButton(id = "raw", in the UI
  #defaultRoot = "Raw" should match name in roots = c(Raw...
  #session should be defined above in function(input, output, session) {
  shinyDirChoose(input, 'raw', roots = roots,
                 defaultRoot = "Raw", session = session)
  
  #set the object raw.dir to be reactive and only created once a folder directory containing the raw files is 
  #selected.
  raw.dir <- reactive({
    # Ensure that input$raw is available and not NULL
    req(input$raw)
    # Use parseDirPath to extract the path from input$raw
    parseDirPath(roots, input$raw)
  })
  
  #Observe = after raw.dir has been created, it will then display images located in selected directory 
  observe({
    if(!is.null(raw.dir())){
      #output$rawpath <- path1
      output$fil_sel <- renderText({
        # paste0("Images Selected: ", length(list.files(raw.dir()))-1)
        paste0(".../", basename(dirname(raw.dir())),"/", basename(raw.dir()), " Selected")
      })
    } else {
      output$fil_sel <- renderText({""})
    }
  })
  
  
  #######B. Record the time the camera is visited once entered######
  vis.tim <- reactive({
    # Ensure that time inputs area available and not NULL
    req(input$visit.dt.time)
    
    #Convert the time to lubridate format with a TZ
    ymd_hms(input$visit.dt.time, tz = "America/Denver")
    
  })
  
  
  #######C. Record the time indicated on the camera with a TZ######
  cam.tim <- reactive({
    req(input$camera.dt.time)
    
    ymd_hms(input$camera.dt.time, tz = "America/Denver")
  })
  
  
  
  ######D. Time calculations and corrections####### 
  
  #Calculate difference between camera time and time chosen for camera time.
  #This is the catch errors such as the camera being set to 2023 and the user selecting 2024. 
  ent.time <- reactive({
    req(input$camera.dt.time)
    
    # List raw file paths of the first 10 files in the directory
    first.ten.files <- list.files(raw.dir(), full.names = TRUE, 
                                  #Cameras output various file names
                                  #The 000 ensures it is only those between 0001 and 0009
                                  #only doing the first 10 saves on processing time
                                  # pattern = "IMG_000|DSCF000|IMAG000|IMG_000")
                                  pattern = ".*000\\d+\\.jpe?g$",
                                  ignore.case = T)
    
    if(length(first.ten.files) == 0) {
      first.ten.files <- tryCatch({list.files(raw.dir(), full.names = TRUE, pattern = "\\.jpe?g$", ignore.case = T)[1:10]},
                                  error = function(e){NA})
    }
    
    # Extract time for the first file of the 10
    # first.time <- file.info(first.ten.files)$mtime[1]
    first.time <- tryCatch({file.mtime(first.ten.files)[1]}, error = function(e){cam.tim()})
    
    #round difference in weeks between the time entered in camera time and time indicated in the time selector
    tryCatch({round(abs(difftime(cam.tim(), first.time, units = "weeks")[[1]])/4, 0)}, error = function(e){"Error in reading file times"})
    
  })
  
  
  
  #Display visit time and warning if > 6 months from today
  observe({
    if(!is.null(input$visit.dt.time)){
      
      output$vis_time <- renderText({
        paste0("Visit Time: ", format(vis.tim(), format = "%m-%d-%Y %H:%M"))
      })
      
      if(abs(difftime(vis.tim(), Sys.time(), units = "weeks")[[1]])>24){
        
        output$vis_warn <- renderUI({shiny::p("Warning: Visit time greater than 6 months from now, double check before proceeding",
                                              style = "color: #FF0000")})
      }
      
    }
  })
  
  #Display camera time and warning if > 6 months from today
  observe({
    if(!is.null(input$camera.dt.time)){
      #output$rawpath <- path1
      output$old_time <- renderText({
        paste0("Camera Time: ", format(cam.tim(), format = "%m-%d-%Y %H:%M"))
      })
      
      output$new_time <- renderText({
        paste0("Corrected Time: ",
               format(cam.tim() +
                        difftime(vis.tim(), cam.tim(), units = "secs")[[1]],
                      format = "%m-%d-%Y %H:%M"))
        
      })
    }
    
    
    if(abs(difftime(cam.tim(), Sys.time(), units = "weeks")[[1]])>24){
      
      output$cam_warn <- renderUI({shiny::p("Warning: Camera time greater than 6 months from now, double check before proceeding", 
                                            style = "color: #FF0000")})
    }
    
    if(ent.time()>3){
      
      #if time entered is greater than 3 weeks from the first image file, display a warning
      output$ent_warn <- renderUI({shiny::p(paste0("Warning: You selected a camera time, ", ent.time(), " months from the first image time, double check before proceeding"), 
                                            style = "color: #FF0000")})
    }
    
  })
  
  
  #####E. Processing Functions####
  
  #Upon clicking process, display a progress bar
  output$progressBar <- renderUI({
    # You can use HTML and CSS to style your progress bar if needed
    tags$div(class = "progress",
             tags$div(class = "progress-bar", role = "progressbar", 
                      style = "width: 0%", `aria-valuenow` = "0", 
                      `aria-valuemin` = "0", `aria-valuemax` = "100")
    )
  })
  
  # # Function to add timestamp and deployment data to image
  # add_meta <- function(image_path, meta) {
  #   #read in the image based upon given path
  #   image <- image_read(image_path)
  #   
  #   #append the metadata on top of the image
  #   image_with_meta <- image_annotate(image, text = meta,
  #                                     #meta bar location
  #                                     location = "+10+10", size = 40,
  #                                     
  #                                     #location relative to bottom ie South
  #                                     gravity = "South",
  #                                     
  #                                     #color = font color, box color = black
  #                                     color = "#39FF14", boxcolor = "black",
  #                                     #add a white halo around font
  #                                     strokecolor = "white")
  #   return(image_with_meta)
  # }
  
  # Function to add timestamp and deployment data to image WITH ERROR HANDLING for corrupt images
  # Function to add timestamp and deployment data to image
  add_meta <- function(image_path, meta, cud.back.set) {
    tryCatch({
      # Read in the image based upon the given path
      image <- image_read(image_path)
      
      if(cud.back.set == FALSE){size.cb = 40} else {size.cb = 20}
      
      # Append the metadata on top of the image
      image_with_meta <- image_annotate(image, text = meta,
                                        location = "+10+10", size = size.cb,
                                        gravity = "South",
                                        color = "#39FF14", boxcolor = "black",
                                        strokecolor = "white")
      return(image_with_meta)
    }, error = function(e) {
      # Handle the error (e.g., log it or display a message)
      # print(paste("Error processing image:", image_path, ". File removed."))
      showNotification(paste("Error processing image:", basename(image_path), ". File removed."),
                       type = "message", duration = 10)
      # Delete the corrupted file
      file.remove(image_path)
      
      return(NULL)  # Return NULL to continue execution
    })
  }
  
  
  ######F. After action button is pressed######
  
  
  #######F-1. create directories if needed######

  # observe({
  #   if (length(input$run)) {
  #     if (input$run) {
  #       global$clicked <- TRUE
  #       shinyjs::disable("run")  # Disable the button
  #     }
  #   }
  # })
  # 
  # 
  # 
  # output$run_button <- renderUI({
  #   if (!is.null(input$run) && global$clicked) {
  #     shinyjs::runjs("$('#run').css('color', '#3B3B48').css('background-color', '#C52233');")
  #     tags$button(
  #       id = "run",
  #       "Process Images"
  #     )
  #   } else {
  #     actionButton(inputId = "run", label = "Process Images")
  #   }
  # })

  observeEvent(input$run, {
    st.time <- Sys.time()

    shinyjs::disable("run")  # Disable the button immediately
    invalidateLater(0, session)
    # updateActionButton(session, "run", label = "Processing images", disabled = T)
    # invalidateLater(2000, session)
    
    # msg1$outputText <- renderText("Processing images...")
    # invalidateLater(0, session)
    # 
    
    
    showNotification("Beginning file processing", type = "message", duration = 10)

    
    
    
    
    
    # Obtain file path for the working directory
    # 'gsub' is used to replace 'raw' with 'working' in the directory path
    work.dir <- gsub("raw", "working", raw.dir())
    
    # Check if 'work_dir' is not NULL and not an empty string
    if(!is.null(work.dir) && work.dir != ""){
      # Check if the working directory exists
      if (!dir.exists(work.dir)) {
        # If it doesn't exist, create the directory
        dir.create(work.dir, recursive = TRUE)
        # Text output informs the user that a new directory has been created
        # output$wdir <- renderText({
        #   paste("New working directory created")})
        showNotification("New working directory created", type = "message", duration = 10)
        
      } else {
        # If it exists, inform the user that files will be added to the existing directory
        # output$wdir <- renderText({
        #   "Added files into pre-existing working directory"})
        showNotification("Added files into pre-existing working directory", type = "message", duration = 10)
      }
    }
    
    
    ######F-2. Create working names and extract times####
    
    # raw.dir <- "F:/WILDLIFE DRIVE/MAMMALS/Mountain_Lion/Research_Projects/2025_2028_Peyton_Movement_Diet_HabitatSelection/Photos/raw/dumb/copa_d1a/031924_040424"
    # system.time(
    # raw.file.paths <- list.files(raw.dir, full.names = TRUE, pattern = ".JPG"))
    # system.time(
    # raw.file.names <- basename(raw.file.paths)
    # )
    
    # List raw file paths
    raw.file.paths <- list.files(raw.dir(), full.names = TRUE, pattern = ".JPG")
    
    #to help remove corrupt files, pull their size and filter by ones greater than 3 bytes
    raw.file.size <- file.size(raw.file.paths)
    raw.file.paths <- raw.file.paths[raw.file.size > 3]
    
    # List the raw file names
    # raw.file.names <- list.files(raw.dir(), full.names = FALSE, pattern = ".JPG")
    raw.file.names <- basename(raw.file.paths)
    
    # Extract times for the files
    
    # raw.times <- file.info(raw.file.paths)$mtime
    
    raw.times <- file.mtime(raw.file.paths)

    # Extract difference in seconds between camera time and actual time to correct for errors
    time.adjust <- difftime(ymd_hms(input$visit.dt.time, tz = "America/Denver"),
                            ymd_hms(input$camera.dt.time, tz = "America/Denver"),
                            units = "secs")[[1]]
    
    # Create a vector of correct image times
    adj.times <- raw.times + time.adjust
    
    #Due to duplicates resulting from photo bursts, add 1 second to make sure no dups exist.
    while(any(duplicated(adj.times))) {
      dupes <- duplicated(adj.times)
      adj.times[dupes] <- adj.times[dupes] + 1
    }
    
    # Convert date time to suffix for file name
    time.date.stamp <- format(adj.times, "%m%d%Y_%H%M%S")
    
    # Combine unique naming combinations including site, deployment, feature, serial num, and timestamp
    work.path.names <- unlist(strsplit(work.dir, "/"))[length(unlist(strsplit(work.dir, "/"))) - 2:0]
    # work.names <- paste0(work.path.names[1], "_", work.path.names[2], "_",
    #                      input$dep.feat, "_", input$ser.num, "_", time.date.stamp, ".JPG")
    work.names <- paste0(work.path.names[2], "_", input$dep.feat, "_", input$ser.num, "_", time.date.stamp, ".JPG")
    
    
    ######F-3. Create metadata header to append to image####
    
    # Create a metadata string to append to the image
    meta <- paste0("     Site: ", strsplit(work.names, "_")[[1]][1],
                   "      Dep: ", strsplit(work.names, "_")[[1]][2],
                   "      Feat: ", input$dep.feat,
                   "      SN: ", input$ser.num,
                   "      ", mdy_hms(time.date.stamp, tz = "America/Denver"))
    
    #do you want to overwrite files, read from input
    cud.back.tf <- as.logical(input$cud.back)
    
    
    #######F-4. Copy/paste over files to new directory
    
    #Faster copy/paste with progress bar
    # withProgress(message = 'Copying files...', value = 0, {
    #   for (i in seq_along(raw.file.names)) {
    #     old_path <- file.path(raw.dir(), raw.file.names[i])
    #     new_path <- file.path(work.dir, raw.file.names[i])
    #     fs::file_copy(path = old_path, new_path = new_path, overwrite = TRUE)
    #     incProgress(1 / length(raw.file.names))
    #   }
    # })
    showNotification("Copying files. This may take a while.", type = "message", duration = 20)
    
    raw.par.dir <- raw.dir()
    
    # n_cores <- detectCores(logical = F)
    # cl <- makeCluster(n_cores - 1)
    cores <- as.integer(input$core_select)
    cl <- makeCluster(cores)
    clusterEvalQ(cl, {
      library(fs)
      library(shiny)
      library(magick)})
    
    # withProgress(message = 'Appending metadata ...', value = 0, {
    parLapply(cl, seq_along(raw.file.names), function(i) {
      old_path <- file.path(raw.par.dir, raw.file.names[i])
      new_path <- file.path(work.dir, raw.file.names[i])
      fs::file_copy(path = old_path, new_path = new_path, overwrite = TRUE)
      # incProgress(1 / length(working.file.paths))
    })
    
    # stopCluster(cl)
    
    # Update the status message after copying is complete
    # status_message("Files successfully copied.")
    showNotification("Files successfully copied.", type = "message", duration = 10)
    
    # Invalidate later to allow the message to be displayed
    #2 second delay
    invalidateLater(0, session)
    
    
    ######F-4. Rename file names######
    # # Update the status message for renaming files
    showNotification("Renaming Copied Files.", type = "message", duration = 10)
    
    # Rename files with progress bar
    # withProgress(message = 'Renaming files...', value = 0, {
    #   for (i in seq_along(raw.file.names)) {
    #     org_names <- file.path(work.dir, raw.file.names[i])
    #     new_names <- file.path(work.dir, work.names[i])
    #     file.rename(from = org_names, to = new_names)
    #     incProgress(1 / length(raw.file.names))
    #   }
    # })
    
    # withProgress(message = 'Renaming files...', value = 0, {
      for (i in seq_along(raw.file.names)) {
        org_names <- file.path(work.dir, raw.file.names[i])
        new_names <- file.path(work.dir, work.names[i])
        file.rename(from = org_names, to = new_names)
        # incProgress(1 / length(raw.file.names))
      }
    # })
    
    # raw.file.names <- list.files("F:/WILDLIFE DRIVE/MAMMALS/Mountain_Lion/Research_Projects/2025_2028_Peyton_Movement_Diet_HabitatSelection/Photos/raw/dumb/copa_d1a/031924_040424"
    # , full.names = FALSE, pattern = ".JPG")
    # 
    # work.dir <- "F:/WILDLIFE DRIVE/MAMMALS/Mountain_Lion/Research_Projects/2025_2028_Peyton_Movement_Diet_HabitatSelection/Photos/working/dumb/copa_d1a/031924_040424"
    # 
    # Update the status message after renaming is complete
    showNotification("Files successfully renamed.", type = "message", duration = 10)
    # Invalidate later to allow the message to be displayed
    invalidateLater(2000, session)
    
    
    ######F-5. Append metadata on top of renamed files######
    # Update the status message for appending metadata
    showNotification("Appending metadata to images. This may take a really long time.", type = "message", duration = 20)
    
    #get a list of new names
    
    # working.file.paths <- list.files(work.dir, full.names = T, pattern = ".JPG")
    working.file.paths <- file.path(work.dir, work.names)

    # Append metadata to images with progress bar WITH ERROR HANDLING to skip null image file.
    # withProgress(message = 'Appending metadata ...', value = 0, {
    #   for (i in seq_along(working.file.paths)) {
    #     image_path <- working.file.paths[i]
    #     new_image <- add_meta(image_path, meta = meta[i])
    #     if (!is.null(new_image)) {
    #       output_path <- file.path(work.dir, basename(image_path))
    #       image_write(new_image, path = output_path)
    #     }
    #     incProgress(1 / length(working.file.paths))
    #   }
    # })

    # Append metadata to images with progress bar WITH ERROR HANDLING to skip null image file USING APPLY Fxn.
    
    # plan("multisession")
    # # withProgress(message = 'Appending metadata ...', value = 0, {
    #   future_lapply(seq_along(working.file.paths), function(i) {
    #     image_path <- working.file.paths[i]
    #     new_image <- add_meta(image_path, meta = meta[i])
    #     if (!is.null(new_image)) {
    #       output_path <- file.path(work.dir, basename(image_path))
    #       image_write(new_image, path = output_path)
    #     }
    #     # incProgress(1 / length(working.file.paths))
    #   }, future.seed = T)
    # # })
    # n_cores <- detectCores(logical = F)
    # cl <- makeCluster(n_cores-1)
    # clusterEvalQ(cl, {
    #   library(magick)
    #   library(shiny)})
    
    # withProgress(message = 'Appending metadata ...', value = 0, {
      parLapply(cl, seq_along(working.file.paths), function(i) {
        image_path <- working.file.paths[i]
        new_image <- add_meta(image_path, meta = meta[i], cud.back.set = cud.back.tf)
        if (!is.null(new_image)) {
          output_path <- file.path(work.dir, basename(image_path))
          image_write(new_image, path = output_path)
        }
        # incProgress(1 / length(working.file.paths))
      })
    # })
    stopCluster(cl)
    # Update the status message after appending metadata is complete
    showNotification("Metadata appended to images.", type = "message", duration = 10)
    # Invalidate later to allow the message to be displayed
    invalidateLater(2000, session)
    
    
    ######F-6. Output summary of data from directory#######
    #This summary is saved in the working directory
    
    #create summary data.frame of summary data
    output.summary <- data.frame(site = strsplit(work.names, "_")[[1]][1], 
                                 deployment = strsplit(work.names, "_")[[1]][2],
                                 surv_period = work.path.names[3],
                                 feature = input$dep.feat,
                                 SN = input$ser.num,
                                 #date of first image
                                 first_date = format(min(adj.times), "%m-%d-%Y"),
                                 #time of first image
                                 first_time_24hrs = format(min(adj.times), "%H:%M:%S"),
                                 #date of last image
                                 last_date = format(max(adj.times), "%m-%d-%Y"),
                                 #time of last image
                                 last_time_24hrs = format(max(adj.times), "%H:%M:%S"),
                                 #number of image files
                                 n_imgs = length(adj.times),
                                 #number of hours deployed
                                 time_dep_hrs = difftime(ymd_hms(max(adj.times), tz = "America/Denver"), 
                                                         ymd_hms(min(adj.times), tz = "America/Denver"), 
                                                         units = "hours")[[1]],
                                 #file directory
                                 file_path = work.dir,
                                 copied_on = Sys.time())
    
    
    ######F-7. Create analysis dataframe of data from directory#######
    #This file lists each image in a csv and can be modified by the user for each additional image
    
    output.analysis <- data.frame(site = strsplit(work.names, "_")[[1]][1], 
                                  deployment = strsplit(work.names, "_")[[1]][2],
                                  surv_period = work.path.names[3],
                                  feature = input$dep.feat,
                                  SN = input$ser.num,
                                  date = format(adj.times, "%m-%d-%Y"),
                                  time = format(adj.times, "%H:%M:%S"),
                                  image_file = work.names,
                                  species = NA,
                                  n_obs = NA,
                                  age_lion = NA,
                                  notes = NA,
                                  save = NA,
                                  functioning = 1,
                                  vetted = 0)
    
    #######F-8. Write a csv of summary file######
    #first check if one exists, if so, create a new one with a randomly selected suffix to avoid overwriting
    
    summary.path <- file.path(work.dir, paste0("_", work.path.names[2], "_", work.path.names[3], "_summary.csv"))
    
    if (file.exists(summary.path)) {
      ran_n <- sample(100:999, 1)
      sum.save <- sub("\\.csv$", paste0("_", ran_n, ".csv"), summary.path)
      write.csv(output.summary, sum.save, row.names = F)
      showNotification(paste0("Summary file already exists. A new one with suffix ", ran_n, " saved."), type = "message", duration = 10) 
    } else {
      write.csv(output.summary, summary.path, row.names = F)
      showNotification("Hand vetting file saved successfully.", type = "message", duration = 10)
    }
    
    #######F-9. Write a csv of of the analysis file (aka hand vetting file######
    
    vet.path <- file.path(work.dir,
                          paste0("_", work.path.names[2], "_", work.path.names[3], "_vetting.csv"))
    
    
    if (file.exists(vet.path)) {
      ran_n <- sample(100:999, 1)
      vet.save <- sub("\\.csv$", paste0("_", ran_n, ".csv"), vet.path)
      write.csv(output.analysis, vet.save, row.names = F)
      showNotification(paste0("Hand vetting file already exists. A new one with suffix ", ran_n, " saved."), type = "message", duration = 10) 
    } else {
      write.csv(output.analysis, vet.path, row.names = F)
      showNotification("Hand vetting file saved successfully.", type = "message", duration = 10)
    }
    
    end.time <- Sys.time()
    
      
    ######F-10. Show completed notification
    showNotification("Image processing complete. You may close or select new folder to continue processing.", 
                     type = "message",
                     duration = 10)
    
    # After processing, re-enable the button and reset its appearance

    shinyjs::enable("run")
    showNotification(paste0("Processing Time: ", round(as.double(difftime(end.time, st.time)), 1), units(difftime(end.time, st.time))),
                     type = "message", duration = 5)
    Sys.sleep(5)  # Update the UI immediately
    session$reload()
  })
  
  # observe(output$msg1 <- renderText(HTML(msg1$outputText)))
}



#####4. Run the application ######
shinyApp(ui = ui, server = server)







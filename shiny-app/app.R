# --- Install and Load Required Packages ---
# install.packages(c("shiny", "DT", "shinyjs", "shinydashboard", "dplyr", "tidyr")) # Uncomment and run if you miss any
library(shiny)
library(DT)
library(shinyjs)
library(shinydashboard) # For the collapsible sidebar
library(dplyr)      # For %>% and arrange in generate_well_plate_ui
library(tidyr)   # For expand_grid
library(writexl) # For .xlsx outputs
library(openxlsx) # For writing the complete table into the first sheet
library(ggplot2)# For Graphs
library(readxl)


source("cvd_all_in_one__v2.R")
source("generate_well_names.R")
source("fmf_panel_v1.R")
source("cvd_all_in_one_rotor.R")
source("fmf_panel_rotor.R")


# app.R

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "OctoClario"
  ),
  dashboardSidebar(
    div(style = "display: none;",
        downloadButton("downloadData", "Save As")
    ),
    # Custom CSS to improve visibility and layout of elements within the dashboardSidebar
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css",
                integrity = "sha512-9usAa10IRO0HhonpyAIVpjrylPvoDwiPUiKdWk5t3PyolY1cOd4DSE0Ga+ri4AuTroPR5aQvXU9xC6qOPnzFeg==",
                crossorigin = "anonymous", referrerpolicy = "no-referrer"),
  
      tags$style(HTML("
        /* --- Sidebar Adjustments --- */
        /* Adjust main sidebar width and corresponding content margins */
        .main-sidebar, .left-side {
            width: 200px !important;
            left: 0 !important;
            transform: translateX(0px) !important;
            -webkit-transform: translateX(0px) !important;
        }
        .main-header .navbar {
            margin-left: 200px !important;
        }
        .content-wrapper, .right-side, .main-footer {
            margin-left: 200px !important;
        }

        /* Adjustments for collapsed sidebar */
        .sidebar-collapse .main-sidebar,
        .sidebar-collapse .left-side {
            width: 50px !important;
            display: block !important;
            
            z-index: 810 !important;
            left: 0 !important;
            transform: translateX(0px) !important;
            -webkit-transform: translateX(0px) !important;
            text-align: center !important;
        }
        .sidebar-collapse .main-header .navbar {
            margin-left: 50px !important;
        }
        .sidebar-collapse .content-wrapper,
        .sidebar-collapse .main-footer {
            margin-left: 50px !important;
        }

        /* Hide the main header title when sidebar is collapsed */
        .sidebar-collapse .main-header .logo {
          display: none !important;
        }

        /* Adjustments for the main header title (logo) when sidebar is OPEN */
        .main-header .logo {
            text-align: center;
            padding-left: 5px;
            padding-right: 5px;
            overflow: hidden;
            white-space: nowrap;
            text-overflow: ellipsis;
        }

        /* Sidebar internal element styling for better appearance and alignment */
        .main-sidebar h4 {
            font-size: 1.1em;
            color: white;
            margin-left: 10px;
            margin-right: 10px;
            margin-top: 20px;
            margin-bottom: 10px;
        }
        .main-sidebar .btn {
            width: calc(100% - 20px);
            margin-left: 10px;
            margin-right: 10px;
            margin-bottom: 5px; /* ADJUSTED FOR CLOSER SPACING */
        }
        .main-sidebar .shiny-text-output {
            color: #d2d2d2;
            margin-left: 10px;
            margin-right: 10px;
            margin-bottom: 10px;
            word-wrap: break-word;
        }
        .main-sidebar .help-block {
            color: #b0b0b0;
            margin-left: 10px;
            margin-right: 10px;
            font-size: 0.9em;
            margin-bottom: 15px;
        }
        .main-sidebar hr {
            border-top: 1px solid #4a4a4a;
            margin: 20px 10px;
        }

        /* Adjustments for collapsed sidebar (mini-sidebar) - Hides text elements */
        .sidebar-collapse .main-sidebar h4,
        .sidebar-collapse .main-sidebar .shiny-text-output,
        .sidebar-collapse .main-sidebar .help-block {
          display: none !important;
        }

        /* Styling for ALL buttons in collapsed sidebar */
        .sidebar-collapse .main-sidebar .btn {
            display: block !important;
            width: 35px !important;
            height: 35px !important;
            padding: 0 !important;
            font-size: 0 !important;   /* Hide all direct text on the button */
            margin-left: auto !important;
            margin-right: auto !important;
            margin-bottom: 5px; /* Ensure this matches for consistent spacing in collapsed mode */
            background-color: #555555 !important;
            color: #f8f9fa !important;
            position: relative;
        }
        /* Hide the button text specifically when collapsed (targets the text node) */
        .sidebar-collapse .main-sidebar .btn > :not(svg) {
            display: none !important;
        }
        /* Style the SVG icon within the collapsed button */
        .sidebar-collapse .main-sidebar .btn svg {
            display: block !important;
            width: 18px !important;
            height: 18px !important;
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            -webkit-transform: translate(-50%, -50%);
            fill: currentColor !important;
        }

        /* Ensure icon (SVG) still has margin when sidebar is OPEN (next to text) */
        .main-sidebar .btn svg {
            margin-right: 5px; /* Adjust as needed for spacing */
            vertical-align: middle; /* Align nicely with text */
        }

        .sidebar-mini.sidebar-collapse .main-sidebar .sidebar-menu>li.active>a {
              border-left-color: #f39c12; /* Example: Active item color */
        }
      "))
    ),
    
    h4("1. Select Data Directory"),
    actionButton(
      "select_folder_native",
      HTML(
        '<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="folder-open"
         class="svg-inline--fa fa-folder-open" role="img" xmlns="http://www.w3.org/2000/svg"
         viewBox="0 0 576 512" style="fill: currentColor; width: 18px; height: 18px;">
       <path d="M88 0H200c21.4 0 32.1 25.9 17 41L134.7 128H472c22.1 0 40 17.9 40 40V464c0 22.1-17.9 40-40 40H104c-22.1 0-40-17.9-40-40V96c0-22.1 17.9-40 40-40h48L110.7 23.4C103 15.7 92.9 0 88 0zm0 64V96H472V172c0 6.6-5.4 12-12 12H88c-6.6 0-12-5.4-12-12V64H88zm384 128V464H104V192H472z"></path>
     </svg>
     Select Data Folder'
      )
    ),
    tags$br(),
    textOutput("selected_directory_path"),
    tags$script(HTML("
  document.getElementById('select_folder_native').addEventListener('click', async () => {
    try {
      const folder = await window.electronAPI.chooseFolder();
      if (folder) {
        Shiny.setInputValue('selected_folder', folder, {priority:'event'});
      }
    } catch(e){ console.error(e); }
  });
")),
    hr(),
    
    
    h4("2. Run Analysis"),
    # MODIFIED: Added icon for running analysis
    actionButton(
      "run_analysis",
      HTML(
        "<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"play\" class=\"svg-inline--fa fa-play\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 384 512\" style=\"fill: currentColor; width: 18px; height: 18px;\">
            <path d=\"M361 215C375.3 223.8 384 239.1 384 256C384 272.9 375.3 288.2 361 296.1L73.03 472.1C58.21 482.6 39.66 482.4 25.02 471.5C10.35 460.7 0 440.8 0 416V96C0 71.17 10.35 51.33 25.02 40.46C39.66 29.59 58.21 29.4 73.03 39.87L361 215z\"></path>
         </svg>
         Run Analysis"
      )
    ),
    hr(),
    h4("3. Save Results"),
    actionButton("show_save_modal",
                 HTML(
                   "<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"download\" class=\"svg-inline--fa fa-download\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 512\" style=\"fill: currentColor; width: 18px; height: 18px;\">
        <path d=\"M352 96C352 78.3 337.7 64 320 64C302.3 64 288 78.3 288 96L288 306.7L246.6 265.3C234.1 252.8 213.8 252.8 201.3 265.3C188.8 277.8 188.8 298.1 201.3 310.6L297.3 406.6C309.8 419.1 330.1 419.1 342.6 406.6L438.6 310.6C451.1 298.1 451.1 277.8 438.6 265.3C426.1 252.8 405.8 252.8 393.3 265.3L352 306.7L352 96zM160 384C124.7 384 96 412.7 96 448L96 480C96 515.3 124.7 544 160 544L480 544C515.3 544 544 515.3 544 480L544 448C544 412.7 515.3 384 480 384L433.1 384L376.5 440.6C345.3 471.8 294.6 471.8 263.4 440.6L206.9 384L160 384zM464 440C477.3 440 488 450.7 488 464C488 477.3 477.3 488 464 488C450.7 488 440 477.3 440 464C440 450.7 450.7 440 464 440z\"></path>
     </svg>
                   Save As...")
    ),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('bindSaveButton', function(message) {
      const btn = document.getElementById('show_save_modal');
      if (btn && !btn.dataset.bound) {
        btn.dataset.bound = 'true'; // prevent rebinding
        btn.addEventListener('click', async () => {
          const filePath = await window.electronAPI.saveFile('analysis_results.xlsx', [
            { name: 'Excel File', extensions: ['xlsx'] }
          ]);
          if (filePath) {
            Shiny.setInputValue('save_analysis_path', filePath);
          }
        });
      }
    });
  "))
  ),
  
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    # Load Split.js CSS and JS
    tags$head(
      tags$script(src = "split.min.js"), # <--- Using LOCAL file from www/
      tags$script(HTML("
      Shiny.addCustomMessageHandler('download_file', function(message) {
        var link = document.createElement('a');
        link.href = window.URL.createObjectURL(new Blob([], {type: 'application/octet-stream'}));
        link.download = message.filename;
        link.click();
        window.URL.revokeObjectURL(link.href);
      });
    ")),
      tags$style(
        HTML(
          "
  /* --- Split.js Specific CSS (Only for Vertical Split) --- */
  #split-container {
    display: flex;
    flex-direction: column;
    height: calc(100vh - 50px - 30px);
    overflow: hidden;
  }
  #top-panel, #bottom-panel {
    box-sizing: border-box;
    padding: 15px;
    overflow: auto;
  }
  .gutter {
    background-color: #ccc !important;
    background-repeat: no-repeat;
    background-position: 50%;
    height: 10px !important;
    min-height: 10px;
    border-top: 1px solid #aaa;
    border-bottom: 1px solid #aaa;
    z-index: 10;
  }
  .gutter.gutter-vertical {
    background-image: url('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAeCAYAAADkKxY0AAAABGdBTUEAALGPC/xhBQAAAB50RVh0Q3JlYXRpb24gVGltZQAxMC8xNS8xNiAyMDoxODoyNyBzcllHAAAAV0lEQVQYV2NsaHho/P//Px4+fHgC4T3qUoIAQIAgQEAIEBBIECDAECAYQIAAQYCAQICAwA2o+r/0z78/gAgQAAgQIBAgQEAIECAQECAAECAgAAgQAAAEAAAABAAK4s2L6PjO2QAAAABJRU5ErkJggg==');
    cursor: row-resize;
  }

  /* --- Responsive 96-Well Plate with Legend --- */
  .well-plate-layout-container {
    display: grid;
    grid-template-columns: 1fr 200px;
    gap: 10px;
    width: 100%;
  }
  .well-plate-container {
    width: 100%;
  }
  .plate-grid {
    display: grid;
    grid-template-columns: 30px repeat(12, 1fr);
    grid-template-rows:    30px repeat(8, 1fr);
    gap: 4px;
    width: 100%;
    aspect-ratio: 13 / 9;
  }
  .plate-grid .col-label,
  .plate-grid .row-label {
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 0.8em;
    font-weight: bold;
  }
  .plate-grid .corner {
    /* empty top-left */
  }
  .plate-grid .well-button {
    width: 100%;
    height: 100%;
    padding: 0;
    box-sizing: border-box;
  }

  /* Legend styling */
  .well-plate-legend {
    margin-top: 5px;
    font-size: 0.8em;
    padding-left: 15px;
  }
  .well-plate-legend h4 {
    margin-top: 0;
    font-size: 1.1em;
    color: #333;
  }
  .legend-item {
    display: flex;
    align-items: center;
    margin-bottom: 5px;
  }
  .legend-color-box {
    width: 20px;
    height: 20px;
    border-radius: 50%;
    border: 1px solid #ccc;
    margin-right: 10px;
    flex-shrink: 0;
  }

    /* ---- Rotor-Gene circular view ---- */
  .rotor-container { width: 100%; aspect-ratio: 1 / 1; }
  .rotor-svg { width: 100%; height: auto; display: block; }
  .rg-outline { fill: none; stroke: #e5e5e5; stroke-width: 0.6; }
  .rg-spoke   { stroke: #e5e5e5; stroke-width: 0.4; }

  .rg-well {
    stroke: #66b2ff;
    fill: #e6f7ff;
    cursor: pointer;
    transition: filter .15s ease;
  }
  .rg-well:hover { filter: brightness(0.95); }

  .rg-label { font-size: 2.2px; font-weight: 600; pointer-events: none; }

    /* Genotype fills for rotor wells */
  .rg-well.rg-ntc    { fill: #bbdefb; stroke: #64b5f6; }
  .rg-well.rg-pos    { fill: #c8e6c9; stroke: #81c784; }
  .rg-well.rg-sample { fill: #e6f7ff; stroke: #66b2ff; }
  .rg-well.rg-empty  { fill: #f5f5f5; stroke: #bbb; cursor: not-allowed; }

    /* Legend color squares for rotor */
  .legend-color-box.rg-ntc    { background-color: #bbdefb; border-color: #64b5f6; }
  .legend-color-box.rg-pos    { background-color: #c8e6c9; border-color: #81c784; }
  .legend-color-box.rg-sample { background-color: #e6f7ff; border-color: #66b2ff; }
  .legend-color-box.rg-empty  { background-color: #f5f5f5; border-color: #bbb; }





  /* Well button base */
  .well-button {
    position: relative;
    border: 1px solid #ccc;
    background-color: #eee;
    cursor: pointer;
    transition: background-color 0.2s, border-color 0.2s;
    border-radius: 50%;
    overflow: hidden;
  }
  .well-button span {
    position: absolute;
    top: 0; left: 0; bottom: 0; right: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: calc(1em + 0.2vw);
    white-space: nowrap;
  }
  .well-button:hover {
    background-color: #d0d0d0;
    border-color: #999;
  }

  /* Genotype colors */
  .well-ntc {
    background-color: #bbdefb;
    border-color: #64b5f6;
  }
  .well-ntc:hover {
    background-color: #90caf9;
    border-color: #42a5f5;
  }
  .well-pos-ctrl {
    background-color: #c8e6c9;
    border-color: #81c784;
  }
  .well-pos-ctrl:hover {
    background-color: #a5d6a7;
    border-color: #66bb6a;
  }
  .well-button-active {
    background-color: #e6f7ff;
    border-color: #66b2ff;
  }
  .well-button-active:hover {
    background-color: #cceeff;
  }
  .well-button-inactive {
    background-color: #f5f5f5;
    color: #aaa;
    cursor: not-allowed;
    opacity: 0.7;
  }
"
        )
      )
      
    ),
    
    # H3 and textOutput for Analysis Status are removed
    # h3("Analysis Status:"),
    # textOutput("status_message"),
    
    # --- Outer Split.js Container (Vertical Split) ---
    div(id = "split-container",
        # Top Panel: Now contains standard Bootstrap columns
        div(id = "top-panel",
            fluidRow(
              # Top Left Column: For the 96-Well Plate (width=5)
              column(width = 5,
                     div(class = "well-plate-container",
                         # H3 and hr() for 96-Well Plate Overview are removed
                         # h3("96-Well Plate Overview:"),
                         uiOutput("well_plate_ui")
                         # hr()
                     )
              ),
              # Top Right Column: For Graphs (width=7)
              column(width = 7,
                     # This container will be hidden when a plot is rendered
                     div(id = "plot_headers",
                         h3("Graphs & Visualizations:"),
                         h4("Melting Curve Plots:"),
                         p("Click on a well in the 96-well plate to see its combined melting curve plot.")
                     ),
                     
                     # A single plotOutput for the combined graph
                     plotOutput("combined_well_plot", height = "250px"),
              )
            )
        ),
        # Bottom Panel: Remains the same, hosting analysis results
        div(id = "bottom-panel",
            h3("Analysis Results:"),
            uiOutput("well_specific_results_ui"),
            uiOutput("results_tabs"),
            
        )
    ),
    # Hidden download link to be triggered programmatically
    tags$a(id = "download_file_now", style = "display:none;", `data-name` = "download_file_now")
    # --- END Outer Split.js Container ---
  )
)

server <- function(input, output, session) {
  ## --- Toasts / Modals (reusable) ---
  notify_ok   <- function(msg, duration = 4) showNotification(msg, type = "message", duration = duration)
  notify_warn <- function(msg, duration = 6) showNotification(msg, type = "warning", duration = duration)
  notify_err  <- function(msg, duration = 8) showNotification(msg, type = "error",   duration = duration)
  
  fatal_modal <- function(title = "Something went wrong", body = "Please try again.") {
    showModal(modalDialog(
      title = title,
      div(style="white-space: pre-wrap;", body),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  }
  
  ## Wrap any expression with a nice toast on error; returns NULL on failure
  catch_toast <- function(expr, context = "Operation") {
    tryCatch(
      force(expr),
      error = function(e) {
        notify_err(sprintf("%s failed: %s", context, conditionMessage(e)))
        NULL
      }
    )
  }
  
  
  strip_w <- function(x) sub("^w(?=\\d+$)", "", x, perl = TRUE)
  display_well <- function(x) ifelse(grepl("^w\\d+$", x), strip_w(x), x)
  normalize_genotype <- function(x) {
    x <- trimws(as.character(x))
    dplyr::case_when(
      grepl("^(pos(itive)?\\s*ctrl|positive\\s*control|control)$", x, ignore.case = TRUE) ~ "Pos Ctrl",
      grepl("ntc|no\\s*template", x, ignore.case = TRUE)                                   ~ "NTC",
      TRUE ~ x
    )
  }
  safe_get <- function(x, nm, default = NA_character_) {
    if (is.null(x)) return(default)
    if (is.character(x) || is.list(x)) {
      if (length(names(x)) && nm %in% names(x)) return(x[[nm]])
    }
    default
  }
  
  
  # --- Folder picker UI (restores the 'Select Data Folder' button) ---
  output$folder_picker_ui <- renderUI({
    tagList(
      actionButton(
        "select_folder_native",
        HTML(
          "<svg aria-hidden='true' focusable='false' data-prefix='fas' data-icon='folder-open'
              class='svg-inline--fa fa-folder-open' role='img' xmlns='http://www.w3.org/2000/svg'
              viewBox='0 0 576 512' style='fill: currentColor; width: 18px; height: 18px;'>
           <path d='M88 0H200c21.4 0 32.1 25.9 17 41L134.7 128H472c22.1 0 40 17.9 40 40V464c0 22.1-17.9 40-40 40H104c-22.1 0-40-17.9-40-40V96c0-22.1 17.9-40 40-40h48L110.7 23.4C103 15.7 92.9 0 88 0zm0 64V96H472V172c0 6.6-5.4 12-12 12H88c-6.6 0-12-5.4-12-12V64H88zm384 128V464H104V192H472z'></path>
         </svg>
         Select Data Folder"
        )
      ),
      tags$script(HTML("
      document.getElementById('select_folder_native').addEventListener('click', async () => {
        try {
          const folder = await window.electronAPI.chooseFolder();
          if (folder) Shiny.setInputValue('selected_folder', folder, {priority:'event'});
        } catch (e) { console.error('chooseFolder failed', e); }
      });
    ")),
      tags$br(),
      verbatimTextOutput('selected_directory_path')
    )
  })
  
  # Detect instrument from the well IDs present in your results mapping
  is_rotor_id <- function(x) grepl("^w\\d+$", x)
  
  instrument_detected <- reactive({
    gm <- well_genotype_for_coloring()
    if (is.null(gm) || !length(gm)) return("biorad")   # nothing yet -> show 96 by default later if you want
    
    wells <- names(gm)
    
    # Rotor-Gene if any "w###" well ids appear
    if (any(is_rotor_id(wells))) {
      # Optional override: RG_ROTOR env ("36" or "72")
      env_sz <- suppressWarnings(as.integer(Sys.getenv("RG_ROTOR", "")))
      if (!is.na(env_sz) && env_sz %in% c(36, 72)) {
        return(if (env_sz == 36) "rotor36" else "rotor72")
      }
      
      max_id <- suppressWarnings(max(as.integer(sub("^w", "", wells)), na.rm = TRUE))
      if (is.finite(max_id) && max_id > 36) return("rotor72")
      
      # Ambiguous (e.g., only w1..w10 used) ??? default to full 72 ring
      return("rotor72")
    }
    
    # Otherwise assume Bio-Rad plate
    "biorad"
  })
  

  observe({
    session$sendCustomMessage("bindSaveButton", list())
  })
  ##Reactive calue to store melting curve data
  melting_curves_data <- reactiveVal(NULL)
  
  # --- Initialize ONLY the Vertical Split.js instance once the UI is fully rendered ---
  observeEvent(session$onFlushed, {
    shinyjs::runjs("
      // 1. Vertical Split (Outer Splitter)
      Split(['#top-panel', '#bottom-panel'], {
        direction: 'vertical',
        sizes: [30, 70], // Top panel takes 30% height
        minSize: 100,    // Minimum height for each panel
        gutterSize: 10,
        cursor: 'row-resize'
      });
      console.log('Vertical Split.js instance initialized. Horizontal split is now fixed.');
    ");
  }, once = TRUE) # `once = TRUE` ensures this runs only once per session
  
  
  # Reactive value to store the selected directory path
  selected_directory <- reactiveVal(NULL)
  
  observeEvent(input$selected_folder, {
    if (!is.null(input$selected_folder) && nzchar(input$selected_folder)) {
      selected_directory(input$selected_folder)
      notify_ok(sprintf("Folder selected:\n%s", input$selected_folder), duration = 3)
    } else {
      selected_directory(NULL)
      notify_warn("No folder selected.")
    }
  })
  
  output$selected_directory_path <- renderPrint({
    cat(if (is.null(selected_directory())) "No directory selected." else selected_directory())
  })
  
  
  
  # Reactive value to store the analysis results
  analysis_output <- reactiveVal(NULL)
  
  # Reactive value to store well -> genotype mapping for coloring
  well_genotype_for_coloring <- reactiveVal(NULL)
  
  observeEvent(analysis_output(), {
    df <- analysis_output()
    if (is.null(df)) return()
    
    df$Genotype <- dplyr::case_when(
      grepl("^(pos(itive)?\\s*ctrl|control)$", trimws(df$Genotype), ignore.case = TRUE) ~ "Pos Ctrl",
      grepl("ntc|no\\s*template", df$Genotype, ignore.case = TRUE) ~ "NTC",
      TRUE ~ df$Genotype
    )
    
    well_genotypes <- df %>%
      dplyr::filter(!is.na(Well)) %>%
      dplyr::group_by(Well) %>%
      dplyr::summarise(Genotype_for_Color = dplyr::first(Genotype, na_rm = TRUE), .groups = "drop")
    
    well_genotype_for_coloring(setNames(as.character(well_genotypes$Genotype_for_Color),
                                        as.character(well_genotypes$Well)))
  })
  

  # Reactive value for status messages (now internal, not displayed as H3 in main body)
  current_status <- reactiveVal("Ready to process data. Select a folder and click 'Run Analysis'.")
  
  # Reactive value to store the currently selected well for detailed view
  selected_well_id <- reactiveVal(NULL)

  # Collect every data.frame from any nested list
  .collect_dfs <- function(x, acc = list()) {
    if (is.null(x)) return(acc)
    if (inherits(x, "data.frame")) {
      acc[[length(acc) + 1L]] <- x
      return(acc)
    }
    if (is.list(x)) {
      for (el in x) acc <- .collect_dfs(el, acc)
    }
    acc
  }
  
  extract_results_plots <- function(raw) {
    out <- list(results = list(), plots = list())
    if (is.null(raw)) return(out)
    
    # Case 1: function returned a single data.frame
    if (inherits(raw, "data.frame")) {
      out$results <- list(raw)
      return(out)
    }
    
    if (is.list(raw)) {
      
      # Named list?
      if (!is.null(names(raw))) {
        # Grab plots if present
        if (!is.null(raw$plots) && is.list(raw$plots)) out$plots <- raw$plots
        
        # If there's an explicit results slot, accept df/list/nested list
        if (!is.null(raw$results)) {
          if (inherits(raw$results, "data.frame")) {
            out$results <- list(raw$results)
          } else {
            out$results <- .collect_dfs(raw$results)
          }
          return(out)
        }
        
        # Otherwise: try common synonyms for results
        candidate_keys <- c("tables","data","dfs","frames","results_df","output")
        have <- intersect(candidate_keys, names(raw))
        if (length(have)) {
          out$results <- .collect_dfs(raw[have])
          if (length(out$results)) return(out)
        }
        
        # Last resort: scan everything except 'plots'
        scan <- raw[setdiff(names(raw), "plots")]
        out$results <- .collect_dfs(scan)
        return(out)
      }
      
      # Positional list (e.g., list(<results>, <plots>))
      if (length(raw) >= 1) {
        out$results <- .collect_dfs(raw[[1]])
      }
      if (length(raw) >= 2 && is.list(raw[[2]]) && !inherits(raw[[2]], "data.frame")) {
        out$plots <- raw[[2]]
      }
      return(out)
    }
    
    # Anything else -> empty defaults
    out
  }
  
  is_valid_results <- function(raw) {
    x <- extract_results_plots(raw)$results
    length(x) > 0 && any(vapply(x, nrow, 0L) > 0)
  }
  
  observeEvent(input$run_analysis, {
    withProgress(message = "Running Analysis...", value = 0, {
    showNotification("Starting analysis...", type = "message", duration = 2)
    analysis_output(NULL); melting_curves_data(NULL)
    
    dirpath <- selected_directory()
    if (is.null(dirpath) || !dir.exists(dirpath)) {
      fatal_modal("No data folder",
                  "Please click 'Select Data Folder' and choose a valid directory.")
      return()
    }
    
    # Try Bio-Rad first; if not valid, try Rotor-Gene
    which_engine <- "biorad"
    
    raw_cvd <- try(cvd_all_in_one__v2(dirpath), silent = TRUE)
    if (inherits(raw_cvd, "try-error") || !is_valid_results(raw_cvd)) {
      which_engine <- "rotor"
      raw_cvd <- try(cvd_all_in_one_rotor(dirpath), silent = TRUE)
    }
    
    if (inherits(raw_cvd, "try-error") || !is_valid_results(raw_cvd)) {
      showNotification("CVD analysis failed (both engines). Check files.", type = "message", duration = 6)
      raw_cvd <- NULL
    }
    
    # FMF with the matching engine
    which_engine <- "biorad"
    raw_fmf <- try(fmf_panel_v1(dirpath), silent = TRUE)
    if (inherits(raw_fmf, "try-error") || !is_valid_results(raw_fmf)){
      which_engine <- "rotor"
      raw_fmf <- try(fmf_panel_rotor(dirpath), silent = TRUE)
    }
    if (inherits(raw_fmf, "try-error")){
      showNotification("FMF analysis failed (both engines). Check files.", type = "message", duration = 6)
      raw_fmf <- NULL
    }
    
    # Normalize
    cvd_norm <- extract_results_plots(raw_cvd)
    fmf_norm <- extract_results_plots(raw_fmf)
    
    cvd_results <- cvd_norm$results
    cvd_plots   <- cvd_norm$plots
    fmf_results <- fmf_norm$results
    fmf_plots   <- fmf_norm$plots
    
    # Combine
    all_results_list <- c(cvd_results, fmf_results)
    if (length(all_results_list) == 0) {
      showNotification("No results produced.", type = "warning")
      return()
    }
    
    combined_df <- dplyr::bind_rows(all_results_list) %>%
      dplyr::mutate(
        Genotype = normalize_genotype(Genotype),
        Well     = as.character(Well)
      )
    
    analysis_output(combined_df)
    melting_curves_data(c(cvd_plots, fmf_plots))
    
    
    showNotification(sprintf("Analysis complete via %s.", if (which_engine=="biorad") "Bio-Rad" else "Rotor-Gene"),
                     type = "message", duration = 3)
    })
  })
  
  
  
  
  
  # Render status message (this output is now essentially unused in the main body)
  output$status_message <- renderText({
    current_status()
  })
  
  # Render the 96-well plate UI
  output$well_plate_ui <- renderUI({
      req(analysis_output()[[1]])                  # only draw after analysis (keeps things simple)
      gm <- well_genotype_for_coloring()
      if (is.null(gm)) gm <- setNames(character(0), character(0))
      
      instr <- instrument_detected()               # "biorad" | "rotor36" | "rotor72"
      
    
    make_legend <- function(mode=c("plate","rotor")){
      mode <- match.arg(mode)
      tagList(
        h4("Legend:"),
        div(class="legend-item", span(class=paste("legend-color-box", if (mode=="plate") "well-ntc" else "rg-ntc")), span("NTC")),
        div(class="legend-item", span(class=paste("legend-color-box", if (mode=="plate") "well-pos-ctrl" else "rg-pos")), span("Pos Ctrl")),
        div(class="legend-item", span(class=paste("legend-color-box", if (mode=="plate") "well-button-active" else "rg-sample")), span("Samples")),
        div(class="legend-item", span(class=paste("legend-color-box", if (mode=="plate") "well-button-inactive" else "rg-empty")), span("No Data"))
      )
    }
    
    # ---------- ROTOR-GENE RING (w1???wN) ----------
    if (instr %in% c("rotor36","rotor72")) {
      n <- if (instr == "rotor36") 36 else 72     # 36 or 72 (explicit)
      ids <- paste0("w", seq_len(n))     # click IDs match your analysis (w-prefixed)
      
      # SVG geometry
      cx <- 50; cy <- 50; R <- 43
      step <- 360 / n; a0 <- -90         # start at top
      rWell <- if (n == 72) 1.9 else 2.7
      
      # optional spokes (6)
      spokes <- lapply(seq(0, 300, by = 60), function(ang){
        th <- ang*pi/180
        tags$line(x1=cx, y1=cy,
                  x2=sprintf('%.2f', cx + R*cos(th)),
                  y2=sprintf('%.2f', cy + R*sin(th)),
                  class="rg-spoke")
      })
      
      wells <- lapply(seq_len(n), function(i){
        th <- (a0 + (i-1)*step)*pi/180
        x <- cx + R*cos(th); y <- cy + R*sin(th)
        id <- ids[i]                      # e.g. "w17"
        label <- sub("^w","", id)         # show "17" inside the circle
        
        klass <- "rg-well "
        gv <- safe_get(gm, id)
        
        if (!is.null(gv) && !is.na(gv) && nzchar(gv)) {
          if (grepl("ntc|no\\s*template", gv, ignore.case = TRUE)) {
            klass <- paste0(klass, "rg-ntc")
          } else if (grepl("^(pos(itive)?\\s*ctrl|positive\\s*control|control)$", gv, ignore.case = TRUE)) {
            klass <- paste0(klass, "rg-pos")
          } else {
            klass <- paste0(klass, "rg-sample")
          }
        } else {
          klass <- paste0(klass, "rg-empty")
        }
        
        
        tags$g(`data-well` = id,
               onclick = sprintf("Shiny.setInputValue('well_clicked','%s',{priority:'event'})", id),
               tags$title(id),
               tags$circle(cx=sprintf('%.2f',x), cy=sprintf('%.2f',y), r=sprintf('%.2f',rWell), class=klass),
               tags$text(x=sprintf('%.2f',x), y=sprintf('%.2f',y+0.3),
                         `text-anchor`="middle", `dominant-baseline`="middle",
                         class="rg-label", label))
      })
      
      return(
        div(class="well-plate-layout-container",
            div(class="well-plate-container",
                div(class="rotor-container",
                    tags$svg(viewBox="0 0 100 100", preserveAspectRatio="xMidYMid meet", class="rotor-svg",
                             tags$circle(cx="50", cy="50", r="47", class="rg-outline"),
                             spokes, wells)
                )
            ),
            div(class="well-plate-legend", make_legend("rotor"))
        )
      )
    }
    
    # ---------- 96-WELL GRID (A1???H12) ----------
    nrows <- 8; ncols <- 12
    corner_div <- div(class="corner","",style="grid-column:1; grid-row:1;")
    col_labels <- lapply(seq_len(ncols), function(i)
      div(class="col-label", i, style=sprintf("grid-column:%d; grid-row:1;", i+1)))
    row_labels <- lapply(seq_len(nrows), function(i)
      div(class="row-label", LETTERS[i], style=sprintf("grid-column:1; grid-row:%d;", i+1)))
    
    wells <- lapply(seq_len(nrows), function(r){
      lapply(seq_len(ncols), function(c){
        well_id <- paste0(LETTERS[r], c)
        cls <- if (well_id %in% names(gm)) {
          switch(gm[[well_id]],
                 "NTC"="well-button well-ntc",
                 "Pos Ctrl"="well-button well-pos-ctrl",
                 "well-button well-button-active")
        } else "well-button well-button-inactive"
        tags$button(
          id=paste0("well_",well_id), class=cls,
          style=sprintf("grid-column:%d; grid-row:%d;", c+1, r+1),
          onclick=sprintf("Shiny.setInputValue('well_clicked','%s',{priority:'event'})", well_id),
          tags$span(well_id)
        )
      })
    }) %>% unlist(recursive = FALSE)
    
    div(class="well-plate-layout-container",
        div(class="well-plate-container",
            div(class="plate-grid", corner_div, col_labels, row_labels, wells)
        ),
        div(class="well-plate-legend", make_legend("plate"))
    )
  })
  
  
  
  
  
  # New: Hide the headers once a well is clicked and a plot is rendered
  observeEvent(selected_well_from_click(), {
    req(selected_well_from_click()) # Only run if a well is selected
    shinyjs::hide("plot_headers")
  }, ignoreNULL = TRUE)
  
  # Reactive expression to get unique wells for the dropdown
  # Reactive expression to store the currently selected well from a click
  selected_well_from_click <- reactiveVal(NULL)
  
  # Observe the well click and store it in a reactive value
  observeEvent(input$well_clicked, {
    selected_well_from_click(input$well_clicked)
  })

  # NEW: Render a single plot that combines all parameters for the selected well
  # NEW: Render a single plot that combines all parameters for the selected well
  output$combined_well_plot <- renderPlot({
    well <- selected_well_from_click()
    pretty_well <- strip_w(well)
    req(well, analysis_output(), melting_curves_data())
    
    # Get all parameters for the selected well from the main results
    well_params <- analysis_output() %>%
      filter(Well == well) %>%
      pull(Parameter) %>%
      unique()
    
    if (length(well_params) == 0) {
      return(ggplot() + labs(title = paste("No parameters found for well:", well)))
    }
    
    # --- Combine and reshape all melting curve data for the well ---
    combined_curve_data <- lapply(well_params, function(param) {
      curve_data_wide <- melting_curves_data()[[param]]
      
      # NEW CHECK: Make sure the data exists and has rows before processing
      if (is.null(curve_data_wide) || nrow(curve_data_wide) == 0) {
        return(NULL)
      }
      
      # NEW CHECK: Ensure the well column exists in the wide data
      if (!(well %in% names(curve_data_wide))) {
        return(NULL)
      }
      
      curve_data_wide %>%
        pivot_longer(
          cols = -Temperature,
          names_to = "Well",
          values_to = "dRFU_dT"
        ) %>%
        filter(Well == well) %>%
        mutate(Parameter = param)
    })
    
    # Stack all the data frames on top of each other
    combined_data_long <- do.call(rbind, combined_curve_data)
    
    if (is.null(combined_data_long) || nrow(combined_data_long) == 0) {
      return(ggplot() + labs(title = paste("No melting curve data found for well:", well)))
    }
    
    # Define the color map
    color_map <- c(
      "FV-LEI" = "blue", "FII" = "green", "A1298C" = "purple", "PAI" = "purple",
      "HPAI" = "purple", "FV CAMB" = "purple", "FV-CAMB" = "purple", "ACE" = "purple", "APOE1" = "orange",
      "APOE2" = "purple", "C677T" = "orange", "FXIII" = "orange", "FGB" = "orange",
      "APOB" = "orange", "LTA" = "orange", 
      "E148Q" = "orange", "R761H" = "purple", 
      "F479L" = "orange", "P408Q" = "purple", 
      "V726A" = "orange", "P369S" = "purple", 
      "M694V" = "orange", "M680I" = "purple",
      "A744S" = "orange", "E167D" = "purple"
    )
    
    # Ensure all parameters have a color, using gray for any not in the map
    all_params_in_data <- unique(combined_data_long$Parameter)
    missing_colors <- setdiff(all_params_in_data, names(color_map))
    if (length(missing_colors) > 0) {
      new_colors <- setNames(rep("gray50", length(missing_colors)), missing_colors)
      color_map <- c(color_map, new_colors)
    }
    
    # --- Create the combined plot ---
    p <- ggplot(combined_data_long, aes(x = Temperature, y = dRFU_dT, color = Parameter)) +
      geom_line(size = 1) +
      labs(
        title = paste("Melting Curves for Well", pretty_well),
        x = "Temperature (B0C)",
        y = "-d(RFU)/dT"
      ) +
      scale_color_manual(values = color_map) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    # Add peak points for all parameters, but only if the required columns exist
    peak_data <- analysis_output() %>%
      filter(Well == well) %>%
      select(Parameter, matches("Peak_")) %>%
      distinct()
    
    # NEW CHECK: Only add geom_point if the necessary columns are present in peak_data
    if (nrow(peak_data) > 0 && "Peak_Temp" %in% names(peak_data) && "Peak_dRFU_dT" %in% names(peak_data)) {
      p <- p + geom_point(data = peak_data, aes(x = Peak_Temp, y = Peak_dRFU_dT, color = Parameter),
                          size = 3, shape = 4, stroke = 1.5)
    }
    
    p
  })
  
  # Observe clicks on well buttons
  observeEvent(input$well_clicked, {
    selected_well_id(input$well_clicked)
  })
  
  # Render UI for well-specific results
  output$well_specific_results_ui <- renderUI({
    well <- selected_well_id()
    all_results <- analysis_output()
    
    if (is.null(well) || is.null(all_results)) {
      return(p("Click on a well above to see its specific results."))
    }
    well_results <- all_results[all_results$Well == well, ]
    if (nrow(well_results) == 0) {
      return(div( h4(paste("Results for Well:", well)), p("No analysis data found for this well.")))
    }
    div( h4(paste("Results for Well:", strip_w(well))), DTOutput("well_detail_table"))
  })
  
  # Render detailed table for selected well
  output$well_detail_table <- renderDT({
    well <- selected_well_id()
    all_results <- analysis_output()
    if (is.null(well) || is.null(all_results)) { return(NULL) }
    
    well_results <- all_results[all_results$Well == well, ]
    
    # Keep all rows for the well, but only select the desired columns
    selected_cols <- well_results %>%
      filter(!Parameter %in% c("APOE1", "APOE2")) %>%
      select(Well, `Sample Name`, Parameter, Genotype)
    display_df <- well_results %>%
      dplyr::filter(!Parameter %in% c("APOE1", "APOE2")) %>%
      dplyr::select(Well, `Sample Name`, Parameter, Genotype)
    
    display_df$Well <- strip_w(display_df$Well)
    
    DT::datatable(display_df, options = list(pageLength = 5, dom = 'tip'), rownames = FALSE)
  })
  
  # Render tabs for general parameter results
  output$results_tabs <- renderUI({
    req(analysis_output())
    res <- analysis_output()
    
    # Filter out APOE1/APOE2 as before
    filtered_res <- res[ ! res$Parameter %in% c("APOE1","APOE2"), ]
    
    # Your exact desired order
    desired_order <- c(
      "FV-LEI", "FII", "A1298C", "C677T", "PAI", "FXIII", "HPAI",
      "FGB", "FV CAMB", "APOB", "ACE", "LTA", "APOE",
      "E148Q", "R761H", "F479L", "P408Q", "V726A",
      "P369S", "M694V", "M680I", "A744S", "E167D"
    )
    
    # Only keep those that actually appear in your results
    params_to_show <- intersect(desired_order, unique(filtered_res$Parameter))
    
    # Build one tabPanel per parameter
    tabs <- lapply(params_to_show, function(param) {
      tabPanel(
        title = param,
        DTOutput(paste0("table_", param))
      )
    })
    
    do.call(tabsetPanel, tabs)
  })
  
  # After your renderUI for results_tabs:
  
  observe({
    # Grab the same filtered results and ordering logic
    req(analysis_output())
    res <- analysis_output()
    filtered_res <- res[ ! res$Parameter %in% c("APOE1","APOE2"), ]
    desired_order <- c(
      "FV-LEI","FII","A1298C","C677T","PAI","FXIII","HPAI",
      "FGB","FV-CAMB","APOB","ACE","LTA","APOE",
      "E148Q","R761H","F479L","P408Q","V726A",
      "P369S","M694V","M680I","A744S","E167D"
    )
    params_to_show <- intersect(desired_order, unique(filtered_res$Parameter))
    
    # For each parameter, create a renderDT
    for (param in params_to_show) {
      local({    # capture correct `param` in the loop
        p <- param
        output_id <- paste0("table_", p)
        
        output[[output_id]] <- DT::renderDT({
          df <- filtered_res[filtered_res$Parameter == p, ]
          # select the columns you want displayed
          display_df <- df[, c("Well", "Sample Name", "Parameter", "Genotype"), drop = FALSE]
          display_df$Well <- strip_w(display_df$Well)
          DT::datatable(
            display_df,
            options = list(pageLength = 10, dom = 'tip'),
            rownames = FALSE
          )
        })
      })
    }
  })
  
  
  observeEvent(input$save_analysis_path, {
    req(analysis_output())
    path <- input$save_analysis_path
    if (is.null(path) || path == "") return()
    
    ext <- tools::file_ext(path)
    if (ext != "xlsx") {
      showNotification("Please save as a .xlsx file.", type = "error")
      return()
    }
    
    # Use a display copy where rotor wells are shown as numbers only
    all_results <- analysis_output()
    write_df <- all_results %>%
      dplyr::mutate(Well = as.character(display_well(Well)))
    
    # ---- Summary sheet (unchanged logic, just using write_df) ----
    summary_data <- write_df %>%
      dplyr::filter(!Genotype %in% c("Pos Ctrl", "NTC")) %>%
      dplyr::filter(!Parameter %in% c("APOE1", "APOE2")) %>%
      dplyr::distinct(`Sample Name`, Parameter, .keep_all = TRUE) %>%
      tidyr::pivot_wider(
        id_cols = `Sample Name`,
        names_from = Parameter,
        values_from = Genotype
      )
    
    wb <- openxlsx::createWorkbook()
    addWorksheet(wb, "Summary Table")
    writeData(wb, "Summary Table", summary_data, withFilter = FALSE)
    
    header_style <- createStyle(textDecoration = "Bold", fgFill = "#DCE6F1")
    odd_row_style <- createStyle(fgFill = "#F2F2F2")
    even_row_style <- createStyle(fgFill = "#FFFFFF")
    
    addStyle(wb, "Summary Table", header_style, rows = 1, cols = 1:ncol(summary_data), gridExpand = TRUE)
    
    if (nrow(summary_data) >= 1) {
      odd_rows <- seq(2, nrow(summary_data) + 1, 2)
      addStyle(wb, "Summary Table", odd_row_style, rows = odd_rows, cols = 1:ncol(summary_data), gridExpand = TRUE)
    }
    if (nrow(summary_data) >= 2) {
      even_rows <- seq(3, nrow(summary_data) + 1, 2)
      addStyle(wb, "Summary Table", even_row_style, rows = even_rows, cols = 1:ncol(summary_data), gridExpand = TRUE)
    }
    setColWidths(wb, "Summary Table", cols = 1:ncol(summary_data), widths = "auto")
    
    # ---- One sheet per parameter (Well column uses numbers for rotor) ----
    parameters <- setdiff(unique(write_df$Parameter), c("APOE1", "APOE2"))
    
    for (p in parameters) {
      addWorksheet(wb, p)
      parameter_data <- write_df %>%
        dplyr::filter(Parameter == p) %>%
        dplyr::select(Well, `Sample Name`, Parameter, Genotype)
      writeData(wb, p, parameter_data, withFilter = FALSE)
      addStyle(wb, p, header_style, rows = 1, cols = 1:ncol(parameter_data), gridExpand = TRUE)
      if (nrow(parameter_data)) {
        addStyle(wb, p, odd_row_style, rows = seq(2, nrow(parameter_data) + 1, 2), cols = 1:ncol(parameter_data), gridExpand = TRUE)
        addStyle(wb, p, even_row_style, rows = seq(3, nrow(parameter_data) + 1, 2), cols = 1:ncol(parameter_data), gridExpand = TRUE)
      }
      setColWidths(wb, p, cols = 1:ncol(parameter_data), widths = "auto")
    }
    
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    showNotification(paste("Excel saved to", path), type = "message")
  })
  

  
}

shinyApp(ui = ui, server = server)
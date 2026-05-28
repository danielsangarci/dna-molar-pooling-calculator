
# --- 1. Auto-Install Missing Packages ---
required_packages <- c("shiny", "bslib", "dplyr", "DT", "openxlsx")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    warning(paste("Installing missing package:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(openxlsx)

# --- Auxiliary Functions ---
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Function to convert a vector into a 96-well plate structure (by columns)
format_plate <- function(vec) {
  cols <- max(12, ceiling(length(vec) / 8))
  mat <- matrix(NA, nrow = 8, ncol = cols)
  mat[1:length(vec)] <- vec
  df <- as.data.frame(mat)
  rownames(df) <- LETTERS[1:8]
  colnames(df) <- as.character(1:cols) 
  return(df)
}

# Algoritmo de "Water-filling" para repartir el volumen disponible
allocate_volumes <- function(v_req, ratios, v_total) {
  n <- length(v_req)
  v_out <- numeric(n)
  
  # Si todos tienen concentración 0 (se traduce en volumen infinito requerido)
  if (all(is.infinite(v_req) | is.na(v_req))) {
    return((ratios / sum(ratios)) * v_total)
  }
  
  # Si la suma de lo requerido cabe perfectamente en el volumen total
  if (sum(v_req, na.rm = TRUE) <= (v_total + 1e-6)) {
    return(v_req)
  }
  
  # Reparto iterativo para maximizar el uso del pocillo manteniendo proporciones
  v_rem <- v_total
  active <- 1:n
  
  while(length(active) > 0 && v_rem > 1e-6) {
    current_ratios <- ratios[active]
    # Volumen que le tocaría a cada uno en esta iteración
    shares <- (current_ratios / sum(current_ratios)) * v_rem
    
    # ¿Quiénes necesitan MENOS (o igual) de lo que les tocaría en el reparto?
    satisfied <- active[v_req[active] <= (shares + 1e-6)]
    
    if (length(satisfied) > 0) {
      # Se les asigna exactamente lo que necesitan para llegar a sus moles
      v_out[satisfied] <- v_req[satisfied]
      v_rem <- v_rem - sum(v_req[satisfied])
      # Se sacan de la lista de activos
      active <- active[!active %in% satisfied]
    } else {
      # Ninguno está satisfecho. Se reparte el volumen sobrante según proporciones y fin.
      v_out[active] <- shares
      v_rem <- 0
      active <- integer(0)
    }
  }
  return(v_out)
}


# --- UI: User Interface ---
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  tags$head(
    tags$style(HTML("
      /* General Layout */
      .sample-row { background-color: #f8f9fa; border-radius: 5px; padding: 15px; margin-bottom: 10px; border-left: 5px solid #78c2ad; }
      .batch-header { background-color: #e9ecef; padding: 10px; border-radius: 5px; margin-bottom: 15px; border-left: 5px solid #78c2ad; }
      input[type=number] { font-weight: bold; }
      .table th, .table td { text-align: center; vertical-align: middle; }
    "))
  ),
  
  titlePanel("DNA Molar Pooling Calculator"),
  
  navset_card_pill(
    
    # ===============================================
    # TAB 1: SINGLE MIX (Interactive)
    # ===============================================
    nav_panel("Single Mix (Interactive)",
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  h4("1. Protocol Definition"),
                  helpText("Standard reaction requirements."),
                  numericInput("s_proto_fmol", "Target fmol:", value = 200, step = 10),
                  numericInput("s_proto_vol", "Target Vol (µL):", value = 11.5, step = 0.5),
                  hr(),
                  h4("2. Scale (Prep Volume)"),
                  checkboxInput("s_use_scale", "Custom preparation volume (Scale)", value = FALSE),
                  conditionalPanel(
                    condition = "input.s_use_scale == true",
                    numericInput("s_prep_vol", "Total Vol to Prepare (µL):", value = 11.5, step = 0.5)
                  ),
                  textOutput("s_scale_text"),
                  hr(),
                  h4("3. Mix Structure"),
                  numericInput("s_num", "Amplicons per sample to mix:", value = 2, min = 1, step = 1),
                  hr(),
                  actionButton("s_calc", "Calculate", class = "btn-primary w-100", icon = icon("calculator"))
                ),
                mainPanel(
                  width = 9,
                  card(card_header("Amplicon Definitions"), card_body(uiOutput("s_dynamic_inputs"))),
                  br(),
                  card(card_header("Pipetting Protocol"), 
                       card_body(
                         uiOutput("s_status"), 
                         tableOutput("s_table"),
                         helpText("Note: Yellow cells indicate the volume to pipette.")
                       ))
                )
              )
    ),
    
    # ===============================================
    # TAB 2: BATCH PROCESSING (Excel Paste)
    # ===============================================
    nav_panel("Batch Processing (Excel Paste)",
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  h4("1. Protocol Definition"),
                  helpText("Standard reaction requirements."),
                  numericInput("b_proto_fmol", "Target fmol:", value = 200, step = 10),
                  numericInput("b_proto_vol", "Target Vol (µL):", value = 11.5, step = 0.5),
                  hr(),
                  h4("2. Scale (Prep Volume)"),
                  checkboxInput("b_use_scale", "Custom preparation volume (Scale)", value = FALSE),
                  conditionalPanel(
                    condition = "input.b_use_scale == true",
                    numericInput("b_prep_vol", "Total Vol to Prepare (µL):", value = 11.5, step = 0.5)
                  ),
                  textOutput("b_scale_text"),
                  hr(),
                  h4("3. Mix Structure"),
                  numericInput("b_num_amplicons", "Amplicons per sample to mix:", value = 2, min = 1),
                  hr(),
                  actionButton("b_calc", "Calculate Batch", class = "btn-primary w-100", icon = icon("table"))
                ),
                mainPanel(
                  width = 9,
                  # Hidden Shiny download button triggered via JS
                  div(style = "visibility: hidden; position: absolute; height: 0px; width: 0px;", 
                      downloadButton("b_download_excel", "Download")),
                  
                  card(
                    class = "batch-header",
                    h4("Step 1: Define Amplicon Constants"),
                    p("Enter the Size and Mix Ratio for each amplicon column. (Applies to all samples)"),
                    uiOutput("b_amplicon_definitions")
                  ),
                  card(
                    card_header("Step 2: Paste Concentrations"),
                    textAreaInput("b_paste_data", "Paste from Excel", height = "150px", 
                                  placeholder = "Paste Format:\nSampleName  Conc1  Conc2\nSample01    50.5   45.2\nControl_Neg 0.0    0.0"),
                    helpText("Format: Column 1 = Sample Name, Column 2+ = Concentrations.")
                  ),
                  br(),
                  card(
                    card_header("Batch Protocol"),
                    uiOutput("b_status"),
                    DTOutput("b_table")
                  )
                )
              )
    )
  )
)

# --- Server: Logic ---
server <- function(input, output) {
  
  # =========================================
  # LOGIC FOR TAB 1: SINGLE MIX
  # =========================================
  s_actual_vol <- reactive({
    if (isTRUE(input$s_use_scale)) input$s_prep_vol else input$s_proto_vol
  })
  
  output$s_scale_text <- renderText({
    req(input$s_proto_vol)
    paste0("Scaling Factor: ", round(s_actual_vol() / input$s_proto_vol, 2), "x")
  })
  
  output$s_dynamic_inputs <- renderUI({
    req(input$s_num)
    if(input$s_num < 1) return(NULL)
    lapply(1:input$s_num, function(i) {
      div(class = "sample-row",
          fluidRow(
            column(2, h5(paste("Amplicon", i), style="margin-top:30px; font-weight:bold;")),
            column(3, numericInput(paste0("s_size_", i), "Size (bp)", value = 1500, step = 50)),
            column(3, numericInput(paste0("s_conc_", i), "DNA Concentration (ng/µL)", value = 50, step = 0.1)),
            column(3, numericInput(paste0("s_prop_", i), "Ratio", value = 1, min = 0.1, step = 0.1))
          ))
    })
  })
  
  observeEvent(input$s_calc, {
    req(input$s_proto_fmol, input$s_proto_vol)
    n <- input$s_num
    
    prep_vol <- s_actual_vol()
    scale <- prep_vol / input$s_proto_vol
    total_fmol <- input$s_proto_fmol * scale
    
    sizes <- numeric(n); concs <- numeric(n); props <- numeric(n)
    names <- paste("Amplicon", 1:n)
    
    for(i in 1:n) {
      sizes[i] <- input[[paste0("s_size_", i)]] %||% 1500
      concs[i] <- input[[paste0("s_conc_", i)]] %||% 50
      props[i] <- input[[paste0("s_prop_", i)]] %||% 1
    }
    
    fmol_parts <- (props / sum(props)) * total_fmol
    ng_req <- (fmol_parts * sizes * 650) / 1000000
    
    # Calcular volumen teorico (Infinito si conc es 0)
    vols_req_raw <- ifelse(concs == 0, Inf, ng_req / concs)
    
    # Llamar al algoritmo de optimizacion
    vols <- allocate_volumes(vols_req_raw, props, prep_vol)
    vol_dna_total <- sum(vols)
    
    status_msg <- "✓ Ready to Pipette"
    status_class <- "alert alert-success"
    
    if (sum(vols_req_raw) > prep_vol) {
      status_msg <- paste("⚠ LOW Concentration! Volumes optimized and expanded to fill", prep_vol, "µL prioritizing ratios.")
      status_class <- "alert alert-warning"
      water <- 0
    } else {
      water <- prep_vol - vol_dna_total
    }
    
    df <- data.frame("Component"=names, "Size"=sizes, "Ratio"=props, 
                     "Target_fmol"=round(fmol_parts,1), 
                     "Target_Mass_ng"=round(ng_req,1), "Vol_uL"=round(vols,2))
    
    df_water <- data.frame("Component"="Water", "Size"=NA, "Ratio"=NA, 
                           "Target_fmol"=NA, "Target_Mass_ng"=NA, 
                           "Vol_uL"=round(max(0, water), 2))
    
    df_total <- data.frame("Component"="TOTAL", "Size"=NA, "Ratio"=NA, 
                           "Target_fmol"=round(sum(fmol_parts),1), 
                           "Target_Mass_ng"=round(sum(ng_req),1), 
                           "Vol_uL"=round(vol_dna_total + water, 2))
    
    final <- rbind(df, df_water, df_total)
    
    final$Vol_uL <- as.character(final$Vol_uL)
    rows_hl <- 1:(nrow(final)-1)
    final$Vol_uL[rows_hl] <- paste0("<div style='background-color:#ffeb3b; font-weight:bold; padding:5px; border-radius:4px;'>", final$Vol_uL[rows_hl], "</div>")
    
    output$s_status <- renderUI(div(class=status_class, status_msg))
    output$s_table <- renderTable(final, striped=T, bordered=T, na="-", sanitize.text.function=identity)
  })
  
  # =========================================
  # LOGIC FOR TAB 2: BATCH PROCESSING
  # =========================================
  
  b_actual_vol <- reactive({
    if (isTRUE(input$b_use_scale)) input$b_prep_vol else input$b_proto_vol
  })
  
  output$b_scale_text <- renderText({
    req(input$b_proto_vol)
    paste0("Scaling Factor: ", round(b_actual_vol() / input$b_proto_vol, 2), "x")
  })
  
  output$b_amplicon_definitions <- renderUI({
    req(input$b_num_amplicons)
    num <- input$b_num_amplicons
    fluidRow(
      lapply(1:num, function(i) {
        column(width = max(2, floor(12/num)), 
               div(style="background: white; padding: 10px; border-radius: 5px; border: 1px solid #ccc;",
                   h5(paste("Amplicon", i), style="color: #78c2ad;"),
                   numericInput(paste0("b_size_", i), "Size (bp)", value = 1500, step=50),
                   numericInput(paste0("b_ratio_", i), "Mix Ratio", value = 1, step=0.1)
               )
        )
      })
    )
  })
  
  batch_results <- reactiveVal(NULL)
  
  observeEvent(input$b_calc, {
    req(input$b_paste_data, input$b_proto_fmol, input$b_proto_vol)
    
    prep_vol <- b_actual_vol()
    scale <- prep_vol / input$b_proto_vol
    total_fmol <- input$b_proto_fmol * scale
    
    num_amps <- input$b_num_amplicons
    sizes <- numeric(num_amps)
    ratios <- numeric(num_amps)
    
    for(i in 1:num_amps) {
      sizes[i] <- input[[paste0("b_size_", i)]] %||% 1500
      ratios[i] <- input[[paste0("b_ratio_", i)]] %||% 1
    }
    
    total_ratio <- sum(ratios)
    target_fmols <- (ratios / total_ratio) * total_fmol
    req_ng <- (target_fmols * sizes * 650) / 1000000
    
    raw <- input$b_paste_data
    lines <- unlist(strsplit(raw, "[\n\r]+"))
    lines <- lines[lines != ""]
    
    results_list <- list()
    idx <- 1 
    
    for(line in lines) {
      parts <- unlist(strsplit(line, "[ \t,]+"))
      parts <- parts[parts != ""]
      
      if(length(parts) < (num_amps + 1)) next
      
      s_name <- parts[1]
      s_concs <- as.numeric(parts[2:(num_amps+1)])
      
      # Calcular volumen teórico requerido
      vols_req_raw <- ifelse(s_concs == 0 | is.na(s_concs), Inf, req_ng / s_concs)
      
      # Llamar al algoritmo de reparto
      vols <- allocate_volumes(vols_req_raw, ratios, prep_vol)
      vol_dna_total <- sum(vols)
      
      if (sum(vols_req_raw, na.rm = TRUE) > prep_vol) {
        status <- "LOW"
        water <- 0
      } else {
        status <- "OK"
        water <- prep_vol - vol_dna_total
      }
      
      row_df <- data.frame("Sample" = s_name, stringsAsFactors = FALSE)
      
      for(k in 1:num_amps) {
        row_df[[paste0("Vol_Amp_", k)]] <- round(vols[k], 2)
      }
      
      row_df[["Water_uL"]] <- round(water, 2)
      row_df[["Total_Vol"]] <- round(vol_dna_total + water, 2)
      
      row_idx <- ((idx - 1) %% 8) + 1
      col_idx <- ((idx - 1) %/% 8) + 1
      well_pos <- paste0(LETTERS[row_idx], col_idx)
      
      row_df[["Well"]] <- well_pos
      row_df[["Status"]] <- status
      
      results_list[[length(results_list)+1]] <- row_df
      idx <- idx + 1
    }
    
    if(length(results_list) == 0) {
      output$b_status <- renderUI(div(class="alert alert-warning", "No valid data parsed. Check format."))
      output$b_table <- renderDT(NULL)
      batch_results(NULL)
      return()
    }
    
    final_df <- do.call(rbind, results_list)
    batch_results(final_df) 
    
    n_low <- sum(final_df$Status == "LOW")
    
    output$b_status <- renderUI({
      if(n_low > 0) {
        div(class = "alert alert-warning", role="alert",
            HTML(paste0("<strong>Batch Calculated: ", nrow(final_df), " samples.</strong><br/>",
                        "<span style='color:#b02a37; font-weight:bold;'>⚠ Be careful: ", n_low, " sample(s) have too low DNA concentration! Their volumes have been optimized to maximize target molecules by re-allocating space from high-concentration amplicons to low ones.</span>"))
        )
      } else {
        div(class = "alert alert-success", role="alert",
            paste0("Batch Calculated: ", nrow(final_df), " samples. All checks passed.")
        )
      }
    })
    
    output$b_table <- renderDT({
      datatable(final_df, 
                extensions = 'Buttons',
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left; color: #666; font-style: italic;',
                  'Rows marked in yellow indicate samples where space was fully optimized due to low concentration.'
                ),
                class = 'table table-bordered', 
                options = list(
                  dom = 'Bfrtip',
                  buttons = list(
                    'copy', 
                    'csv',
                    list(
                      extend = 'collection',
                      text = 'Excel',
                      action = DT::JS("function(e, dt, node, config) {
                        var btn = document.getElementById('b_download_excel');
                        if (btn) { btn.click(); }
                      }")
                    )
                  ),
                  pageLength = 24,
                  autoWidth = TRUE
                ),
                rownames = FALSE
      ) %>%
        formatStyle(
          'Status',
          target = 'row',
          backgroundColor = styleEqual("LOW", "#fff3cd"), 
          color = styleEqual("LOW", "#856404")            
        )
    }, server = FALSE)
  })
  
  # Excel Creation Logic
  output$b_download_excel <- downloadHandler(
    filename = function() {
      paste0("Molar_Pooling_Plates_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df <- batch_results()
      req(df)
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Summary Table")
      writeData(wb, "Summary Table", df)
      
      sheet_plates <- "Plates Layout"
      addWorksheet(wb, sheet_plates)
      pageSetup(wb, sheet_plates, paperSize = 9, orientation = "portrait", fitToWidth = TRUE)
      
      current_row <- 1
      
      write_plate <- function(wb, sheet_name, title_text, data_vec, start_row, bg_color) {
        title_style <- createStyle(textDecoration = "bold", fontSize = 12, fontColour = "#000000")
        border_style <- createStyle(border = "TopBottomLeftRight", borderColour = "#000000", borderStyle = "thin", halign = "center", valign = "center")
        header_style <- createStyle(textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#000000", borderStyle = "thin", halign = "center", fgFill = bg_color)
        
        writeData(wb, sheet_name, title_text, startRow = start_row, startCol = 1)
        addStyle(wb, sheet_name, style = title_style, rows = start_row, cols = 1)
        
        plate_df <- format_plate(data_vec)
        writeData(wb, sheet_name, plate_df, startRow = start_row + 1, rowNames = TRUE, borders = "none")
        
        n_cols <- ncol(plate_df) + 1 
        n_rows <- 9 
        
        addStyle(wb, sheet_name, style = border_style, rows = (start_row + 2):(start_row + n_rows), cols = 1:n_cols, gridExpand = TRUE)
        addStyle(wb, sheet_name, style = header_style, rows = start_row + 1, cols = 1:n_cols, gridExpand = TRUE)
        setColWidths(wb, sheet_name, cols = 1:n_cols, widths = 8)
        
        return(start_row + 12) 
      }
      
      current_row <- write_plate(wb, sheet_plates, "1. Samples Layout", df$Sample, current_row, bg_color = "#f0f0f0")
      current_row <- write_plate(wb, sheet_plates, "2. Water Volume (µL)", df$Water_uL, current_row, bg_color = "#d9d9d9")
      
      amp_colors <- c("#fff2cc", "#ddebf7", "#e2efda", "#fce4d6", "#e4dfec", "#ffebf0", "#e5ecec", "#fff7e6")
      num_amps <- input$b_num_amplicons
      
      for(k in 1:num_amps) {
        col_k <- if(k <= length(amp_colors)) amp_colors[k] else "#ffffff"
        current_row <- write_plate(wb, sheet_plates, paste0("3.", k, " Amplicon ", k, " Vol (µL)"), df[[paste0("Vol_Amp_", k)]], current_row, bg_color = col_k)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)


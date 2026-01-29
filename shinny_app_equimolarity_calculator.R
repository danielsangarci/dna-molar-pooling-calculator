# --- 1. Auto-Install Missing Packages ---
required_packages <- c("shiny", "bslib", "dplyr", "DT")
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

# --- UI: User Interface ---
ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  tags$head(
    tags$style(HTML("
      /* General Layout */
      .sample-row { background-color: #f8f9fa; border-radius: 5px; padding: 15px; margin-bottom: 10px; border-left: 5px solid #78c2ad; }
      
      /* CHANGED: Swapped Purple (#6f42c1) for Minty Green (#78c2ad) */
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
                  helpText("Actual volume you want to make."),
                  numericInput("s_prep_vol", "Total Vol to Prepare (µL):", value = 11.5, step = 0.5),
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
                  helpText("Actual volume you want to make."),
                  numericInput("b_prep_vol", "Total Vol to Prepare (µL):", value = 11.5, step = 0.5),
                  textOutput("b_scale_text"),
                  hr(),
                  h4("3. Mix Structure"),
                  numericInput("b_num_amplicons", "Amplicons per sample to mix:", value = 2, min = 1),
                  hr(),
                  actionButton("b_calc", "Calculate Batch", class = "btn-primary w-100", icon = icon("table"))
                ),
                mainPanel(
                  width = 9,
                  card(
                    class = "batch-header",
                    h4("Step 1: Define Amplicon Constants"),
                    p("Enter the Size and Mix Ratio for each amplicon column. (Applies to all samples)"),
                    uiOutput("b_amplicon_definitions")
                  ),
                  card(
                    card_header("Step 2: Paste Concentrations"),
                    textAreaInput("b_paste_data", "Paste from Excel", height = "150px", 
                                  placeholder = "Paste Format:\nSampleName  Conc1  Conc2\nSample01    50.5   45.2\nSample02    5.0    4.0"),
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
  output$s_scale_text <- renderText({
    req(input$s_proto_vol, input$s_prep_vol)
    paste0("Scaling Factor: ", round(input$s_prep_vol / input$s_proto_vol, 2), "x")
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
    req(input$s_proto_fmol, input$s_prep_vol, input$s_proto_vol)
    n <- input$s_num
    
    scale <- input$s_prep_vol / input$s_proto_vol
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
    vols <- ng_req / concs
    water <- input$s_prep_vol - sum(vols)
    
    df <- data.frame("Component"=names, "Size"=sizes, "Ratio"=props, 
                     "Target_fmol"=round(fmol_parts,1), 
                     "Mass_ng"=round(ng_req,1), "Vol_uL"=round(vols,2))
    
    df_water <- data.frame("Component"="Water", "Size"=NA, "Ratio"=NA, 
                           "Target_fmol"=NA, "Mass_ng"=NA, 
                           "Vol_uL"=round(max(0, water),2))
    
    df_total <- data.frame("Component"="TOTAL", "Size"=NA, "Ratio"=NA, 
                           "Target_fmol"=round(sum(fmol_parts),1), 
                           "Mass_ng"=round(sum(ng_req),1), 
                           "Vol_uL"=round(ifelse(water>=0, input$s_prep_vol, sum(vols)), 2))
    
    final <- rbind(df, df_water, df_total)
    
    final$Vol_uL <- as.character(final$Vol_uL)
    rows_hl <- 1:(nrow(final)-1)
    final$Vol_uL[rows_hl] <- paste0("<div style='background-color:#ffeb3b; font-weight:bold; padding:5px; border-radius:4px;'>", final$Vol_uL[rows_hl], "</div>")
    
    output$s_status <- renderUI({
      if(water >= 0) div(class="alert alert-success", "✓ Ready to Pipette")
      else div(class="alert alert-danger", paste("⚠ Low Concentration! Need", round(sum(vols),2), "µL"))
    })
    output$s_table <- renderTable(final, striped=T, bordered=T, na="-", sanitize.text.function=identity)
  })
  
  # =========================================
  # LOGIC FOR TAB 2: BATCH PROCESSING
  # =========================================
  
  output$b_scale_text <- renderText({
    req(input$b_proto_vol, input$b_prep_vol)
    paste0("Scaling Factor: ", round(input$b_prep_vol / input$b_proto_vol, 2), "x")
  })
  
  output$b_amplicon_definitions <- renderUI({
    req(input$b_num_amplicons)
    num <- input$b_num_amplicons
    fluidRow(
      lapply(1:num, function(i) {
        column(width = max(2, floor(12/num)), 
               div(style="background: white; padding: 10px; border-radius: 5px; border: 1px solid #ccc;",
                   # CHANGED: Replaced Purple color with Minty Green (#78c2ad)
                   h5(paste("Amplicon", i), style="color: #78c2ad;"),
                   numericInput(paste0("b_size_", i), "Size (bp)", value = 1500, step=50),
                   numericInput(paste0("b_ratio_", i), "Mix Ratio", value = 1, step=0.1)
               )
        )
      })
    )
  })
  
  observeEvent(input$b_calc, {
    req(input$b_paste_data, input$b_proto_fmol, input$b_proto_vol, input$b_prep_vol)
    
    scale <- input$b_prep_vol / input$b_proto_vol
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
    
    for(line in lines) {
      parts <- unlist(strsplit(line, "[ \t,]+"))
      parts <- parts[parts != ""]
      
      if(length(parts) < (num_amps + 1)) next
      
      s_name <- parts[1]
      s_concs <- as.numeric(parts[2:(num_amps+1)])
      
      vols <- req_ng / s_concs
      vol_dna_total <- sum(vols)
      water <- input$b_prep_vol - vol_dna_total
      
      status <- ifelse(water >= -0.01, "OK", "LOW")
      
      row_df <- data.frame("Sample" = s_name, stringsAsFactors = FALSE)
      
      for(k in 1:num_amps) {
        row_df[[paste("Vol_Amp", k)]] <- round(vols[k], 2)
      }
      
      row_df[["Water_uL"]] <- round(max(0, water), 2)
      row_df[["Total_Vol"]] <- round(ifelse(water>=0, input$b_prep_vol, vol_dna_total), 2)
      row_df[["Status"]] <- status
      
      results_list[[length(results_list)+1]] <- row_df
    }
    
    if(length(results_list) == 0) {
      output$b_status <- renderUI(div(class="alert alert-warning", "No valid data parsed. Check format."))
      output$b_table <- renderDT(NULL)
      return()
    }
    
    final_df <- do.call(rbind, results_list)
    
    n_low <- sum(final_df$Status == "LOW")
    
    output$b_status <- renderUI({
      if(n_low > 0) {
        div(class = "alert alert-warning", role="alert",
            HTML(paste0("<strong>Batch Calculated: ", nrow(final_df), " samples.</strong><br/>",
                        "<span style='color:#b02a37; font-weight:bold;'>⚠ Be careful: ", n_low, " sample(s) have too low DNA concentration! Check rows with LOW status.</span>"))
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
                  'Rows marked in red indicate the sample concentration is too low.'
                ),
                class = 'table table-bordered', 
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel'),
                  pageLength = 24,
                  autoWidth = TRUE
                ),
                rownames = FALSE
      ) %>%
        formatStyle(
          'Status',
          target = 'row',
          backgroundColor = styleEqual("LOW", "#ffcccc"), 
          color = styleEqual("LOW", "#900000")            
        )
    }, server = FALSE)
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui, server)
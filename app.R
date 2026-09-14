library(shiny)
library(readxl)
library(writexl)
library(DT)

# Workaround for Chromium Issue
myDownloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# =========================
# UTILS
# =========================

safe_id <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

ensure_col <- function(df, col, default = "") {
  if (!col %in% names(df)) df[[col]] <- rep(default, nrow(df))
  df[[col]][is.na(df[[col]])] <- default
  df
}

ensure_bin_cols <- function(df, keys) {
  for (k in keys) {
    id <- safe_id(k)
    
    if (!id %in% names(df)) {
      df[[id]] <- rep(0, nrow(df))
    } else {
      df[[id]] <- as.numeric(df[[id]])
      df[[id]][is.na(df[[id]])] <- 0
    }
  }
  df
}

# =========================
# MAPPE (SOLO LABEL UI)
# =========================

INTERAZIONALI <- c(
  "Presa di turno",
  "Richiesta di accordo/conferma",
  "Accordo",
  "Conferma dell'attenzione",
  "Interruzione",
  "Cessione del turno",
  "Marcatura della conoscenza condivisa",
  "Richiesta di attenzione",
  "Strategia di cortesia"
)

METATESTUALI <- c(
  "Introduzione o ripresa del topic",
  "Chiusura del topic",
  "Prolettico",
  "Riformulazione",
  "Citazione/Discorso Riportato",
  "Esemplificazione",
  "Filler",
  "Marca di generalizzazione"
)

COGNITIVE <- c(
  "Marcatura dell'inferenza",
  "Modulazione del grado di confidenza",
  "Approssimazione",
  "Specificazione",
  "Attenuazione",
  "Intensificazione"
)

ALL_KEYS <- c(
  INTERAZIONALI,
  METATESTUALI,
  COGNITIVE
)

# =========================
# DEFINIZIONI
# =========================

DEFINIZIONI <- list(
  "Presa di turno" = 'Segnala che un/a parlante sta iniziando il proprio turno. Es: "allora…", "dunque…"',
  "Interruzione" = 'Serve ad interrompere un/a parlante per iniziare il proprio turno. Es: "scusa...", "no…", "però…"',
  "Cessione del turno" = "È usato dal/la parlante per richiedere risposta all'interlocutore/trice o per chiudere il proprio turno. Es: \"prego\", \"va bene?\", \"no?\"",
  "Marcatura della conoscenza condivisa" = "È usato dal/la parlante per riconoscere o per assicurarsi che l'altra persona è a conoscenza di una certa informazione. Es: \"sai\", \"non so se mi spiego\"",
  "Richiesta di accordo/conferma" = 'Serve al/la parlante per chiedere all\'interlocutore/trice una conferma, di attenzione o di accordo. Es: "capito?", "no?"',
  "Accordo" = 'Serve al/la parlante per segnalare all\'interlocutore/trice che è d\'accordo con quanto detto prima. Es: "sìsì", "eh direi!"',
  "Richiesta di attenzione" = 'Serve al/la parlante per richiamare l\'attenzione su ciò che sta per dire. Es: "senti...", "guarda..."',
  "Conferma dell'attenzione" = 'Segnala che un/a parlante segue ancora il discorso e/o incoraggia l\'interlocutore/trice a continuare. Es: "mhmh", "certo"',
  "Strategia di cortesia" = 'Serve al/la parlante come strategia di mantenimento della faccia. Es: "gentilmente", "scusa"',
  
  "Introduzione o ripresa del topic" = 'È usato dal/la parlante per introdurre o riprendere un topic. Es: "quindi…", "dopodiché..."',
  "Chiusura del topic" = 'È usato dal/la parlante per chiudere un argomento. Es: "ok", "bene", "bon"',
  "Prolettico" = 'Serve al/la parlante per segnalare una formulazione che ritiene adeguata di ciò che sta per dire. Es: "ecco…", "direi…"',
  "Riformulazione" = 'Viene usato per riformulare quanto appena detto, che sia per ragioni stilistiche, di precisione o di cortesia. Es: "cioè nel senso...", "dico..."',
  "Citazione/Discorso Riportato" = 'È usato per introdurre qualcosa detto da qualcun altro/a. Es: "mi fa…", "ah…"',
  "Esemplificazione" = 'Introduce un esempio o parafrasa rispetto a un elemento presente nel testo. Es: "mettiamo", "tipo"',
  "Filler" = 'Viene usato dal/la parlante come pausa piena per coprire i tempi di formulazione. Es: "quindi direi tipo", "mh diciamo"',
  "Marca di generalizzazione" = 'È un modo di rimandare a una categoria più ampia che il/la parlante dà per condivisa. Es: "eccetera", "capito no"',
  
  "Marcatura dell'inferenza" = 'Serve per segnalare che il/la parlante è arrivato/a a una conclusione per deduzione. Es: "allora", "quindi", "insomma"',
  "Modulazione del grado di confidenza" = 'Serve per segnalare che il/la parlante non è completamente sicuro/a di ciò che sostiene. Es: "mi pare", "credo"',
  "Approssimazione" = 'È usato per rendere più vago quello che si dice. Es: "diciamo", "tipo"',
  "Specificazione" = 'È usato per rendere meno vago quello che si dice. Es: "proprio", "letteralmente"',
  "Attenuazione" = 'Serve al/la parlante per mitigare la portata di un certo termine o espressione. Es: "direi", "tipo"',
  "Intensificazione" = 'Serve al/la parlante per intensificare un certo termine o espressione. Es: "decisamente", "assolutamente"'
)

# =========================
# UI
# =========================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .drawer {
        position: fixed;
        right: 0;
        top: 0;
        width: 90vw;
        max-width: 1300px;
        height: 100%;
        background: #fff;
        border-left: 1px solid #ddd;
        padding: 18px 22px;
        overflow-y: auto;
        z-index: 99999;
      }

      .kwic {
        font-weight: 700;
        color: #1565c0;
      }

      .kwic-box {
        font-weight: 500;
        margin: 8px 0 12px 0;
        line-height: 1.6;
      }

      .micro-row {
        display: grid;
        grid-template-columns: 1fr 30px; /* Testo a sinistra (flessibile), checkbox in colonna fissa a destra */
        align-items: center;
        gap: 8px;
        margin-bottom: 6px;
        width: 100%;
      }

        .micro-label {
          display: flex;
          align-items: center;
          gap: 6px;
          line-height: 1.1;
        }

        /* Rimuove i margini di default dei container di Shiny per evitare disallineamenti */
        .micro-row .shiny-input-container {
          margin: 0 !important;
          padding: 0 !important;
          width: auto !important;
          justify-self: end;
        }

        .micro-row .checkbox {
          margin: 0 !important;
          padding: 0 !important;
        }

      .micro-label {
        display: flex;
        align-items: center;
        gap: 6px;
        line-height: 1.1;
      }

      .i {
        cursor: help;
        color: #999;
        font-weight: bold;
      }

      .material-box {
        display: flex;
        gap: 20px;
        align-items: flex-start;
        justify-content: space-between;
      }

      .material-left {
        flex: 1;
      }

      .material-right {
        width: 45%;
      }

      .save-fixed {
        position: sticky;
        top: 0;
        background: white;
        padding: 10px 0;
        z-index: 10000;
        border-bottom: 1px solid #eee;
      }

      .title-wrap {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 10px;
      }

      .title-wrap img {
        height: 44px;
      }

      .nav-row {
        display: flex;
        gap: 10px;
        margin-bottom: 10px;
      }

      table.dataTable tbody td {
        vertical-align: top;
      }
      
      
    ")),
    
    tags$script(HTML("
      let isSaved = true;

      $(document).on('click', '#save, #save_bottom', function() {
        isSaved = true;
      });

      $(document).on('change', 'input, textarea', function() {
        isSaved = false;
      });

      window.addEventListener('beforeunload', function (e) {
        if (!isSaved) {
          e.preventDefault();
          e.returnValue = '';
        }
      });
    "))
  ),
  
  div(
    class = "title-wrap",
    tags$img(src = "logo.png"),
    h3("COLÀ - Console Online per L'Annotazione (dei segnali discorsivi)")
  ),
  
  fileInput("file", "Carica Excel/CSV"),
  myDownloadButton("download", "export"),
  hr(),
  DTOutput("table"),
  uiOutput("drawer")
)

# =========================
# SERVER
# =========================

server <- function(input, output, session) {
  
  df_r <- reactiveVal(NULL)
  idx <- reactiveVal(1)
  drawer <- reactiveVal(FALSE)
  
  # ================= LOAD =================
  observeEvent(input$file, {
    ext <- tools::file_ext(input$file$name)
    
    df <- tryCatch({
      if (ext == "xlsx") {
        read_excel(input$file$datapath)
      } else {
        read.csv(
          input$file$datapath,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }, error = function(e) NULL)
    
    if (is.null(df)) return()
    
    df <- as.data.frame(df)
    
    if ("labels" %in% names(df)) {
      df$labels <- gsub(";", "|", df$labels)
      df$labels <- gsub("\\s*\\|\\s*", "|", df$labels)
    }
    
    df <- ensure_col(df, "SD", "")
    df <- ensure_col(df, "labels", "")
    df <- ensure_col(df, "commenti", "")
    df <- ensure_col(df, "cumulo", 0) 
    df <- ensure_col(df, "cumulo_testo", "")
    
    df <- ensure_col(df, "annotation.audio_file", "")
    df <- ensure_col(df, "doc.full_conversation", "")
    df <- ensure_col(df, "doc.full_jefferson", "")
    
    df <- ensure_col(df, "Left", "")
    df <- ensure_col(df, "KWIC", "")
    df <- ensure_col(df, "Right", "")
    
    df <- ensure_bin_cols(df, ALL_KEYS)
    
    df_r(df)
    idx(1)
  })
  
  # ================= TABLE =================
  output$table <- renderDT({
    df <- df_r()
    req(df)
    
    view <- data.frame(
      Occorrenza = paste0(
        df$Left,
        " <span class='kwic'>",
        df$KWIC,
        "</span> ",
        df$Right
      ),
      SD = df$SD,
      Funzioni = gsub("\\|", "; ", df$labels),
      Commento = substr(df$commenti, 1, 40),
      stringsAsFactors = FALSE
    )
    
    datatable(
      view,
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      options = list(
        scrollY = "600px",
        paging = FALSE,
        dom = "tip"
      )
    )
  })
  
  observeEvent(input$table_rows_selected, {
    idx(input$table_rows_selected)
    drawer(TRUE)
  })
  
  # ================= DRAWER =================
  output$drawer <- renderUI({
    if (!drawer()) return(NULL)
    
    current_idx <- idx()
    df_snapshot <- isolate(df_r())
    req(df_snapshot)
    
    r <- df_snapshot[current_idx, ]
    total_n <- nrow(df_snapshot)
    
    tagList(
      div(
        class = "drawer",
        
        div(
          style = "display:flex; justify-content:space-between; align-items:center;",
          h3(paste0("Dettaglio — Occorrenza ", current_idx, "/", total_n)),
          actionButton("close", "✖", style = "border:none; background:none; font-size:22px;")
        ),
        
        hr(),
        
        div(
          class = "nav-row",
          actionButton("prev_occ", "⬅ Occorrenza precedente"),
          actionButton("next_occ", "Occorrenza successiva ➡"),
          actionButton("save", "💾 salva occorrenza", style = "font-weight:bold;")
        ),
        
        hr(),
        
        div(
          class = "material-box",
          
          div(
            class = "material-left",
            h4("Materiali"),
            
            if (nzchar(r$`annotation.audio_file`))
              tags$a("🎧 Audio", href = r$`annotation.audio_file`, target = "_blank"),
            
            br(),
            
            if (nzchar(r$`doc.full_conversation`))
              tags$a("📄 Conversazione", href = r$`doc.full_conversation`, target = "_blank"),
            
            br(),
            
            if (nzchar(r$`doc.full_jefferson`))
              tags$a("📄 Jefferson", href = r$`doc.full_jefferson`, target = "_blank"),
            
            br(),
            
            tags$a("🔎 Guida alle funzioni", href = "https://laboratorioparole.github.io/cosi/guida-funzioni.html", target = "_blank"),
            
            br(),
            
            hr(),
            
            fluidRow(
              column(
                8,
                h4("Occorrenza"),
                div(
                  class = "kwic-box",
                  HTML(paste0(r$Left, " <span class='kwic'>", r$KWIC, "</span> ", r$Right))
                ),
                radioButtons(
                  "sd", "SD",
                  choices = c("Non SD", "SD"),
                  selected = character(0),
                  inline = TRUE
                )
              ),
              
              column(
                4,
                checkboxInput(
                  "cumulo", "Cumulo",
                  value = !is.na(r$cumulo) && as.numeric(r$cumulo) == 1
                ),
                conditionalPanel(
                  condition = "input.cumulo == true",
                  textAreaInput(
                    "cumulo_testo", NULL,
                    value = ifelse(is.na(r$cumulo_testo), "", r$cumulo_testo),
                    width = "100%", height = "70px",
                    placeholder = "Elementi in cumulo..."
                  )
                )
              )
            )
          ),
          
          div(
            class = "material-right",
            textAreaInput(
              "commenti", "Commenti",
              value = r$commenti, width = "100%", height = "90px"
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.sd == 'SD'",
          
          fluidRow(
            column(
              4, style = "display:flex; flex-direction:column;",
              h4("Interazionali"),
              lapply(INTERAZIONALI, function(k) {
                id <- safe_id(k)
                val <- r[[id]]
                val <- !is.na(val) && as.numeric(val) == 1
                div(
                  class = "micro-row",
                  div(class = "micro-label", k, span("ℹ", class = "i", title = DEFINIZIONI[[k]])),
                  checkboxInput(id, NULL, value = val)
                )
              })
            ),
            
            column(
              4, style = "display:flex; flex-direction:column;",
              h4("Metatestuali"),
              lapply(METATESTUALI, function(k) {
                id <- safe_id(k)
                val <- r[[id]]
                val <- !is.na(val) && as.numeric(val) == 1
                div(
                  class = "micro-row",
                  div(class = "micro-label", k, span("ℹ", class = "i", title = DEFINIZIONI[[k]])),
                  checkboxInput(id, NULL, value = val)
                )
              })
            ),
            
            column(
              4, style = "display:flex; flex-direction:column;",
              h4("Cognitive"),
              lapply(COGNITIVE, function(k) {
                id <- safe_id(k)
                val <- r[[id]]
                val <- !is.na(val) && as.numeric(val) == 1
                div(
                  class = "micro-row",
                  div(class = "micro-label", k, span("ℹ", class = "i", title = DEFINIZIONI[[k]])),
                  checkboxInput(id, NULL, value = val)
                )
              })
            )
          )
        ),
        
        hr(),
        
        div(
          class = "nav-row",
          actionButton("prev_occ_bottom", "⬅ Occorrenza precedente"),
          actionButton("next_occ_bottom", "Occorrenza successiva ➡"),
          actionButton("save_bottom", "💾 salva occorrenza", style = "font-weight:bold;")
        ),
        
        hr(),
        
        actionButton("close", "chiudi")
      )
    )
  })
  
  # ================= CLOSE =================
  observeEvent(input$close, {
    drawer(FALSE)
  })
  
  # ================= PREV/NEXT =================
  observeEvent(input$prev_occ, {
    if (idx() > 1) idx(idx() - 1)
  })
  
  observeEvent(input$prev_occ_bottom, {
    if (idx() > 1) idx(idx() - 1)
  })
  
  observeEvent(input$next_occ, {
    req(df_r())
    if (idx() < nrow(df_r())) idx(idx() + 1)
  })
  
  observeEvent(input$next_occ_bottom, {
    req(df_r())
    if (idx() < nrow(df_r())) idx(idx() + 1)
  })
  
  # ================= SYNC =================
  observe({
    req(df_r())
    r <- df_r()[idx(), ]
    if (is.null(r)) return()
    
    selected_sd <- if (is.null(r$SD) || is.na(r$SD) || !nzchar(as.character(r$SD))) {
      character(0)
    } else {
      as.character(r$SD)
    }
    
    updateRadioButtons(session, "sd", selected = selected_sd)
    updateTextAreaInput(session, "commenti", value = ifelse(is.na(r$commenti), "", r$commenti))
    updateCheckboxInput(session, "cumulo", value = !is.na(r$cumulo) && as.numeric(r$cumulo) == 1) 
    updateTextAreaInput(session, "cumulo_testo", value = ifelse(is.na(r$cumulo_testo), "", r$cumulo_testo))
    
    for (k in ALL_KEYS) {
      id <- safe_id(k)
      val <- suppressWarnings(as.numeric(r[[id]]))
      updateCheckboxInput(session, id, value = !is.na(val) && val == 1)
    }
  })
  
  observeEvent(input$sd, {
    if (!identical(input$sd, "SD")) {
      for (k in ALL_KEYS) {
        updateCheckboxInput(session, safe_id(k), value = FALSE)
      }
    }
  })
  
  # ================= SAVE =================
  save_current <- function() {
    df <- df_r()
    i <- idx()
    
    df$SD[i] <- if (is.null(input$sd)) "" else input$sd
    df$commenti[i] <- input$commenti
    df$cumulo[i] <- ifelse(isTRUE(input$cumulo), 1, 0)
    df$cumulo_testo[i] <- ifelse(isTRUE(input$cumulo), input$cumulo_testo, "")
    
    selected <- c()
    for (k in ALL_KEYS) {
      id <- safe_id(k)
      val <- isTRUE(input[[id]])
      df[[id]][i] <- ifelse(val, 1, 0)
      if (val) selected <- c(selected, k)
    }
    
    df$labels[i] <- paste(selected, collapse = "|")
    df_r(df)
  }
  
  observeEvent(input$save, save_current())
  observeEvent(input$save_bottom, save_current())
  
  # ================= DOWNLOAD =================
  output$download <- downloadHandler(
    filename = function() {
      "annotato.csv"
    },
    content = function(file) {
      write.csv(
        df_r(),
        file,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  )
}

shinyApp(ui, server)


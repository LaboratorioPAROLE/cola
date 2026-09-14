library(shiny)
library(readxl)
library(writexl)
library(DT)

# =========================
# UTILS & HELPERS
# =========================

myDownloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

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

# Helper per etichette con tooltip informativo (ℹ)
lbl_info <- function(name, tooltip) {
  tagList(name, span("ℹ", class = "i", title = tooltip))
}

# =========================
# FUNZIONI DI CALCOLO METRICHE (IAA)
# =========================

# 1. Calcolo IAA su singola variabile binaria (es. SD)
calc_binary_iaa <- function(v1, v2) {
  n <- length(v1)
  if (n == 0) return(list(po = 0, jaccard = 0, kappa = 0, ac1 = 0, alpha = 0))
  
  po <- mean(v1 == v2)
  
  intersect_cnt <- sum(v1 == 1 & v2 == 1)
  union_cnt <- sum(v1 == 1 | v2 == 1)
  jaccard <- if (union_cnt == 0) 1 else intersect_cnt / union_cnt
  
  p1_1 <- mean(v1 == 1); p1_0 <- mean(v1 == 0)
  p2_1 <- mean(v2 == 1); p2_0 <- mean(v2 == 0)
  pe_kappa <- (p1_1 * p2_1) + (p1_0 * p2_0)
  kappa <- if (pe_kappa == 1) 1 else (po - pe_kappa) / (1 - pe_kappa)
  
  p1_avg <- (p1_1 + p2_1) / 2
  pe_ac1 <- 2 * p1_avg * (1 - p1_avg)
  ac1 <- if (pe_ac1 == 1) 1 else (po - pe_ac1) / (1 - pe_ac1)
  
  pe_alpha <- (p1_avg^2) + ((1 - p1_avg)^2)
  alpha <- if (pe_alpha == 1) 1 else (po - pe_alpha) / (1 - pe_alpha)
  
  list(
    po = po * 100,
    jaccard = jaccard * 100,
    kappa = kappa,
    ac1 = ac1,
    alpha = alpha
  )
}

# 2. Calcolo Nominal IAA (per Boot-Match / Exact Set Match)
calc_nominal_iaa <- function(vec1, vec2) {
  n <- length(vec1)
  if (n == 0) return(list(po = 0, kappa = 0, ac1 = 0, alpha = 0))
  
  po <- mean(vec1 == vec2)
  cats <- unique(c(vec1, vec2))
  q <- length(cats)
  
  p1 <- table(factor(vec1, levels = cats)) / n
  p2 <- table(factor(vec2, levels = cats)) / n
  
  pe_kappa <- sum(p1 * p2)
  kappa <- if (pe_kappa == 1) 1 else (po - pe_kappa) / (1 - pe_kappa)
  
  pi_avg <- (p1 + p2) / 2
  pe_ac1 <- if (q <= 1) 1 else (1 / (q - 1)) * sum(pi_avg * (1 - pi_avg))
  ac1 <- if (pe_ac1 == 1) 1 else (po - pe_ac1) / (1 - pe_ac1)
  
  pe_alpha <- sum(pi_avg^2)
  alpha <- if (pe_alpha == 1) 1 else (po - pe_alpha) / (1 - pe_alpha)
  
  list(po = po * 100, kappa = kappa, ac1 = ac1, alpha = alpha)
}

# 3. Calcolo Completo Multilabel (Boot-Match + Soft-Match + Jaccard)
calc_multilabel_iaa_full <- function(m1, m2) {
  n <- nrow(m1)
  if (n == 0) return(NULL)
  
  # Set strings per Boot-match
  str1 <- apply(m1, 1, function(r) paste(which(r == 1), collapse = ","))
  str2 <- apply(m2, 1, function(r) paste(which(r == 1), collapse = ","))
  
  boot_res <- calc_nominal_iaa(str1, str2)
  
  # Indice Jaccard Medio
  jaccard_vec <- sapply(1:n, function(i) {
    u <- sum(m1[i, ] | m2[i, ])
    if (u == 0) return(1)
    sum(m1[i, ] & m2[i, ]) / u
  })
  mean_jaccard <- mean(jaccard_vec) * 100
  
  # Soft-match (Item-wise decision level)
  v1 <- as.vector(m1)
  v2 <- as.vector(m2)
  soft_res <- calc_binary_iaa(v1, v2)
  
  # Soft observed at token level (At-least-one overlap or both empty)
  soft_obs_token <- mean(sapply(1:n, function(i) {
    any(m1[i, ] == 1 & m2[i, ] == 1) || (sum(m1[i, ]) == 0 && sum(m2[i, ]) == 0)
  })) * 100
  
  list(
    boot = list(
      po = boot_res$po,
      jaccard = mean_jaccard,
      kappa = boot_res$kappa,
      ac1 = boot_res$ac1,
      alpha = boot_res$alpha
    ),
    soft = list(
      po = soft_obs_token,
      jaccard = mean_jaccard,
      kappa = soft_res$kappa,
      ac1 = soft_res$ac1,
      alpha = soft_res$alpha
    )
  )
}

# =========================
# CATEGORIE E DEFINIZIONI
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

ALL_KEYS <- c(INTERAZIONALI, METATESTUALI, COGNITIVE)

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
  title = "COLÀ - Console Online per L'Annotazione",
  
  tags$head(
    tags$title("COLÀ - Console Online per L'Annotazione"),
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
      .kwic { font-weight: 700; color: #1565c0; }
      .kwic-box { font-weight: 500; margin: 8px 0 12px 0; line-height: 1.6; }
      
      .micro-row {
        display: grid;
        grid-template-columns: 1fr 30px;
        align-items: center;
        gap: 8px;
        margin-bottom: 6px;
        width: 100%;
      }
      .micro-label { display: flex; align-items: center; gap: 6px; line-height: 1.1; }
      .micro-row .shiny-input-container { margin: 0 !important; padding: 0 !important; width: auto !important; justify-self: end; }
      .micro-row .checkbox { margin: 0 !important; padding: 0 !important; }
      
      .i { cursor: help; color: #1565c0; font-weight: bold; margin-left: 5px; font-style: normal; }
      .material-box { display: flex; gap: 20px; align-items: flex-start; justify-content: space-between; }
      .material-left { flex: 1; }
      .material-right { width: 45%; }
      .title-wrap { display: flex; align-items: center; gap: 10px; margin-bottom: 10px; }
      .title-wrap img { height: 44px; }
      .nav-row { display: flex; gap: 10px; margin-bottom: 10px; }
      table.dataTable tbody td { vertical-align: top; }
    ")),
    
    tags$script(HTML("
      let isSaved = true;
      $(document).on('click', '#save, #save_bottom', function() { isSaved = true; });
      $(document).on('change', 'input, textarea', function() { isSaved = false; });
      window.addEventListener('beforeunload', function (e) {
        if (!isSaved) { e.preventDefault(); e.returnValue = ''; }
      });
    "))
  ),
  
  div(
    class = "title-wrap",
    tags$img(src = "logo.png"),
    h3("COLÀ - Console Online per L'Annotazione (dei segnali discorsivi)")
  ),
  
  # CONTENITORE VERTICALE PER FILE BROWSE E PULSANTI SOTTO
  div(
    style = "margin-bottom: 20px; max-width: 420px;",
    fileInput("file", "Carica Excel/CSV", buttonLabel = "Sfoglia...", width = "100%"),
    div(
      style = "display: flex; gap: 10px; margin-top: -10px;",
      myDownloadButton("download", "export"),
      actionButton("open_compare", "📊 Confronta Annotazioni (IAA)", class = "btn-info")
    )
  ),
  
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
      if (ext == "xlsx") read_excel(input$file$datapath)
      else read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
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
      Occorrenza = paste0(df$Left, " <span class='kwic'>", df$KWIC, "</span> ", df$Right),
      SD = df$SD,
      Funzioni = gsub("\\|", "; ", df$labels),
      Commento = substr(df$commenti, 1, 40),
      stringsAsFactors = FALSE
    )
    
    datatable(view, escape = FALSE, selection = "single", rownames = FALSE,
              options = list(scrollY = "600px", paging = FALSE, dom = "tip"))
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
            if (nzchar(r$`annotation.audio_file`)) tags$a("🎧 Audio", href = r$`annotation.audio_file`, target = "_blank"),
            br(),
            if (nzchar(r$`doc.full_conversation`)) tags$a("📄 Conversazione", href = r$`doc.full_conversation`, target = "_blank"),
            br(),
            if (nzchar(r$`doc.full_jefferson`)) tags$a("📄 Jefferson", href = r$`doc.full_jefferson`, target = "_blank"),
            hr(),
            fluidRow(
              column(8,
                     h4("Occorrenza"),
                     div(class = "kwic-box", HTML(paste0(r$Left, " <span class='kwic'>", r$KWIC, "</span> ", r$Right))),
                     radioButtons("sd", "SD", choices = c("Non SD", "SD"), selected = character(0), inline = TRUE)
              ),
              column(4,
                     checkboxInput("cumulo", "Cumulo", value = !is.na(r$cumulo) && as.numeric(r$cumulo) == 1),
                     conditionalPanel(
                       condition = "input.cumulo == true",
                       textAreaInput("cumulo_testo", NULL, value = ifelse(is.na(r$cumulo_testo), "", r$cumulo_testo), width = "100%", height = "70px", placeholder = "Elementi in cumulo...")
                     )
              )
            )
          ),
          div(
            class = "material-right",
            textAreaInput("commenti", "Commenti", value = r$commenti, width = "100%", height = "90px")
          )
        ),
        
        conditionalPanel(
          condition = "input.sd == 'SD'",
          fluidRow(
            column(4,
                   h4("Interazionali"),
                   lapply(INTERAZIONALI, function(k) {
                     id <- safe_id(k)
                     val <- r[[id]]
                     val <- !is.na(val) && as.numeric(val) == 1
                     div(class = "micro-row",
                         div(class = "micro-label", k, span("ℹ", class = "i", title = DEFINIZIONI[[k]])),
                         checkboxInput(id, NULL, value = val))
                   })
            ),
            column(4,
                   h4("Metatestuali"),
                   lapply(METATESTUALI, function(k) {
                     id <- safe_id(k)
                     val <- r[[id]]
                     val <- !is.na(val) && as.numeric(val) == 1
                     div(class = "micro-row",
                         div(class = "micro-label", k, span("ℹ", class = "i", title = DEFINIZIONI[[k]])),
                         checkboxInput(id, NULL, value = val))
                   })
            ),
            column(4,
                   h4("Cognitive"),
                   lapply(COGNITIVE, function(k) {
                     id <- safe_id(k)
                     val <- r[[id]]
                     val <- !is.na(val) && as.numeric(val) == 1
                     div(class = "micro-row",
                         div(class = "micro-label", k, span("ℹ", class = "i", title = DEFINIZIONI[[k]])),
                         checkboxInput(id, NULL, value = val))
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
  
  observeEvent(input$close, { drawer(FALSE) })
  observeEvent(input$prev_occ, { if (idx() > 1) idx(idx() - 1) })
  observeEvent(input$prev_occ_bottom, { if (idx() > 1) idx(idx() - 1) })
  observeEvent(input$next_occ, { req(df_r()); if (idx() < nrow(df_r())) idx(idx() + 1) })
  observeEvent(input$next_occ_bottom, { req(df_r()); if (idx() < nrow(df_r())) idx(idx() + 1) })
  
  # ================= SYNC & SAVE =================
  observe({
    req(df_r())
    r <- df_r()[idx(), ]
    if (is.null(r)) return()
    
    selected_sd <- if (is.null(r$SD) || is.na(r$SD) || !nzchar(as.character(r$SD))) character(0) else as.character(r$SD)
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
      for (k in ALL_KEYS) updateCheckboxInput(session, safe_id(k), value = FALSE)
    }
  })
  
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
  
  # ================= MODALE CONFRONTO (IAA) =================
  observeEvent(input$open_compare, {
    showModal(modalDialog(
      title = "Analisi Inter-Annotator Agreement (IAA)",
      size = "l",
      easyClose = TRUE,
      
      fluidRow(
        column(8, fileInput("file_ref", "Carica File di Riferimento / Gold Standard (CSV/XLSX)")),
        column(4, checkboxInput("only_non_empty", "Filtra solo token annotati (non vuoti)", value = TRUE))
      ),
      hr(),
      uiOutput("iaa_results_ui"),
      footer = modalButton("Chiudi")
    ))
  })
  
  output$iaa_results_ui <- renderUI({
    req(input$file_ref)
    req(df_r())
    
    ext <- tools::file_ext(input$file_ref$name)
    df_ref <- if (ext == "xlsx") read_excel(input$file_ref$datapath) else read.csv(input$file_ref$datapath, check.names = FALSE)
    df_ref <- as.data.frame(df_ref)
    
    df_curr <- df_r()
    n <- min(nrow(df_curr), nrow(df_ref))
    df1 <- df_curr[1:n, ]
    df2 <- df_ref[1:n, ]
    
    df1 <- ensure_bin_cols(df1, ALL_KEYS)
    df2 <- ensure_bin_cols(df2, ALL_KEYS)
    
    keys_id <- sapply(ALL_KEYS, safe_id)
    m1 <- as.matrix(df1[, keys_id])
    m2 <- as.matrix(df2[, keys_id])
    
    if (isTRUE(input$only_non_empty)) {
      mask <- rowSums(m1) > 0 | rowSums(m2) > 0 | (df1$SD == "SD") | (df2$SD == "SD")
      df1 <- df1[mask, ]
      df2 <- df2[mask, ]
      m1 <- m1[mask, ]
      m2 <- m2[mask, ]
    }
    
    if (nrow(df1) == 0) return(p("Nessun token disponibile per il confronto con i filtri selezionati."))
    
    # 1. SD (Binario)
    sd1 <- ifelse(df1$SD == "SD", 1, 0)
    sd2 <- ifelse(df2$SD == "SD", 1, 0)
    res_sd <- calc_binary_iaa(sd1, sd2)
    
    # 2. Overall Macrofunzioni (matrice a 3 colonne per Interazionali, Metatestuali, Cognitive)
    m1_macro <- cbind(
      Interazionali = (rowSums(m1[, safe_id(INTERAZIONALI), drop=FALSE]) > 0) * 1,
      Metatestuali  = (rowSums(m1[, safe_id(METATESTUALI), drop=FALSE])  > 0) * 1,
      Cognitive     = (rowSums(m1[, safe_id(COGNITIVE), drop=FALSE])    > 0) * 1
    )
    
    m2_macro <- cbind(
      Interazionali = (rowSums(m2[, safe_id(INTERAZIONALI), drop=FALSE]) > 0) * 1,
      Metatestuali  = (rowSums(m2[, safe_id(METATESTUALI), drop=FALSE])  > 0) * 1,
      Cognitive     = (rowSums(m2[, safe_id(COGNITIVE), drop=FALSE])    > 0) * 1
    )
    
    res_macro_overall <- calc_multilabel_iaa_full(m1_macro, m2_macro)
    
    # 3. Overall Microfunzioni (tutte le 23 microfunzioni contemporaneamente)
    res_micro_overall <- calc_multilabel_iaa_full(m1, m2)
    
    # Tooltip usati per le definizioni delle metriche
    tt_po <- "Percentuale di accordo diretto osservato tra gli annotatori sul totale dei casi."
    tt_jaccard <- "Rapporto tra l'intersezione e l'unione delle etichette (misura di similarità tra set)."
    tt_kappa <- "Indice di accordo corretto per la probabilità di accordo casuale (Cohen per coppie, Fleiss per dati categoriali)."
    tt_ac1 <- "Indice Gwet's AC1, robusto ai paradossi del Kappa in presenza di classi fortemente sbilanciate."
    tt_alpha <- "Krippendorff's Alpha, coefficiente di affidabilità generalizzabile a qualsiasi livello di misurazione."
    
    tagList(
      p(strong("Token analizzati per il confronto: "), nrow(df1)),
      
      tabsetPanel(
        # TAB 1: SD
        tabPanel("1. Livello SD",
                 br(),
                 h5("Accordo sull'identificazione del Segnale Discorsivo (SD vs Non SD)"),
                 tags$table(class = "table table-bordered", style="width: auto;",
                            tags$tr(tags$td(lbl_info("Accordo Osservato (%):", tt_po)), tags$td(sprintf("%.2f%%", res_sd$po))),
                            tags$tr(tags$td(lbl_info("Indice Jaccard (%):", tt_jaccard)), tags$td(sprintf("%.2f%%", res_sd$jaccard))),
                            tags$tr(tags$td(lbl_info("Cohen's Kappa:", tt_kappa)), tags$td(round(res_sd$kappa, 3))),
                            tags$tr(tags$td(lbl_info("Gwet's AC1:", tt_ac1)), tags$td(round(res_sd$ac1, 3))),
                            tags$tr(tags$td(lbl_info("Krippendorff's Alpha:", tt_alpha)), tags$td(round(res_sd$alpha, 3)))
                 )
        ),
        
        # TAB 2: MACROFUNZIONI (UNICO SCORE OVERALL)
        tabPanel("2. Macrofunzioni",
                 br(),
                 h5("Score Complessivo (Overall) per le Macrofunzioni"),
                 tags$table(class = "table table-bordered",
                            tags$thead(
                              tags$tr(
                                tags$th("Tipo Matching"),
                                tags$th(lbl_info("Accordo Osservato (%)", tt_po)), 
                                tags$th(lbl_info("Indice Jaccard (%)", tt_jaccard)), 
                                tags$th(lbl_info("Cohen / Fleiss Kappa", tt_kappa)), 
                                tags$th(lbl_info("Gwet's AC1", tt_ac1)),
                                tags$th(lbl_info("Krippendorff's Alpha", tt_alpha))
                              )
                            ),
                            tags$tbody(
                              tags$tr(
                                tags$td(strong("Boot-Match (Exact Set)")),
                                tags$td(sprintf("%.2f%%", res_macro_overall$boot$po)),
                                tags$td(sprintf("%.2f%%", res_macro_overall$boot$jaccard)),
                                tags$td(round(res_macro_overall$boot$kappa, 3)),
                                tags$td(round(res_macro_overall$boot$ac1, 3)),
                                tags$td(round(res_macro_overall$boot$alpha, 3))
                              ),
                              tags$tr(
                                tags$td(strong("Soft-Match (At Least One)")),
                                tags$td(sprintf("%.2f%%", res_macro_overall$soft$po)),
                                tags$td(sprintf("%.2f%%", res_macro_overall$soft$jaccard)),
                                tags$td(round(res_macro_overall$soft$kappa, 3)),
                                tags$td(round(res_macro_overall$soft$ac1, 3)),
                                tags$td(round(res_macro_overall$soft$alpha, 3))
                              )
                            )
                 )
        ),
        
        # TAB 3: MICROFUNZIONI (SOLO OVERALL, RIMOZIONE DEGLI SCORE SINGOLI)
        tabPanel("3. Microfunzioni",
                 br(),
                 h5("Score Complessivo (Overall) per le Microfunzioni"),
                 tags$table(class = "table table-bordered",
                            tags$thead(
                              tags$tr(
                                tags$th("Tipo Matching"),
                                tags$th(lbl_info("Accordo Osservato (%)", tt_po)), 
                                tags$th(lbl_info("Indice Jaccard (%)", tt_jaccard)), 
                                tags$th(lbl_info("Cohen / Fleiss Kappa", tt_kappa)), 
                                tags$th(lbl_info("Gwet's AC1", tt_ac1)),
                                tags$th(lbl_info("Krippendorff's Alpha", tt_alpha))
                              )
                            ),
                            tags$tbody(
                              tags$tr(
                                tags$td(strong("Boot-Match (Exact Set)")),
                                tags$td(sprintf("%.2f%%", res_micro_overall$boot$po)),
                                tags$td(sprintf("%.2f%%", res_micro_overall$boot$jaccard)),
                                tags$td(round(res_micro_overall$boot$kappa, 3)),
                                tags$td(round(res_micro_overall$boot$ac1, 3)),
                                tags$td(round(res_micro_overall$boot$alpha, 3))
                              ),
                              tags$tr(
                                tags$td(strong("Soft-Match (At Least One)")),
                                tags$td(sprintf("%.2f%%", res_micro_overall$soft$po)),
                                tags$td(sprintf("%.2f%%", res_micro_overall$soft$jaccard)),
                                tags$td(round(res_micro_overall$soft$kappa, 3)),
                                tags$td(round(res_micro_overall$soft$ac1, 3)),
                                tags$td(round(res_micro_overall$soft$alpha, 3))
                              )
                            )
                 )
        )
      )
    )
  })
  
  # ================= DOWNLOAD =================
  output$download <- downloadHandler(
    filename = function() { "annotato.csv" },
    content = function(file) {
      write.csv(df_r(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
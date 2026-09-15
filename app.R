library(shiny)
library(readxl)
library(writexl)
library(DT)
library(shinycssloaders)

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

lbl_info <- function(name, tooltip) {
  tagList(name, span("i", class = "i", title = tooltip))
}

# =========================
# FUNZIONI DI CALCOLO METRICHE (IAA)
# =========================

calc_binary_standard <- function(v1, v2) {
  v1 <- as.numeric(v1)
  v2 <- as.numeric(v2)
  
  ok <- !is.na(v1) & !is.na(v2)
  v1 <- v1[ok]
  v2 <- v2[ok]
  
  n <- length(v1)
  
  if (n == 0) {
    return(list(
      n = 0, po = NA_real_, jaccard = NA_real_,
      kappa = NA_real_, ac1 = NA_real_, alpha = NA_real_
    ))
  }
  
  po <- mean(v1 == v2)
  
  intersection <- sum(v1 == 1 & v2 == 1)
  union <- sum(v1 == 1 | v2 == 1)
  
  jaccard <- if (union == 0) NA_real_ else intersection / union
  
  p1_1 <- mean(v1 == 1)
  p1_0 <- mean(v1 == 0)
  p2_1 <- mean(v2 == 1)
  p2_0 <- mean(v2 == 0)
  
  pe_kappa <- p1_1 * p2_1 + p1_0 * p2_0
  
  kappa <- if (abs(1 - pe_kappa) < 1e-12) {
    if (abs(po - 1) < 1e-12) 1 else NA_real_
  } else {
    (po - pe_kappa) / (1 - pe_kappa)
  }
  
  p_avg <- (p1_1 + p2_1) / 2
  pe_ac1 <- 2 * p_avg * (1 - p_avg)
  
  ac1 <- if (abs(1 - pe_ac1) < 1e-12) {
    if (abs(po - 1) < 1e-12) 1 else NA_real_
  } else {
    (po - pe_ac1) / (1 - pe_ac1)
  }
  
  observed_disagreement <- mean(v1 != v2)
  p_avg_0 <- mean(c(v1 == 0, v2 == 0))
  p_avg_1 <- mean(c(v1 == 1, v2 == 1))
  expected_disagreement <- 2 * p_avg_0 * p_avg_1
  
  alpha <- if (abs(expected_disagreement) < 1e-12) {
    if (observed_disagreement == 0) 1 else NA_real_
  } else {
    1 - observed_disagreement / expected_disagreement
  }
  
  list(
    n = n, po = po * 100,
    jaccard = ifelse(is.na(jaccard), NA_real_, jaccard * 100),
    kappa = kappa, ac1 = ac1, alpha = alpha
  )
}

calc_positive_binary <- function(v1, v2) {
  v1 <- as.numeric(v1)
  v2 <- as.numeric(v2)
  
  ok <- !is.na(v1) & !is.na(v2)
  v1 <- v1[ok]
  v2 <- v2[ok]
  
  keep <- (v1 == 1 | v2 == 1)
  v1 <- v1[keep]
  v2 <- v2[keep]
  
  n <- length(v1)
  
  if (n == 0) {
    return(list(
      n = 0, po = NA_real_, kappa = NA_real_, ac1 = NA_real_, alpha = NA_real_
    ))
  }
  
  po <- mean(v1 == v2)
  
  p1_1 <- mean(v1 == 1)
  p1_0 <- mean(v1 == 0)
  p2_1 <- mean(v2 == 1)
  p2_0 <- mean(v2 == 0)
  
  pe_kappa <- p1_1 * p2_1 + p1_0 * p2_0
  
  kappa <- if (abs(1 - pe_kappa) < 1e-12) {
    if (abs(po - 1) < 1e-12) 1 else NA_real_
  } else {
    (po - pe_kappa) / (1 - pe_kappa)
  }
  
  p_avg <- (p1_1 + p2_1) / 2
  pe_ac1 <- 2 * p_avg * (1 - p_avg)
  
  ac1 <- if (abs(1 - pe_ac1) < 1e-12) {
    if (abs(po - 1) < 1e-12) 1 else NA_real_
  } else {
    (po - pe_ac1) / (1 - pe_ac1)
  }
  
  observed_disagreement <- mean(v1 != v2)
  p0 <- mean(c(v1 == 0, v2 == 0))
  p1 <- mean(c(v1 == 1, v2 == 1))
  
  expected_disagreement <- 2 * p0 * p1
  
  alpha <- if (abs(expected_disagreement) < 1e-12) {
    if (observed_disagreement == 0) 1 else NA_real_
  } else {
    1 - observed_disagreement / expected_disagreement
  }
  
  list(
    n = n, po = po * 100, kappa = kappa, ac1 = ac1, alpha = alpha
  )
}

row_to_set <- function(m) {
  apply(m, 1, function(x) {
    names(x)[which(as.numeric(x) == 1)]
  })
}

set_exact <- function(a, b) {
  identical(sort(a), sort(b))
}

set_soft <- function(a, b) {
  length(intersect(a, b)) > 0
}

set_jaccard <- function(a, b) {
  u <- union(a, b)
  if (length(u) == 0) return(NA_real_)
  length(intersect(a, b)) / length(u)
}

calc_set_alpha <- function(sets1, sets2, distance = c("exact", "soft", "jaccard")) {
  distance <- match.arg(distance)
  
  # Normalizza gli input rimuovendo valori NA e NULL
  clean_set <- function(s) {
    if (is.null(s) || all(is.na(s))) character(0) else na.omit(as.character(s))
  }
  sets1 <- lapply(sets1, clean_set)
  sets2 <- lapply(sets2, clean_set)
  
  keep <- mapply(function(a, b) length(a) > 0 || length(b) > 0, sets1, sets2)
  sets1 <- sets1[keep]
  sets2 <- sets2[keep]
  
  n <- length(sets1)
  if (n == 0) return(NA_real_)
  
  distances <- mapply(
    function(a, b) {
      if (distance == "exact") {
        if (isTRUE(set_exact(a, b))) 0 else 1
      } else if (distance == "soft") {
        if (isTRUE(set_soft(a, b))) 0 else 1
      } else {
        1 - set_jaccard(a, b)
      }
    }, sets1, sets2
  )
  
  Do <- mean(distances, na.rm = TRUE)
  if (is.na(Do)) return(NA_real_)
  
  pooled <- c(sets1, sets2)
  M <- length(pooled)
  if (M < 2) return(NA_real_)
  
  if (distance == "exact") {
    keys <- sapply(pooled, function(x) paste(sort(x), collapse = "|"))
    counts <- as.vector(table(keys))
    sum_same <- sum(as.numeric(counts) * (as.numeric(counts) - 1))
    De <- 1 - (sum_same / (M * (M - 1)))
  } else {
    d_mat <- matrix(0, nrow = M, ncol = M)
    for (i in seq_len(M - 1)) {
      a <- pooled[[i]]
      for (j in (i + 1):M) {
        b <- pooled[[j]]
        d <- if (distance == "soft") {
          if (isTRUE(set_soft(a, b))) 0 else 1
        } else {
          1 - set_jaccard(a, b)
        }
        d_mat[i, j] <- d
        d_mat[j, i] <- d
      }
    }
    De <- sum(d_mat, na.rm = TRUE) / (M * (M - 1))
  }
  
  if (is.na(De) || abs(De) < 1e-12) {
    return(if (!is.na(Do) && abs(Do) < 1e-12) 1 else NA_real_)
  }
  
  1 - Do / De
}

calc_multilabel_matching <- function(m1, m2, label_names = NULL) {
  if (nrow(m1) == 0) return(NULL)
  if (is.null(label_names)) label_names <- colnames(m1)
  
  sets1 <- row_to_set(m1)
  sets2 <- row_to_set(m2)
  
  non_empty <- mapply(function(a, b) length(a) > 0 || length(b) > 0, sets1, sets2)
  s1 <- sets1[non_empty]
  s2 <- sets2[non_empty]
  
  n <- length(s1)
  if (n == 0) return(NULL)
  
  exact_vec <- mapply(set_exact, s1, s2)
  exact_po <- mean(exact_vec) * 100
  exact_alpha <- calc_set_alpha(s1, s2, distance = "exact")
  
  soft_vec <- mapply(set_soft, s1, s2)
  soft_po <- mean(soft_vec) * 100
  soft_alpha <- calc_set_alpha(s1, s2, distance = "soft")
  
  jaccard_vec <- mapply(set_jaccard, s1, s2)
  jaccard_po <- mean(jaccard_vec) * 100
  jaccard_alpha <- calc_set_alpha(s1, s2, distance = "jaccard")
  
  list(
    n = n,
    exact = list(po = exact_po, kappa = NA_real_, ac1 = NA_real_, alpha = exact_alpha),
    soft = list(po = soft_po, kappa = NA_real_, ac1 = NA_real_, alpha = soft_alpha),
    jaccard = list(po = jaccard_po, kappa = NA_real_, ac1 = NA_real_, alpha = jaccard_alpha),
    exact_vec = exact_vec, soft_vec = soft_vec, jaccard_vec = jaccard_vec,
    sets1 = s1, sets2 = s2
  )
}

per_label_df <- function(m1, m2, label_names) {
  out <- lapply(seq_along(label_names), function(j) {
    r <- calc_positive_binary(m1[, j], m2[, j])
    data.frame(
      Etichetta = label_names[j],
      `N positivo` = r$n,
      `Accordo positivo (%)` = ifelse(is.na(r$po), "—", sprintf("%.1f%%", r$po)),
      Kappa = ifelse(is.na(r$kappa), "—", sprintf("%.3f", r$kappa)),
      AC1 = ifelse(is.na(r$ac1), "—", sprintf("%.3f", r$ac1)),
      Alpha = ifelse(is.na(r$alpha), "—", sprintf("%.3f", r$alpha)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

calc_micro_positive <- function(m1, m2) {
  calc_positive_binary(as.vector(m1), as.vector(m2))
}

calc_macro_positive <- function(m1, m2) {
  results <- lapply(seq_len(ncol(m1)), function(j) {
    calc_positive_binary(m1[, j], m2[, j])
  })
  
  macro_mean <- function(name) {
    x <- sapply(results, function(x) x[[name]])
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else mean(x)
  }
  
  list(
    po = macro_mean("po"),
    kappa = macro_mean("kappa"),
    ac1 = macro_mean("ac1"),
    alpha = macro_mean("alpha")
  )
}

calc_polyfunctionality <- function(m1, m2) {
  card1 <- rowSums(m1)
  card2 <- rowSums(m2)
  
  list(
    n = length(card1),
    mean_1 = mean(card1), mean_2 = mean(card2),
    median_1 = median(card1), median_2 = median(card2),
    sd_1 = sd(card1), sd_2 = sd(card2),
    zero_1 = mean(card1 == 0) * 100, zero_2 = mean(card2 == 0) * 100,
    mono_1 = mean(card1 == 1) * 100, mono_2 = mean(card2 == 1) * 100,
    poly_1 = mean(card1 >= 2) * 100, poly_2 = mean(card2 >= 2) * 100,
    same_cardinality = mean(card1 == card2) * 100,
    mean_abs_difference = mean(abs(card1 - card2)),
    diff_1 = mean(abs(card1 - card2) == 1) * 100,
    diff_2plus = mean(abs(card1 - card2) >= 2) * 100
  )
}

detect_csv_sep <- function(path) {
  first_lines <- tryCatch(readLines(path, n = 5, warn = FALSE), error = function(e) character(0))
  txt <- paste(first_lines, collapse = "\n")
  counts <- c(
    ","  = lengths(regmatches(txt, gregexpr(",", txt, fixed = TRUE))),
    ";"  = lengths(regmatches(txt, gregexpr(";", txt, fixed = TRUE))),
    "\t" = lengths(regmatches(txt, gregexpr("\t", txt, fixed = TRUE)))
  )
  best <- names(which.max(counts))
  if (length(best) == 0 || counts[best] == 0) "," else best
}

read_csv_robust <- function(path) {
  sep <- detect_csv_sep(path)
  dec <- if (sep == ";") "," else "."
  out <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, sep = sep, dec = dec, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(out) || ncol(out) <= 1) {
    out <- tryCatch(
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, sep = sep, dec = dec),
      error = function(e) NULL
    )
  }
  out
}

renderTable_iaa <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tags$table(
    class = "table table-striped table-sm",
    tags$thead(tags$tr(lapply(names(df), tags$th))),
    tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        tags$tr(lapply(df[i, ], function(v) tags$td(as.character(v))))
      })
    )
  )
}

# =========================
# CATEGORIE E DEFINIZIONI
# =========================

INTERAZIONALI <- c(
  "Presa di turno", "Richiesta di accordo/conferma", "Accordo",
  "Conferma dell'attenzione", "Interruzione", "Cessione del turno",
  "Marcatura della conoscenza condivisa", "Richiesta di attenzione", "Strategia di cortesia"
)

METATESTUALI <- c(
  "Introduzione o ripresa del topic", "Chiusura del topic", "Prolettico",
  "Riformulazione", "Citazione/Discorso Riportato", "Esemplificazione",
  "Filler", "Marca di generalizzazione"
)

COGNITIVE <- c(
  "Marcatura dell'inferenza", "Modulazione del grado di confidenza",
  "Approssimazione", "Specificazione", "Attenuazione", "Intensificazione"
)

ALL_KEYS <- c(INTERAZIONALI, METATESTUALI, COGNITIVE)

DEFINIZIONI <- list(
  "Presa di turno" = 'Segnala che un/a parlante sta iniziando il proprio turno. Es: "allora...", "dunque..."',
  "Interruzione" = 'Serve ad interrompere un/a parlante per iniziare il proprio turno. Es: "scusa...", "no...", "pero..."',
  "Cessione del turno" = "E' usato dal/la parlante per richiedere risposta all'interlocutore/trice o per chiudere il proprio turno. Es: \"prego\", \"va bene?\", \"no?\"",
  "Marcatura della conoscenza condivisa" = "E' usato dal/la parlante per riconoscere o per assicurarsi che l'altra persona e' a conoscenza di una certa informazione. Es: \"sai\", \"non so se mi spiego\"",
  "Richiesta di accordo/conferma" = 'Serve al/la parlante per chiedere all\'interlocutore/trice una conferma, di attenzione o di accordo. Es: "capito?", "no?"',
  "Accordo" = 'Serve al/la parlante per segnalare all\'interlocutore/trice che e\' d\'accordo con quanto detto prima. Es: "sisi", "eh direi!"',
  "Richiesta di attenzione" = 'Serve al/la parlante per richiamare l\'attenzione su cio\' che sta per dire. Es: "senti...", "guarda..."',
  "Conferma dell'attenzione" = 'Segnala che un/a parlante segue ancora il discorso e/o incoraggia l\'interlocutore/trice a continuare. Es: "mhmh", "certo"',
  "Strategia di cortesia" = 'Serve al/la parlante come strategia di mantenimento della faccia. Es: "gentilmente", "scusa"',
  "Introduzione o ripresa del topic" = 'E\' usato dal/la parlante per introdurre o riprendere un topic. Es: "quindi...", "dopodiche..."',
  "Chiusura del topic" = 'E\' usato dal/la parlante per chiudere un argomento. Es: "ok", "bene", "bon"',
  "Prolettico" = 'Serve al/la parlante per segnalare una formulazione che ritiene adeguata di cio\' che sta per dire. Es: "ecco...", "direi..."',
  "Riformulazione" = 'Viene usato per riformulare quanto appena detto, che sia per ragioni stilistiche, di precisione o di cortesia. Es: "cioe\' nel senso...", "dico..."',
  "Citazione/Discorso Riportato" = 'E\' usato per introdurre qualcosa detto da qualcun altro/a. Es: "mi fa...", "ah..."',
  "Esemplificazione" = 'Introduce un esempio o parafrasa rispetto a un elemento presente nel testo. Es: "mettiamo", "tipo"',
  "Filler" = 'Viene usato dal/la parlante come pausa piena per coprire i tempi di formulazione. Es: "quindi direi tipo", "mh diciamo"',
  "Marca di generalizzazione" = 'E\' un modo di rimandare a una categoria piu\' ampia che il/la parlante da\' per condivisa. Es: "eccetera", "capito no"',
  "Marcatura dell'inferenza" = 'Serve per segnalare che il/la parlante e\' arrivato/a a una conclusione per deduzione. Es: "allora", "quindi", "insomma"',
  "Modulazione del grado di confidenza" = 'Serve per segnalare che il/la parlante non e\' completamente sicuro/a di cio\' che sostiene. Es: "mi pare", "credo"',
  "Approssimazione" = 'E\' usato per rendere piu\' vago quello che si dice. Es: "diciamo", "tipo"',
  "Specificazione" = 'E\' usato per rendere meno vago quello che si dice. Es: "proprio", "letteralmente"',
  "Attenuazione" = 'Serve al/la parlante per mitigare la portata di un certo termine o espressione. Es: "direi", "tipo"',
  "Intensificazione" = 'Serve al/la parlante per intensificare un certo termine o expressione. Es: "decisamente", "assolutamente"'
)

ALIAS_MAP <- c(
  "Manifestazione di accordo" = "Accordo",
  "Strategie di cortesia" = "Strategia di cortesia",
  "Gestione del topic: introduzione o ripresa di topic" = "Introduzione o ripresa del topic",
  "Chiusura di un topic" = "Chiusura del topic",
  "Prolettico, marca di formulazione: ecco" = "Prolettico",
  "Marcatura di citazione/discorso riportato" = "Citazione/Discorso Riportato",
  "Filler, riempimento dei tempi di formulazione" = "Filler",
  "General extenders e marche di generalizzazione" = "Marca di generalizzazione",
  "Modulazione del grado di confidenza del parlante" = "Modulazione del grado di confidenza"
)

normalize_uploaded_df <- function(df) {
  for (k in ALL_KEYS) {
    id <- safe_id(k)
    if (!id %in% names(df) && k %in% names(df)) names(df)[names(df) == k] <- id
  }
  for (alias in names(ALIAS_MAP)) {
    canon <- ALIAS_MAP[[alias]]
    id <- safe_id(canon)
    if (!id %in% names(df) && alias %in% names(df)) names(df)[names(df) == alias] <- id
  }
  if (!"commenti" %in% names(df) && "Commenti" %in% names(df)) names(df)[names(df) == "Commenti"] <- "commenti"
  if (!"cumulo" %in% names(df) && "Cumulo" %in% names(df)) names(df)[names(df) == "Cumulo"] <- "cumulo"
  df
}

reconstruct_annotations <- function(df) {
  keys_id <- sapply(ALL_KEYS, safe_id)
  is_marker_1 <- function(v) toupper(trimws(as.character(v))) %in% c("1", "1.0", "TRUE")
  is_marker_0 <- function(v) toupper(trimws(as.character(v))) %in% c("0", "0.0", "FALSE")
  
  has_non_sd_col <- "Non SD" %in% names(df)
  
  for (i in seq_len(nrow(df))) {
    labels_empty <- !nzchar(trimws(df$labels[i]))
    sd_val <- trimws(as.character(df$SD[i]))
    non_sd_val <- if (has_non_sd_col) trimws(as.character(df[["Non SD"]][i])) else ""
    trigger <- labels_empty || is_marker_1(sd_val) || is_marker_1(df$cumulo[i]) || is_marker_1(non_sd_val)
    if (!trigger) next
    
    if (!sd_val %in% c("SD", "Non SD")) {
      if (is_marker_1(sd_val)) df$SD[i] <- "SD"
      else if (is_marker_1(non_sd_val)) df$SD[i] <- "Non SD"
      else if (is_marker_0(sd_val)) df$SD[i] <- "Non SD"
    }
    
    if (labels_empty) {
      active <- ALL_KEYS[sapply(keys_id, function(id) {
        v <- suppressWarnings(as.numeric(df[[id]][i]))
        !is.na(v) && v == 1
      })]
      df$labels[i] <- paste(active, collapse = "|")
    }
  }
  df
}

# =========================
# UI
# =========================

ui <- fluidPage(
  title = "COLÀ - Console Online per L'Annotazione",
  
  tags$head(
    tags$title("COLÀ - Console Online per L'Annotazione"),
    tags$style(HTML("
      .drawer {
        position: fixed; right: 0; top: 0; width: 90vw; max-width: 1300px;
        height: 100%; background: #fff; border-left: 1px solid #ddd;
        padding: 18px 22px; overflow-y: auto; z-index: 99999;
      }
      .kwic { font-weight: 700; color: #1565c0; }
      .kwic-box { font-weight: 500; margin: 8px 0 12px 0; line-height: 1.6; }
      .micro-row {
        display: grid; grid-template-columns: 1fr 30px; align-items: center;
        gap: 8px; margin-bottom: 6px; width: 100%;
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
      .note-box { background:#f5f7fa; border-left:4px solid #1565c0; padding:10px 14px; margin-bottom:12px; font-size:0.92em; }
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
    tags$a(href = "[https://laboratorioparole.github.io/cosi/](https://laboratorioparole.github.io/cosi/)", target = "_blank", tags$img(src = "logo.png")),
    h3("COLÀ - Console Online per L'Annotazione (dei segnali discorsivi)")
  ),
  
  div(
    style = "margin-bottom: 20px; max-width: 420px;",
    fileInput("file", "Carica Excel/CSV", buttonLabel = "Sfoglia...", width = "100%"),
    div(
      style = "display: flex; gap: 10px; margin-top: -10px;",
      myDownloadButton("download", "export"),
      actionButton("open_compare", "Confronta Annotazioni (IAA)", class = "btn-info")
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
  
  observeEvent(input$file, {
    ext <- tools::file_ext(input$file$name)
    df <- tryCatch({
      if (ext == "xlsx") read_excel(input$file$datapath)
      else read_csv_robust(input$file$datapath)
    }, error = function(e) NULL)
    
    if (is.null(df)) return()
    df <- as.data.frame(df)
    
    df <- normalize_uploaded_df(df)
    
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
    df <- reconstruct_annotations(df)
    
    df_r(df)
    idx(1)
  })
  
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
          h3(paste0("Dettaglio - Occorrenza ", current_idx, "/", total_n)),
          actionButton("close", "x", style = "border:none; background:none; font-size:22px;")
        ),
        hr(),
        div(
          class = "nav-row",
          actionButton("prev_occ", "\U00002B05 Occorrenza precedente"),
          actionButton("next_occ", "Occorrenza successiva \U000027A1"),
          actionButton("save", "\U0001F4BE salva occorrenza", style = "font-weight:bold;")
        ),
        hr(),
        div(
          class = "material-box",
          div(
            class = "material-left",
            h4("Materiali"),
            if (nzchar(r$`annotation.audio_file`)) tags$a("\U0001F3A7 Audio", href = r$`annotation.audio_file`, target = "_blank"),
            br(),
            if (nzchar(r$`doc.full_conversation`)) tags$a("\U0001F4C4 Conversazione", href = r$`doc.full_conversation`, target = "_blank"),
            br(),
            if (nzchar(r$`doc.full_jefferson`)) tags$a("\U0001F4C4 Jefferson", href = r$`doc.full_jefferson`, target = "_blank"),
            br(),
            tags$a("\U0001F50D Guida alle funzioni", href = "https://laboratorioparole.github.io/cosi/guida-funzioni.html", target = "_blank"),
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
                         div(class = "micro-label", k, span("i", class = "i", title = DEFINIZIONI[[k]])),
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
                         div(class = "micro-label", k, span("i", class = "i", title = DEFINIZIONI[[k]])),
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
                         div(class = "micro-label", k, span("i", class = "i", title = DEFINIZIONI[[k]])),
                         checkboxInput(id, NULL, value = val))
                   })
            )
          )
        ),
        hr(),
        div(
          class = "nav-row",
          actionButton("prev_occ_bottom", "\U00002B05 Occorrenza precedente"),
          actionButton("next_occ_bottom", "Occorrenza successiva \U000027A1"),
          actionButton("save_bottom", "\U0001F4BE salva occorrenza", style = "font-weight:bold;")
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
        column(4, checkboxInput("only_non_empty", "Filtra solo token annotati (SD o Non SD)", value = TRUE))
      ),
      hr(),
      withSpinner(
        uiOutput("iaa_results_ui"),
        type = 6,
        color = "#0d6efd"
      ),
      footer = modalButton("Chiudi")
    ))
  })
  
  # Helper per rimuovere colonne Kappa/AC1 dalle tabelle di dettaglio
  clean_per_label <- function(df) {
    if (is.null(df)) return(NULL)
    cols_to_remove <- grep("kappa|gwet|ac1", names(df), ignore.case = TRUE, value = TRUE)
    if (length(cols_to_remove) > 0) {
      df <- df[, !names(df) %in% cols_to_remove, drop = FALSE]
    }
    return(df)
  }
  
  iaa_data <- reactive({
    req(input$file_ref)
    req(df_r())
    
    withProgress(message = "Calcolo IAA in corso...", value = 0, {
      
      incProgress(0.20, detail = "20% - Caricamento e allineamento dati...")
      
      ext <- tools::file_ext(input$file_ref$name)
      df_ref <- if (tolower(ext) == "xlsx") {
        read_excel(input$file_ref$datapath)
      } else {
        read_csv_robust(input$file_ref$datapath)
      }
      
      df_ref <- as.data.frame(df_ref)
      df_ref <- normalize_uploaded_df(df_ref)
      
      if ("labels" %in% names(df_ref)) {
        df_ref$labels <- gsub(";", "|", df_ref$labels)
        df_ref$labels <- gsub("\\s*\\|\\s*", "|", df_ref$labels)
      }
      
      df_ref <- ensure_col(df_ref, "SD", "")
      df_ref <- ensure_col(df_ref, "labels", "")
      df_ref <- ensure_col(df_ref, "cumulo", 0)
      df_ref <- ensure_bin_cols(df_ref, ALL_KEYS)
      df_ref <- reconstruct_annotations(df_ref)
      
      df_curr <- df_r()
      n <- min(nrow(df_curr), nrow(df_ref))
      
      df1 <- df_curr[1:n, , drop = FALSE]
      df2 <- df_ref[1:n, , drop = FALSE]
      
      df1 <- ensure_bin_cols(df1, ALL_KEYS)
      df2 <- ensure_bin_cols(df2, ALL_KEYS)
      
      incProgress(0.30, detail = "50% - Calcolo metriche di primo livello (SD)...")
      
      if (isTRUE(input$only_non_empty)) {
        sd_mask <- (df1$SD %in% c("SD", "Non SD")) | (df2$SD %in% c("SD", "Non SD"))
      } else {
        sd_mask <- rep(TRUE, n)
      }
      
      df1_sd <- df1[sd_mask, , drop = FALSE]
      df2_sd <- df2[sd_mask, , drop = FALSE]
      
      if (nrow(df1_sd) == 0) return(NULL)
      
      sd1 <- ifelse(df1_sd$SD == "SD", 1, 0)
      sd2 <- ifelse(df2_sd$SD == "SD", 1, 0)
      
      res_sd <- calc_binary_standard(sd1, sd2)
      
      incProgress(0.30, detail = "80% - Calcolo Macro e Microfunzioni...")
      
      both_sd_mask <- (df1$SD == "SD") & (df2$SD == "SD")
      df1_fn <- df1[both_sd_mask, , drop = FALSE]
      df2_fn <- df2[both_sd_mask, , drop = FALSE]
      
      n_fn <- nrow(df1_fn)
      res_micro <- NULL
      res_macro <- NULL
      res_poly <- NULL
      per_label <- NULL
      macro_detail <- NULL
      
      if (n_fn > 0) {
        keys_id <- sapply(ALL_KEYS, safe_id)
        
        m1 <- as.matrix(df1_fn[, keys_id, drop = FALSE])
        m2 <- as.matrix(df2_fn[, keys_id, drop = FALSE])
        storage.mode(m1) <- "numeric"
        storage.mode(m2) <- "numeric"
        
        m1_macro <- cbind(
          Interazionali = (rowSums(m1[, safe_id(INTERAZIONALI), drop = FALSE]) > 0) * 1,
          Metatestuali   = (rowSums(m1[, safe_id(METATESTUALI), drop = FALSE]) > 0) * 1,
          Cognitive     = (rowSums(m1[, safe_id(COGNITIVE), drop = FALSE]) > 0) * 1
        )
        
        m2_macro <- cbind(
          Interazionali = (rowSums(m2[, safe_id(INTERAZIONALI), drop = FALSE]) > 0) * 1,
          Metatestuali   = (rowSums(m2[, safe_id(METATESTUALI), drop = FALSE]) > 0) * 1,
          Cognitive     = (rowSums(m2[, safe_id(COGNITIVE), drop = FALSE]) > 0) * 1
        )
        
        res_macro <- calc_multilabel_matching(m1_macro, m2_macro, c("Interazionali", "Metatestuali", "Cognitive"))
        res_micro <- calc_multilabel_matching(m1, m2, ALL_KEYS)
        
        per_label <- clean_per_label(per_label_df(m1, m2, ALL_KEYS))
        macro_detail <- clean_per_label(per_label_df(
          m1_macro, m2_macro, c("Interazionali", "Metatestuali", "Cognitive")
        ))
        
        res_poly <- calc_polyfunctionality(m1, m2)
      }
      
      incProgress(0.20, detail = "100% - Generazione tabelle completata!")
      
      list(
        n_sd = nrow(df1_sd),
        n_fn = n_fn,
        res_sd = res_sd,
        res_macro = res_macro,
        res_micro = res_micro,
        macro_detail = macro_detail,
        per_label = per_label,
        res_poly = res_poly
      )
    })
  })
  
  output$iaa_results_ui <- renderUI({
    d <- iaa_data()
    if (is.null(d)) return(p("Nessun token disponibile per il confronto con i filtri selezionati."))
    
    tt_po_sd <- "Accordo osservato sull'identificazione SD vs Non SD."
    tt_exact <- "Percentuale di token non vuoti in cui i due annotatori hanno assegnato esattamente lo stesso insieme di funzioni."
    tt_soft <- "Percentuale di token non vuoti in cui i due annotatori condividono almeno una funzione."
    tt_jaccard <- "Media del Jaccard per token: intersezione delle funzioni divisa per l'unione."
    tt_alpha_exact <- "Krippendorff Alpha applicato alla distanza set-valued Exact."
    tt_alpha_soft <- "Krippendorff Alpha applicato a una distanza Soft."
    tt_alpha_jaccard <- "Krippendorff Alpha con distanza 1 - Jaccard."
    
    make_matching_table <- function(res) {
      if (is.null(res)) return(p("Nessun token SD condiviso disponibile per il calcolo delle funzioni."))
      
      tags$table(
        class = "table table-bordered",
        tags$thead(
          tags$tr(
            tags$th("Tipo Matching"),
            tags$th(lbl_info("Accordo / Similarità (%)", "Misura non corretta per l'accordo casuale.")),
            tags$th(lbl_info("Krippendorff Alpha", "Alpha con distanza set-valued."))
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(lbl_info(strong("Exact-Match"), tt_exact)),
            tags$td(sprintf("%.2f%%", res$exact$po)),
            tags$td(lbl_info(round(res$exact$alpha, 3), tt_alpha_exact))
          ),
          tags$tr(
            tags$td(lbl_info(strong("Soft-Match"), tt_soft)),
            tags$td(sprintf("%.2f%%", res$soft$po)),
            tags$td(lbl_info(round(res$soft$alpha, 3), tt_alpha_soft))
          ),
          tags$tr(
            tags$td(lbl_info(strong("Jaccard-Match"), tt_jaccard)),
            tags$td(sprintf("%.2f%%", res$jaccard$po)),
            tags$td(lbl_info(round(res$jaccard$alpha, 3), tt_alpha_jaccard))
          )
        )
      )
    }
    
    make_poly_table <- function(p) {
      if (is.null(p)) return(p("Nessun dato disponibile."))
      
      tags$table(
        class = "table table-bordered",
        tags$thead(
          tags$tr(tags$th("Misura"), tags$th("Annotatore 1"), tags$th("Annotatore 2"))
        ),
        tags$tbody(
          tags$tr(tags$td("Media funzioni per token"), tags$td(sprintf("%.2f", p$mean_1)), tags$td(sprintf("%.2f", p$mean_2))),
          tags$tr(tags$td("Mediana funzioni per token"), tags$td(sprintf("%.1f", p$median_1)), tags$td(sprintf("%.1f", p$median_2))),
          tags$tr(tags$td("% token senza funzioni"), tags$td(sprintf("%.1f%%", p$zero_1)), tags$td(sprintf("%.1f%%", p$zero_2))),
          tags$tr(tags$td("% token monofunzionali"), tags$td(sprintf("%.1f%%", p$mono_1)), tags$td(sprintf("%.1f%%", p$mono_2))),
          tags$tr(tags$td("% token polifunzionali (≥2 funzioni)"), tags$td(sprintf("%.1f%%", p$poly_1)), tags$td(sprintf("%.1f%%", p$poly_2)))
        )
      )
    }
    
    make_poly_agreement_table <- function(p) {
      if (is.null(p)) return(NULL)
      
      tags$table(
        class = "table table-bordered", style = "width:auto;",
        tags$thead(
          tags$tr(tags$th("Confronto della cardinalità"), tags$th("Percentuale"))
        ),
        tags$tbody(
          tags$tr(tags$td("Stesso numero di funzioni"), tags$td(sprintf("%.1f%%", p$same_cardinality))),
          tags$tr(tags$td("Differenza di 1 funzione"), tags$td(sprintf("%.1f%%", p$diff_1))),
          tags$tr(tags$td("Differenza ≥2 funzioni"), tags$td(sprintf("%.1f%%", p$diff_2plus))),
          tags$tr(tags$td("Differenza assoluta media"), tags$td(sprintf("%.2f", p$mean_abs_difference)))
        )
      )
    }
    
    tagList(
      div(
        class = "note-box",
        tags$p(strong("Riepilogo token per il confronto:")),
        tags$ul(
          tags$li(strong("Stadio 1 — Identificazione SD: "), d$n_sd, " token valutati."),
          tags$li(strong("Stadio 2 — Funzioni: "), d$n_fn, " token in cui entrambi gli annotatori hanno assegnato SD = 'SD'.")
        ),
        tags$small("Per le funzioni, i token classificati come Non SD da almeno uno degli annotatori sono esclusi.")
      ),
      
      tabsetPanel(
        tabPanel(
          "1. Livello SD", br(),
          h5("Accordo sull'identificazione del Segnale Discorsivo (SD vs Non SD)"),
          tags$table(
            class = "table table-bordered", style = "width:auto;",
            tags$tr(tags$td(lbl_info("Accordo Osservato (%):", tt_po_sd)), tags$td(sprintf("%.2f%%", d$res_sd$po))),
            tags$tr(tags$td("Indice Jaccard (%):"), tags$td(ifelse(is.na(d$res_sd$jaccard), "—", sprintf("%.2f%%", d$res_sd$jaccard)))),
            tags$tr(tags$td("Cohen's Kappa:"), tags$td(ifelse(is.na(d$res_sd$kappa), "—", round(d$res_sd$kappa, 3)))),
            tags$tr(tags$td("Gwet's AC1:"), tags$td(ifelse(is.na(d$res_sd$ac1), "—", round(d$res_sd$ac1, 3)))),
            tags$tr(tags$td("Krippendorff's Alpha:"), tags$td(ifelse(is.na(d$res_sd$alpha), "—", round(d$res_sd$alpha, 3))))
          )
        ),
        
        tabPanel(
          "2. Macrofunzioni", br(),
          h5("Score complessivo per le Macrofunzioni"),
          p("Le tre macrofunzioni sono considerate come un problema multi-label."),
          make_matching_table(d$res_macro), br(),
          h5("Dettaglio per singola macrofunzione"),
          renderTable_iaa(d$macro_detail)
        ),
        
        tabPanel(
          "3. Microfunzioni", br(),
          h5("Score complessivo per le Microfunzioni"),
          make_matching_table(d$res_micro), br(),
          h5("Dettaglio per singola microfunzione"),
          renderTable_iaa(d$per_label)
        ),
        
        tabPanel(
          "4. Polifunzionalità", br(),
          h5("Distribuzione del numero di funzioni assegnate"),
          make_poly_table(d$res_poly), br(),
          h5("Accordo sulla cardinalità dell'annotazione"),
          make_poly_agreement_table(d$res_poly)
        )
      )
    )
  })
  
  output$download <- downloadHandler(
    filename = function() { "annotato.csv" },
    content = function(file) {
      write.csv(df_r(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
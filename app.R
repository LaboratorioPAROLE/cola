library(shiny)
library(readxl)
library(writexl)
library(DT)
library(shinycssloaders)
library(shinyFiles)

# =========================
# IMPOSTAZIONI
# =========================

# Alla prima apertura di un file ne crea una copia di sicurezza "nomefile.xlsx.bak"
# accanto all'originale (una sola volta, mai sovrascritta). Metti FALSE per disattivarla.
BACKUP_PRIMA_APERTURA <- TRUE

# Dove l'app ricorda i file recenti (cartella utente, non quella dell'app)
CONFIG_DIR   <- tools::R_user_dir("cola", which = "config")
RECENTI_PATH <- file.path(CONFIG_DIR, "recenti.rds")
MAX_RECENTI  <- 10

# Nome della colonna con il numero univoco di riga (viene creata se manca)
ID_COL <- "id_riga"

# I test definiti dagli annotatori sono colonne del file il cui nome inizia con questo prefisso
# (es. "TEST: omissibile"). Valori: 1 = spuntato, 0 = non spuntato.
TEST_PREFIX <- "TEST: "
test_cols <- function(df) if (is.null(df)) character(0) else grep(paste0("^", TEST_PREFIX), names(df), value = TRUE)
nome_test <- function(col) sub(paste0("^", TEST_PREFIX), "", col)
a_binario <- function(v) {
  x <- toupper(trimws(as.character(v)))
  as.numeric(!is.na(x) & x %in% c("1", "1.0", "TRUE", "VERO", "X", "SI", "SÌ", "S"))
}

# =========================
# UTILS & HELPERS
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
ALL_IDS  <- safe_id(ALL_KEYS)

GRUPPI_MACRO <- list(Interazionali = INTERAZIONALI, Metatestuali = METATESTUALI, Cognitive = COGNITIVE)
MACRO_DI <- setNames(rep(names(GRUPPI_MACRO), lengths(GRUPPI_MACRO)), ALL_IDS)

# Matrice logica righe x 3 macrofunzioni (TRUE se la riga ha almeno una micro di quel gruppo)
macro_matrice <- function(df) {
  m <- as.matrix(df[, ALL_IDS, drop = FALSE])
  storage.mode(m) <- "numeric"
  out <- do.call(cbind, lapply(GRUPPI_MACRO, function(g) rowSums(m[, safe_id(g), drop = FALSE] == 1, na.rm = TRUE) > 0))
  if (is.null(dim(out))) out <- matrix(out, nrow = nrow(df), dimnames = list(NULL, names(GRUPPI_MACRO)))
  out
}

DEFINIZIONI <- list(
  "Presa di turno" = 'Segnala che un/a parlante sta iniziando il proprio turno. Es: "allora...", "dunque..."',
  "Interruzione" = 'Serve ad interrompere un/a parlante per iniziare il proprio turno. Es: "scusa...", "no...", "pero..."',
  "Cessione del turno" = "E' usato dal/la parlante per richiedere risposta all'interlocutore/trice o per chiudere il proprio turno. Es: \"prego\", \"va bene?\", \"no?\"",
  "Marcatura della conoscenza condivisa" = "E' usato dal/la parlante per riconoscere o per assicurarsi che l'altra persona e' a conoscenza di una certa informazione. Es: \"sai\", \"non so se mi spiego\"",
  "Richiesta di accordo/conferma" = 'Serve al/la parlante per chiedere all\'interlocutore/trice una conferma, di attenzione o di accordo. Es: "capito?", "no?"',
  "Accordo" = 'Serve al/la parlante per segnalare all\'interlocutore/trice che è d\'accordo con quanto detto prima. Es: "sisi", "eh direi!"',
  "Richiesta di attenzione" = 'Serve al/la parlante per richiamare l\'attenzione su cio\' che sta per dire. Es: "senti...", "guarda..."',
  "Conferma dell'attenzione" = 'Segnala che un/a parlante segue ancora il discorso e/o incoraggia l\'interlocutore/trice a continuare. Es: "mhmh", "certo"',
  "Strategia di cortesia" = 'Serve al/la parlante come strategia di mantenimento della faccia. Es: "gentilmente", "scusa"',
  "Introduzione o ripresa del topic" = 'E\' usato dal/la parlante per introdurre o riprendere un topic. Es: "quindi...", "dopodiche..."',
  "Chiusura del topic" = 'E\' usato dal/la parlante per chiudere un argomento. Es: "ok", "bene", "bon"',
  "Prolettico" = 'Serve al/la parlante per segnalare una formulazione che ritiene adeguata di cio\' che sta per dire. Es: "ecco...", "direi..."',
  "Riformulazione" = 'Viene usato per riformulare quanto appena detto, che sia per ragioni stilistiche, di precisione o di cortesia. Es: "cioè nel senso...", "dico..."',
  "Citazione/Discorso Riportato" = 'E\' usato per introdurre qualcosa detto da qualcun altro/a. Es: "mi fa...", "ah..."',
  "Esemplificazione" = 'Introduce un esempio o parafrasa rispetto a un elemento presente nel testo. Es: "mettiamo", "tipo"',
  "Filler" = 'Viene usato dal/la parlante come pausa piena per coprire i tempi di formulazione. Es: "quindi direi tipo", "mh diciamo"',
  "Marca di generalizzazione" = 'E\' un modo di rimandare a una categoria piu\' ampia che il/la parlante dà per condivisa. Es: "eccetera", "capito no"',
  "Marcatura dell'inferenza" = 'Serve per segnalare che il/la parlante è arrivato/a a una conclusione per deduzione. Es: "allora", "quindi", "insomma"',
  "Modulazione del grado di confidenza" = 'Serve per segnalare che il/la parlante non è completamente sicuro/a di cio\' che sostiene. Es: "mi pare", "credo"',
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

# Prepara un data frame letto da file
prepara_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- normalize_uploaded_df(df)
  if ("labels" %in% names(df)) {
    df$labels <- as.character(df$labels)
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
  for (col in test_cols(df)) df[[col]] <- a_binario(df[[col]])
  df <- reconstruct_annotations(df)
  assegna_id(df)
}

# Numero univoco di riga: se manca viene creato (1, 2, 3...) come prima colonna;
# se c'è, le righe senza numero ricevono i numeri successivi al massimo.
# Le righe vengono sempre rimesse in ordine di ID, così tornano al loro posto
# anche se il file è stato riordinato in Excel.
assegna_id <- function(df) {
  if (!ID_COL %in% names(df)) {
    df <- data.frame(seq_len(nrow(df)), df, check.names = FALSE, stringsAsFactors = FALSE)
    names(df)[1] <- ID_COL
  } else {
    id <- suppressWarnings(as.integer(df[[ID_COL]]))
    mancanti <- is.na(id)
    if (any(mancanti)) {
      base <- if (all(mancanti)) 0L else max(id, na.rm = TRUE)
      id[mancanti] <- base + seq_len(sum(mancanti))
    }
    df[[ID_COL]] <- id
    df <- df[order(id), , drop = FALSE]
  }
  rownames(df) <- NULL
  df
}

# =========================
# LETTURA / SCRITTURA FILE SU DISCO
# =========================

leggi_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) read_excel(path) else read_csv_robust(path)
}

# Scrive prima su un file temporaneo e poi sostituisce l'originale:
# se qualcosa va storto a metà, il file originale resta intatto.
scrivi_file <- function(df, path) {
  ext <- tolower(tools::file_ext(path))
  tmp <- tempfile(fileext = paste0(".", ext))
  on.exit(unlink(tmp), add = TRUE)
  
  if (ext == "xlsx") {
    write_xlsx(df, tmp)
  } else {
    sep <- detect_csv_sep(path)
    dec <- if (sep == ";") "," else "."
    write.table(df, tmp, sep = sep, dec = dec, row.names = FALSE, col.names = TRUE,
                qmethod = "double", fileEncoding = "UTF-8", na = "")
  }
  ok <- file.copy(tmp, path, overwrite = TRUE)
  if (!isTRUE(ok)) stop("impossibile sovrascrivere il file")
  invisible(TRUE)
}

# =========================
# FILE RECENTI
# =========================

leggi_recenti <- function() {
  r <- tryCatch(readRDS(RECENTI_PATH), error = function(e) NULL)
  if (is.null(r) || !is.data.frame(r)) {
    return(data.frame(path = character(0), idx = integer(0), stringsAsFactors = FALSE))
  }
  r[file.exists(r$path), , drop = FALSE]
}

salva_recenti <- function(r) {
  dir.create(CONFIG_DIR, recursive = TRUE, showWarnings = FALSE)
  tryCatch(saveRDS(head(r, MAX_RECENTI), RECENTI_PATH), error = function(e) NULL)
}

segna_recente <- function(path, idx = 1L) {
  r <- leggi_recenti()
  r <- r[r$path != path, , drop = FALSE]
  r <- rbind(data.frame(path = path, idx = as.integer(idx), stringsAsFactors = FALSE), r)
  salva_recenti(r)
}

aggiorna_idx_recente <- function(path, idx) {
  r <- leggi_recenti()
  if (path %in% r$path) {
    r$idx[r$path == path] <- as.integer(idx)
    salva_recenti(r)
  }
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
        padding: 18px 22px; overflow-y: auto; z-index: 1040;
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
      .nav-row { display: flex; gap: 10px; margin-bottom: 10px; align-items: center; }
      .autosave-note { color: #2e7d32; font-size: 0.9em; margin-left: auto; }
      .file-bar { display: flex; gap: 10px; align-items: center; flex-wrap: wrap; margin-bottom: 8px; }
      .file-corrente { font-size: 0.9em; color: #555; }
      .stato-salvataggio { font-size: 0.85em; color: #2e7d32; }
      .recenti { max-width: 700px; }
      .recenti h4 { margin-top: 10px; }
      .recente { display: block; padding: 8px 12px; border: 1px solid #e3e3e3; border-radius: 6px;
                 margin-bottom: 6px; cursor: pointer; text-decoration: none !important; color: inherit; }
      .recente:hover { background: #f5f7fa; border-color: #1565c0; }
      .recente .nome { font-weight: 600; color: #1565c0; }
      .recente .cartella { font-size: 0.85em; color: #777; }
      table.dataTable tbody td { vertical-align: top; }
      #table td.col-sd { white-space: nowrap; }
      #table td.col-commento { white-space: normal; word-break: break-word; font-size: 0.92em; }
      .pos-occ { font-size:0.6em; color:#777; font-weight:normal; }
      .test-box { margin: 4px 0 10px 0; }
      .test-box h5 { margin: 0 0 4px 0; font-weight: 600; }
      .test-row label { font-weight: normal; margin: 0 0 3px 0; cursor: pointer; }
      .test-row input { margin-right: 6px; }
      #table td.col-test { text-align: center; color: #1565c0; font-weight: 700; }
      #table th.col-test { white-space: normal; }
      .test-aggiungi { display: flex; gap: 8px; align-items: center; max-width: 640px; }
      .test-aggiungi .form-group { flex: 1; margin-bottom: 0; }
      .note-box { background:#f5f7fa; border-left:4px solid #1565c0; padding:10px 14px; margin-bottom:12px; font-size:0.92em; }
    "))
  ),
  
  div(
    class = "title-wrap",
    tags$a(href = "https://laboratorioparole.github.io/cosi/", target = "_blank", class = "link-esterno", tags$img(src = "logo.png")),
    h3("COLÀ - Console Online per L'Annotazione (dei segnali discorsivi)")
  ),
  
  div(
    class = "file-bar",
    shinyFilesButton("apri", "\U0001F4C2 Apri file…", "Scegli il file da annotare (Excel o CSV)",
                     multiple = FALSE, class = "btn-primary"),
    actionButton("open_compare", "Confronta Annotazioni (IAA)", class = "btn-info"),
    actionButton("chiudi_file", "Chiudi file")
  ),
  uiOutput("file_corrente"),
  
  hr(),
  uiOutput("recenti_ui"),
  conditionalPanel(
    condition = "output.file_aperto",
    tabsetPanel(
      id = "schede",
      tabPanel("Occorrenze", br(), DTOutput("table")),
      tabPanel("Statistiche", br(), uiOutput("statistiche")),
      tabPanel(
        "Test", br(),
        p("I test sono criteri da verificare per decidere se un'occorrenza è SD o Non SD ",
          "(per esempio «omissibile senza cambiare il senso»). Definiscili qui prima di annotare: ",
          "compariranno come caselle da spuntare nel pannello di ogni occorrenza e come colonne nella tabella ",
          "e nel file (con il prefisso «", TEST_PREFIX, "»)."),
        div(
          class = "test-aggiungi",
          textInput("nuovo_test", NULL, placeholder = "Nuovo test, es. omissibile senza cambiare il senso", width = "100%"),
          actionButton("aggiungi_test", "Aggiungi test")
        ),
        br(),
        uiOutput("test_ui")
      )
    )
  ),
  uiOutput("drawer"),
  
  tags$script(HTML("
    // Ogni modifica fatta a mano nel pannello invia lo stato completo dell'occorrenza.
    // Lo stato viene letto subito (con il numero di occorrenza giusto), quindi anche
    // se si passa all'occorrenza successiva l'ultima modifica non si perde.
    const timers = {};
    function statoDrawer() {
      const d = $('.drawer');
      if (!d.length) return null;
      return {
        idx: parseInt(d.attr('data-idx'), 10),
        sd: d.find('input[name=sd]:checked').val() || '',
        commenti: $('#commenti').val() || '',
        cumulo: $('#cumulo').is(':checked'),
        cumulo_testo: $('#cumulo_testo').val() || '',
        funzioni: d.find('.micro-row input[type=checkbox]:checked').map(function () { return this.id; }).get(),
        test: d.find('input.test-input').map(function () { return { col: $(this).attr('data-col'), v: this.checked }; }).get(),
        t: Date.now()
      };
    }
    function programma(ritardo) {
      const stato = statoDrawer();
      if (!stato || isNaN(stato.idx)) return;
      clearTimeout(timers[stato.idx]);
      timers[stato.idx] = setTimeout(() => {
        delete timers[stato.idx];
        Shiny.setInputValue('edit', stato, { priority: 'event' });
      }, ritardo);
    }
    // Clic su SD / cumulo / funzioni: salvataggio immediato
    $(document).on('change', '.drawer input[type=checkbox], .drawer input[type=radio]', function (e) {
      if (!e.originalEvent) return;   // ignora gli aggiornamenti fatti dal server
      programma(0);
    });
    // Testo (commenti, cumulo): salvataggio dopo una breve pausa di digitazione
    $(document).on('input', '.drawer textarea', function () { programma(500); });

    // Invio nel campo del nuovo test = Aggiungi
    $(document).on('keyup', '#nuovo_test', function (e) { if (e.key === 'Enter') $('#aggiungi_test').click(); });

    // Solo i link con classe 'link-esterno' (logo, audio, conversazione, Jefferson)
    // vengono aperti da R nel browser predefinito (o, per i file locali, con il
    // programma predefinito). Tutti gli altri link si comportano come prima.
    $(document).on('click', 'a.link-esterno[href]', function (e) {
      const href = $(this).attr('href');
      if (!href || href.startsWith('#') || href.startsWith('javascript:')) return;
      e.preventDefault();
      Shiny.setInputValue('apri_link', { url: this.href || href, t: Date.now() }, { priority: 'event' });
    });
  "))
)

# =========================
# SERVER
# =========================

server <- function(input, output, session) {
  
  df_r <- reactiveVal(NULL)
  idx <- reactiveVal(1)
  drawer <- reactiveVal(FALSE)
  percorso <- reactiveVal(NULL)       # percorso del file aperto
  ultimo_salvataggio <- reactiveVal(NULL)
  tabella_ver <- reactiveVal(0)       # ridisegna la tabella solo quando serve
  recenti_ver <- reactiveVal(0)
  
  aggiorna_tabella <- function() tabella_ver(isolate(tabella_ver()) + 1)
  aggiorna_recenti <- function() recenti_ver(isolate(recenti_ver()) + 1)
  
  # ---------- Apertura file ----------
  
  apri_file <- function(path) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (!file.exists(path)) {
      showNotification("Il file non esiste più in quella posizione.", type = "error")
      aggiorna_recenti()
      return(invisible(FALSE))
    }
    
    df <- tryCatch(leggi_file(path), error = function(e) NULL)
    if (is.null(df) || NROW(df) == 0) {
      showNotification("Impossibile leggere il file.", type = "error")
      return(invisible(FALSE))
    }
    
    # I vecchi .xls non si possono riscrivere: si salva accanto un .xlsx con lo stesso nome
    if (tolower(tools::file_ext(path)) == "xls") {
      path <- sub("\\.xls$", ".xlsx", path, ignore.case = TRUE)
      showNotification("I file .xls vengono salvati come .xlsx con lo stesso nome.", type = "warning", duration = 8)
    } else if (BACKUP_PRIMA_APERTURA) {
      bak <- paste0(path, ".bak")
      if (!file.exists(bak)) file.copy(path, bak)
    }
    
    aveva_id <- ID_COL %in% names(df)
    df <- prepara_df(df)
    if (anyDuplicated(df[[ID_COL]])) {
      showNotification(paste0("Attenzione: nella colonna '", ID_COL, "' ci sono numeri ripetuti."),
                       type = "warning", duration = 10)
    }
    
    rec <- leggi_recenti()
    i <- rec$idx[rec$path == path]
    i <- if (length(i) == 1 && !is.na(i)) min(max(1L, i), nrow(df)) else 1L
    
    drawer(FALSE)
    percorso(path)
    df_r(df)
    idx(i)
    ultimo_salvataggio(NULL)
    segna_recente(path, i)
    aggiorna_recenti()
    aggiorna_tabella()
    # Se il file non aveva ancora i numeri di riga, li scrive subito
    if (!aveva_id) salva_su_file(df)
    invisible(TRUE)
  }
  
  volumi <- c(Utente = normalizePath(Sys.getenv("USERPROFILE", path.expand("~")), winslash = "/"),
              getVolumes()())
  shinyFileChoose(input, "apri", roots = volumi, filetypes = c("xlsx", "xls", "csv", "txt", "tsv"))
  
  observeEvent(input$apri, {
    f <- parseFilePaths(volumi, input$apri)
    if (nrow(f) == 0) return()
    apri_file(as.character(f$datapath[1]))
  })
  
  # ---------- Link esterni: browser predefinito del sistema ----------
  observeEvent(input$apri_link, {
    url <- input$apri_link$url
    if (is.null(url) || !nzchar(url)) return()
    # Link a un file locale (es. audio): apri il file con il programma predefinito
    if (grepl("^file:", url, ignore.case = TRUE)) {
      url <- utils::URLdecode(sub("^file:/*", "", url, ignore.case = TRUE))
      if (.Platform$OS.type != "windows") url <- paste0("/", url)
    }
    # Un indirizzo che punta al server interno dell'app (link relativo) è un file locale
    if (grepl("^https?://(127\\.0\\.0\\.1|localhost)", url, ignore.case = TRUE)) {
      showNotification("Questo link punta a un file interno all'app e non può essere aperto nel browser.", type = "warning")
      return()
    }
    ok <- tryCatch({ utils::browseURL(url); TRUE }, error = function(e) FALSE)
    if (!ok) showNotification(paste("Impossibile aprire:", url), type = "error")
  })
  
  observeEvent(input$apri_recente, {
    rec <- leggi_recenti()
    i <- as.integer(input$apri_recente)
    if (!is.na(i) && i >= 1 && i <= nrow(rec)) apri_file(rec$path[i])
  })
  
  observeEvent(input$chiudi_file, {
    drawer(FALSE)
    df_r(NULL)
    percorso(NULL)
    idx(1)
    aggiorna_recenti()
    aggiorna_tabella()
  })
  
  output$file_aperto <- reactive(!is.null(df_r()))
  outputOptions(output, "file_aperto", suspendWhenHidden = FALSE)
  
  # ---------- Schermata iniziale: file recenti ----------
  
  output$recenti_ui <- renderUI({
    recenti_ver()
    if (!is.null(df_r())) return(NULL)
    rec <- leggi_recenti()
    if (nrow(rec) == 0) {
      return(div(class = "recenti", p("Apri un file Excel o CSV per iniziare. Le annotazioni vengono salvate direttamente nel file.")))
    }
    div(
      class = "recenti",
      h4("File recenti"),
      lapply(seq_len(nrow(rec)), function(i) {
        tags$a(
          class = "recente",
          onclick = sprintf("Shiny.setInputValue('apri_recente', %d, {priority: 'event'})", i),
          div(class = "nome", basename(rec$path[i])),
          div(class = "cartella", dirname(rec$path[i]), " · occorrenza ", rec$idx[i])
        )
      })
    )
  })
  
  output$file_corrente <- renderUI({
    p <- percorso()
    if (is.null(p)) return(NULL)
    t <- ultimo_salvataggio()
    div(
      span(class = "file-corrente", "\U0001F4C4 ", p),
      if (!is.null(t)) span(class = "stato-salvataggio", " — salvato alle ", format(t, "%H:%M:%S"))
    )
  })
  
  # ---------- Ogni modifica viene scritta subito nel file ----------
  
  errore_mostrato <- FALSE
  salva_su_file <- function(df) {
    p <- isolate(percorso())
    if (is.null(p)) return()
    ok <- tryCatch({ scrivi_file(df, p); TRUE }, error = function(e) FALSE)
    if (ok) {
      ultimo_salvataggio(Sys.time())
      errore_mostrato <<- FALSE
    } else if (!errore_mostrato) {
      errore_mostrato <<- TRUE
      showNotification(
        "Impossibile salvare nel file. È aperto in Excel? Chiudilo: il salvataggio riprenderà alla prossima modifica.",
        type = "error", duration = NULL
      )
    }
  }
  
  observeEvent(input$edit, {
    s <- input$edit
    df <- df_r()
    req(df)
    i <- suppressWarnings(as.integer(s$idx))
    if (length(i) != 1 || is.na(i) || i < 1 || i > nrow(df)) return()
    
    sd <- if (is.null(s$sd)) "" else as.character(s$sd)
    funzioni_spuntate <- as.character(unlist(s$funzioni))
    attive <- if (sd == "SD") funzioni_spuntate else character(0)
    cumulo <- isTRUE(s$cumulo)
    
    df$SD[i] <- sd
    df$commenti[i] <- if (is.null(s$commenti)) "" else as.character(s$commenti)
    df$cumulo[i] <- as.numeric(cumulo)
    df$cumulo_testo[i] <- if (cumulo && !is.null(s$cumulo_testo)) as.character(s$cumulo_testo) else ""
    for (id in ALL_IDS) df[[id]][i] <- as.numeric(id %in% attive)
    df$labels[i] <- paste(ALL_KEYS[ALL_IDS %in% attive], collapse = "|")
    tt <- s$test
    if (is.data.frame(tt)) tt <- split(tt, seq_len(nrow(tt)))
    for (x in tt) {
      col <- as.character(x$col)
      if (length(col) == 1 && col %in% test_cols(df)) df[[col]][i] <- as.numeric(isTRUE(as.logical(x$v)))
    }
    df_r(df)
    salva_su_file(df)
    
    # Se l'occorrenza non è SD, togli le spunte anche a schermo
    if (sd != "SD" && length(funzioni_spuntate) > 0 && i == idx()) {
      for (id in ALL_IDS) updateCheckboxInput(session, id, value = FALSE)
    }
    # Se il cumulo è stato tolto, svuota anche il campo di testo
    if (!cumulo && i == idx()) updateTextAreaInput(session, "cumulo_testo", value = "")
  })
  
  # Ricorda su quale occorrenza eri, per riprendere da lì
  observeEvent(idx(), {
    p <- percorso()
    if (!is.null(p)) aggiorna_idx_recente(p, idx())
  })
  
  # ---------- Test definiti dagli annotatori ----------
  
  # Cambia l'elenco dei test (colonne): salva il file e ridisegna tabella e pannello
  applica_test <- function(df) {
    df_r(df)
    salva_su_file(df)
    aggiorna_tabella()
    if (isTRUE(isolate(drawer()))) { drawer(FALSE); drawer(TRUE) }
  }
  
  valida_nome_test <- function(nome, df, escludi = NULL) {
    nome <- trimws(gsub("[\r\n\t]+", " ", nome))
    if (!nzchar(nome)) return(list(ok = FALSE, msg = "Scrivi il nome del test."))
    esistenti <- setdiff(tolower(nome_test(test_cols(df))), tolower(escludi))
    if (tolower(nome) %in% esistenti) return(list(ok = FALSE, msg = "Esiste già un test con questo nome."))
    if (paste0(TEST_PREFIX, nome) %in% names(df) && !identical(nome, escludi))
      return(list(ok = FALSE, msg = "Nel file esiste già una colonna con questo nome."))
    list(ok = TRUE, nome = nome)
  }
  
  observeEvent(input$aggiungi_test, {
    df <- df_r()
    if (is.null(df)) {
      showNotification("Apri prima un file: i test vengono salvati come colonne del file.", type = "warning")
      return()
    }
    v <- valida_nome_test(input$nuovo_test, df)
    if (!v$ok) { showNotification(v$msg, type = "warning"); return() }
    df[[paste0(TEST_PREFIX, v$nome)]] <- 0
    updateTextInput(session, "nuovo_test", value = "")
    applica_test(df)
  })
  
  observeEvent(input$rinomina_test, {
    col <- input$rinomina_test$col
    req(col %in% test_cols(df_r()))
    showModal(modalDialog(
      title = "Rinomina test",
      textInput("rinomina_test_nome", NULL, value = nome_test(col), width = "100%"),
      footer = tagList(modalButton("Annulla"), actionButton("rinomina_test_ok", "Rinomina", class = "btn-primary"))
    ))
    test_in_modifica(col)
  })
  test_in_modifica <- reactiveVal(NULL)
  observeEvent(input$rinomina_test_ok, {
    df <- df_r(); col <- test_in_modifica()
    req(df, col %in% names(df))
    v <- valida_nome_test(input$rinomina_test_nome, df, escludi = nome_test(col))
    if (!v$ok) { showNotification(v$msg, type = "warning"); return() }
    removeModal()
    names(df)[names(df) == col] <- paste0(TEST_PREFIX, v$nome)
    applica_test(df)
  })
  
  observeEvent(input$elimina_test, {
    col <- input$elimina_test$col
    df <- df_r()
    req(col %in% test_cols(df))
    n <- sum(df[[col]] == 1)
    test_in_modifica(col)
    showModal(modalDialog(
      title = "Eliminare il test?",
      sprintf("Il test «%s» verrà tolto dal file%s.", nome_test(col),
              if (n > 0) sprintf(", insieme alle sue %d spunte", n) else ""),
      footer = tagList(modalButton("Annulla"), actionButton("elimina_test_ok", "Elimina", class = "btn-danger"))
    ))
  })
  observeEvent(input$elimina_test_ok, {
    df <- df_r(); col <- test_in_modifica()
    req(df, col %in% names(df))
    removeModal()
    df[[col]] <- NULL
    applica_test(df)
  })
  
  output$test_ui <- renderUI({
    df <- df_r()
    if (is.null(df)) return(p(class = "text-muted", "Apri un file per definire i test."))
    tc <- test_cols(df)
    if (length(tc) == 0) return(p(class = "text-muted", "Nessun test definito."))
    sd <- as.character(df$SD)
    azione <- function(input_id, col, etichetta, classe = "btn-default") {
      tags$button(
        type = "button", class = paste("btn btn-xs btn-sm", classe), etichetta,
        onclick = sprintf("Shiny.setInputValue('%s', {col: %s, t: Date.now()}, {priority: 'event'})",
                          input_id, as.character(jsonlite::toJSON(col, auto_unbox = TRUE)))
      )
    }
    tags$table(
      class = "table table-striped table-sm", style = "width:auto;",
      tags$thead(tags$tr(
        tags$th("#"), tags$th("Test"), tags$th("Spuntato"), tags$th("di cui SD"), tags$th("di cui Non SD"), tags$th("")
      )),
      tags$tbody(lapply(seq_along(tc), function(k) {
        col <- tc[k]
        si <- df[[col]] == 1
        tags$tr(
          tags$td(k), tags$td(nome_test(col)),
          tags$td(sum(si)), tags$td(sum(si & sd == "SD")), tags$td(sum(si & sd == "Non SD")),
          tags$td(style = "white-space:nowrap;",
                  azione("rinomina_test", col, "Rinomina"), " ",
                  azione("elimina_test", col, "Elimina", "btn-danger"))
        )
      }))
    )
  })
  
  # ---------- Tabella principale ----------
  
  tabella_view <- function(df) {
    base <- data.frame(
      N. = df[[ID_COL]],
      Occorrenza = paste0(df$Left, " <span class='kwic'>", df$KWIC, "</span> ", df$Right),
      SD = df$SD,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    # Colonne dei test: in tabella si vede solo la spunta
    for (col in test_cols(df)) base[[nome_test(col)]] <- ifelse(df[[col]] == 1, "sì", "no")
    base$Funzioni <- gsub("\\|", "; ", df$labels)
    base$Commento <- as.character(df$commenti)
    base
  }
  
  output$table <- renderDT({
    tabella_ver()
    df <- isolate(df_r())
    req(df)
    tc <- test_cols(df)
    nt <- length(tc)
    
    datatable(
      tabella_view(df), escape = FALSE, selection = "single", rownames = FALSE,
      filter = "top",
      options = list(
        scrollY = "600px", paging = FALSE, dom = "ftip", autoWidth = TRUE,
        search = list(caseInsensitive = TRUE, regex = FALSE),
        initComplete = JS(
          "function() {",
          "  var c = $(this.api().table().container());",
          "  c.find('thead input[type=search]').attr('placeholder', 'filtra');",
          sprintf("  [%s].forEach(function (k) { c.find('thead tr').eq(1).children().eq(k).empty(); });",
                  paste(2 + seq_len(nt), collapse = ",")),
          "}"
        ),
        language = list(
          search = "Cerca in tutti i campi:",
          info = "_TOTAL_ occorrenze", infoFiltered = "(filtrate da _MAX_)",
          infoEmpty = "Nessuna occorrenza", zeroRecords = "Nessun risultato"
        ),
        columnDefs = c(
          list(
            # Larghezze: l'occorrenza prende lo spazio, SD e N. il minimo, il commento va a capo
            list(targets = 0, width = "40px"),
            list(targets = 1, width = "55%"),
            list(targets = 2, width = "60px", className = "col-sd"),
            list(targets = 3 + nt, width = "20%"),
            list(targets = 4 + nt, width = "15%", className = "col-commento", render = JS(
              "function(d, t) {",
              "  if (t !== 'display') return d;",
              "  return $('<div>').text(d || '').html();",
              "}"
            ))
          ),
          # Colonne dei test: solo la spunta (si mette e si toglie dal pannello dell'occorrenza)
          lapply(seq_len(nt), function(k) {
            list(targets = 2 + k, width = "70px", className = "col-test", searchable = FALSE, render = JS(
              "function(d, t) { return t === 'display' ? (d === 'sì' ? '\u2713' : '') : d; }"
            ))
          })
        )
      )
    )
  })
  
  # Aggiorna i dati della tabella senza perdere ricerca, filtri e ordinamento
  proxy_tabella <- dataTableProxy("table")
  aggiorna_righe <- function() {
    df <- isolate(df_r())
    if (is.null(df)) return()
    replaceData(proxy_tabella, tabella_view(df), resetPaging = FALSE, rownames = FALSE)
  }
  
  # Ordine delle righe come appare ora in tabella (dopo ricerca, filtri e ordinamento)
  ordine_corrente <- function() {
    n <- nrow(df_r())
    o <- input$table_rows_all
    if (is.null(o) || length(o) == 0) seq_len(n) else o
  }
  
  observeEvent(input$table_rows_selected, {
    idx(input$table_rows_selected)
    drawer(TRUE)
  })
  
  # ---------- Pannello di dettaglio ----------
  
  output$drawer <- renderUI({
    if (!drawer()) return(NULL)
    
    current_idx <- idx()
    df_snapshot <- isolate(df_r())
    req(df_snapshot)
    
    r <- df_snapshot[current_idx, ]
    total_n <- nrow(df_snapshot)
    sd_sel <- if (is.na(r$SD) || !nzchar(as.character(r$SD))) character(0) else as.character(r$SD)
    cumulo_val <- !is.na(r$cumulo) && as.numeric(r$cumulo) == 1
    
    casella <- function(k) {
      id <- safe_id(k)
      val <- r[[id]]
      val <- !is.na(val) && as.numeric(val) == 1
      div(class = "micro-row",
          div(class = "micro-label", k, span("i", class = "i", title = DEFINIZIONI[[k]])),
          checkboxInput(id, NULL, value = val))
    }
    
    tagList(
      div(
        class = "drawer",
        `data-idx` = current_idx,
        div(
          style = "display:flex; justify-content:space-between; align-items:center;",
          {
            o <- isolate(ordine_corrente())
            pos <- match(current_idx, o)
            h3(paste0("Occorrenza n. ", r[[ID_COL]]),
               if (!is.na(pos)) span(class = "pos-occ",
                                     sprintf(" (%d di %d%s)", pos, length(o), if (length(o) < total_n) " nella selezione" else "")))
          },
          actionButton("close", "x", style = "border:none; background:none; font-size:22px;")
        ),
        hr(),
        div(
          class = "nav-row",
          actionButton("prev_occ", "\U00002B05 Occorrenza precedente"),
          actionButton("next_occ", "Occorrenza successiva \U000027A1"),
          span(class = "autosave-note", "\U00002714 Le modifiche vengono salvate nel file automaticamente")
        ),
        hr(),
        div(
          class = "material-box",
          div(
            class = "material-left",
            h4("Materiali"),
            if (nzchar(r$`annotation.audio_file`)) tags$a("\U0001F3A7 Audio", href = r$`annotation.audio_file`, target = "_blank", class = "link-esterno"),
            br(),
            if (nzchar(r$`doc.full_conversation`)) tags$a("\U0001F4C4 Conversazione", href = r$`doc.full_conversation`, target = "_blank", class = "link-esterno"),
            br(),
            if (nzchar(r$`doc.full_jefferson`)) tags$a("\U0001F4C4 Jefferson", href = r$`doc.full_jefferson`, target = "_blank", class = "link-esterno"),
            br(),
            tags$a("\U0001F50D Guida alle funzioni", href = "https://laboratorioparole.github.io/cosi/guida-funzioni.html", target = "_blank"),
            hr(),
            fluidRow(
              column(8,
                     h4("Occorrenza"),
                     div(class = "kwic-box", HTML(paste0(r$Left, " <span class='kwic'>", r$KWIC, "</span> ", r$Right))),
                     {
                       tc <- test_cols(df_snapshot)
                       div(
                         class = "test-box",
                         h5("Test"),
                         if (length(tc) == 0) {
                           tags$small(class = "text-muted", "Nessun test definito: puoi aggiungerli nella scheda «Test».")
                         } else {
                           lapply(tc, function(col) {
                             spuntato <- isTRUE(as.numeric(r[[col]]) == 1)
                             div(class = "test-row", tags$label(
                               tags$input(type = "checkbox", class = "test-input", `data-col` = col,
                                          checked = if (spuntato) NA else NULL),
                               nome_test(col)
                             ))
                           })
                         }
                       )
                     },
                     radioButtons("sd", "SD", choices = c("Non SD", "SD"), selected = sd_sel, inline = TRUE)
              ),
              column(4,
                     checkboxInput("cumulo", "Cumulo", value = cumulo_val),
                     conditionalPanel(
                       condition = "input.cumulo == true",
                       textAreaInput("cumulo_testo", NULL, value = ifelse(is.na(r$cumulo_testo), "", r$cumulo_testo), width = "100%", height = "70px", placeholder = "Elementi in cumulo...")
                     )
              )
            )
          ),
          div(
            class = "material-right",
            textAreaInput("commenti", "Commenti", value = ifelse(is.na(r$commenti), "", r$commenti), width = "100%", height = "90px")
          )
        ),
        
        conditionalPanel(
          condition = "input.sd == 'SD'",
          fluidRow(
            column(4, h4("Interazionali"), lapply(INTERAZIONALI, casella)),
            column(4, h4("Metatestuali"),  lapply(METATESTUALI, casella)),
            column(4, h4("Cognitive"),     lapply(COGNITIVE, casella))
          )
        ),
        hr(),
        div(
          class = "nav-row",
          actionButton("prev_occ_bottom", "\U00002B05 Occorrenza precedente"),
          actionButton("next_occ_bottom", "Occorrenza successiva \U000027A1")
        ),
        hr(),
        actionButton("close_bottom", "chiudi")
      )
    )
  })
  
  chiudi_drawer <- function() {
    drawer(FALSE)
    aggiorna_righe()   # mostra le annotazioni aggiornate, mantenendo ricerca e ordinamento
  }
  observeEvent(input$close, chiudi_drawer())
  observeEvent(input$close_bottom, chiudi_drawer())
  
  # Precedente/successiva seguono l'ordine e la ricerca attuali della tabella
  vai <- function(passo) {
    req(df_r())
    o <- ordine_corrente()
    pos <- match(idx(), o)
    if (is.na(pos)) {
      nuovo <- idx() + passo
      if (nuovo >= 1 && nuovo <= nrow(df_r())) idx(nuovo)
      return()
    }
    np <- pos + passo
    if (np >= 1 && np <= length(o)) idx(o[np])
  }
  vai_prec <- function() vai(-1)
  vai_succ <- function() vai(1)
  observeEvent(input$prev_occ, vai_prec())
  observeEvent(input$prev_occ_bottom, vai_prec())
  observeEvent(input$next_occ, vai_succ())
  observeEvent(input$next_occ_bottom, vai_succ())
  
  # ================= SCHEDA STATISTICHE =================
  output$statistiche <- renderUI({
    df <- df_r()
    req(df)
    
    sd <- as.character(df$SD)
    n_tot <- nrow(df)
    n_sd <- sum(sd == "SD")
    n_nonsd <- sum(sd == "Non SD")
    n_ann <- n_sd + n_nonsd
    
    dsd <- df[sd == "SD", , drop = FALSE]
    m <- as.matrix(dsd[, ALL_IDS, drop = FALSE])
    storage.mode(m) <- "numeric"
    m1 <- !is.na(m) & m == 1
    n_micro <- colSums(m1)
    n_macro <- colSums(macro_matrice(dsd))
    senza_funzione <- sum(rowSums(m1) == 0)
    media_funz <- if (n_sd > 0) mean(rowSums(m1)) else NA
    
    pct <- function(n) if (n_sd == 0) "—" else sprintf("%.1f%%", 100 * n / n_sd)
    
    tabella_freq <- function(etichette, n, gruppo = NULL) {
      ord <- order(-n, etichette)
      tags$table(
        class = "table table-striped table-sm",
        tags$thead(tags$tr(
          tags$th("#"), tags$th("Funzione"),
          if (!is.null(gruppo)) tags$th("Macro"),
          tags$th("Occorrenze"), tags$th("% su SD")
        )),
        tags$tbody(lapply(seq_along(ord), function(k) {
          i <- ord[k]
          tags$tr(
            tags$td(k), tags$td(etichette[i]),
            if (!is.null(gruppo)) tags$td(gruppo[i]),
            tags$td(n[i]), tags$td(pct(n[i]))
          )
        }))
      )
    }
    
    riga <- function(l, v) tags$tr(tags$td(l), tags$td(v))
    
    tagList(
      h4("Riepilogo"),
      tags$table(
        class = "table table-bordered", style = "width:auto;",
        riga("Occorrenze totali", n_tot),
        riga("Annotate (SD o Non SD)", sprintf("%d (%.0f%%)", n_ann, if (n_tot) 100 * n_ann / n_tot else 0)),
        riga("SD", n_sd),
        riga("Non SD", n_nonsd),
        riga("Funzioni medie per SD", if (is.na(media_funz)) "—" else sprintf("%.2f", media_funz)),
        riga("SD senza funzioni assegnate", sprintf("%d (%s)", senza_funzione, pct(senza_funzione)))
      ),
      tags$small(class = "text-muted",
                 "Le funzioni sono contate sulle occorrenze annotate come SD. Un'occorrenza può avere più funzioni, ",
                 "quindi le percentuali possono superare il 100% se sommate."),
      hr(),
      fluidRow(
        column(5,
               h4("Macrofunzioni"),
               tabella_freq(names(n_macro), as.numeric(n_macro))
        ),
        column(7,
               h4("Microfunzioni"),
               tabella_freq(ALL_KEYS, as.numeric(n_micro), gruppo = unname(MACRO_DI[ALL_IDS]))
        )
      )
    )
  })
  
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
      id_ref <- if (ID_COL %in% names(df_ref)) suppressWarnings(as.integer(df_ref[[ID_COL]])) else NULL
      
      if (!is.null(id_ref) && any(!is.na(id_ref))) {
        # Entrambi i file hanno il numero di riga: si confrontano le stesse righe anche se riordinate
        comuni <- intersect(df_curr[[ID_COL]], id_ref[!is.na(id_ref)])
        df1 <- df_curr[match(comuni, df_curr[[ID_COL]]), , drop = FALSE]
        df2 <- df_ref[match(comuni, id_ref), , drop = FALSE]
        n <- length(comuni)
        allineamento <- "per numero di riga"
      } else {
        n <- min(nrow(df_curr), nrow(df_ref))
        df1 <- df_curr[seq_len(n), , drop = FALSE]
        df2 <- df_ref[seq_len(n), , drop = FALSE]
        allineamento <- "per posizione (il file di riferimento non ha la colonna id_riga)"
      }
      if (n == 0) return(NULL)
      
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
        allineamento = allineamento,
        n_allineati = n,
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
          tags$li(strong("Allineamento: "), d$n_allineati, " righe confrontate ", d$allineamento, "."),
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
}

shinyApp(ui, server)
# COLÀ - Console Online per L'Annotazione (dei segnali discorsivi)

COLÀ è un'applicazione Shiny per l'annotazione manuale di segnali discorsivi (SD), basata sul formato di concordanza KWIC, e compatibile con il formato di esportazione delle occorrenze dal corpus [KIParla](https://kiparla.it/). Ogni riga della tabella rappresenta un'occorrenza da valutare, mostrata includendo il contesto sinistro e destro, e per ciascuna occorrenza l'annotatore/trice decide se si tratta di un segnale discorsivo e, in caso affermativo, quali funzioni pragmatiche esso realizza, seguendo lo schema di annotazione del progetto [COSÌ](https://github.com/LaboratorioPAROLE/cosi).

L'applicazione include inoltre un modulo di confronto tra due annotazioni (Inter-Annotator Agreement), descritto nella seconda parte di questo documento.

---

## Come si annota

1. **Caricamento del file**: tramite il pulsante "Sfoglia..." si carica un file Excel (.xlsx) o CSV contenente le occorrenze da annotare. Il file viene caricato interamente in tabella.

2. **Selezione dell'occorrenza**: cliccando su una riga della tabella si apre un pannello laterale ("drawer") con il dettaglio dell'occorrenza, mostrata nel suo contesto (contesto sinistro — parola chiave evidenziata — contesto destro).

3. **Annotazione della singola occorrenza**:
   - si stabilisce se l'occorrenza è un Segnale Discorsivo (SD) o Non SD tramite il selettore dedicato;
   - se marcata come SD, si spuntano una o più funzioni tra le 23 disponibili, organizzate nelle tre colonne Interazionali / Metatestuali / Cognitive; ogni funzione ha una descrizione consultabile passando il mouse sull'icona informativa accanto al nome;
   - è possibile segnalare un **cumulo** (presenza di altri segnali discorsivi adiacenti) e annotare quali elementi lo compongono in un campo di testo dedicato;
   - è disponibile un campo di commento libero per osservazioni aggiuntive.

4. **Salvataggio dell'occorrenza**: è indispensabile premere il pulsante "salva occorrenza" dopo aver compilato i campi di una riga, prima di passare a quella successiva. Le modifiche non salvate vengono perse se si cambia occorrenza senza aver salvato. L'applicazione mostra un avviso del browser se si tenta di chiudere la pagina con modifiche non salvate.

5. **Navigazione**: i pulsanti "Occorrenza precedente" / "Occorrenza successiva" permettono di scorrere le righe della tabella senza dover chiudere e riaprire il pannello.

### Attenzione: nessun salvataggio persistente automatico

In questa versione dell'applicazione **i dati non vengono conservati in memoria tra una sessione e l'altra** e non esiste alcun salvataggio automatico su server o su file esterno. Questo significa che:

- ogni sessione di lavoro parte dal file caricato in quel momento;
- tutte le annotazioni fatte esistono solo nella sessione attiva del browser;
- se si chiude la scheda, si ricarica la pagina, o si perde la connessione con l'applicazione, **tutto il lavoro non esportato viene perso**;
- al termine della sessione (o periodicamente durante il lavoro, come precauzione) è necessario usare il pulsante **"export"** per scaricare il file CSV con le annotazioni fatte fino a quel momento;
- per riprendere il lavoro in una sessione successiva, il file scaricato (con le annotazioni già presenti) va ricaricato tramite "Sfoglia...", **non** il file originale vuoto.

Si raccomanda di esportare il file, specialmente prima di interruzioni prolungate.

---

## Struttura del file di input

Il file caricato deve essere in formato XLSX o CSV. L'applicazione riconosce e gestisce automaticamente le seguenti colonne; se assenti, vengono create vuote (o a zero) senza generare errori, ma alcune sono necessarie perché lo strumento sia effettivamente utilizzabile.

### Campi indispensabili per la visualizzazione dell'occorrenza (KWIC)

| Colonna | Contenuto |
|---|---|
| `Left` | Contesto sinistro dell'occorrenza |
| `KWIC` | La parola o sequenza target (evidenziata in tabella) |
| `Right` | Contesto destro dell'occorrenza |

Senza questi tre campi correttamente compilati la concordanza non è leggibile: l'occorrenza appare vuota o priva di contesto.

### Campi di annotazione (gestiti automaticamente dall'applicazione)

| Colonna | Contenuto |
|---|---|
| `SD` | Esito dell'annotazione: `SD` oppure `Non SD` |
| `labels` | Elenco delle funzioni assegnate, separate da `\|` |
| `commenti` | Commento libero dell'annotatore/trice |
| `cumulo` | 1 se è stato segnalato un cumulo, 0 altrimenti |
| `cumulo_testo` | Descrizione testuale del cumulo |
| *(una colonna per ciascuna delle 23 microfunzioni)* | 1 se la funzione è presente sull'occorrenza, 0 altrimenti |

Queste colonne non devono essere precompilate: se assenti nel file caricato, l'applicazione le crea automaticamente vuote/a zero e le popola durante l'annotazione. Vengono scritte nel file scaricato con "export", in modo che il lavoro fatto sia sempre ricostruibile riaprendo quel file.

### Campi opzionali per i rimandi al contesto (audio e testo integrale, es. KIParla)

| Colonna | Contenuto |
|---|---|
| `annotation.audio_file` | Link al file audio corrispondente all'occorrenza |
| `doc.full_conversation` | Link alla trascrizione integrale della conversazione |
| `doc.full_jefferson` | Link alla trascrizione in notazione Jefferson |

Quando questi campi sono compilati (ad esempio con URL a risorse del corpus KIParla o di altri repository), nel pannello di dettaglio compaiono i link "Audio", "Conversazione" e "Jefferson", che aprono la risorsa corrispondente in una nuova scheda.

Se questi campi sono assenti o vuoti, i relativi link semplicemente non vengono mostrati; l'annotazione resta comunque possibile sulla sola base del contesto KWIC.

---

## Analisi dell'Inter-Annotator Agreement (IAA)

L'applicazione include un modulo per il confronto tra due annotazioni indipendenti dello stesso set di occorrenze (ad esempio due annotatori che hanno lavorato sullo stesso file, o un'annotazione confrontata con un gold standard). Si accede al modulo tramite il pulsante "Confronta Annotazioni (IAA)": si carica un secondo file (di riferimento) e il sistema confronta riga per riga le due annotazioni.

Poiché a ciascuna occorrenza può essere assegnato più di un'etichetta contemporaneamente (annotazione multilabel), il confronto viene condotto secondo due strategie di matching distinte — **Exact-Match** e **Soft-Match** — applicate a tre livelli di analisi: **Livello SD**, **Macrofunzioni**, **Microfunzioni**.

### Le due strategie di matching

**Exact-Match (matching sull'intero set di etichette)**
L'insieme completo di etichette assegnato da un annotatore a un'occorrenza viene trattato come un'unica categoria nominale. Ad esempio, se un'occorrenza riceve le etichette "Accordo" e "Filler", questo insieme viene rappresentato come un'unica categoria composita (es. `"2,7"`, dove i numeri indicano le posizioni delle etichette attive). Si ha accordo su quell'occorrenza se e solo se i due annotatori hanno prodotto esattamente lo stesso insieme di etichette, senza eccezioni: un'etichetta in più o in meno da parte di uno dei due annotatori conta come disaccordo totale su quel token. È la misura più severa e riflette quanto spesso le due annotazioni coincidono come giudizio complessivo sul token.

**Soft-Match (accordo sul token se almeno un'etichetta è condivisa)**
Si ha accordo su un token se i due annotatori condividono almeno una delle etichette assegnate a quel token, oppure se nessuno dei due ne ha assegnata alcuna. È una misura più permissiva dell'Exact-Match: un'occorrenza in cui i due annotatori hanno scelto due etichette in parte diverse ma con almeno una sovrapposizione conta comunque come accordo, mentre nell'Exact-Match conterebbe come disaccordo totale.

Per calcolare Kappa, AC1 e Alpha in modo coerente con questa stessa nozione di "sovrapposizione", tutte le decisioni etichetta × token dei due annotatori (per tutte le etichette del livello considerato: le 3 macrofunzioni o le 23 microfunzioni) vengono impilate in un'unica coppia di vettori binari e trattate come un'unica variabile "presenza/assenza di etichetta". In questo modo Po, e la base su cui vengono calcolati Kappa/AC1/Alpha, derivano dalla stessa logica di fondo.

*Nota metodologica*: questa aggregazione (impilare tutte le etichette in un unico vettore, talvolta chiamata "micro-media") ha un costo: se le etichette del livello hanno prevalenze molto diverse fra loro, la probabilità di accordo casuale stimata su Kappa/AC1/Alpha riflette una media pesata su tutte le decisioni piuttosto che l'accordo specifico di ciascuna etichetta presa singolarmente, e può non essere rappresentativa per le etichette più rare. Per questo, sotto ogni tabella riassuntiva di Macrofunzioni e Microfunzioni è disponibile una tabella di dettaglio con Po, Kappa, AC1 e Alpha calcolati separatamente per ciascuna etichetta, utile come diagnostica per verificare se il valore aggregato nasconde una forte eterogeneità tra etichette.

### I tre livelli di analisi

1. **Livello SD**: confronta direttamente la decisione binaria "SD / Non SD" tra i due annotatori. Non essendoci più etichette per token a questo livello, non si applica la distinzione Exact/Soft-Match: viene calcolato un solo set di metriche sulla variabile binaria.

2. **Macrofunzioni**: le 23 microfunzioni vengono aggregate nelle tre macro-categorie (Interazionali, Metatestuali, Cognitive). Una macro-categoria è considerata presente su un'occorrenza se almeno una delle microfunzioni che la compongono è stata selezionata da quell'annotatore. Il confronto Exact/Soft-Match viene quindi condotto trattando queste tre macro-categorie come le "etichette" del token.

3. **Microfunzioni**: il confronto Exact/Soft-Match viene condotto usando direttamente le 23 microfunzioni originali come etichette.

Per il confronto è possibile filtrare i token, includendo solo quelli "annotati" (cioè con almeno un'etichetta attiva da parte di uno dei due annotatori, oppure marcati SD da almeno uno dei due), per evitare che un largo numero di token pacificamente non-SD e non annotati gonfi artificialmente l'accordo osservato.

### Le cinque metriche calcolate

Per ciascun livello e ciascuna modalità di matching (dove applicabile) vengono calcolati cinque indici.

#### 1. Accordo Osservato (Po)

Percentuale semplice di concordanza diretta tra i due annotatori, senza alcuna correzione per l'accordo dovuto al caso.

- **Livello SD / singola etichetta (base di calcolo diagnostico per-etichetta)**:
  $$P_o = \frac{\text{numero di token in cui } v_1 = v_2}{\text{numero totale di token}}$$
  dove $v_1$ e $v_2$ sono i valori binari (0/1) assegnati dai due annotatori a quella specifica etichetta.

- **Exact-Match**: $P_o$ è calcolato sulla stessa formula, ma applicata al confronto tra le due stringhe che rappresentano l'intero insieme di etichette di ciascun token: accordo (1) solo se le due stringhe coincidono esattamente.

- **Soft-Match**: $P_o$ è la percentuale di token in cui $|A_i \cap B_i| > 0$ (almeno un'etichetta condivisa) oppure $|A_i \cup B_i| = 0$ (nessuno dei due annotatori ha assegnato etichette a quel token), con $A_i$ e $B_i$ gli insiemi di etichette dei due annotatori sul token $i$.

#### 2. Indice di Jaccard medio

Misura la similarità tra gli insiemi di etichette assegnati dai due annotatori a ciascun token, indipendentemente dal numero di etichette coinvolte. Per ogni token $i$, con $A_i$ e $B_i$ gli insiemi di etichette attive rispettivamente per l'annotatore 1 e l'annotatore 2:

$$J_i = \dfrac{|A_i \cap B_i|}{|A_i \cup B_i|} \times 100 $$

L'indice riportato è la media di $J_i$ su tutti i token del campione. È un indicatore puramente descrittivo di sovrapposizione tra insiemi: non entra nel calcolo di Kappa, AC1 o Alpha (che seguono sempre le rispettive formule standard basate su Po e sulla probabilità di accordo casuale Pe), ed è per costruzione identico nella riga Exact-Match e nella riga Soft-Match della stessa tabella, perché calcolato una sola volta a livello di token.

#### 3. Cohen's / Fleiss' Kappa (κ)

Corregge l'accordo osservato sottraendo la quota di accordo attesa per puro caso ($P_e$):

$$\kappa = \frac{P_o - P_e}{1 - P_e}$$

- **Livello SD / singola etichetta**: con $p_{1,1}, p_{1,0}$ le proporzioni con cui l'annotatore 1 ha assegnato rispettivamente 1 e 0, e $p_{2,1}, p_{2,0}$ le stesse proporzioni per l'annotatore 2:
  $$P_e = (p_{1,1} \cdot p_{2,1}) + (p_{1,0} \cdot p_{2,0})$$

- **Exact-Match**: le "categorie" sono tutte le combinazioni di etichette effettivamente osservate nel campione (unione delle combinazioni prodotte dai due annotatori). Con $p_{1,c}$ e $p_{2,c}$ le proporzioni con cui ciascun annotatore ha prodotto la combinazione $c$:
  $$P_e = \sum_{c} p_{1,c} \cdot p_{2,c}$$

- **Soft-Match**: tutte le decisioni etichetta × token dei due annotatori vengono impilate in un'unica coppia di vettori binari $v_1, v_2$ (presenza/assenza di etichetta, su tutte le etichette del livello insieme), e Kappa è calcolato su questi vettori con la stessa formula binaria sopra descritta.

#### 4. Gwet's AC1

Indice di accordo corretto per il caso, costruito per essere più stabile del Kappa nei casi di forte sbilanciamento tra le classi (ad esempio quando una funzione è molto rara o quando molti token sono vuoti su entrambi i lati):

$$AC1 = \frac{P_o - P_e^{AC1}}{1 - P_e^{AC1}}$$

- **Livello SD / singola etichetta**: con $\bar{p}_1$ la proporzione media di "presenza" (valore 1) tra i due annotatori su quella variabile:
  $$P_e^{AC1} = 2\,\bar{p}_1\,(1 - \bar{p}_1)$$

- **Exact-Match**: con $q$ il numero di combinazioni nominali distinte osservate nel campione, e $\bar{\pi}_c$ la proporzione media (tra i due annotatori) della combinazione $c$:
  $$P_e^{AC1} = \frac{1}{q-1} \sum_{c=1}^{q} \bar{\pi}_c\,(1 - \bar{\pi}_c)$$

- **Soft-Match**: AC1 è calcolato sugli stessi vettori binari impilati $v_1, v_2$ descritti sopra per il Kappa, con la formula binaria.

#### 5. Krippendorff's Alpha (α)

Valuta l'affidabilità complessiva del confronto in termini di disaccordo osservato rispetto al disaccordo atteso per caso:

$$\alpha = 1 - \frac{D_o}{D_e} \quad\Longleftrightarrow\quad \alpha = \frac{P_o - P_e^{\alpha}}{1 - P_e^{\alpha}}$$

Nell'implementazione qui adottata, $P_e^{\alpha}$ è approssimato a partire dalla distribuzione marginale media tra i due annotatori:

- **Livello SD / singola etichetta**: con $\bar{p}_1$ la proporzione media di presenza come sopra:
  $$P_e^{\alpha} = \bar{p}_1^2 + (1 - \bar{p}_1)^2$$

- **Exact-Match**: con $\bar{\pi}_c$ la proporzione media della combinazione nominale $c$:
  $$P_e^{\alpha} = \sum_{c} \bar{\pi}_c^2$$

- **Soft-Match**: Alpha è calcolato sugli stessi vettori binari impilati $v_1, v_2$, con la formula binaria.

**Avvertenza metodologica**: questa è un'approssimazione dell'Alpha di Krippendorff basata sulle distribuzioni marginali medie, non il calcolo canonico sulla matrice di coincidenza con la correzione per campione finito (fattore $n/(n-1)$) prevista dalla formulazione originale. Per campioni piccoli o medi i due calcoli possono divergere in modo non trascurabile. Se i valori sono destinati a una pubblicazione, si raccomanda di validarli confrontandoli con un'implementazione di riferimento (ad esempio la funzione `kripp.alpha()` del pacchetto R `irr`) su un sottoinsieme di controllo dei dati.

### Dettaglio per singola etichetta (diagnostica)

Per i livelli Macrofunzioni e Microfunzioni, oltre alla tabella riassuntiva (Exact-Match / Soft-Match) è disponibile una tabella di dettaglio che riporta Po, Kappa, AC1 e Alpha calcolati separatamente per ciascuna etichetta, usando la stessa formula binaria del livello SD applicata a quella singola etichetta. Questa tabella non entra nel calcolo dell'aggregato Soft-Match sopra (che, come descritto, impila tutte le etichette insieme): serve come diagnostica per verificare se il numero aggregato nasconde una forte eterogeneità tra etichette — ad esempio un'ottima concordanza sulle etichette frequenti insieme a un forte disaccordo su una singola etichetta rara.

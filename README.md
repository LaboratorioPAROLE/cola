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

L'applicazione include un modulo per il confronto tra due annotazioni indipendenti dello stesso set di occorrenze (ad esempio due annotatori che hanno lavorato sullo stesso file, o un'annotazione confrontata con un gold standard). Si accede al modulo tramite il pulsante "Confronta Annotazioni (IAA)": si carica un secondo file (di riferimento) e il sistema confronta le due annotazioni riga per riga.

Poiché a ciascuna occorrenza può essere assegnata più di un'etichetta contemporaneamente (annotazione multilabel), il confronto delle funzioni viene condotto a due livelli — **Macrofunzioni** e **Microfunzioni** — e secondo tre modalità di confronto tra insiemi: **Exact-Match**, **Soft-Match** e **Jaccard-Match**. A questi si aggiungono il confronto al **Livello SD** e l'analisi della **Polifunzionalità**.

### Il filtraggio dei token

Il confronto può essere effettuato su tutti i token oppure limitato ai token annotati.

Quando è attivo il filtro "solo token annotati", per il **Livello SD** vengono considerati i token per i quali almeno uno dei due annotatori ha assegnato esplicitamente `SD` oppure `Non SD`.

Per il confronto delle funzioni vengono invece considerati esclusivamente i token classificati come `SD` da **entrambi** gli annotatori. I token classificati come `Non SD` da almeno uno dei due annotatori vengono quindi esclusi dall'analisi delle funzioni.

Inoltre, nel confronto multilabel vengono esclusi i token per i quali entrambi gli annotatori non hanno assegnato alcuna funzione.

Il numero di token utilizzato per il Livello SD e quello utilizzato per il confronto delle funzioni può quindi essere differente.

### Le tre modalità di confronto delle funzioni

**Exact-Match (corrispondenza esatta dell'intero insieme di etichette)**

L'insieme completo di etichette assegnato da un annotatore a un'occorrenza viene trattato come un unico insieme.

Si ha accordo se e solo se i due annotatori hanno assegnato esattamente lo stesso insieme di etichette, indipendentemente dall'ordine con cui le etichette sono rappresentate.

Ad esempio, se un'occorrenza riceve le etichette "Accordo" e "Filler", l'annotazione è considerata identica anche se le due etichette compaiono in ordine diverso nei due file.

Un'etichetta in più o in meno da parte di uno dei due annotatori determina invece un disaccordo.

L'Exact-Match misura quindi quanto spesso i due annotatori coincidono completamente nell'insieme di funzioni assegnate a un token.

**Soft-Match (sovrapposizione di almeno una funzione)**

Si ha accordo su un token se i due annotatori condividono almeno una delle etichette assegnate:

$$
|A_i \cap B_i| > 0
$$

dove $A_i$ e $B_i$ sono gli insiemi di etichette assegnati dai due annotatori al token $i$.

Ad esempio, se un annotatore assegna "Accordo" e "Filler" e l'altro "Accordo" e "Attenuazione", il token è considerato in accordo secondo il Soft-Match perché almeno una funzione è condivisa.

Se invece i due annotatori non condividono alcuna funzione, il token è considerato in disaccordo.

I token in cui entrambi gli annotatori non hanno assegnato alcuna funzione vengono esclusi dal confronto e non sono quindi considerati accordi Soft-Match.

**Jaccard-Match (grado di sovrapposizione tra insiemi)**

Il Jaccard misura quantitativamente quanto i due insiemi di funzioni si sovrappongono.

Per ogni token $i$ viene calcolato:

$$
J_i =
\frac{|A_i \cap B_i|}
{|A_i \cup B_i|}
$$

Il valore complessivo riportato è la media dei Jaccard calcolati sui token considerati:

$$
J =
\frac{1}{n}\sum_{i=1}^{n} J_i
$$

Il risultato viene espresso in percentuale.

Il Jaccard distingue quindi, a differenza dell'Exact-Match, tra una sovrapposizione completa e una sovrapposizione parziale. Ad esempio, se due annotatori assegnano rispettivamente tre e due funzioni e una sola funzione è condivisa, il Jaccard riflette questa sovrapposizione parziale invece di classificare semplicemente il token come accordo o disaccordo.

### I tre livelli di analisi

1. **Livello SD**: confronta direttamente la decisione binaria "SD / Non SD" tra i due annotatori. A questo livello viene calcolato un unico insieme di metriche sulla classificazione binaria.

2. **Macrofunzioni**: le 23 microfunzioni vengono aggregate nelle tre macro-categorie (Interazionali, Metatestuali, Cognitive). Una macro-categoria è considerata presente su un'occorrenza se almeno una delle microfunzioni che la compongono è stata selezionata da quell'annotatore. Le tre macro-categorie vengono quindi trattate come un insieme di etichette e confrontate mediante Exact-Match, Soft-Match e Jaccard-Match.

3. **Microfunzioni**: il confronto viene effettuato direttamente sulle 23 microfunzioni originali, considerate come insieme di etichette assegnabili contemporaneamente allo stesso token.

Per Macrofunzioni e Microfunzioni, oltre alle misure di matching, viene calcolato un Alpha basato sulla distanza tra gli insiemi di etichette.

### Le metriche calcolate

Per il **Livello SD** vengono calcolati cinque indicatori:

* Accordo Osservato (Po);
* Indice di Jaccard;
* Cohen's Kappa;
* Gwet's AC1;
* Krippendorff's Alpha.

Per **Macrofunzioni** e **Microfunzioni** vengono calcolati:

* Exact-Match;
* Soft-Match;
* Jaccard-Match;
* Krippendorff's Alpha per ciascuna delle tre modalità di confronto.

Per ciascuna singola Macrofunzione e Microfunzione viene inoltre fornito un dettaglio diagnostico con:

* Accordo positivo (Po);
* Cohen's Kappa;
* Gwet's AC1;
* Alpha.

---

#### 1. Accordo Osservato (Po)

L'Accordo Osservato misura la percentuale di casi in cui i due annotatori forniscono la stessa classificazione.

Al **Livello SD**, essendo la variabile binaria, viene calcolato come:

$$
P_o =
\frac{\text{numero di token in cui } v_1=v_2}
{\text{numero totale di token}}
$$

dove $v_1$ e $v_2$ rappresentano rispettivamente la classificazione dei due annotatori (`1 = SD`, `0 = Non SD`).

Per le **Macrofunzioni** e le **Microfunzioni**, l'Accordo Osservato viene espresso attraverso le tre modalità di matching:

* **Exact-Match**: percentuale di token in cui i due insiemi di funzioni coincidono esattamente;
* **Soft-Match**: percentuale di token in cui i due insiemi condividono almeno una funzione;
* **Jaccard-Match**: media percentuale del Jaccard tra i due insiemi.

Il confronto delle funzioni viene effettuato sui token non vuoti, cioè sui token per i quali almeno uno dei due annotatori ha assegnato almeno una funzione.

---

#### 2. Indice di Jaccard medio

L'indice di Jaccard misura la similarità tra gli insiemi di etichette assegnati dai due annotatori a ciascun token.

Per ogni token $i$:

$$
J_i =
\frac{|A_i \cap B_i|}
{|A_i \cup B_i|}
\times 100
$$

Il valore riportato è la media dei valori $J_i$ sui token considerati.

Il Jaccard è quindi una misura graduata della sovrapposizione: una corrispondenza completa produce un valore pari al 100%, mentre una sovrapposizione parziale produce un valore compreso tra 0% e 100%.

Il Jaccard è una misura descrittiva della similarità tra insiemi e non costituisce una correzione per l'accordo atteso casualmente.

---

#### 3. Cohen's Kappa (κ)

Al Livello SD, il Kappa corregge l'Accordo Osservato tenendo conto dell'accordo atteso per caso:

$$
\kappa =
\frac{P_o-P_e}{1-P_e}
$$

dove:

$$
P_e =
(p_{1,1}\cdot p_{2,1})
+
(p_{1,0}\cdot p_{2,0})
$$

con $p_{1,1}$ e $p_{2,1}$ proporzioni di classificazioni `SD` e $p_{1,0}$ e $p_{2,0}$ proporzioni di classificazioni `Non SD` dei due annotatori.

Per il dettaglio delle singole Macrofunzioni e Microfunzioni, la stessa misura viene calcolata trattando ciascuna funzione separatamente come una variabile binaria presenza/assenza.

In questo caso vengono considerati i token per i quali almeno uno dei due annotatori ha assegnato quella specifica funzione.

---

#### 4. Gwet's AC1

L'AC1 viene utilizzato come misura di accordo corretta per l'accordo atteso.

La formula utilizzata è:

$$AC1 =\frac{P_o-P_e^{AC1}}{1-P_e^{AC1}}$$

dove la probabilità di accordo atteso viene calcolata a partire dalla proporzione media di assegnazioni positive dei due annotatori:

$$
\bar p =
\frac{p_{1,1}+p_{2,1}}{2}
$$

e:

$$P\_e^{AC1} = 2\bar p(1-\bar p)$$

Al Livello SD il calcolo viene effettuato sulla classificazione `SD / Non SD`.

Nel dettaglio per singola Macrofunzione e Microfunzione viene effettuato sulla presenza/assenza della specifica funzione.

---

#### 5. Krippendorff's Alpha (α)

L'Alpha misura l'accordo in termini di disaccordo osservato rispetto al disaccordo atteso.

Nella classificazione binaria viene calcolato come:

$$
\alpha =
1-\frac{D_o}{D_e}
$$

dove $D_o$ è il disaccordo osservato e $D_e$ il disaccordo atteso sulla base della distribuzione complessiva delle categorie.

Al Livello SD, il disaccordo osservato è la proporzione di token sui quali i due annotatori forniscono classificazioni differenti.

Per il dettaglio delle singole funzioni viene utilizzato lo stesso principio sulla variabile binaria presenza/assenza della funzione.

Per **Macrofunzioni e Microfunzioni**, invece, l'Alpha viene calcolato direttamente sulla natura set-valued dell'annotazione, utilizzando una distanza specifica per ciascuna modalità di matching.

##### Alpha Exact-Match

La distanza tra due insiemi è:

$$
d(A_i,B_i)=
\begin{cases}
0 & \text{se } A_i=B_i\\
1 & \text{se } A_i\ne B_i
\end{cases}
$$

L'Alpha misura quindi il disaccordo considerando identici soltanto gli insiemi completamente coincidenti.

##### Alpha Soft-Match

La distanza è:

$$
d(A_i,B_i)=
\begin{cases}
0 & \text{se } |A_i\cap B_i|>0\\
1 & \text{altrimenti}
\end{cases}
$$

Due annotazioni vengono quindi considerate identiche ai fini della distanza quando condividono almeno una funzione.

##### Alpha Jaccard

La distanza è definita come:

$$
d(A_i,B_i)=1-J(A_i,B_i)
$$

ovvero:

$$
d(A_i,B_i)=
1-
\frac{|A_i\cap B_i|}
{|A_i\cup B_i|}
$$

In questo caso la distanza cresce progressivamente al diminuire della sovrapposizione tra le annotazioni.

Per tutte e tre le modalità, il disaccordo osservato viene ottenuto come media delle distanze tra le annotazioni dei due annotatori.

Il disaccordo atteso viene invece calcolato mettendo insieme le annotazioni dei due annotatori in un unico campione e confrontando le diverse coppie di insiemi presenti nel campione complessivo.

L'Alpha viene infine ottenuto come:

$$
\alpha =
1-\frac{D_o}{D_e}
$$

In questo modo l'Alpha tiene conto non soltanto della distanza tra le due annotazioni dello stesso token, ma anche della distribuzione degli insiemi di etichette presenti complessivamente nel campione.

---

### Dettaglio per singola etichetta (diagnostica)

Per i livelli Macrofunzioni e Microfunzioni, oltre alle misure complessive basate sul confronto degli insiemi, è disponibile una tabella di dettaglio per ciascuna etichetta.

Ogni funzione viene considerata separatamente come una variabile binaria:

* `1` = funzione assegnata;
* `0` = funzione non assegnata.

Per ciascuna funzione vengono considerati i token in cui almeno uno dei due annotatori ha assegnato quella funzione.

Per ogni etichetta vengono quindi riportati:

* **N positivo**: numero di token in cui la funzione è stata assegnata da almeno uno dei due annotatori;
* **Accordo positivo (%)**: percentuale di accordo tra i due annotatori nel campione considerato;
* **Kappa**;
* **AC1**;
* **Alpha**.

Queste misure sono calcolate separatamente per ciascuna etichetta e consentono di osservare il comportamento delle singole funzioni, distinguendo ad esempio funzioni molto frequenti da funzioni più rare.

I valori per singola etichetta costituiscono quindi un'analisi diagnostica complementare alle misure complessive basate sugli insiemi di funzioni.

---

### Polifunzionalità

La sezione dedicata alla polifunzionalità descrive il numero di funzioni assegnate a ciascun token dai due annotatori.

Per ogni token viene calcolata la cardinalità dell'insieme di funzioni:

$$
c_i=|A_i|
$$

e vengono confrontate le cardinalità dei due annotatori.

Sono riportati:

* numero medio di funzioni per token;
* mediana del numero di funzioni;
* deviazione standard;
* percentuale di token senza funzioni;
* percentuale di token monofunzionali;
* percentuale di token polifunzionali, cioè con almeno due funzioni.

Viene inoltre calcolata la concordanza sul numero di funzioni assegnate:

* **stesso numero di funzioni**:

$$
|c_{1i}-c_{2i}|=0
$$

* **differenza di una funzione**:

$$
|c_{1i}-c_{2i}|=1
$$

* **differenza di almeno due funzioni**:

$$
|c_{1i}-c_{2i}|\ge2
$$

e la **differenza assoluta media**:

$$
\frac{1}{n}
\sum_i |c_{1i}-c_{2i}|
$$

La polifunzionalità non misura quindi l'identità delle funzioni assegnate, ma la concordanza tra i due annotatori rispetto alla **quantità di funzioni** attribuite a ciascun token.

---

### Riepilogo

| Livello              | Misure                                                                                                     |
| -------------------- | ---------------------------------------------------------------------------------------------------------- |
| **SD**               | Accordo Osservato, Jaccard, Cohen's Kappa, Gwet's AC1, Alpha                                               |
| **Macrofunzioni**    | Exact-Match, Soft-Match, Jaccard-Match, Alpha per Exact/Soft/Jaccard + dettaglio per singola macrofunzione |
| **Microfunzioni**    | Exact-Match, Soft-Match, Jaccard-Match, Alpha per Exact/Soft/Jaccard + dettaglio per singola microfunzione |
| **Polifunzionalità** | Distribuzione del numero di funzioni e accordo sulla cardinalità                                           |

Le misure di matching descrivono diversi gradi di accordo tra annotazioni multilabel: l'**Exact-Match** richiede la coincidenza completa dell'insieme, il **Soft-Match** richiede almeno una funzione condivisa, mentre il **Jaccard-Match** misura quantitativamente il grado di sovrapposizione tra i due insiemi.

## L'**Alpha** integra invece il confronto con una stima del disaccordo atteso sulla base della distribuzione degli insiemi presenti nel campione, utilizzando una distanza coerente con la modalità di matching considerata.

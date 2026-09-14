# COLÀ - Console Online per L'Annotazione (dei segnali discorsivi)
Test

## Calcolo dell'Inter-Annotator Agreement (IAA)

L'applicazione include un modulo integrato per la valutazione dell'accordo tra due annotatori (**Inter-Annotator Agreement - IAA**). 

Poiché ogni elemento/token può ricevere più etichette contemporaneamente (**annotazione multilabel**), il sistema calcola le metriche confrontando le annotazioni attraverso due strategie di matching (**Exact-Match** e **Soft-Match**) su tre diversi livelli di analisi (*Livello SD*, *Macrofunzioni*, *Microfunzioni*).

---

### Modalità di Matching: Exact-Match vs Soft-Match

La differenza principale risiede nel modo in cui l'insieme di etichette assegnato a ciascun token viene interpretato e confrontato.

| Caratteristica | Exact-Match (Exact Set Match) | Soft-Match (Decision Level / Overlap) |
| :--- | :--- | :--- |
| **Concetto** | Valuta l'accordo **esatto** sull'intero set di etichette. | Valuta l'accordo **parziale** o sulla singola decisione binaria. |
| **Accordo sul singolo token** | Si ha accordo ($1$) solo se i due annotatori hanno selezionato **esattamente le stesse identiche etichette**. | Si ha accordo ($1$) se gli annotatori condividono **almeno un'etichetta** oppure se **entrambi non ne hanno assegnata alcuna**. |
| **Trattamento dei dati** | Ogni combinazione di etichette viene convertita in una stringa nominale unica (es. `"Accordo,Filler"`). | La matrice delle annotazioni viene convertita in vettori binari (Presenza/Assenza $0/1$ per ciascuna categoria). |
| **Obiettivo** | Misurare la frequenza con cui gli annotatori concordano sull'analisi complessiva del token. | Misurare la precisione a livello delle singole scelte categoriali, evitando penalizzazioni eccessive. |

---

### Le Metriche Statistiche Calcolate

Per ciascun livello e modalità di matching vengono calcolati 5 indici statistici:

#### 1. Accordo Osservato ($P_o$)
Indica la percentuale semplice di concordanza diretta tra gli annotatori.
* **In Exact-Match**: Percentuale di token per cui l'insieme di etichette è identico al 100%.
  $$P_o = \frac{\text{Token con set identico}}{\text{Totale token}} \times 100$$
* **In Soft-Match**: Percentuale di token con almeno un'etichetta sovrapposta (o entrambi vuoti).
  $$P_o = \frac{\text{Token con } (|A_i \cap B_i| > 0 \text{ oppure } |A_i \cup B_i| = 0)}{\text{Totale token}} \times 100$$

---

#### 2. Indice Jaccard Medio
Misura la percentuale di sovrapposizione tra i set di etichette per ogni token, calcolando il rapporto tra intersezione e unione:
$$J_i = \begin{cases} 
100\% & \text{se } |A_i \cup B_i| = 0 \text{ (entrambi vuoti)} \\
\frac{|A_i \cap B_i|}{|A_i \cup B_i|} \times 100 & \text{altrimenti}
\end{cases}$$
L'indice finale visualizzato è la **media di $J_i$ su tutti i token**.
> *Nota: L'Indice Jaccard misura la similarità di set ed è identico in entrambe le righe di riepilogo.*

---

#### 3. Cohen's / Fleiss' Kappa ($\kappa$)
Corregge l'accordo osservato rimuovendo la probabilità di accordo dovuta unicamente al caso ($P_e$):
$$\kappa = \frac{P_o - P_e}{1 - P_e}$$
* **In Exact-Match**: Calcolato sulle frequenze delle combinazioni nominali esatte ($P_e = \sum p_{1,c} \cdot p_{2,c}$ per ogni combinazione $c$).
* **In Soft-Match**: Calcolato a livello di singola decisione binaria su tutte le combinazioni *token-categoria* ($P_e$ basato sulle frequenze marginali di presenza/assenza di ciascuna etichetta).

---

#### 4. Gwet's AC1
Un indice di accordo corretto per il caso **robusto ai paradossi del Kappa** (es. quando una classe è fortemente prevalente o quando molte righe sono vuote):
$$AC1 = \frac{P_o - P_e(AC1)}{1 - P_e(AC1)}$$
* **In Exact-Match**:
  $$P_e(AC1) = \frac{1}{q - 1} \sum_{c=1}^{q} \pi_c (1 - \pi_c)$$
  *(dove $q$ è il numero di combinazioni nominali uniche trovate e $\pi_c$ è la proporzione media della combinazione $c$)*.
* **In Soft-Match**: Applicato sui vettori binari $0/1$ delle decisioni individuali, garantendo una stima attendibile anche con forte sbilanciamento delle classi.

---

#### 5. Krippendorff's Alpha ($\alpha$)
Valuta l'affidabilità complessiva dell'annotazione basandosi sul rapporto tra il disaccordo osservato ($D_o$) e il disaccordo atteso per caso ($D_e$):
$$\alpha = 1 - \frac{D_o}{D_e}$$
* **In Exact-Match**: Calcolato su scala nominale prendendo le combinazioni esatte come valori di categoria.
* **In Soft-Match**: Calcolato su scala binaria per le singole decisioni di presenza/assenza di ciascuna funzione.

---

### Livelli di Analisi

1. **Livello SD (Binario)**: Valuta l'accordo sulla classificazione primaria del token (`SD` vs `Non SD`). Viene calcolato direttamente senza distinzione Exact/Soft.
2. **Macrofunzioni (Overall)**: Aggrega le 23 microfunzioni nei 3 macro-settori (*Interazionali*, *Metatestuali*, *Cognitive*). Un macro-settore è considerato presente ($1$) se almeno una delle sue microfunzioni è stata selezionata.
3. **Microfunzioni (Overall)**: Valuta l'accordo globale multilabel su tutte le 23 microfunzioni contemporaneamente.

# analisi-assoli-jazz
Questa repository contiene il codice in R completo della tesi triennale "Metodi statistici per l'analisi di assoli nella musica jazz".

Descrizione dei file csv:
- "corrispondenze.csv" contiene la tabella per le corrispondenze tra i salti intervallari scritti come interi e rimappati come caratteri.
- "info_assoli_non_sistemato.csv" è stato ottenuto a partire dal Weimar Jazz Database tramite comandi SQL (contenuti nel file query.sql"). Contiene alcuni metadati per ogni assolo, come artista, nome del brano, anno, ecc. ma dev'essere sistemato (nel capitolo 1).
- "info_assoli.csv" contiene i metadati degli assoli sistemati e ampliati nel capitolo 1.
- "melody.csv" contiene le informazioni sulle note degli assoli così come compaiono nel Weimar Jazz Database.

Il codice è diviso in 5 file R, in modo che "Capitolo X.R" contenga il codice per il capitolo X. 
Fa eccezione il capitolo 4 per il quale il codice è diviso in 2 parti: "Capitolo 4.1.R" è riferito alla prima parte del capitolo 4 sull'analisi delle frasi ricorrenti, "Capitolo 4.2.R" si riferisce alla seconda parte sul modello di scoring.

Nel capitolo 1 vengono introdotti alcuni oggetti e tabelle che vengono poi riutilizzati nei capitoli successivi. Per questo per il funzionamento del codice per il capitolo 2 viene caricato il file "dati_capitolo_2.Rdata", mentre per i successivi viene caricato "dati_capitolo_3.Rdata". 

Nella cartella "Figure" sono presenti tutte le figure della tesi.

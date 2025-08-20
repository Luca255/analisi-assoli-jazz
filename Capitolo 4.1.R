
# Caricamento dei pacchetti -----------------------------------------------

library(tidyverse)
library(patchwork)
library(Rcpp)
library(stringr)
library(gm)
library(pheatmap)

load("dati_capitolo_3.Rdata")


# Funzioni utili del Capitolo 3 ---------------------------------------------

# Funzione per rimappare un vettore di numeri a stringa di caratteri
remap = function(s, uniques, map) {
  uniques = sort(uniques)
  idx = sapply(s, FUN = function(x) which(x == uniques))
  new_s = map[idx]
  paste(new_s, collapse="")
}


# Funzione per il procedimento inverso (da stringa a vettore di numeri)
demap = function(s, uniques, map, type="vector", sep="/") {
  s = strsplit(s, split="")[[1]]
  uniques = sort(uniques)
  idx = sapply(s, FUN = function(x) which(x == map))
  s_demapped = uniques[idx]
  if(type=="string")
    paste(as.character(s_demapped), collapse=sep)
  else 
    s_demapped
}


cppFunction('
int levenshtein_cpp(std::string s, std::string t) {
  int n = s.size();
  int m = t.size();
  std::vector<std::vector<int>> d(n + 1, std::vector<int>(m + 1));

  for (int i = 0; i <= n; ++i) d[i][0] = i;
  for (int j = 0; j <= m; ++j) d[0][j] = j;

  for (int i = 1; i <= n; ++i) {
    for (int j = 1; j <= m; ++j) {
      int cost = (s[i - 1] == t[j - 1]) ? 0 : 1;
      d[i][j] = std::min({
        d[i - 1][j] + 1,
        d[i][j - 1] + 1,
        d[i - 1][j - 1] + cost
      });
    }
  }

  return d[n][m];
}
')


# Preparazione dati (differenze di pitch in caratteri) --------------------

pitchdifflist = lapply(pitchlist, diff)

# Prendo le differenze uniche 
uniques = c()
for(i in 1:456) {
  uniques = c(uniques, unique(pitchdifflist[[i]]))
}
uniques = sort(unique(uniques))
length(uniques)
uniques

# Setto la mappa
mappa = c(letters, LETTERS, as.character(0:9), 
          intToUtf8(33, multiple = TRUE), "/", intToUtf8(35:38, multiple = TRUE)  
          ,"?", intToUtf8(40:44, multiple = TRUE))
length(mappa)
mappa

write.csv2(data.frame(intero = uniques, carattere = mappa), "corrispondenze.csv")


# Applico le funzioni per rimappare e trasformare in stringhe i vettori di interi
newpitchv = rep(NA, 456)
for(i in 1:456) {
  s = diff(pitchlist[[i]])
  newpitchv[i] = remap(s, uniques, mappa)
}
newpitchv[1]



# Grafici a barre per n-grammi --------------------------------------------

# Funzione per estrarre gli n-grammi da una stringa
get_ngrams = function(s, n) {
  sapply(1:(str_length(s) - n+1), function(i) str_sub(s, i, i+n-1))
}


# Funzione per disegnare il barplot con le frequenze degli n-grammi
draw_barplot_ngrams = function(melody, n, type="integer", prob=FALSE) {
  grams = get_ngrams(melody, n)
  tab = table(grams)
  
  if(prob==TRUE) {
    gramsdf = data.frame(sort(tab, decreasing=F)/
                           sum(tab))[(length(tab)-20):length(tab),]
  }
  else {
    gramsdf = data.frame(sort(tab, decreasing=F))[(length(tab)-20):length(tab),]
  }
  
  if(type=="character") {
    ggplot(gramsdf, aes(x=grams, y=Freq)) +
      geom_bar(stat="identity", fill=scales::hue_pal()(4)[3]) +
      coord_flip() +
      labs(title=paste(n, "-grammi", sep=""), y="Frequenza", x=paste(n, "-grammo", sep="")) +
      theme_bw()
  }
  else {
    gramsdf$intgram = sapply(as.character(gramsdf$grams), FUN = function(gram) {
      demap(gram, uniques, mappa) %>% 
        as.character() %>% 
        paste(collapse="/")
    })
    gramsdf$intgram = factor(gramsdf$intgram, levels =
                               gramsdf$intgram[order(gramsdf$Freq)])
    ggplot(gramsdf, aes(x=intgram, y=Freq)) +
      geom_col(fill=scales::hue_pal()(4)[3]) +
      coord_flip() +
      labs(title=paste(n, "-grammi", sep=""), y="Frequenza", x=paste(n, "-grammo", sep="")) +
      theme_bw()
  }
}


g2 = draw_barplot_ngrams(newpitchv[1], 2)
g4 = draw_barplot_ngrams(newpitchv[1], 4)

g2 + g4

g9 = draw_barplot_ngrams(newpitchv[1], 9)
g10 = draw_barplot_ngrams(newpitchv[1], 10)

g9 + g10

# Le frase 
# 5/-2/-1/-4/-5/-1/-2/1/4/-1
# è la versione più lunga di 
# 5/-2/-1/-4/-5/-1/-2/1/4

phrase1 = c(5,-2,-1,-4,-5,-1,-2,1,4,-1)
phrase2 = c(5,-2,-1,-4,-5,-1,-2,1,4)



# Codice per generare gli spartiti musicali:

# Funzione che dato il vettore con gli intervalli ritorna le note in caratteri
get_notes = function(v, type="it", start="do5", bemolle=FALSE) {
  if(type=="en") {
    if(bemolle==TRUE) {
      note.names = c("C", "D-", "D", "E-", "E", "F", "G-", "G", "A-", "A", "B-", "B")
    }
    else {
      note.names = c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
    }
  }
  else {
    if(bemolle==TRUE) {
      note.names = c("do", "reb", "re", "mib", "mi", "fa", "solb", "sol", "lab", "la", "sib", "si")
    }
    else {
      note.names = c("do", "reb", "re", "re#", "mi", "fa", "fa#", "sol", "sol#", "la", "la#", "si")
    }
  }
  octaves = 0:8
  keyboard = as.vector(sapply(octaves, function(o) paste0(note.names, o)))
  
  idx_i = which(keyboard == start)
  notes.output = start
  for(i in 1:length(v)) {
    idx_i = idx_i + v[i]
    notes.output = c(notes.output, keyboard[idx_i])
  }
  
  notes.output
}

phrase_notes1 = get_notes(phrase1, type="en", start="C5")
phrase_notes2 = get_notes(phrase2, type="en", start="C5")

line1 = Line(pitches = phrase_notes1, durations = rep(0.5, length(phrase_notes1)))
line2 = Line(pitches = phrase_notes2, durations = rep(0.5, length(phrase_notes2)))

music1 = Music() + Meter(4, 4) + line1
export(music1, path="phrase1_score.png")
music2 = Music() + Meter(4, 4) + line2
export(music2, path="phrase2_score.png")



# Primo modo per trovare le frasi in comune -------------------------------

# Funzione che ritorna un dataframe con le frasi ricorrenti in una melodia
get_phrases_df = function(melody, id, length_min = 4) {
  phrases_freq = c()
  for(n in length_min:100) {
    ngrams = get_ngrams(melody, n)
    tab = table(ngrams)
    if(sum(tab > 1) == 0)
      next
    tab = sort(tab[tab > 1], decreasing=T)
    phrases_freq = rbind(phrases_freq, cbind(names(tab), n, tab))
  }
  if(!is.null(phrases_freq))
    melody_phrases_df = data.frame(phrase = phrases_freq[,1], melodyID = id,
                                   length = as.integer(phrases_freq[,2]),
                                   freq = as.integer(phrases_freq[,3]))
  else
    data.frame()
}

# Frasi ripetute nell'assolo 1
melody1_phrases_df = get_phrases_df(newpitchv[1], 1)
head(melody1_phrases_df)
dim(melody1_phrases_df)

# Frasi ripetute nell'assolo 2
melody2_phrases_df = get_phrases_df(newpitchv[2], 2)
head(melody2_phrases_df)

# Insieme
melody_phrases_df = rbind(melody1_phrases_df, melody2_phrases_df)

df1 = melody_phrases_df %>% 
  group_by(melodyID, length) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  complete(melodyID, length = full_seq(length, 1), fill = list(count = 0))
df1$melodyID = as.factor(df1$melodyID)

g1 = ggplot(df1, aes(x=length, y=count, fill=melodyID)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw() +
  labs(x="Lunghezza", y="Frequenza", fill="Assolo") +
  scale_x_continuous(breaks = 1:10)

df2 = melody_phrases_df %>% 
  group_by(melodyID, freq) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  complete(melodyID, freq = full_seq(freq, 1), fill = list(count = 0))
df2$melodyID = as.factor(df2$melodyID)

g2 = ggplot(df2, aes(x=freq, y=count, fill=melodyID)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw() +
  labs(x="Ripetizioni", y="Frequenza", fill="Assolo") +
  scale_x_continuous(breaks = 1:8)

g1 + g2


shared_phrases = intersect(melody1_phrases_df$phrase, melody2_phrases_df$phrase)
sapply(shared_phrases, function(p) demap(p, uniques, mappa, type="string"))



# Secondo modo per trovare le frasi in comune -----------------------------

length_min = 4      # Lunghezza minima delle frasi
length_max = 40     # Lunghezza massima
melody_phrases_list = list()
for(id in 1:length(newpitchv)) {       # Impiega 2-3 minuti
  phrases_freq_list = list()
  i = 1
  for(n in length_min:length_max) {             
    ngrams = get_ngrams(newpitchv[id], n)   # Per ogni assolo salvo tutte le sottostringhe
    tab = table(ngrams)
    tab = sort(tab, decreasing=T)
    phrases_freq_list[[i]] = cbind(names(tab), n, tab)
    i = i+1
  }
  phrases_freq = do.call(rbind, phrases_freq_list)
  melody_phrases_list[[id]] = cbind(phrases_freq[,1], id, 
                                    phrases_freq[,2], phrases_freq[,3])
  print(id)
}

melody_phrases_df = as.data.frame(do.call(rbind, melody_phrases_list))
colnames(melody_phrases_df) = c("phrase", "melodyID", "length", "freq")

melody_phrases_df = melody_phrases_df %>% 
  as_tibble() %>% 
  mutate(across(melodyID:freq, as.numeric))


# Dataframe per la frequenza di ogni frase nel complesso degli assoli
unique_phrases_df = melody_phrases_df %>% 
  group_by(phrase) %>%                              # raggruppo per frase 
  summarize(length = unique(length), freq = sum(freq), .groups = "drop") %>%
  filter(freq > 1) %>%  # tolgo le frasi che compaiono solo 1 volta in tutti gli assoli
  arrange(length, freq)

# Filtro per le frasi che compaiono almeno 2 volte in tutti gli assoli
melody_shared_phrases = melody_phrases_df %>%
  filter(phrase %in% unique_phrases_df$phrase) %>%
  arrange(melodyID, length, freq)

# Numero di frasi uniche
nrow(unique_phrases_df)

# Aggiungo le colonne performer, style, instrument
melody_shared_phrases2 = melody_shared_phrases %>% 
  left_join(info %>% select(melid, performer, style, instrument), by = c("melodyID" = "melid"))




# Funzioni per le frasi ricorrenti ----------------------------------------

# Funzione per la ricerca di una frase con similarità impostabile
similarity_search = function(p, dist_max) {
  phr = melody_shared_phrases2$phrase
  tib = melody_shared_phrases2 %>% slice(0)
  for(i in 1:nrow(melody_shared_phrases2)) {
    dist = levenshtein_cpp(p, phr[i])         # funzione per la distanza in c++
    if(dist <= dist_max) {
      tib = tib %>% add_row(melody_shared_phrases2[i,])
    }
  }
  tib %>% 
    mutate(phrase = as.character(phrase)) %>% 
    mutate(phrase = map_chr(phrase, ~ paste(demap(.x, uniques, mappa), collapse = "/"))) %>% 
    left_join(info %>% select(melid, title), by = c("melodyID" = "melid")) %>% 
    left_join(info %>% select(melid, recordingyear), by = c("melodyID" = "melid")) %>%
    select(1,2,3,4,5,8,6,7,9)
}


# Frasi più utilizzate all'interno di uno stile musicale
frasi_per_stile = function(stile, prop = 0) {
  # Conto per ogni frase in quanti assoli diversi dello stile compare e la frequenza totale
  conteggio_frasi = melody_shared_phrases2 %>% 
    filter(style == stile) %>% 
    group_by(phrase, length) %>% 
    summarise(n_assoli_con_frase = n_distinct(melodyID),
              freq_tot = sum(freq),
              .groups = "drop") %>% 
    arrange(desc(n_assoli_con_frase))
  
  # Valuto le proporzioni di assoli in cui compare ogni frase
  conteggio_frasi %>% 
    mutate(proporzione = n_assoli_con_frase / sum(info$style == stile),
           stile = stile) %>% 
    filter(proporzione >= prop) %>% 
    arrange(desc(proporzione), desc(n_assoli_con_frase)) %>% 
    mutate(phrase = map_chr(phrase, ~ paste(demap(.x, uniques, mappa), collapse = "/")))
}


# Frasi più utilizzate da un artista
frasi_per_artista = function(artista, prop = 0) {
  # Conto per ogni frase in quanti assoli diversi dell'artista compare e la frequenza totale
  conteggio_frasi = melody_shared_phrases2 %>% 
    filter(performer == artista) %>% 
    group_by(phrase, length) %>% 
    summarise(n_assoli_con_frase = n_distinct(melodyID),
              freq_tot = sum(freq),
              .groups = "drop") %>% 
    arrange(desc(n_assoli_con_frase))
  
  # Valuto le proporzioni di assoli in cui compare ogni frase
  conteggio_frasi %>% 
    mutate(proporzione = n_assoli_con_frase / sum(info$performer == artista),
           artista = artista) %>% 
    filter(proporzione >= prop) %>% 
    arrange(desc(proporzione), desc(n_assoli_con_frase)) %>% 
    mutate(phrase = map_chr(phrase, ~ paste(demap(.x, uniques, mappa), collapse = "/")))
}


# Frasi più utilizzate su uno strumento
frasi_per_strumento = function(strumento, prop = 0) {
  # Conto per ogni frase in quanti assoli diversi dello strumento compare e la frequenza totale
  conteggio_frasi = melody_shared_phrases2 %>% 
    filter(instrument == strumento) %>% 
    group_by(phrase, length) %>% 
    summarise(n_assoli_con_frase = n_distinct(melodyID),
              freq_tot = sum(freq),
              .groups = "drop") %>% 
    arrange(desc(n_assoli_con_frase))
  
  # Valuto le proporzioni di assoli in cui compare ogni frase
  conteggio_frasi %>% 
    mutate(proporzione = n_assoli_con_frase / sum(info$instrument == strumento),
           strumento = strumento) %>% 
    filter(proporzione >= prop) %>% 
    arrange(desc(proporzione), desc(n_assoli_con_frase)) %>% 
    mutate(phrase = map_chr(phrase, ~ paste(demap(.x, uniques, mappa), collapse = "/")))
}


# Frasi in comune tra due o più stili
frasi_comuni_stili = function(stili) {
  map_dfr(stili, frasi_per_stile) %>%
    group_by(phrase) %>%
    filter(n_distinct(stile) == length(stili)) %>%
    ungroup() %>%
    select(phrase, stile, proporzione) %>%
    spread(stile, proporzione) %>%
    mutate(proporzione_min = do.call(pmin, c(across(all_of(stili)), na.rm = F))) %>%
    arrange(desc(proporzione_min))
}


# Frasi in comune tra due o più artisti
frasi_comuni_artisti = function(artisti) {
  map_dfr(artisti, frasi_per_artista) %>%
    group_by(phrase) %>%
    filter(n_distinct(artista) == length(artisti)) %>%
    ungroup() %>%
    select(phrase, artista, proporzione) %>%
    spread(artista, proporzione) %>%
    mutate(proporzione_min = do.call(pmin, c(across(all_of(artisti)), na.rm = TRUE))) %>%
    arrange(desc(proporzione_min))
}

# Frasi in comune tra due o più strumenti
frasi_comuni_strumenti = function(strumenti) {
  map_dfr(strumenti, frasi_per_strumento) %>%
    group_by(phrase) %>%
    filter(n_distinct(strumento) == length(strumenti)) %>%
    ungroup() %>%
    select(phrase, strumento, proporzione) %>%
    spread(strumento, proporzione) %>%
    mutate(proporzione_min = do.call(pmin, c(across(all_of(strumenti)), na.rm = TRUE))) %>%
    arrange(desc(proporzione_min))
}



# Applicazioni e risultati ------------------------------------------------

## Ricerca della frase "The Lick" -----------------------------------------

# Generazione dello spartito
phrase_notes = get_notes(c(2,1,2,-3,-4,2), type="en", start="D4")
line = Line(pitches = phrase_notes, durations = c(0.5, 0.5, 0.5, 0.5, 1, 0.5, 4.5))
music = Music() + Meter(4, 4) + line
export(music, path="thelick_score.png")

# Ricerca
p = remap(c(2,1,2,-3,-4,2), uniques, mappa)
similarity_search(p, 0)     # ricerca esatta
ricerca = similarity_search(p, 1)    # ricerca fino a 1 modifica
ricerca %>% print(n=200)
ricerca %>% pull(phrase) %>% unique()     # frasi uniche trovate
ricerca %>% pull(freq) %>% sum()          # numero di ripetizioni
ricerca %>% arrange(recordingyear) %>% print(n=200)    # ordinate per anno 
ricerca %>% filter(recordingyear < 1955) %>%    # numero di ripetizioni pre 1955 
  pull(freq) %>% sum()   
ricerca %>% group_by(performer) %>%     # frequenze per artista
  summarise(freq_tot=sum(freq)) %>%   
  arrange(desc(freq_tot))
ricerca %>% group_by(style) %>%        # frequenze per stile
  summarise(total_freq = sum(freq), .groups = "drop")

# Valuto la significatività dell'associazione tra stile e presenza della frase
ricerca %>% pull(style) %>% table()
assoli_tot = c(BEBOP=56, COOL=54, FREE=5, FUSION=20, HARDBOP=76, POSTBOP=147, SWING=66, TRADITIONAL=32)
assoli_con_frase = c(BEBOP=7, COOL=23, FREE=0, FUSION=1, HARDBOP=31, POSTBOP=53, SWING=6, TRADITIONAL=2)
assoli_senza_frase <- assoli_tot - assoli_con_frase
tabella <- rbind(Frase = assoli_con_frase, NoFrase = assoli_senza_frase)
tabella = tabella[,c(8,7,1,2,5,6,3,4)]
tabella
chisq.test(tabella[,-7])
chisq.test(tabella[,-7])$expected
chisq.test(tabella[,-7])$stdres




## Valutazioni sugli stili -------------------------------------------------



### Tipica frase bebop ------------------------------------------------------

# Generazione dello spartito
phrase_notes = get_notes(c(-1,-2,-1,-9,3,3,-1,-2), type="en", start="F5")
line = Line(pitches = c(NA, phrase_notes), durations = c(rep(0.5, 10)))
music = Music() + Meter(4, 4) + line
export(music, path="frase_bebop_score.png")

# Ricerca
p = remap(c(-1,-2,-1,-9,3,3,-1,-2), uniques, mappa)
similarity_search(p, 0)     # ricerca esatta
ricerca = similarity_search(p, 1)    # ricerca fino a 1 modifica
ricerca %>% print(n=200)
ricerca %>% pull(phrase) %>% unique()    # frasi uniche trovate
ricerca %>% pull(freq) %>% sum()          # numero di ripetizioni
ricerca %>% arrange(recordingyear) %>% print(n=200)    # ordinate per anno 
ricerca %>% group_by(performer) %>%     # frequenze per artista
  summarise(freq_tot=sum(freq)) %>%   
  arrange(desc(freq_tot))
ricerca %>% group_by(style) %>%     # frequenze per genere
  summarise(freq_tot=sum(freq)) %>%   
  arrange(desc(freq_tot))




### Ricerca della frase più frequente nel bebop -----------------------------

ricerca = frasi_per_stile("BEBOP")
ricerca %>% print(n=100)
ricerca %>% arrange(desc(freq_tot))
ricerca %>% arrange(desc(proporzione))    # frasi con frequenza totale più alta

# Generazione dello spartito
phrase_notes = get_notes(c(-2,-1,-2,-1), type="en", start="D5")
line = Line(pitches = c(phrase_notes, NA), durations = c(rep(0.5, 4), 1, 5))
music = Music() + Meter(4, 4) + line
export(music, path="frase_bebop_score2.png")

p = remap(c(-2,-1,-2,-1), uniques, mappa)
ricerca = similarity_search(p, 0)
ricerca
ricerca %>% pull(freq) %>% sum()          # numero di ripetizioni
ricerca %>% arrange(recordingyear) %>% print(n=260)    # ordinate per anno 
ricerca %>% group_by(performer) %>%     # frequenze per artista
  summarise(freq_tot=sum(freq)) %>%   
  arrange(desc(freq_tot))
ricerca %>% group_by(style) %>%     # frequenze per genere
  summarise(freq_tot=sum(freq)) %>%   
  arrange(desc(freq_tot))

# Valuto la significatività dell'associazione tra stile e presenza della frase
ricerca %>% pull(style) %>% table()
assoli_tot = c(BEBOP=56, COOL=54, FREE=5, FUSION=20, HARDBOP=76, POSTBOP=147, SWING=66, TRADITIONAL=32)
assoli_con_frase = ricerca %>% pull(style) %>% table()
assoli_senza_frase <- assoli_tot - assoli_con_frase
tabella <- rbind(Frase = assoli_con_frase, NoFrase = assoli_senza_frase)
tabella = tabella[,c(8,7,1,2,5,6,3,4)]
tabella
chisq.test(tabella[,-7])
chisq.test(tabella[,-7])$expected
chisq.test(tabella[,-7])$stdres




### Confronti tra stili a coppie ------------------------------------------

stili = c("TRADITIONAL", "SWING", "BEBOP", "COOL",
          "HARDBOP", "POSTBOP", "FREE", "FUSION")
perc_stili = matrix(0, nrow=8, ncol=8)
for(i in 1:8) {                # Impiega circa 5 minuti
  for(j in i:8) {
    if(i==j) {
      perc_stili[i,j] = NA
    }
    else {
      perc = frasi_comuni_stili(c(stili[i], stili[j])) %>% 
        pull(proporzione_min) %>% head(20) %>% mean()
      perc_stili[i,j] = perc_stili[j,i] = perc
      print(c(i,j))
    }
  }
}

rownames(perc_stili) = colnames(perc_stili) = stili
pheatmap(perc_stili,
         cluster_rows = F,
         cluster_cols = F,
         display_numbers = TRUE,   # mostra i valori
         fontsize_row = 10,
         fontsize_col = 10,
         angle_col = 0,
         fontsize_number = 11.5)



## Valutazioni sugli artisti -----------------------------------------------

### Ricerca dei Coltrane Patterns -------------------------------------------

p = remap(c(2,2,3,-4,2,2,3), uniques, mappa)
ricerca = similarity_search(p, 0)
ricerca

# Generazione dello spartito
phrase_notes = get_notes(c(2,2,3,-4,2,2,3), type="en", start="C4", bemolle = T)
line = Line(pitches = c(phrase_notes), durations = rep(0.5, 8))
music = Music() + Meter(4, 4) + line
export(music, path="coltrane_patterns1.png")


p = remap(c(2,2,3,-2,2,2,3), uniques, mappa)
ricerca = similarity_search(p, 0)
ricerca

# Generazione dello spartito
phrase_notes = get_notes(c(2,2,3,-2,2,2,3), type="en", start="C4", bemolle = T)
line = Line(pitches = c(phrase_notes), durations = rep(0.5, 8))
music = Music() + Meter(4, 4) + line
export(music, path="coltrane_patterns2.png")



### Frase più frequente di Coltrane -----------------------------------------

ricerca = frasi_per_artista("John Coltrane")
ricerca %>% print(n=10)

# Generazione dello spartito
phrase_notes = get_notes(c(2,1,2,2), type="en", start="C5", bemolle = T)
line = Line(pitches = c(phrase_notes, NA), durations = c(rep(0.5, 4), 1, 5))
music = Music() + Meter(4, 4) + line
export(music, path="coltrane_patterns3.png")

p = remap(c(2,1,2,2), uniques, mappa)
ricerca = similarity_search(p, 0)
ricerca
ricerca %>% pull(freq) %>% sum()          # numero di ripetizioni
ricerca %>% arrange(recordingyear) %>% print(n=280)    # ordinate per anno 
ricerca %>% group_by(performer) %>%     # frequenze per artista
  summarise(freq_tot=sum(freq)) %>%   
  arrange(desc(freq_tot))
ricerca %>% group_by(style) %>%     # frequenze per genere
  summarise(freq_tot=sum(freq)) %>%   
  arrange(desc(freq_tot))


# Valuto la significatività dell'associazione tra stile e presenza della frase
ricerca %>% pull(style) %>% table()
assoli_tot = c(BEBOP=56, COOL=54, FREE=5, FUSION=20, HARDBOP=76, POSTBOP=147, SWING=66, TRADITIONAL=32)
assoli_con_frase = ricerca %>% pull(style) %>% table()
assoli_senza_frase <- assoli_tot - assoli_con_frase
tabella <- rbind(Frase = assoli_con_frase, NoFrase = assoli_senza_frase)
tabella = tabella[,c(8,7,1,2,5,6,3,4)]
tabella
chisq.test(tabella[,-7])
chisq.test(tabella[,-7])$expected
chisq.test(tabella[,-7])$stdres



### Confronto tra artisti a coppie -----------------------------------------

artisti = c("Louis Armstrong", "Benny Goodman", "Charlie Parker", "Dizzy Gillespie",
            "Chet Baker", "Miles Davis", "Lee Konitz", "John Coltrane", 
            "Sonny Rollins", "Herbie Hancock",
            "Michael Brecker", "Chris Potter")
perc_artisti = matrix(0, nrow=length(artisti), ncol=length(artisti))
for(i in 1:length(artisti)) {
  for(j in i:length(artisti)) {
    if(i==j) {
      perc_artisti[i,j] = NA
    }
    else {
      perc = frasi_comuni_artisti(c(artisti[i], artisti[j])) %>% 
        pull(proporzione_min) %>% head(20) %>% mean()
      perc_artisti[i,j] = perc_artisti[j,i] = perc
      print(c(i,j))
    }
  }
}

rownames(perc_artisti) = colnames(perc_artisti) = artisti
pheatmap(perc_artisti,
         cluster_rows = F,
         cluster_cols = F,
         display_numbers = TRUE,   # mostra i valori
         fontsize_row = 10,
         fontsize_col = 10,
         angle_col = 315,
         fontsize_number = 10.5)

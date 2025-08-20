
# Caricamento dei pacchetti -----------------------------------------------

library(rvest)
library(tidyverse)
library(patchwork)


# Sistemazione tabella info_assoli ----------------------------------------

info = read.csv("info_assoli_non_sistemato.csv", header=T)
melody = read.csv2("melody.csv")


# CODICE PER AGGIUNGERE IL NUMERO DI NOTE

head(melody)
df.numnote = melody %>% 
  group_by(melid) %>%
  summarise(n = n()) %>%
  as.data.frame()

info$numnotes = df.numnote[,2]



# CODICE PER L'ESTRAZIONE DELLA DURATA IN SECONDI

estrai_duration = function(solo_number) {
  print(solo_number)
  url = paste0("https://jazzomat.hfm-weimar.de/dbformat/synopsis/solo", 
               solo_number, ".html")
  page = tryCatch(read_html(url), error = function(e) return(NA))
  if (is.na(page)) return(NA)
  
  duration = page %>%
    html_nodes("tr:contains('Total Duration') td") %>%
    html_text(trim = TRUE)
  
  return(duration)
}

# Chiamata della funzione (richiede molto tempo)
durations = sapply(1:456, estrai_duration)

# Bisogna selezionare nella stringa solo la parte numerica (sono del tipo "110.4 s")
s.positions = rep(NA,456)
for(i in 1:456) {
  s.positions[i] = gregexpr("s", durations[2,i])[[1]][1]
}

durations_num = as.numeric(substr(durations[2,], 1, s.positions-2))
info$duration = durations_num



# CODICE PER L'ANNO DI REGISTRAZIONE

url = "https://jazzomat.hfm-weimar.de/dbformat/dbcontent.html"
page = read_html(url)

# Estraggo gli anni di registrazione
anni = page %>%
  html_nodes("tr td:nth-child(6)") %>%   # 6 è l'indice della colonna con gli anni
  html_text(trim = TRUE)

info$recordingyear = as.numeric(anni)

# info_assoli sistemato
write.csv2(info, "info_assoli.csv", row.names = FALSE)




# Analisi descrittiva del dataset -----------------------------------------

# Caricando info_assoli direttamente non serve eseguire il blocco di codice precedente.
info = read.csv2("info_assoli.csv", header=T)


# Grafici artisti 

tab.perf = sort(table(info$performer), decreasing=T)

g1 = tibble(artist=names(tab.perf), n=as.vector(tab.perf)) %>% 
  filter(n > 7) %>% 
  mutate(artist = fct_reorder(artist, n)) %>% 
  ggplot(aes(x=n, y=artist)) + 
  geom_bar(stat="identity", fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Numero di assoli", y="Artista")

g2 = tibble(artist=names(tab.perf), n=as.vector(tab.perf)) %>% 
  ggplot(aes(x=n)) +
  geom_histogram(binwidth = 1, fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Numero di assoli", y="Numero di artisti")

g1 + g2

mean(tab.perf)
median(tab.perf)



# Grafici strumenti 

tab.inst = table(info$instrument)

tibble(inst=names(tab.inst), n=as.vector(tab.inst)) %>% 
  ggplot(aes(x=inst, y=n)) + 
  geom_bar(stat="identity", fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Strumento", y="Numero di assoli")

sum(tab.inst[c(1,10,11)]) / sum(tab.inst)



# Grafici stili

tab.style = table(info$style)

tibble(style=names(tab.style), n=as.vector(tab.style)) %>% 
  ggplot(aes(x=style, y=n)) + 
  geom_bar(stat="identity", fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Stile", y="Numero di assoli")



# Grafici ritmi

tab.rh = table(info$rhythmfeel)

tibble(rhythm=names(tab.rh), n=as.vector(tab.rh)) %>% 
  ggplot(aes(x=rhythm, y=n)) + 
  geom_bar(stat="identity", fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Stile ritmico", y="Numero di assoli")

tab.rh[8] / 456



# Grafici tempo

tab.bpm = table(info$avgtempo)

g1 = tibble(tempo=info$avgtempo) %>% 
  ggplot(aes(y=tempo)) +
  geom_boxplot(fill=scales::hue_pal()(4)[3]) +
  theme_bw() +
  labs(y="Tempo medio in BPM") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


g2 = tibble(tempo=info$avgtempo) %>% 
  ggplot(aes(x=tempo)) +
  geom_histogram(binwidth = 8, fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Tempo medio in BPM", y="Numero di assoli") +
  geom_vline(xintercept=c(60,100,140,180), linetype="dashed", lwd=0.8)

g1 + g2

tab.tempoclass = table(info$tempoclass)
tibble(class=names(tab.tempoclass), n=as.vector(tab.tempoclass)) %>% 
  mutate(class = factor(class, levels = c("SLOW", "MEDIUM SLOW", "MEDIUM", "MEDIUM UP", "UP"))) %>% 
  ggplot(aes(x=class, y=n)) + 
  geom_bar(stat="identity", fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Stile ritmico", y="Numero di assoli")

mean(info$avgtempo)
median(info$avgtempo)

tab.tempoclass[5] / 456
sum(info$avgtempo >= 300) / 456



# Grafici per numero di note e durata

g1 = tibble(numnotes=info$numnotes) %>% 
  ggplot(aes(x=numnotes)) +
  geom_boxplot(fill=scales::hue_pal()(4)[3]) +
  theme_bw() +
  labs(x="Numero di note") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

g2 = tibble(numnotes=info$numnotes) %>% 
  ggplot(aes(x=numnotes)) +
  geom_histogram(binwidth = 60, fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Numero di note", y="Numero di assoli")

g3 = tibble(duration=info$duration) %>% 
  ggplot(aes(x=duration)) +
  geom_boxplot(fill=scales::hue_pal()(4)[3]) +
  theme_bw() +
  labs(x="Durata (s)") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

g4 = tibble(duration=info$duration) %>% 
  ggplot(aes(x=duration)) +
  geom_histogram(binwidth = 10, fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Durata (s)", y="Numero di assoli")


(g1 | g2) / (g3 | g4)

max(info$numnotes)
max(info$duration)

mean(info$numnotes)
median(info$numnotes)
mean(info$duration)
median(info$duration)



# Grafici per l'anno di registrazione

tibble(year=info$recordingyear) %>% 
  ggplot(aes(x=year)) +
  geom_histogram(binwidth = 2, fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Anno di registrazione", y="Numero di assoli") +
  scale_x_continuous(breaks = seq(1920, 2010, by = 10))

breaks = seq(1920,2010,by=10)
labels = paste(breaks[-length(breaks)], breaks[-1], sep = "-")
tab.year = table(cut(info$recordingyear, breaks, labels))
tab.year = cbind(tab.year, round(tab.year/456*100, 1))
colnames(tab.year) = c("Freq", "%")
tab.year

mean(info$recordingyear)



# Grafici per la tonalità

tab.key = table(info$key)
tibble(key=names(tab.key), n=as.vector(tab.key)) %>% 
  ggplot(aes(x=n, y=key)) + 
  geom_bar(stat="identity", fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() +
  labs(x="Numero di assoli", y="Tonalità")

info$key = ifelse(info$key=="Ab", "Ab-maj", info$key)
type.key = substr(info$key, nchar(info$key)-2, nchar(info$key))
type.key = ifelse(type.key=="maj" | type.key=="min", type.key, "mix")
tab.type.key = cbind(table(type.key), round(table(type.key)/456*100))
colnames(tab.type.key) = c("Freq", "%")
rownames(tab.type.key) = c("Maggiore", "Minore", "Mista")
tab.type.key




# Codifica dei dati -------------------------------------------------------

## Serie storiche discrete -------------------------------------------------

N = 456

# Inizializzo le liste con la prima sequenza di informazioni (prima melodia)
onsetlist = list(melody$onset[1:info$numnotes[1]])
durationlist = list(melody$duration[1:info$numnotes[1]])
pitchlist = list(melody$pitch[1:info$numnotes[1]])

# Aggiungo a ogni iterazione alle liste le informazioni sulla i-esima melodia
for(i in 2:N) {
  onset_i = 
    melody$onset[(sum(info$numnotes[1:(i-1)])+1):sum(info$numnotes[1:i])]
  onsetlist = append(onsetlist, list(onset_i))
  
  duration_i = 
    melody$duration[(sum(info$numnotes[1:(i-1)])+1):sum(info$numnotes[1:i])]
  durationlist = append(durationlist, list(duration_i))
  
  pitch_i = 
    melody$pitch[(sum(info$numnotes[1:(i-1)])+1):sum(info$numnotes[1:i])]
  pitchlist = append(pitchlist, list(pitch_i))
}

pitch1 = pitchlist[[1]]
onset1 = onsetlist[[1]]
duration1 = durationlist[[1]]


df_long = tibble(
  idx = 1:length(pitch1),
  onset = onset1,
  duration = duration1,
  pitch = pitch1
) %>%
  pivot_longer(cols = c(onset, duration, pitch),
               names_to = "feature",
               values_to = "value") %>% 
  mutate(feature = factor(feature, levels = c("onset", "duration", "pitch"),
                          labels = c("Onset", "Durate", "Pitch")))

ggplot(df_long, aes(x = idx, y = value)) +
  geom_line(linewidth = 0.6, color = scales::hue_pal()(4)[3]) +
  facet_wrap(~ feature, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(x = "Indice", y = NULL)



## Matrice unica di pitch --------------------------------------------------

matmel = matrix(NA, nrow=456, ncol=max(info$numnotes))
for(i in 1:456) {
  matmel[i,1:info$numnotes[i]] = pitchlist[[i]]
}


par(mai=rep(0.01,4))
image(t(matmel), main="", axes=F)
box()
par(mai=rep(1,4))


# Scelta della soglia: massimizzazione del numero di note
total_numnotes = function(s) {
  sum(info$numnotes > s) * s
}

# Massimizzo la funzione e trovo la soglia ottima
soglia = optimize(function(x) -total_numnotes(x), interval=c(300,400))$minimum
soglia


soglie = 1:1000
valori = sapply(soglie, total_numnotes)

tibble(Soglie = 1:1000, Valori = valori) %>% 
  ggplot(aes(x=Soglie, y=Valori)) +
  geom_line(col=scales::hue_pal()(4)[3], linewidth=0.8) +
  theme_bw() +
  geom_vline(xintercept = soglia, linetype="dashed") +
  geom_hline(yintercept = sum(info$numnotes > soglia)*soglia, linetype="dashed") +
  labs(x = "Numero massimo di note per riga", y = "Numero totale di note")

# Nuova soglia di 150 note
soglia2 = 150
mean(info$numnotes > soglia2)
sum(info$numnotes > soglia2)

idx = which(info$numnotes > soglia2)
melody_rid = melody[melody$melid %in% idx,]

matmel2 = c()
for(i in idx) {
  matmel2 = rbind(matmel2, pitchlist[[i]][1:soglia2])
}
dim(matmel2)

par(mai=rep(0.01,4))
image(t(matmel2), main="", axes=F)
box()
par(mai=rep(1,4))


# Riordinata
order = order(as.numeric(as.factor(info$instrument[idx])))
par(mai=rep(0.01,4))
image(t(matmel2[order,]), main="", axes=F)
box()
cumsum(round(table(info$instrument[idx][order]) / 406, 3))
abline(h=cumsum(table(info$instrument[idx][order]) / 406), lty=2)
par(mai=rep(1,4))



## Array di matrici (piano roll) -------------------------------------------

numsec = 60
soglia = 0.02
melodyarray = array(NA, dim=c(100, 1/soglia*numsec, 456))

# Richiede qualche minuto
for(m in 1:456) {
  d = durationlist[[m]]
  p = pitchlist[[m]]
  o = onsetlist[[m]]
  onsets = o - o[1]
  
  v = rep(NA, 1/soglia*numsec)
  
  # Inizializzo un indice per ciclare all'interno della sequenza di note
  j = 1
  # Ciclo all'interno del vettore inizialmente vuoto che conterrà pitch e NA
  for(i in 1:length(v)) {
    sec = seq(0, numsec, by=soglia)
    # Se sono in un buco vuoto, a sinistra della prossima nota:
    if(sec[i] < onsets[j] & sec[i] < onsets[j]+d[j]) {
      # non faccio nulla, tengo NA nella cella. In una prossima iterazione
      next                                        # arriverò dentro la nota.
    }
    # Altrimenti se sono proprio dentro la nota:
    else if(sec[i] >= onsets[j] & sec[i] < onsets[j]+d[j]) {
      # pongo la cella i-esima uguale al pitch j-esimo per indicare
      # che ho una nota con quel pitch.
      v[i] = p[j]
    }
    # Altrimenti se sono in un buco vuoto a destra della nota:
    else if(sec[i] >= onsets[j] & sec[i] >= onsets[j]+d[j]) {
      # Se ho ancora altre note a destra, tengo NA e mi sposto
      # di una nota a destra.
      if(j < length(d)) 
        j = j+1
      # Provo a vedere se sono già dentro la nota successiva alla j-esima
      if(sec[i] >= onsets[j] & sec[i] < onsets[j]+d[j]) {
        v[i] = p[j]     # Segno il pitch della nota j nella cella j
      }
    }
  }
  
  # Metto il vettore dentro la m-esima matrice
  for(i in 1:length(v)) {
    if(!is.na(v[i])) {
      melodyarray[v[i],i,m] = 1
    }
  }
  print(m)
}

# Rappresentazione del primo assolo con image
par(mai=rep(0.01,4))
image(t(melodyarray[,,1]), main="", axes=F)
box()
par(mai=rep(1,4))



# Serie per il volume
N = 456
loudnesslist = list(melody$loud_med[1:info$numnotes[1]])
for(i in 2:N) {
  loudness_i = 
    melody$loud_med[(sum(info$numnotes[1:(i-1)])+1):sum(info$numnotes[1:i])]
  # Sistemo i valori NA o < 0
  idx_na = which(is.na(loudness_i) | loudness_i < 0)
  loudness_i[idx_na] = mean(loudness_i[-idx_na])
  loudnesslist = append(loudnesslist, list(loudness_i))
}

# Esempio di grafico dei volumi per la prima melodia:
plot(loudnesslist[[1]], type="l", main="Volumi delle note della prima melodia")

# Ora vado a ricreare la matrice finale considerando anche i volumi
numsec = 60
soglia = 0.05
melodyarray2 = array(NA, dim=c(100, 1/soglia*numsec, 456))

for(m in 1:456) {
  l = loudnesslist[[m]]
  d = durationlist[[m]]
  p = pitchlist[[m]]
  o = onsetlist[[m]]
  onsets = o - o[1]
  
  v = rep(NA, 1/soglia*numsec)
  loud = rep(NA, 1/soglia*numsec)
  
  # Inizializzo un indice per ciclare all'interno della sequenza di note
  j = 1
  # Ciclo all'interno del vettore inizialmente vuoto che conterrà pitch e NA
  for(i in 1:length(v)) {
    sec = seq(0, numsec, by=soglia)
    # Se sono in un buco vuoto, a sinistra della prossima nota:
    if(sec[i] < onsets[j] & sec[i] < onsets[j]+d[j]) {
      # non faccio nulla, tengo NA nella cella. In una prossima iterazione 
      # arriverò dentro la nota
      next
    }
    # Altrimenti se sono proprio dentro la nota:
    else if(sec[i] >= onsets[j] & sec[i] < onsets[j]+d[j]) {
      v[i] = p[j]    # pongo la cella i-esima di v uguale al pitch j-esimo 
      loud[i] = l[j]    # pongo la cella i-esima di loud uguale al volume j-esimo
    }
    # Altrimenti se sono in un buco vuoto a destra della nota:
    else if(sec[i] >= onsets[j] & sec[i] >= onsets[j]+d[j]) {
      # Se ho ancora altre note a destra, tengo NA e mi sposto di una nota a destra
      if(j < length(d)) 
        j = j+1
      # Provo a vedere se sono già dentro la nota successiva alla j-esima
      if(sec[i] >= onsets[j] & sec[i] < onsets[j]+d[j]) {
        v[i] = p[j]      # Segno il pitch della nota nella cella i
        loud[i] = l[j]    # segno il volume della nota nella cella i
      }
    }
  }
  
  # Metto i volumi mediani delle note nelle m-esime matrici 
  for(i in 1:length(v)) {
    if(!is.na(v[i])) {
      melodyarray2[v[i],i,m] = loud[i]
    }
  }
  print(m)
}

# Rappresento graficamente con image
par(mai=rep(0.01,4))
image(t(melodyarray2[,,1]), main="", axes=F)
box()
par(mai=c(1,1,1,1))


# Caricamento dei pacchetti ------------------------------------------------

library(tidyverse)
library(patchwork)
library(Rtsne)
library(ggfortify)
library(dlm)
library(reshape2)
library(scales)
library(forecast)


# PCA e t-SNE  -------------------------------------------------------------

## Sulla matrice unica di pitch --------------------------------------------

# La matrice matmel usata qua è la seconda del capitolo precedente (matmel2)

load("dati_capitolo_2.Rdata")
info_rid = info[which(info$numnotes > 150),]

# tSNE
set.seed(10)
tsne = Rtsne(matmel, perplexity = 20)
colour = as.factor(info_rid$instrument)
df1 = data.frame("X1" = tsne$Y[,1], "X2" = tsne$Y[,2], 
                 "Strumento"=colour)
g1 = ggplot(df1, aes(x=X1, y=X2, color=Strumento)) + geom_point() + theme_bw()

# PCA
g2 = autoplot(prcomp(matmel, scale.=T), data=df1, colour="Strumento") +
  theme_bw()

g1 + g2


# Solo i 5 strumenti più rappresentati

ins = c("ts", "tp", "as", "tb", "ss")
idx = info_rid$instrument %in% ins

# t-SNE
set.seed(10)
tsne = Rtsne(matmel[idx,], perplexity = 20)
colour = as.factor(info_rid$instrument[idx])
df1 = data.frame("X1" = tsne$Y[,1], "X2" = tsne$Y[,2], 
                 "Strumento"=colour)
g1 = ggplot(df1, aes(x=X1, y=X2, color=Strumento)) + geom_point() + theme_bw()

# PCA
g2 = autoplot(prcomp(matmel[idx,], scale.=T), data=df1, colour="Strumento") +
  theme_bw()

g1 + g2


# Solo per i 4 musicisti di sax tenore più rappresentati

info_rid[info_rid$instrument=="ts", "performer"] %>% 
  table() %>% sort(decreasing=T)


perf = c("John Coltrane", "Sonny Rollins", "Michael Brecker", 
         "Wayne Shorter")
idx = info_rid$instrument == "ts" & info_rid$performer %in% perf

# t-SNE
set.seed(10)
tsne = Rtsne(matmel[idx,], perplexity = 5)
colour = as.factor(info_rid$performer[idx])
df1 = data.frame("X1" = tsne$Y[,1], "X2" = tsne$Y[,2], 
                 "Artista"=colour)
g1 = ggplot(df1, aes(x=X1, y=X2, color=Artista)) + geom_point() + theme_bw()

# PCA
g2 = autoplot(prcomp(matmel[idx,], scale.=T), data=df1, colour="Artista") +
  theme_bw()

g1 + g2


## Con la matrice dei pitch centrati per strumento ---------------------

ins.uni = unique(info_rid$instrument)
matmelstd = matrix(NA,406,150)
for(ins in ins.uni) {
  idx.ins = which(info_rid$instrument==ins)
  meanins = mean(matmel[idx.ins,])
  matmelstd[idx.ins,] = matmel[idx.ins,] - meanins
}
dim(matmelstd)

# t-SNE
set.seed(10)
tsne = Rtsne(matmelstd, perplexity = 20)
colour = as.factor(info_rid$instrument)
df1 = data.frame("X1" = tsne$Y[,1], "X2" = tsne$Y[,2], 
                 "Strumento"=colour)
g1 = ggplot(df1, aes(x=X1, y=X2, color=Strumento)) + geom_point() + theme_bw()

# PCA
g2 = autoplot(prcomp(matmelstd, scale.=F), data=df1, colour="Strumento", loadings=F) +
  theme_bw()

g1 + g2



## Sulla nuova matrice info2 -----------------------------------------------

info2 = info %>% 
  select(c(numnotes, duration, avgtempo)) %>% 
  add_column(mean_pitch=rep(NA,456), 
             sd_pitch=rep(NA,456),
             mean_duration=rep(NA,456),
             sd_duration=rep(NA,456),
             mean_loudness=rep(NA,456),
             sd_loudness=rep(NA,456))


for(i in 1:456) {
  info2[i,"mean_pitch"] = mean(pitchlist[[i]])
  info2[i,"sd_pitch"] = sd(pitchlist[[i]])
  info2[i,"mean_duration"] = mean(durationlist[[i]])
  info2[i,"sd_duration"] = sd(durationlist[[i]])
  info2[i,"mean_loudness"] = mean(loudnesslist[[i]])
  info2[i,"sd_loudness"] = sd(loudnesslist[[i]])
}

set.seed(20)
tsne = Rtsne(info2, perplexity=30, ndims=2)
df = tibble("X1"=tsne$Y[,1], "X2"=tsne$Y[,2], Stile=info$style,
            Anno=info$recordingyear, 
            Tempo=factor(info$tempoclass, levels=
                           c("UP", "MEDIUM UP", "MEDIUM", "MEDIUM SLOW", "SLOW")),
            Durata=info$duration)
g1 = ggplot(df, aes(x=X1, y=X2, col=Stile)) +
  geom_point() +
  theme_bw()
g2 = ggplot(df, aes(x=X1, y=X2, col=Anno)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_c(option="D", direction=-1)
g1 + g2


g3 = ggplot(df, aes(x=X1, y=X2, col=Tempo)) +
  geom_point() +
  theme_bw()

g4 = ggplot(df, aes(x = X1, y = X2, color = log(Durata))) +
  geom_point() +
  scale_color_gradientn(colours = viridisLite::viridis(5),
                        values = scales::rescale(c(7,4,1))) +
  theme_bw()

g3 + g4



cor.test(info$duration, info$recordingyear)

ggplot(df, aes(x=Anno, y=Durata)) +
  geom_point() +
  geom_smooth(method="lm", se=T, color=scales::hue_pal()(1)) +
  theme_bw()

lm1 = lm(info$duration ~ info$recordingyear)
summary(lm1)
confint(lm1)





# Modelli lineari dinamici ------------------------------------------------

matmel_long = reshape2::melt(t(matmel))
matmel_long$instrument = as.factor(info_rid$instrument[matmel_long$Var2])
matmel_long$performer = as.factor(info_rid$performer[matmel_long$Var2])
colnames(matmel_long) = c("ID_note", "ID_melody", "pitch", "instrument", "performer")
matmel_long$ID_melody = as.factor(matmel_long$ID_melody)

matmel_long_meanins = matmel_long %>%
  group_by(ID_note, instrument) %>%
  summarise(meanpitch = mean(pitch), .groups = "drop") %>% 
  filter(instrument %in% c("ts", "ss", "as", "tp", "tb"))

ggplot(matmel_long_meanins, aes(x=ID_note, y=meanpitch, color=instrument)) +
  geom_line(linewidth=0.8) +
  labs(x="Indice della nota", y="Pitch medio", col="Strumento") +
  theme_bw()

# Funzione da passare all'ottimizzatore
buildTemp = function(x, m, v) {
  dlm(FF = 1, V = exp(x[2]), GG = 1, W = 1.5,
      m0 = m, C0 = v)
}

matsmoothed = matrix(0, nrow=nrow(matmelstd), ncol=150)
for(i in 1:nrow(matmelstd)) {
  y = matmelstd[i,]
  mean5 = mean(matmelstd[i, 1:5])
  var5 = var(matmelstd[i, 1:5])
  
  fitTemp = dlmMLE(y, parm = rep(1,2), hessian = TRUE,
                   build = function(x) buildTemp(x, mean5, var5))
  modTemp = buildTemp(fitTemp$par, mean5, var5)
  filtered = dlmFilter(y, modTemp)
  smoothed = dlmSmooth(filtered)$s
  
  matsmoothed[i,] = smoothed[-1]
  print(i)
}

df1 = data.frame(pitch=c(matmelstd[1,], matsmoothed[1,]), index=1:150, 
                 status=factor(c(rep("Originale",150), rep("Lisciata", 150)), 
                               levels=c("Originale", "Lisciata")))
g1 = ggplot(df1, aes(x=index, y=pitch, color=status)) +
  geom_line(linewidth=0.8) +
  labs(title="Assolo 1: Art Pepper - Anthropology (1979)",
       x="Indice della nota", y="Pitch", col="Serie") +
  scale_color_manual(values = c("#707070", "red")) +
  theme_bw()

df2 = data.frame(pitch=c(matmelstd[220,], matsmoothed[220,]), index=1:150, 
                 status=factor(c(rep("Originale",150), rep("Lisciata", 150)), 
                               levels=c("Originale", "Lisciata")))
g2 = ggplot(df2, aes(x=index, y=pitch, color=status)) +
  geom_line(linewidth=0.8) +
  labs(title="Assolo 220: John Coltrane - Body and Soul (1960)",
       x="Indice della nota", y="Pitch", col="Serie") +
  scale_color_manual(values = c("#707070", "red")) +
  theme_bw()

g1 / g2




# Grafici degli assoli lisciati per alcuni artisti

matsmoothed_long = melt(t(matsmoothed))
colnames(matsmoothed_long) = c("ID_note", "ID_melody", "pitch") 
matsmoothed_long$ID_melody = as.factor(matsmoothed_long$ID_melody)
matsmoothed_long$performer = matmel_long$performer
matsmoothed_long$recordingyear = 
  as.factor(info_rid$recordingyear[matsmoothed_long$ID_melody])
matsmoothed_long$style = 
  as.factor(info_rid$style[matsmoothed_long$ID_melody])


# Setto la palette di colori
style_colors = c("TRADITIONAL" = hue_pal()(8)[1],
                 "SWING" = hue_pal()(8)[2],
                 "POSTBOP" = hue_pal()(8)[3],
                 "HARDBOP" = hue_pal()(8)[4],
                 "FUSION" = hue_pal()(8)[5],
                 "FREE" = hue_pal()(8)[6],
                 "COOL" = hue_pal()(8)[7],
                 "BEBOP" = hue_pal()(8)[8])

perf = c("Louis Armstrong", "Benny Goodman", "Charlie Parker", "Miles Davis",
         "John Coltrane", "Michael Brecker")

matsmoothed_long_filt = matsmoothed_long %>% 
  filter(performer %in% perf) 
str(matsmoothed_long_filt)


IDs = c(258,259,260,262,
        18,19,20,24,
        51,53,55,59,
        273,277,281,285,
        195,202,206,208,
        263,265,267,272)
matsmoothed_long_filt = matsmoothed_long_filt %>% 
  filter(ID_melody %in% IDs) %>% 
  mutate(ID_melody = factor(ID_melody, levels=IDs)) %>% 
  mutate(performer = factor(performer, levels=perf))


hue = hue_pal()(6)

g1 = matsmoothed_long_filt %>% filter(ID_melody %in% IDs[1:12]) %>% 
  ggplot(aes(x=ID_note, y=pitch, group=ID_melody, color=performer)) +
  geom_line(size = 0.8) + 
  facet_wrap(~ ID_melody) +
  labs(x="Indice della nota",
       y="Pitch", col="Artista") +
  theme_bw() +
  scale_color_manual(values = c("Louis Armstrong" = hue[1],
                                "Benny Goodman" = hue[2],
                                "Charlie Parker" = hue[3])) +
  ylim(-20,20)

g2 = matsmoothed_long_filt %>% filter(ID_melody %in% IDs[13:24]) %>% 
  ggplot(aes(x=ID_note, y=pitch, group=ID_melody, color=performer)) +
  geom_line(size = 0.8) + 
  facet_wrap(~ ID_melody) +
  labs(x="Indice della nota",
       y="Pitch", col="Artista") +
  theme_bw() +
  scale_color_manual(values = c("Miles Davis" = hue[4],
                                "John Coltrane" = hue[5],
                                "Michael Brecker" = hue[6])) +
  ylim(-20,20)


g1
g2



# Nuova serie di pitch sul tempo ------------------------------------------

o = list()
for(i in 1:length(onsetlist)) {
  o = append(o, list(onsetlist[[i]] - onsetlist[[i]][1]))
}

# Bpm mediano:
medtempo = median(info$avgtempo)
medtempo

# Creazione della nuova serie
diffs = list()
for(i in 1:length(onsetlist)) {
  molt = info$avgtempo[i] / medtempo   # molt per adeguamento al tempo mediano
  newseries = diff(pitchlist[[i]]) / diff(o[[i]] * molt)
  diffs[[i]] = newseries
}

# Nuova serie: differenza di pitch diviso la durata
diffs2 = list()
for(i in 1:length(durationlist)) {
  molt = info$avgtempo[i] / medtempo
  newseries = diff(pitchlist[[i]]) /
    (durationlist[[i]][-length(durationlist[[i]])] * molt)
  diffs2[[i]] = newseries
}


# Alcuni grafici

df = tibble(Valore = diffs2[[10]],
            Indice = 1:length(diffs2[[10]]),
            ID = rep(10,length(diffs2[[10]])))
ggplot(df, aes(x=Indice, y=Valore)) +
  geom_line(linewidth=0.5, col=scales::hue_pal()(4)[3]) +
  facet_wrap(~ID, scales="free_y", nrow=3) +
  theme_bw()


# Correzione delle durate anomale
diffs2 = list()
for(i in 1:length(durationlist)) {
  molt = info$avgtempo[i] / medtempo
  
  # Al posto della durata esageratamente bassa metto la seconda più bassa
  if(sum(durationlist[[i]] < 0.0001) != 0) {
    idx.which = which(durationlist[[i]] < 0.0001)
    durationlist[[i]][idx.which] = mean(durationlist[[i]][-idx.which])
  }
  
  newseries = diff(pitchlist[[i]]) /
    (durationlist[[i]][-length(durationlist[[i]])] * molt)
  diffs2[[i]] = newseries
}

# Altre rappresentazioni grafiche
df = tibble(Valore = c(diffs2[[1]], diffs2[[10]], diffs2[[220]]),
            Indice = c(1:length(diffs2[[1]]), 1:length(diffs2[[10]]),
                       1:length(diffs2[[220]])),
            ID = c(rep(1,length(diffs2[[1]])), rep(10,length(diffs2[[10]])),
                   rep(220,length(diffs2[[220]]))))
ggplot(df, aes(x=Indice, y=Valore)) +
  geom_line(linewidth=0.5, col=scales::hue_pal()(4)[3]) +
  facet_wrap(~ID, scales="free_y", nrow=3) +
  theme_bw()




## Analisi con modelli ARIMA -----------------------------------------------

soglia = 149
idx = which(info$numnotes > soglia+1)

# Ora mi salvo tutte le serie in una matrice
matdiff = matrix(NA, nrow=406, ncol=149)
for(i in 1:406) {
  matdiff[i,] = diffs2[[idx[i]]][1:soglia]
}

armapar = matrix(0, nrow=nrow(matdiff), ncol=2)
for(i in 1:nrow(matdiff)) {
  mod = Arima(matdiff[i,], order=c(1,0,1), include.constant=F)
  armapar[i,] = coef(mod)
}

df = data.frame(phi=armapar[,1], theta=armapar[,2],
                instrument = info_rid$instrument,
                performer = info_rid$performer,
                style = info_rid$style)

# idxperf = which(info_rid$performer %in% performers)
g1 = ggplot(df, aes(x=phi, y=theta, col=instrument)) +
  geom_point() +
  theme_bw() +
  labs(col="Strumento")

g2 = ggplot(df, aes(x=phi, y=theta, col=style)) +
  geom_point() +
  theme_bw() +
  labs(col="Stile")

g1 + g2

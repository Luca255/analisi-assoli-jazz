
# Caricamento dei pacchetti -----------------------------------------------

library(tidyverse)
library(fmsb)
library(rsample)
library(rpart)
library(nnet)


# Operazioni preliminari --------------------------------------------------

load("dati_capitolo_3.Rdata")
pitchdifflist = lapply(pitchlist, diff)


breaks = c(-Inf, seq(-8,7,by=1), Inf)
tab = table(cut(pitchdifflist[[200]], breaks))
tab

tags_intervals = c("-8 in giù", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0",
                   "+1", "+2", "+3", "+4", "+5", "+6", "+7", "+8 in su")

# Grafico delle proporzioni di intervalli nell'assolo 1
as_tibble(as.vector(tab)) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(interval = factor(tags_intervals, levels = tags_intervals)) %>% 
  ggplot(aes(y=value, x=interval)) + 
  geom_col(fill=scales::hue_pal()(4)[3], col="black") +
  theme_bw() + 
  labs(y = "Frequenza", x="Intervallo (semitoni)")




# Calcolo dei theta -------------------------------------------------------

# Frequenze relative di occorrenza per tutti gli assoli
intervals_class = matrix(NA, nrow=456, ncol=length(tab))
for(i in 1:456) {
  classes = cut(pitchdifflist[[i]], breaks)
  intervals_class[i,] = table(classes) / length(classes)
}


# Invece le frequenze assolute
n = matrix(NA, nrow=456, ncol=length(tab))
for(i in 1:456) {
  classes = cut(pitchdifflist[[i]], breaks)
  n[i,] = table(classes)
}


# Calcolo dei theta stimati di riferimento (9 per ognuno degli 8 generi)
# Vado a fare la mediana per ogni classe, per ogni stile

theta = matrix(NA, nrow=8, ncol=length(tab))
styles = unique(info$style)
for(i in 1:length(styles)) {
  idx_style = which(info$style == styles[i])
  medians = apply(intervals_class[idx_style,], 2, median)
  theta[i,] = medians
}

# Controllo e correggo eventuali frequenze nulle
sum(theta == 0)
theta = ifelse(theta == 0, 0.000001, theta)

# Pongo somma dei theta = 1
apply(theta, 1, sum)
theta = t(apply(theta, 1, function(th) th/sum(th)))
apply(theta, 1, sum)



# Grafico dei profili mediani per gli stili

df <- as.data.frame(cbind(styles, theta), stringsAsFactors = FALSE)
colnames(df)[1] <- "style"
df[,-1] <- lapply(df[,-1], as.numeric)
glimpse(df)

df_long <- df %>%
  pivot_longer(-style, names_to = "phrase", values_to = "freq") %>%
  mutate(phrase = as.integer(gsub("V", "", phrase))-1) %>%
  mutate(phrase = factor(phrase, levels = 1:length(tags_intervals), labels = tags_intervals))


ggplot(df_long, aes(x = phrase, y = freq, color = style, group=style)) +
  geom_line(linewidth=0.8) +
  theme_bw() +
  labs(x = "Intervallo (semitoni)", y = "Proporzione", color = "Stile") +
  scale_color_discrete()



# Stima dei pesi omega tramite massima verosimiglianza ------------------------

# Funzione di verosimiglianza negativa
negloglik = function(w, theta, n) {
  w = w / sum(w)        # riparametrizzazione per somma = 1
  res = sum(sapply(1:length(n), function(j) n[j] * sum(w*log(theta[,j]))))
  res2 = sum(sapply(1:length(n), function(h) prod(theta[,h]^w)))
  -(res - sum(n)*log(res2))
}


w.hat = matrix(NA, nrow=456, ncol=8)
for(i in 1:456) {
  w.hat[i,] = optim(par=rep(1,8), negloglik, theta=theta, n=n[i,],
                    lower = rep(0,8), method="L-BFGS-B")$par
  print(i)
}
w.hat = t(apply(w.hat, 1, function(zz) zz / sum(zz)))   # riparametrizzazione per somma = 1
head(round(w.hat, 2))



# Grafici a radar ---------------------------------------------------------

draw_radar_chart = function(style) {
  df = as.data.frame(
    rbind(rep(1,8), rep(0,8), w.hat[which(info$style==style),]))
  colnames(df) = styles
  df = df[,c(3,2,7,1,6,4,8,5)]
  radarchart(df, axistype=1, 
             cglty = 1,       # Grid line type
             cglcol = "gray", # Grid line color
             cglwd = 2,       # Line width of the grid
             plwd = 2,        # Width of the line
             plty = 1,        # Line type of the line
             pty = 19, 
             vlcex = 1.3)        
}

par(mai=rep(0,4))
draw_radar_chart("TRADITIONAL")
draw_radar_chart("SWING")
draw_radar_chart("BEBOP")
draw_radar_chart("COOL")
draw_radar_chart("HARDBOP")
draw_radar_chart("POSTBOP")
draw_radar_chart("FREE")
draw_radar_chart("FUSION")
par(mai=rep(1,4))




# Classificazione col modello di scoring ----------------------------------

data = data.frame(style = as.factor(info$style), intervals_class, n)
glimpse(data)

set.seed(123)
split = initial_split(data)
train = training(split)
test = testing(split)



# Dati di train

style_frequencies = matrix(NA, nrow=8, ncol=length(tab)+1)
styles = unique(info$style)
for(i in 1:length(styles)) {
  idx_style = which(train$style == styles[i])
  means = apply(train[idx_style, 2:18], 2, median)
  style_frequencies[i,] = c(styles[i], means)
}

matplot(t(style_frequencies[,-1]), col=1:8, type="l")


theta.train = style_frequencies[,-1]
theta.train = apply(theta.train, 2, as.numeric)
sum(theta.train == 0)
theta.train = ifelse(theta.train == 0, 0.000001, theta.train)

# Pongo somma dei theta = 1
for(i in 1:nrow(theta.train)) {
  theta.train[i,] = theta.train[i,] / sum(theta.train[i,])
}
matplot(t(theta.train), type="l")



w.hat.train = matrix(NA, nrow=nrow(train), ncol=8)
for(i in 1:nrow(train)) {
  w.hat.train[i,] = optim(par=rep(1,8), negloglik, theta=theta.train, n=as.vector(t(train[i,19:35])),
                    lower = rep(0,8), method="L-BFGS-B")$par
  print(i)
}
w.hat.train = t(apply(w.hat.train, 1, function(zz) zz / sum(zz)))

head(round(w.hat.train, 2))
# Classificazione del genere in base all'omega più alto
style_id = apply(w.hat.train, 1, which.max)

# Verifica della classificazione 
table(styles[style_id])
table(styles[style_id], train$style)
sum(styles[style_id] == train$style) / nrow(train)        # 36.5 %    
table(styles[style_id], train$style) %>% prop.table(2) %>% round(3)



# Dati di test

w.hat.test = matrix(NA, nrow=nrow(test), ncol=8)
for(i in 1:nrow(test)) {
  w.hat.test[i,] = optim(par=rep(1,8), negloglik, theta=theta.train, n=as.vector(t(test[i,19:35])),
                    lower = rep(0.00001,8), method="L-BFGS-B")$par
  print(i)
}
w.hat.test = t(apply(w.hat.test, 1, function(zz) zz / sum(zz)))

round(w.hat.test, 2)
# Classificazione del genere in base all'omega più alto
style_id = apply(w.hat.test, 1, which.max)

# Verifica della classificazione 
table(styles[style_id])
table(styles[style_id], test$style)
sum(styles[style_id] == test$style) / nrow(train)        # 12.28
table(styles[style_id], test$style) %>% prop.table(2) %>% round(3)



# Albero di classificazione -----------------------------------------------

set.seed(123)
data = data.frame(info$style, intervals_class, w.hat)
split = initial_split(data)
train = training(split)
test = testing(split)

nuovi_nomi <- c("style", "nmInf", "nm7", "nm6", "nm5", "nm4", "nm3", "nm2", "nm1", 
                "n0", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "nInf",
                "w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8")
colnames(train) <- nuovi_nomi
colnames(test) <- nuovi_nomi

train$style = as.factor(train$style)
test$style = as.factor(test$style)



# Albero con solo le frequenze assolute
set.seed(123)
tree_mod = rpart(style ~ nmInf + nm7 + nm6 + nm5 + nm4 + nm3 + nm2 + nm1 + n0 +
                   n1 + n2 + n3 + n4 + n5 + n6 + n7 + nInf, data=train)    # con solo n
rpart.plot::rpart.plot(tree_mod, fallen.leaves = F)

# Dati di train
pred_styles = predict(tree_mod, type="class", newdata=train)
table(pred_styles, train$style)
table(pred_styles, train$style) %>% prop.table(2) %>% round(2)
sum(pred_styles == train$style) / 456    # 42.1 % 

# Dati di test
pred_styles = predict(tree_mod, type="class", newdata=test)
table(pred_styles, test$style)
table(pred_styles, test$style) %>% prop.table(2) %>% round(2)
sum(pred_styles == test$style) / 456     # 8.3 %




# Albero con frequenze assolute e pesi w.hat 
set.seed(123)
tree_mod = rpart(style ~ ., data=train)
tree_mod
rpart.plot::rpart.plot(tree_mod, fallen.leaves = F)

# Direttamente sui dati di test
pred_styles = predict(tree_mod, type="class", newdata=test)
table(pred_styles, test$style)
table(pred_styles, test$style) %>% prop.table(2) %>% round(2)
sum(pred_styles == test$style) / 456        # 9.6 % 



# Rete neurale ------------------------------------------------------------

# Con solo frequenze assolute
set.seed(123)
m1 = nnet(style ~ nmInf + nm7 + nm6 + nm5 + nm4 + nm3 + nm2 + nm1 + n0 +
            n1 + n2 + n3 + n4 + n5 + n6 + n7 + nInf, data=train,
          size=20, maxit=3000, abstol=9e-10, decay=0.1, entropy=TRUE)

# Dati di train
pred_styles = predict(m1, type="class", newdata=train)
table(pred_styles, train$style)
table(pred_styles, train$style) %>% prop.table(2) %>% round(2)
sum(pred_styles == train$style) / 456        # 31.3 %

# Dati di test
pred_styles = predict(m1, type="class", newdata=test)
table(pred_styles, test$style)
table(pred_styles, test$style) %>% prop.table(2) %>% round(2)
sum(pred_styles == test$style) / 456        # 10.7 %



# Con intervals_class e w.hat
set.seed(123)
m3 = nnet(style ~ ., data=train,
          size=20, maxit=3000, abstol=9e-10, decay=0.1, entropy=TRUE)

# Direttamente sui dati di test
pred_styles = predict(m3, type="class", newdata=test)
table(pred_styles, test$style)
table(pred_styles, test$style) %>% prop.table(2) %>% round(2)
sum(pred_styles == test$style) / 456        # 11.8 %

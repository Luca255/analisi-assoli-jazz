
# Caricamento dei pacchetti -----------------------------------------------

library(tidyverse)
library(cluster)
library(stringdist)
library(Rcpp)

load("dati_capitolo_3.Rdata")


# Clustering con distanza euclidea ----------------------------------------

numnotes = info$numnotes
max_len = max(numnotes[numnotes < 4000])
matbig = matrix(NA, nrow=456, ncol=max_len)
for(i in 1:456) {
  n_times = trunc(max_len / numnotes[i])
  remaining = max_len - n_times * numnotes[i]
  pitches = pitchlist[[i]]
  if(remaining!=0) 
    matbig[i,] = c(rep(pitches, n_times), pitches[1:remaining])
  else  
    matbig[i,] = rep(pitches, n_times)
}

# image(t(matbig))

D = dist(matbig, method="euclidean")
summary(D)

heatmap(as.matrix(D))


hcw = hclust(D, method="ward.D2")
plot(hcw)
rect.hclust(hcw, k=4)

gruppi = cutree(hcw, k=4)
names(gruppi) <- 1:length(gruppi)

plot(silhouette(gruppi, D))

table(info$style, gruppi)
table(info$instrument, gruppi)

pitchlist[which(gruppi==1)] %>% unlist() %>% mean()
pitchlist[which(gruppi==2)] %>% unlist() %>% mean()
pitchlist[which(gruppi==3)] %>% unlist() %>% mean()
pitchlist[which(gruppi==4)] %>% unlist() %>% mean()


## Euclidea e pitch centrati per strumento ---------------------------------

ins_uni = unique(info$instrument)
matbigstd = matbig
for(ins in ins_uni) {
  idx.ins = which(info$instrument==ins)
  meanins = mean(matbig[idx.ins,])
  matbigstd[idx.ins,] = matbig[idx.ins,] - meanins
}
matbigstd = round(matbigstd)
dim(matbigstd)


Dstd = dist(matbigstd, method="euclidean")
summary(Dstd)
heatmap(as.matrix(Dstd))


hcw = hclust(Dstd, method="ward.D2")
plot(hcw)
rect.hclust(hcw, k=4)

gruppi = cutree(hcw, k=4)
names(gruppi) <- 1:length(gruppi)
plot(silhouette(gruppi, Dstd))

table(info$style, gruppi)
table(info$instrument, gruppi)
table(info$performer, gruppi)


plot(silhouette(gruppi, Dstd))

matbigstd[which(gruppi==1),] %>% as.vector() %>% mean()
matbigstd[which(gruppi==2),] %>% as.vector() %>% mean()
matbigstd[which(gruppi==3),] %>% as.vector() %>% mean()
matbigstd[which(gruppi==4),] %>% as.vector() %>% mean()

table(gruppi)



# Clustering con distanza di Jaccard --------------------------------------

# Direttamente sui pitch centrati per strumenti

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

# Salvataggio dei pitch unici
uniques_std_list = list()
for(i in 1:456) {
  uniques_std_list[[i]] = unique(matbigstd[i,])
}
uniques_std = unlist(uniques_std_list)
uniques_std = sort(unique(uniques_std))

# Setto la mappa
mappa = c(letters, LETTERS, as.character(0:3))


# Rimappo matbigstd in stringhe
pitchremappedstd_list = list()
for(i in 1:456) {
  pitchremappedstd_list[[i]] = remap(matbigstd[i,], uniques_std, mappa)
}
pitchremappedstd = unlist(pitchremappedstd_list, use.names = F)



# Monogrammi:

diststd.q1 = stringdistmatrix(pitchremappedstd, pitchremappedstd, method = "jaccard", q = 1)
heatmap(diststd.q1)

hcw = hclust(as.dist(diststd.q1), method="ward.D2")
plot(hcw)
rect.hclust(hcw, k=4)

gruppi = cutree(hcw, k=4)
plot(silhouette(gruppi, diststd.q1))

table(info$style, gruppi)
table(info$instrument, gruppi)
table(info$performer, gruppi)



# Bigrammi

diststd.q2 = stringdistmatrix(pitchremappedstd, pitchremappedstd, method = "jaccard", q = 2)
heatmap(diststd.q2)

hcw = hclust(as.dist(diststd.q2), method="ward.D2")
plot(hcw)
rect.hclust(hcw, k=4)
plot(silhouette(cutree(hcw, k=4), diststd.q2))

table(info$style, cutree(hcw, k=4))
table(info$instrument, cutree(hcw, k=4))
table(info$performer, cutree(hcw, k=4))

cbind(cutree(hcw, k=4), info[,c(2,3,6,7)])

table(info$performer, cutree(hcw, k=4))




# Clustering con distanza di Levenshtein ---------------------------------

# Distanza di Levenshtein in C++
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

# Funzione per generare la matrice di distanze
lev_dist_matrix = function(strings) {
  n = length(strings)
  dist_matrix = matrix(0, n, n)
  rownames(dist_matrix) = colnames(dist_matrix) = 1:n
  
  for (i in 1:n) {
    for (j in i:n) {
      d = levenshtein_cpp(strings[i], strings[j])
      dist_matrix[i,j] = dist_matrix[j,i] = d
    }
    print(i)
  }
  as.dist(dist_matrix)
}

# Prendo solo le prime 500 colonne della matrice usata prima
pitchremappedstd_rid = substr(pitchremappedstd, 1, 500)

distlev = lev_dist_matrix(pitchremappedstd_rid)
dim(as.matrix(distlev))

summary(as.vector(distlev))
heatmap(as.matrix(distlev))


hcw = hclust(as.dist(distlev), method="ward.D2")
plot(hcw)
rect.hclust(hcw, k=4)
plot(silhouette(cutree(hcw, k=4), distlev))

table(info$style, cutree(hcw, k=4))
table(info$instrument, cutree(hcw, k=4))
table(info$performer, cutree(hcw, k=4))




# Clustering con Dynamic Time Warping -------------------------------------

# Funzione in C++
cppFunction('
double dtw_abs_cpp(NumericVector x, NumericVector y) {
  int n = x.size();
  int m = y.size();
  
  NumericMatrix D(n + 1, m + 1);

  for (int i = 0; i <= n; ++i)
    for (int j = 0; j <= m; ++j)
      D(i, j) = R_PosInf;

  D(0, 0) = 0;

  for (int i = 1; i <= n; ++i) {
    for (int j = 1; j <= m; ++j) {
      double cost = std::abs(x[i - 1] - y[j - 1]);
      D(i, j) = cost + std::min({ D(i - 1, j),
                                  D(i, j - 1),
                                  D(i - 1, j - 1)
                                });
    }
  }

  return D(n, m);
}
')

dist_matrix_dtw = function(m) {
  n = nrow(m)
  dist_matrix = matrix(0, n, n)
  rownames(dist_matrix) = colnames(dist_matrix) = 1:n
  
  for (i in 1:n) {
    for (j in i:n) {
      d = dtw_abs_cpp(m[i,], m[j,])
      dist_matrix[i,j] = dist_matrix[j,i] = d
      print(paste(i,j))
    }
  }
  as.dist(dist_matrix)
}

# Impiega circa 5 minuti
distmatdtw = dist_matrix_dtw(matbigstd[,1:500])
heatmap(as.matrix(distmatdtw))

hcw = hclust(distmatdtw, method="ward.D2")
plot(hcw)
rect.hclust(hcw, k=4)

plot(silhouette(cutree(hcw, k=4), distmatdtw))

table(info$style, cutree(hcw, k=4))
table(info$instrument, cutree(hcw, k=4))

heatmap(as.matrix(distmatdtw2)[1:20,1:20])
cbind(cutree(hcw, k=4), info[,c(2,3,7)])

table(info$performer, cutree(hcw, k=4))
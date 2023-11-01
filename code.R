####################################
### ACT-7100 : Travail 1 code
### Jérémie Barde et Assane Kholle
###################################
##### Library utiles #####
library(ReIns)
library(tea)
library(QRM)
library(ismev)
library(latex2exp)
library(ggplot2)
library(readxl)
library(xtable)

#############################
### Code pour la méthode POT -------------------------------------------------------
#############################

##### Importation des données #####
data <- read_excel('Donnees_Intervalles.xlsx')
data <- subset(data, Catastrophe != 'Storms')
data$`Reporting Year` <- as.factor(data$`Reporting Year`)

dat1 <- data # Nom données par Assane

##### Analyse de la base de données #####
str(data)

##### Analyse de la base de données #####
## Dimension de la base de données
dimension <- dim(data)

## Analyse de la variable 'Insured_loss_jitter'
perte <- data$Insured_loss_M

summary(perte) # Statistique descriptive
var(perte)
n <- dimension[1]

# Graphique des contriubtions
vk <- seq(0.01, 0.1, by = 0.01)
g <- sapply(vk, function(i) sum(sort(perte, dec = T)[1:(floor(i*length(perte)))])/sum(perte))
contrib <- data.frame(vk, g)
ggplot2::ggplot(contrib, aes(x=vk, y=g)) + 
  geom_col() + 
  labs(y="Contribution au total des pertes estimées", x="Pourcentage des données",
       title="") +
  theme_bw()

# Graphique des montants de pertes
borne <- c(0, 8e3)
plot(data$date_start[data$Insured_loss_M >= borne[1] & data$Insured_loss_M < borne[2]], 
     y=data$Insured_loss_M[data$Insured_loss_M >= borne[1] & data$Insured_loss_M < borne[2]]/1000, "h", 
     ylab = "Pertes (milliard $)", xlab = "Dates d'occurence", 
     main='')
grid(col = "grey")
abline(h=1000/1e3, col= "red")


# diagramme boite moustache des pertes
boxplot(dat1$Insured_loss_M)

# Tracer un diagramme en boîte sans les valeurs les plus extremes pour la variable "Insured_loss_M"
boxplot(dat1$Insured_loss_M, outline=FALSE)

# Créer un histogramme avec la médiane
ggplot(dat1, aes(x = Insured_loss_M)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  geom_vline(aes(xintercept = median(Insured_loss_M)),
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution de Insured_loss_M avec la Médiane",
       x = "Insured_loss_M",
       y = "Fréquence") +
  theme_minimal()

# Agréger les données par catégorie (Catastrophe) et calculer la fréquence
agg_data <- table(dat1$Catastrophe)

# Créer le diagramme en bande
barplot(agg_data, col = "blue", main = "Diagramme en Bande des Catastrophes")

# Agréger les données par catégorie (Catastrophe) et calculer la fréquence
agg_data <- table(dat1$Catastrophe)

# Créer le diagramme en bande
bp <- barplot(agg_data, col = "blue", main = "Diagramme en Bande des Catastrophes")

# Personnaliser l'axe des abscisses
axis(1, at = bp, labels = names(agg_data), las = 2)  # las = 2 pour l'orientation en diagonale

#table de la perte maximale par type de catastrophe
tapply(dat1$Insured_loss_M, dat1$Catastrophe, max)

#effectif par type de catastrophe
table(dat1$Catastrophe)

# Appliquer tapply pour obtenir les valeurs maximales par Catastrophe
max_values <- tapply(dat1$Insured_loss_M, dat1$Catastrophe, max)

# Créer un graphique à barres
barplot(max_values, col = "blue", 
        main = "Valeurs Maximales d'Insured_loss_M par Catastrophe",
        xlab = "Catastrophe", ylab = "Valeur Maximale d'Insured_loss_M")



##### Méthode par bloc de maxima ##### -----------------------------------------
# table avec les regroupements
dat1$Catastrophe2 <- ifelse(dat1$Catastrophe %in% c("Earthquake", "Earthquakes"), "Earthquake",
                            ifelse(dat1$Catastrophe %in% c("Floods", "Flood"), "Flood", dat1$Catastrophe))
tapply(dat1$Insured_loss_M, dat1$Catastrophe2, max)

# Cette ligne de code permet de subdiviser en base en bloc trimestriel
fin.trim <- timeLastDayInQuarter(dat1$date_start)

# Calculons les maxima par trimestre. Utilisez la fonction aggregate.
dat1.max.tri <- aggregate(dat1$Insured_loss_M, by = list(as.character(fin.trim)), FUN = max)


# Graphiques et statistiques descriptives
barplot(dat1.max.tri$x, names.arg = dat1.max.tri$Group.1, 
        main = "Valeurs Maximales par Trimestre ou Groupe",
        xlab = "Trimestre ou Groupe", ylab = "Valeur Maximale",
        col = "lightblue", border = "black")

#dat1$date_start[dat1$Insured_loss_M==max(dat1$Insured_loss_M)]


#renommer les colonnes
colnames(dat1.max.tri) <- c("GMT","Insured_loss_M")
dat1.max.tri

#taille des blocs
table(fin.trim)

#graphique taille des blocs
plot(table(fin.trim))

#histogramme taille des blocs
hist(table(fin.trim))

#histograame du logarithme
hist(log(dat1.max.tri$Insured_loss_M))
summary(dat1.max.tri$Insured_loss_M)

# la fonction fit.GEV du paquetage QRM pour ajuster la GEV.
(mod <- fit.GEV(dat1.max.tri$Insured_loss_M))

#diagramme pp
kk <- (1:length(dat1.max.tri$Insured_loss_M))/(length(dat1.max.tri$Insured_loss_M)+1)
plot(kk, pGEV(dat1.max.tri$Insured_loss_M[order(dat1.max.tri$Insured_loss_M)],mod$par.ests[1],mod$par.ests[2],mod$par.ests[3]), xlab="Probabilité empirique", ylab="Probabilité selon la GEV ajustée")
abline(a=0,b=1)

#diagramme QQ plot
plot(dat1.max.tri$Insured_loss_M[order(dat1.max.tri$Insured_loss_M)], qGEV(kk,mod$par.ests[1],mod$par.ests[2],mod$par.ests[3]), xlab="Quantile empirique", ylab="Quantile selon la GEV ajustée")
abline(a=0, b=1)

#la VaR kappa=0.95
qGEV(0.95,mod$par.ests[1],mod$par.ests[2],mod$par.ests[3])

#la VaR kappa=0.995
qGEV(0.995,mod$par.ests[1],mod$par.ests[2],mod$par.ests[3])

#la VaR kappa=0.999
qGEV(0.999,mod$par.ests[1],mod$par.ests[2],mod$par.ests[3])

ya <- -log(1-0.05)
mod$par.ests[2]-mod$par.ests[3]/mod$par.ests[1] * (1-ya^(-mod$par.ests[1]))

# alpha = 0.05
(int.conf <- mod$par.ests[1]+ c(-1,1)*qnorm(0.975)*mod$par.ses[1])

# alpha = 0.1
(int.conf1 <- mod$par.ests[1]+ c(-1,1)*qnorm(0.95)*mod$par.ses[1])

# alpha = 0.01
(int.conf2 <-mod$par.ests[1]+ c(-1,1)*qnorm(0.995)*mod$par.ses[1])

#digramme profile des intervalles de confiance
mod.ismev <- gev.fit(dat1.max.tri$Insured_loss_M)
gev.profxi(mod.ismev, -0.15, 1)
abline(v= int.conf[1], col=2, lty=2)
abline(v= int.conf[2], col=2, lty=2)

abline(v= int.conf1[1], col=3, lty=2)
abline(v= int.conf1[2], col=3, lty=2)

abline(v= int.conf2[1], col=7, lty=2)
abline(v= int.conf2[2], col=7, lty=2)

legend("right", legend=c("alpha = 0.1", "alpha = 0.05", "alpha = 0.01"), col=c(3, 7, 2), lty = 1:2, cex=1)

#diagramme niveau de retour
dat1.df <- subset(dat1, select = c(Insured_loss_M, date_start))

dat1_fintrim <- cbind(dat1.df, "fin.trim" = fin.trim)
colnames(dat1_fintrim)[3] <- "fin.trim"
detach("package:QRM", unload=TRUE)

dat1.max.tri <- aggregate(Insured_loss_M ~ fin.trim, max, data = dat1_fintrim)

Mn_vector <- sort(dat1.max.tri$Insured_loss_M)

fit_mn <- gev.fit(Mn_vector)

#diagramme niveau de retour
gev.diag(fit_mn)

#Estimation par maximum de vraisemblance pour tester le modele de Gumbel
# Fonction de densité de probabilité (PDF) de la GEV
pdf_gev <- function(x, xi) {
  pdf <- exp(-(1 + xi * x)^(-1 / xi))
  return(pdf)
}
pdf_gumb <- function(x, xi) {
  fg <- exp(-exp(-x))
  return(fg)
}

# Fonction de log-vraisemblance pour la GEV
log_likelihood_gev <- function(para, data = dat1$Insured_loss_M) {
  xi <- para
  
  log_lik <- -2 * (sum(log(pdf_gev(data, xi))) - sum(log(pdf_gumb(data, xi))))
  return(-log_lik)  # On retourne l'opposé de la log-vraisemblance pour maximisation
}

# Estimation des paramètres MLE en maximisant la log-vraisemblance
result <- optimize(f = log_likelihood_gev, interval = c(0, 0.8), maximum = T)

# Paramètre MLE estimé
mle_para <- result$minimum

# Log-vraisemblance maximale
max_log_likelihood <- -result$objective

# Affichage des résultats
cat("Estimation MLE de xi : ", mle_para, "\n")
cat("Log-vraisemblance maximale : ", max_log_likelihood, "\n")

#quantile theorique
qchisq(0.95,1)

# Installer et charger la bibliothèque ismev si ce n'est pas déjà fait
# install.packages("ismev")
library(ismev)

# Estimation des paramètres de la GEV avec xi fixé à 0
fit_gumb <- gum.fit(dat1$Insured_loss_M)

# Affichage des résultats
print(fit_gumb)
#nous avons fixé le paramètre xi à 0 (la distribution Gumbel) en spécifiant shape = 0.

#diagramme pp de la gumbel
library(QRM)
kk <- (1:length(dat1.max.tri$Insured_loss_M))/(length(dat1.max.tri$Insured_loss_M)+1)
plot(kk, pGEV(dat1.max.tri$Insured_loss_M[order(dat1.max.tri$Insured_loss_M)],0,fit_gumb$mle[1],fit_gumb$mle[2]), xlab="Probabilité empirique", ylab="Probabilité selon la GEV ajustée")
abline(a=0,b=1)

 #diagramme qq de la Gumbel
plot(dat1.max.tri$Insured_loss_M[order(dat1.max.tri$Insured_loss_M)], qGEV(kk,0,fit_gumb$mle[1],fit_gumb$mle[2]), xlab="Quantile empirique", ylab="Quantile selon la GEV ajustée")
abline(a=0, b=1)

##### Méthode par exces de seuil ##### -----------------------------------------
## Fonction de réparition empirqiue
Fn <- ecdf(perte)

## Méthodes de détection automatique
mindist(perte, method = 'ks')

## QQplot Pareto : On cherche le point ou le qqplot devient linéaire
ParetoQQ(perte, xlim=c(0, 20))
abline(h=log(2000), col='red')
abline(h=log(550), col='red')
abline(h=log(450), col='red')

## Fonction dexces moyen
mrl.plot(perte, umax = 1200)
MEplot(perte, omit = 3)
abline(v=400)
abline(v=450)

gpd.fitrange(perte, 300, 3000)

## seuil
u <- 1800
l <- 400

## Maximisation de la vraisemblance
# vraisemblance pour xi > 0
mle_gpd <- fit.GPD(perte, threshold = u, type = 'ml')
par_gpd <- unname(mle_gpd$par.ests)
xi <- par_gpd[1]
b <-  par_gpd[2] 
cbind("alpha"=1/xi, "lambda"=b/xi, xi, b)

mle_gpd2 <- fit.GPD(perte, threshold = l, type = 'ml')
par_gpd2 <- unname(mle_gpd2$par.ests)
xi2 <- par_gpd2[1]
b2 <-  par_gpd2[2] 
cbind("alpha"=1/xi2, "lambda"=b2/xi2, xi2, b2)

# Intervalle de confiance
IC_xi <- xi + c(-1, 1) * qnorm(0.975) * mle_gpd$par.ses[1]
IC_xi2 <- xi2 + c(-1, 1) * qnorm(0.975) * mle_gpd2$par.ses[1]

# vraisemblance pour xi = 0
logvrais <- function(par, u){
  -sum(log(dexp(perte[perte > u] - u, 1/par)))
}
mle_exp <- optimize(logvrais, c(0, 2000), u=u)
par_exp <- mle_exp$minimum

mle_exp2 <- optimize(logvrais, c(0, 2000), u=l)
par_exp2 <- mle_exp2$minimum



## Comparaison avec fonction de réparition empirique
Fx_gpd <- function(x, u, b, xi) ifelse(x >= u, (Fn(u) + (1-Fn(u)) * pgpd(x, u, b, xi)), NA)
Fx_exp <- function(x, u, b) ifelse(x >= u, (Fn(u) + (1-Fn(u)) * pexp(x - u, 1/b)), NA)

plot(Fn, xlim=c(0, 1e4), ylim=c(0.65, 1),
     lwd = 2, ylab=TeX('$F_n(x)$'),
     main='Valeurs extrêmes modélisées avec la méthode POT')
curve(Fx_gpd(x, u, b, xi), col = "#00BFC4", lwd = 2, add = T)
curve(Fx_gpd(x, l, b, xi), col = "skyblue", lwd = 2, add = T)
curve(Fx_exp(x, u, par_exp), col = "#F8766D", lwd = 2, add = T)
curve(Fx_gpd(x, l, b, xi), col = "darkblue", lwd = 2, add = T)
legend("bottomright",legend = c(TeX("$GPD(x, \\xi,\\beta, 2000)$"),
                                TeX("$GPD(x, \\xi,\\beta, 400)$"),
                                TeX("$GPD(x, 0,\\beta, 2000)$"),
                                TeX("$GPD(x, 0,\\beta, 400)$")), 
       col = c("#00BFC4", "skyblue","#F8766D", "darkblue"), lwd = 2)

## Information sur le seuil
info_seuil <- function(u){
  a <- 1 - Fn(u)
  b <- length(perte[perte > u])
  d <- sum(perte * I(perte > u))/sum(perte) * 100
  c(a, b, d)
}
info_seuil(u)
info_seuil(l)


## Espérance et mesure de risque
# Espérance
Esp_gpd <- function(u, xi, b) (1 - Fn(u))*(b/(1 - xi) + u)
Esp_Exp <- function(u, b) (1 - Fn(u))*(b + u)
Esp_emp <- function(u) (1 - Fn(u)) * mean(perte[perte > u])

rbind(Esp_gpd(u, xi, b), Esp_Exp(u, par_exp), Esp_emp(u))
rbind(Esp_gpd(l, xi2, b2), Esp_Exp(l, par_exp2), Esp_emp(l))

# Fonction pour les calcules
k <- c(0.95, 0.975, 0.99, 0.995)
VaR_gpd <- function(k, u, b, xi){
  (b/xi * (((1 - k)/(1 - Fn(u)))^(-xi) - 1) + u) * I(k >= Fn(u))
}
VaR_exp <- function(k, u, b){
  -b*log((1 - k)/(1 - Fn(u))) + u
  }
TVaR_gpd <- function(k, u, b, xi){
  (b/xi * (1/(1 - xi)*((1 - k)/(1 - Fn(u)))^(-xi) - 1) + u) * I(k >= Fn(u))
} 
TVaR_exp <- function(k, u, b){
  VaR_exp(k, u, b) + b
}

# Mesure VaR
VaRE <- quantile(perte, k)
xtable(rbind(VaRE, VaR_gpd(k, u, b, xi), VaR_exp(k, u, par_exp)), digits = 0)
xtable(rbind(VaRE, VaR_gpd(k, l, b2, xi2), VaR_exp(k, l, par_exp2)), digits = 0)

# Mesure TVaR
TVaREmp <- sapply(VaRE, function(i) mean(perte[perte > i]))
xtable(rbind(TVaREmp, TVaR_gpd(k, l, b2, xi2), TVaR_exp(k, l, par_exp2)), digits = 0)
xtable(rbind(TVaREmp, TVaR_gpd(k, u, b, xi), TVaR_exp(k, u, par_exp)), digits = 0)

## Validation du modèle
# QQplot pour GPD xi > 0 avec seuil à 2000
q <- function(u) 1:length(perte[perte > u])/(length(perte[perte > u]) + 1)
plot(qgpd(q(u), u, b, xi), sort(perte[perte > u]), ylab="Quantiles observes",
     xlab="Quantiles theoriques", main = "")
abline(0, 1, col='red')

# QQplot pour GPD xi > 0 avec seuil à 400
plot(qgpd(q(l), l, b2, xi2), sort(perte[perte > l]), ylab="Quantiles observes",
     xlab="Quantiles theoriques", main = "")
abline(0, 1, col='red')

# QQplot pour GPD xi = 0 avec seuil à 2000
plot(qexp(q(u), 1/par_exp) + u, sort(perte[perte > u]), ylab="Quantiles observes",
     xlab="Quantiles theoriques", main = "")
abline(0, 1, col='red')

# QQplot pour GPD xi = 0 avec seuil à 400
plot(qexp(q(l), 1/par_exp2) + l, sort(perte[perte > l]), ylab="Quantiles observes",
     xlab="Quantiles theoriques", main = "")
abline(0, 1, col='red')

###########################################
### Code pour la prime d'un contrat CAT XL ------------------------------------------
###########################################
annee <- data$`Reporting Year` # Variable pour les années seulement
rho <- 0.2
n_u <- sum(perte > u) # nombre de perte au dessus de seuil
lam_an <- sapply(levels(annee), function(i) sum(annee == i)) # lambda par année processus de poisson
lam <- length(perte)/5 # lambda pour le processus de poisson homogène

V <- n_u*lam*b/(n*(1 - xi)) + rho*b*sqrt(2*lam*n_u)/(sqrt(n*(1 - xi)*(1 - 2*xi))) # Prime payé par l'assureur
Rea <- aggregate(data$Insured_loss_M ~ data$`Reporting Year`, data, function(i) sum(pmax(i - u, 0)))[, 2] # perte assumeé par le réassureur

# xtable(rbind(V, Rea, Rea - V), digits = 0) # Produire le tableau pour le rapport

sum(Rea - V) # Somme des gains pour comparer les résultats selon le seuil choisit

##### Vérification processus de Poisson homogène #####
dif <- diff(sort(unique(data$date_start)))
tis <- as.numeric(dif) # Temps entre les sinsitres
Fc <- ecdf(tis)

## Maximum de vraisemblance pour une exponentielle
param_tis <- length(tis)/sum(tis)

## graphique comparaison
plot(Fc, ylim=c(0, 1), xlim=c(0, 100), lwd = 2, xlab= "Heures", main="")
#title(main = "Comparaison de la fonction de répartition de la loi exponentielle \navec la fonction de répartition empirique pour le\n temps entre les sinsitres supérieur à 1 million")
curve(pexp(x, param_tis), col = "blue", lwd = 2, add = T)

ks.test(tis + rnorm(length(tis), 0, 0.001), function(x) pexp(x, param_tis))$p.val # rnorm enlever les égalitées


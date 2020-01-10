### Alec James van Rassel
### ACT-2003: Modèles linéaires en actuariat
###
###  Modèles linéaires généralisés (5)
###
### Exercice 5.8
###
# BritishCar_imp <- read.csv(file = "../ACT2003-exercices/exercices_modeles_lineaires/data/BritishCar.csv", sep = ";") 
# BritishCar <- BritishCar_imp[is.na(BritishCar_imp$AvCost) == F,]
# 
# mod_gam <- glm(formula = AvCost ~ OwnerAge + Model + CarAge, family = "Gamma", data = BritishCar)

Graph.residual.distr <- function(Y, mod, distribution = "gamma", residual = "pearson", col.points = "lightskyblue4", col.bars = "steelblue2", col.bars.outline = "steelblue2") {
    require(ggplot2) 
    require(ggExtra) # pour ggMarginal
    require(extrafont) # Pour la fonte Latex (il faut déjà l'avoir installé sur le PC)
    
    mu <- fitted(mod)
    if (residual == "pearson") {
        residuals.mod <- residuals.glm(mod_gam, type = "pearson")
    } else if (residual == "deviance") {
        residuals.mod <- residuals.glm(mod_gam, type = "deviance")
    } else if (residual == "anscombe") {
        if (distribution == "gamma") {
            residuals.mod <- 3 * (Y^(1/3) - mu^(1/3))/mu^(1/3)
        } else if (distribution == "poisson") {
            residuals.mod <- 1.5 * (Y^(2/3) - mu^(2/3))/mu^(1/6)
        }
    } 
    data.mod <- data.frame(mu, residuals.mod)
    Graph.residual <- ggplot(data.mod, aes(mu, residuals.mod)) + 
        geom_jitter(width = .5, size = 1, pch = 1, colour = col.points) +
        theme_light() +
        labs(title = "", 
             subtitle = "", 
             y = paste("Résidu", residual),
             x = "Estimation du paramètre") + 
        theme(axis.title.y = element_text(margin = margin(r = 10)),
              axis.title.x = element_text(margin = margin(t = 10)),
              axis.text.x = element_text(angle = 90))
    ggMarginal(Graph.residual, type = "histogram", fill = col.bars,
               margins = "y", colour = col.bars.outline, alpha = 0.5)
    
}
# 
# residual_plots <- sapply(c("pearson", "anscombe", "deviance"), function(i) Graph.residual.distr(Y = BritishCar$AvCost, mod = mod_gam, distribution = "gamma", residual = i, col.points = "#14367A"))
# residual_plots[1]
# residual_plots[2]
# residual_plots[3]

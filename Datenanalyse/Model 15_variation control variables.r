Mo1 <- glm(Union ~ Micro + Age + East + Education + Income + Union_Identification, data = G21, 
          family = binomial(link = "logit"))
Mo2 <- glm(Union ~ Micro + Gender + East + Education + Income + Union_Identification, data = G21, 
          family = binomial(link = "logit"))
Mo3 <- glm(Union ~ Micro + Gender + Age, data = G21, 
          family = binomial(link = "logit"))
Mo4 <- glm(Union ~ Micro + Age + Union_Identification, data = G21, 
          family = binomial(link = "logit"))

nagel_r2_Mo1 <- PseudoR2(Mo1, which = "Nagelkerke") 
nagel_r2_Mo2 <- PseudoR2(Mo2, which = "Nagelkerke")
nagel_r2_Mo3 <- PseudoR2(Mo3, which = "Nagelkerke")
nagel_r2_Mo4 <- PseudoR2(Mo4, which = "Nagelkerke")


promargins <- function(x) {
    y <- summary(margins(x))   
    y$AME <- signif(y$AME * 100, 4)
    y$lower <- signif(y$lower * 100, 4)
    y$upper <- signif(y$upper * 100, 4)
    return(y)
  }


ame_Mo1 <- promargins(Mo1)
ame_Mo2 <- promargins(Mo2)
ame_Mo3 <- promargins(Mo3)
ame_Mo4 <- promargins(Mo4)

ameo1 <- as.vector(ame_Mo1$AME)
ameo2 <- as.vector(ame_Mo2$AME)
ameo3 <- as.vector(ame_Mo3$AME)
ameo4 <- as.vector(ame_Mo4$AME)

names(ameo1) <- ame_Mo1$factor
names(ameo2) <- ame_Mo2$factor
names(ameo3) <- ame_Mo3$factor
names(ameo4) <- ame_Mo4$factor

po1 <- ame_Mo1$p
po2 <- ame_Mo2$p
po3 <- ame_Mo3$p
po4 <- ame_Mo4$p

names(po1) <- ame_Mo1$factor
names(po2) <- ame_Mo2$factor
names(po3) <- ame_Mo3$factor
names(po4) <- ame_Mo4$factor

cio1 <- ame_Mo1[, c("lower", "upper")]
cio2 <- ame_Mo2[, c("lower", "upper")]
cio3 <- ame_Mo3[, c("lower", "upper")]
cio4 <- ame_Mo4[, c("lower", "upper")]

rownames(cio1) <- ame_Mo1$factor
rownames(cio2) <- ame_Mo2$factor
rownames(cio3) <- ame_Mo3$factor
rownames(cio4) <- ame_Mo4$factor

ameMo1 <- Mo1
ameMo2 <- Mo2
ameMo3 <- Mo3
ameMo4 <- Mo4

ameMo1$coefficients <- ameo1
ameMo2$coefficients <- ameo2
ameMo3$coefficients <- ameo3
ameMo4$coefficients <- ameo4

predictMo1 <- predict(Mo1, type = "response")
predictMo2 <- predict(Mo2, type = "response")
predictMo3 <- predict(Mo3, type = "response")
predictMo4 <- predict(Mo4, type = "response")

confusionMo1 <- data.frame(Vote = as.factor(G21$Union), predictMo1)
confusionMo2 <- data.frame(Vote = as.factor(G21$Union), predictMo2)
confusionMo3 <- data.frame(Vote = as.factor(G21$Union), predictMo3)
confusionMo4 <- data.frame(Vote = as.factor(G21$Union), predictMo4)

predictMo1 <- as.factor(ifelse(confusionMo1$predictMo1 < 0.5, 0, 1))
predictMo2 <- as.factor(ifelse(confusionMo2$predictMo2 < 0.5, 0, 1))
predictMo3 <- as.factor(ifelse(confusionMo3$predictMo3 < 0.5, 0, 1))
predictMo4 <- as.factor(ifelse(confusionMo4$predictMo4 < 0.5, 0, 1))

conmao1 <- confusionMatrix(predictMo1, confusionMo1$Vote)
conmao2 <- confusionMatrix(predictMo2, confusionMo2$Vote)
conmao3 <- confusionMatrix(predictMo3, confusionMo3$Vote)
conmao4 <- confusionMatrix(predictMo4, confusionMo4$Vote)

stargazer(ameMo1, ameMo2, ameMo3, ameMo4, 
          out = "Output/Reg_Tabelle_spezial_2013.html",
          p = list(po1, po2, po3, po4),
          ci.custom = list(cio1, cio2, cio3, cio4),  
          add.lines = list(c("Pseudo R\U00B2 (Nagelkerke)", signif(nagel_r2_Mo1, 3), signif(nagel_r2_Mo2, 3), 
                             signif(nagel_r2_Mo3, 3), signif(nagel_r2_Mo4, 3)),
                           c("Accuracy (%)", signif(conmao1$overall[1]*100, 3), signif(conmao2$overall[1]*100, 3),
                           signif(conmao3$overall[1]*100, 3), signif(conmao4$overall[1]*100, 3))),
          order = order1, 
          model.numbers = FALSE, 
          omit.stat = "aic",
          column.labels = c("Modell 15-1", "Modell 15-2", "Modell 15-3", "Modell 15-4"),
          title= "Average Marginal Effects (AME) in %, Variationen Modell 15")

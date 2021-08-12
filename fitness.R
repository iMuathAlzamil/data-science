varPlot <- function(xvar, yvar, title, xlabel, ylabel, winnum, func) {
	plot(xvar, yvar, main=title, xlab=xlabel, ylab=ylabel, pch=winnum)
	abline(func, col="red")
	cor(xvar, yvar)
	cor.test(xvar, yvar)
	summary(func)
}

elimOutliers <- function(vals, ...) {
	quant <- quantile(vals, probs=c(0.25,0.75), na.rm = TRUE)
	rng <- 1.5 * IQR(vals, na.rm = TRUE)
	newVals <- vals
	newVals[vals < (quant[1] - rng)] <- NA
	newVals[vals > (quant[2] - rng)] <- NA
	return(newVals)
}

fitness_score <- function(input) {
	weight <- input$Wt
	forwFlex <- input$FF
	return(0.161940 * (forwFlex - weight) + 90.251095) 
}

setwd("~")
fitnessData <- read.csv(file = 'Fitness and Obesity - F20.csv')

age <- fitnessData$Age
height <- fitnessData$Ht
weight <- fitnessData$Wt
forwFlex <- fitnessData$FF
sbp <- fitnessData$SBP
dbp <- fitnessData$DBP
rgm <- fitnessData$RGM
lgm <- fitnessData$LGM
vc <- fitnessData$VC
hr1 <- fitnessData$HR.1
hr2 <- fitnessData$HR.2
hr3 <- fitnessData$HR.3
pl1 <- fitnessData$PL.1
pl2 <- fitnessData$PL.2
pl3 <- fitnessData$PL.3
fitness <- fitnessData$FitnessScore

newHR1 <- elimOutliers(fitnessData$HR.1)
newHR2 <- elimOutliers(fitnessData$HR.2)


dev.new()
par(mfrow=c(2,2))
varPlot(hr1, fitness, "HR1 vs. Fitness", "HR1", "Fitness", 1, lm(fitness~hr1))
varPlot(newHR1, fitness, "No Outlier HR1 vs. Fitness", "HR1", "Fitness", 2, lm(fitness~newHR1))
varPlot(hr2, fitness, "HR2 vs. Fitness", "HR2", "Fitness", 3, lm(fitness~hr2))
varPlot(newHR2, fitness, "No Outlier HR2 vs. Fitness", "HR2", "Fitness", 4, lm(fitness~newHR2))

dev.new()
par(mfrow=c(3,5))
varPlot(age, fitness, "Age vs. Fitness", "Age", "Fitness", 1, lm(fitness~age))
varPlot(height, fitness, "Height vs. Fitness", "Height", "Fitness", 2, lm(fitness~height))
varPlot(weight, fitness, "Weight vs. Fitness", "Weight", "Fitness", 3, lm(fitness~weight))
varPlot(forwFlex, fitness, "ForwardFlex vs. Fitness", "ForwardFlex", "Fitness", 4, lm(fitness~forwFlex))
varPlot(sbp, fitness, "SBP vs. Fitness", "SBP", "Fitness", 5, lm(fitness~sbp))
varPlot(dbp, fitness, "DBP vs. Fitness", "DBP", "Fitness", 6, lm(fitness~dbp))
varPlot(rgm, fitness, "RGM vs. Fitness", "RGM", "Fitness", 7, lm(fitness~rgm))
varPlot(lgm, fitness, "LGM vs. Fitness", "LGM", "Fitness", 8, lm(fitness~lgm))
varPlot(vc, fitness, "VC vs. Fitness", "VC", "Fitness", 9, lm(fitness~vc))
varPlot(hr1, fitness, "HR1 vs. Fitness", "HR1", "Fitness", 10, lm(fitness~hr1))
varPlot(hr2, fitness, "HR2 vs. Fitness", "HR2", "Fitness", 11, lm(fitness~hr2))
varPlot(hr3, fitness, "HR3 vs. Fitness", "HR3", "Fitness", 12, lm(fitness~hr3))
varPlot(pl1, fitness, "PL1 vs. Fitness", "PL1", "Fitness", 13, lm(fitness~pl1))
varPlot(pl2, fitness, "PL2 vs. Fitness", "PL2", "Fitness", 14, lm(fitness~pl2))
varPlot(pl3, fitness, "PL3 vs. Fitness", "PL3", "Fitness", 15, lm(fitness~pl3))

dev.new()
par(mfrow=c(2,3))
fw1 = forwFlex + weight
fw2 = forwFlex - weight
fw3 = forwFlex^2 + weight
fw4 = forwFlex + weight^2
fw5 = sbp + dbp
lfit = log(fitness)
varPlot(fw1, fitness, "ForwardFlex + Weight vs. Fitness", "FF + Weight", "Fitness", 1, lm(fitness~fw1))
varPlot(fw1, lfit, "ForwardFlex + Weight vs. Log(Fitness)", "FF + Weight", "Log(Fitness", 2, lm(lfit~fw1))
varPlot(fw2, fitness, "ForwardFlex - Weight vs. Fitness", "FF - Weight", "Fitness", 3, lm(fitness~fw2))
varPlot(fw3, fitness, "ForwardFlex^2 + Weight vs. Fitness", "FF^2 + Weight", "Fitness", 4, lm(fitness~fw3))
varPlot(fw4, fitness, "ForwardFlex + Weight^2 vs. Fitness", "FF + Weight^2", "Fitness", 5, lm(fitness~fw4))
varPlot(fw5, fitness, "SBP + DBP vs. Log(Fitness)", "SBP + DBP", "Fitnes)", 6, lm(fitness~fw5))

preds <- fitness_score(fitnessData)
sqrt(mean((fitnessData$FitnessScore - preds)^2, na.rm = TRUE)) # RMSE
sum(is.na(preds))


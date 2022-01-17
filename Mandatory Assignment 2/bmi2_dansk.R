# Indlæs 'bmi2_data.csv' filen med data
D <- read.table("bmi2_data.csv", header = TRUE, sep = ";")
summary (D)
sd(D$bmi)

 ###########################################################################################################################
# a) 

# Tilføj log-BMI til datasættet
D$logbmi <- log(D$bmi)

#Scatter
plot(D$logbmi,D$age, col = "red", xlab = "LogBMI", ylab = "Age", pch = 16, main = "LogBMI vs Age") 
plot(D$logbmi,D$fastfood, col = "red", xlab = "LogBMI", ylab = "Fastfood", pch = 16, main = "LogBMI vs Fastfood")

#Density Histogram of LogBMI
hist(D$logbmi, xlab = "LogBMI", xlim = c(2.8,3.8), breaks = 20, col = "#E7444E", prob = TRUE, main = "Density Histogram of LogBMI")
#Box Plot of LogBMI
boxplot(D$logbmi, names = "LogBMI", col = "#E7444E", main = "Box Plot of LogBMI", ylab = "LogBMI")
summary(D$logbmi)
length(D$logbmi)
mean(D$logbmi)
sd(D$logbmi)
mean(D$logbmi)

#Density Histogram of Age
hist(D$age, xlab = "Age", xlim = c(10,80), breaks = 20, col = "#E7444E", prob = TRUE, main = "Density Histogram of Age")
#Box Plot of Age
boxplot(D$age, names = "Age", col = "#E7444E", main = "Box Plot of Age", ylab = "Age")
summary(D$age)
length(D$age)
mean(D$age)
sd(D$age)
mean(D$age)

#Density Histogram of Fastfood
hist(D$fastfood, xlab = "Fastfood",xlim = c(0,400), breaks = 20, col = "#E7444E", prob = TRUE, main = "Density Histogram of Fastfood")
#Box Plot of Fastfood
boxplot(D$fastfood, names = "Fastfood", col = "#E7444E", main = "Box Plot of Fastfood", ylab = "Fastfood")
summary(D$fastfood)
length(D$fastfood) 
mean(D$fastfood)
sd(D$fastfood)
mean(D$fastfood) 
#############################################################################################################################

#b)
# Y_i = beta0hat + beta1hat * x1_i + beta2hat * x2_i 
# Y_i: LogBMI 
# x1_i: Age 
# x2_i: Fastfood 


##############################################################################################################################

#c)


# Del datasæt med de første 840 observationer (til model)
D_model <- subset(D, id <= 840)

# Deldatasæt med de sidste 7 observationer (til validering)
D_test <- subset(D, id >= 841)

# Estimer multipel lineær regressionsmodel
fit <- lm(logbmi ~ age + fastfood, data = D_model)

# Vis estimerede parametre mm.
summary(fit)

# Residual varians
sigma_residual = 0.1573 
var_residual = sigma_residual^2
var_residual 

###########################################################################################################################

#d) Model validering

# Plots til modelkontrol

# Observationer mod fittede værdier
#Fittede vaerider vs logBMI
plot(fit$fitted.values, D_model$logbmi, xlab = "Fittede værdier", ylab = "log(BMI)", col = "#E7444E", pch = 16) 

# Residualer mod hver af de forklarende variable 
#Age vs Residual
plot(D_model$age, fit$residuals, xlab = "Age", ylab = "Residuals", col = "#E7444E", pch = 16)

#LogBMI vs Residual
plot(D_model$logbmi, fit$residuals, xlab = "LogBMI", ylab = "Residuals", col = "#E7444E", pch = 16)

#Fastfood vs Residual
plot(D_model$fastfood, fit$residuals, xlab = "Fastfood", ylab = "Residuals", col = "#E7444E", pch = 16) 

# Residualer mod fittede værdier
plot(fit$fitted.values, fit$residuals, xlab = "Fittede værdier",      ylab = "Residualer")

# Normal QQ-plot af residualerne
qqnorm(fit$residuals, ylab = "Residualer", xlab = "Z-scores", main = "",col = "#E7444E", pch = 16)
qqline(fit$residuals)

########################################################################################################
#e) Kvantil
qt(0.975, 837)
#95% konfidens interval
0.0024 + c(-1,1) * 1.96 * 0.00039
# Konfidensintervaller for modellens koefficienter
confint(fit, level = 0.95)
#########################################################################################################
#f)
beta_h0 = 0.001
beta_hat1 = 0.0024
sigma_hat1 = 0.00039
t_obs_b1 = (beta_hat1 - beta_h0)/sigma_hat1
t_obs_b1 
# p-vaerdi
2 * (1 - pt(abs(t_obs_b1), 840-1)) 
######################################################################################
# h)

# Prædiktioner og 95% prædiktionsintervaller
pred <- predict(fit, newdata = D_test, interval = "prediction", level = 0.95)

# Observerede værdier sammen med prædiktioner
cbind(id = D_test$id, logbmi = D_test$logbmi, pred)


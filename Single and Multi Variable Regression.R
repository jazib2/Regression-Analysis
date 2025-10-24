#Installing the required packages 

install.packages('readx1')
install.packages("datarium")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("rcompanion")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("carData")
install.packages('FSA')
install.packages("ggstatsplot")
install.packages ('caret')
install.packages("car")
install.packages('RVAideMemoire')
install.packages('ggplot2')

#Calling the required packages

library(ggstatsplot)
library(FSA)
library(doBy)
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(ggplot2)
library(qqplotr)
library(readxl)
library(carData)
library(dplyr)
library(caret)
library(car)
library(RVAideMemoire)

#Reading the file into dataframe

data <- read_excel("concrete compressive strength.xlsx")

#Exploring the data EDA

head(data)

summary(data)

plot(data)

# changing names to more meaningful ones

colnames(data) <- c("Cement",
                    "Blast_Furnace_Slag", 
                    "Fly_Ash", 
                    "Water", 
                    "Superplasticizer", 
                    "Coarse_Aggregate", 
                    "Fine_Aggregate", 
                    "Age", 
                    "Concrete_Category", 
                    "Contains_Fly_Ash", 
                    "Concrete_compressive_strength")



# Plotting graphs against of different vaiables against Concrete Strenght


ggplot(data, aes(Cement , Concrete_compressive_strength)) + geom_point()

ggplot(data, aes(Blast_Furnace_Slag , Concrete_compressive_strength)) + geom_point()

ggplot(data, aes(Superplasticizer , Concrete_compressive_strength)) + geom_point()

ggplot(data, aes(Age , Concrete_compressive_strength)) + geom_point()

ggplot(data, aes(Water, Concrete_compressive_strength)) + geom_point()

ggplot(data, aes(Cement, Concrete_compressive_strength, colour = Water)) + geom_point()

ggplot(data, aes(Water, Cement)) + geom_point()

ggplot(data, aes(Age, Concrete_compressive_strength, colour = Cement)) + geom_point()



# QQ plot test for visually checking normality 

ggplot(mapping = aes(sample=data$Cement)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=data$Water)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=data$Fly_Ash)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=data$Blast_Furnace_Slag)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=data$Superplasticizer)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=data$Concrete_compressive_strength)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")



#Shapiro test for checking normallity. Statistical Method

shapiro.test(data$Cement)
shapiro.test(data$Water)
shapiro.test(data$Fly_Ash)
shapiro.test(data$Blast_Furnace_Slag)
shapiro.test(data$Superplasticizer)
shapiro.test(data$Age)
shapiro.test(data$Concrete_compressive_strength)

#Checking correlation between concrete strength and other variables using non-parametric correlation method

cor(data$Cement, data$Concrete_compressive_strength,method = "spearman")
cor(data$Water,data$Concrete_compressive_strength,method = "spearman")
cor(data$Fly_Ash,data$Concrete_compressive_strength,method = "spearman")
cor(data$Blast_Furnace_Slag,data$Concrete_compressive_strength,method = "spearman")
cor(data$Superplasticizer,data$Concrete_compressive_strength,method = "spearman")
cor(data$Age,data$Concrete_compressive_strength,method = "spearman")
cor(data$Coarse_Aggregate,data$Concrete_compressive_strength,method = "spearman")
cor(data$Fine_Aggregate,data$Concrete_compressive_strength,method = "spearman")


#Storing Numerical Variables Separately using the pipe operator for Correlation Matrix

numeric_data <-data %>% select(- Contains_Fly_Ash, - Concrete_Category)



head(numeric_data, 10)  

#Visualizing Correlation Matrix

corrplot(round(cor(numeric_data, method = "spearman"), digits = 2))



################################################################################



#Hypothesis Testing


#Applying Transformations to make non-normal data normal and visualizing the results

hist(data$Cement, main = "Histogram of Skewed Cement", xlab = "Cement", col =
      "lightblue", border = "black")


data$log_y <- log(data$Cement)

hist(data$log_y, main = "Histogram of Log-transformed 'Cement'", xlab = "log(Cement)",
     col = "lightgreen", border = "black")


data$sqrt_y <- sqrt(data$Cement)


hist(data$sqrt_y, main = "Histogram of sqaure root-transformed 'Cement'", xlab = "log(Cement)",
     col = "pink", border = "black")



data$cube_root_y <- data$Cement^(1/3)


hist(data$cube_root_y, main = "Histogram of Cube Root-transformed 'Cement'", xlab
   = "Cement^(1/3)", col = "lightyellow", border = "black")



hist(data$cube_root_y, main = "Histogram of Cube Root-transformed 'Cement'", xlab
     = "y^(1/3)", col = "lightyellow", border = "black")


shapiro.test(data$log_y)
shapiro.test(data$cube_root_y)
shapiro.test(data$sqrt_y)



data$cuberoot_logy <- data$log_y^(1/3)
data$sqrt_logy <- sqrt(data$log_y)
data$sqrt_cube_logy <- data$sqrt_logy^(1/3)



# checking normality after applying transformations both visually and statistically

shapiro.test(data$cube_root_y)
shapiro.test(data$sqrt_y)
shapiro.test(data$log_y)
shapiro.test(data$sqrt_cube_logy)


ggplot(mapping = aes(sample=data$cube_root_y)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")


ggplot(mapping = aes(sample=data$sqrt_y)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")


ggplot(mapping = aes(sample=data$log_y)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=data$sqrt_cube_logy)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")


# Checking normality of cement grouped by different categories

Coarse_strength<-data$Concrete_compressive_strength[data$Concrete_Category=="Coarse"]

ggplot(mapping = aes(sample = Coarse_strength)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")



Fine_strength<-data$Concrete_compressive_strength[data$Concrete_Category=="Fine"]

ggplot(mapping = aes(sample = Fine_strength)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

# plotting using histogram

options(scipen=999)
hist(Fine_strength)

options(scipen=999)
hist(Coarse_strength)

median(Coarse_strength)
median(Fine_strength)


#Non parametric hypothesis testing because of Non-Normal data


data$Concrete_Category <- as.factor(data$Concrete_Category)

# 1st Hypothesis Test 

wilcox.test(Concrete_compressive_strength ~ Concrete_Category , data = data)



# 2nd Hypothesis Test ANOVA Non-Parametric   

#New column to distribute age according to a given range and assigning a value

data <- data %>% 
  mutate(Age_Distribution = case_when(
    data$Age>= 1 & data$Age <= 120 ~ 'A',
    data$Age >= 121 & data$Age <= 240 ~ 'B',
    data$Age >= 241 & data$Age <= 365 ~ 'C'
  ))

data$Age_Distribution

# Checking Normality for each group in Age Distribution

byf.shapiro(Concrete_compressive_strength ~ Age_Distribution , data=data)

#Checking for Homogeneity

bartlett.test(Concrete_compressive_strength ~ Age_Distribution, data=data)


# box whisker plot

ggplot(data) +
  aes(x = Age_Distribution, y = Concrete_compressive_strength, 
      fill = Age_Distribution) +geom_boxplot() +
  theme(legend.position = "none")

#Hypothesis Test

kruskal.test(Concrete_compressive_strength ~ Age_Distribution, data=data)


#3rd Hypothesis Test

data <- data %>% 
  mutate(Superplasticizer_Distribution = case_when(
    data$Superplasticizer>= 0 & data$Superplasticizer <= 10 ~ 'X',
    data$Superplasticizer > 10 & data$Superplasticizer <= 20 ~ 'Y',
    data$Superplasticizer > 20 & data$Superplasticizer <= 32.2 ~ 'Z'
  ))

kruskal.test(Concrete_compressive_strength ~ Superplasticizer_Distribution , data=data)




#4th Hypothesis Test on Contains_Fly_Ash column

wilcox.test( Concrete_compressive_strength ~ Contains_Fly_Ash  , data = data)




#Post-hoc tests to see difference between the groups after Kruskal test

dunnTest(Concrete_compressive_strength ~ Age_Distribution, data=data,method = "bonferroni")

dunnTest(Concrete_compressive_strength ~ Age_Distribution, data=data,method = "holm")

dunnTest(Concrete_compressive_strength ~ Superplasticizer_Distribution, data=data,method = "bonferroni")



ggbetweenstats(
  data = data,
  x = Age_Distribution,
  y = Concrete_compressive_strength,
  type = "nonparametric", 
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)


###############################################################################


#Regression Testing


#forward step wise
#1st Regression Model
#SLR

model_1 <-lm(Concrete_compressive_strength ~ Cement, continuous_data )
summary.lm(model_1)

abline(lm( data$Concrete_compressive_strength~data$Cement , data = data), col = "red")
plot(Concrete_compressive_strength ~ Cement,data,
     col = "blue",
     main = "Regression: Concrete Strength & Cement",
     xlab = "Cement",
     ylab = "Concrete Strength")

abline(lm( data$Concrete_compressive_strength~data$Cement , data = data), col = "red")



plot(model_1, 1)

plot(model_1, 2)

plot(model_1, 3)


#MLR (2 variables)

model_2 <-lm(Concrete_compressive_strength ~ Cement + Superplasticizer, continuous_data )

summary.lm(model_2)

plot(Concrete_compressive_strength ~Superplasticizer ,continuous_data,
     col = "blue",
     main = "Regression: House Sale Price & Total Ground Area",
     xlab = "Totoal Ground Area",
     ylab = "House Sale Price")


pairs(data[,c(1,6)], lower.panel = NULL, pch = 19,cex = 0.2)


#MLR(3 variables)

model_3 <-lm(Concrete_compressive_strength ~ Cement + Age + Water ,data )
summary.lm(model_3)

pairs(continuous_data[,c(8,1,5,2)], lower.panel = NULL, pch = 19,cex = 0.2)




#MLR(4 variables)
model_4 <-lm(Concrete_compressive_strength ~ Cement + Superplasticizer + Water + Age , data )
summary.lm(model_4)

pairs(data[,c(11,1,4,5,8)], lower.panel = NULL, pch = 19,cex = 0.2)




#GAM Regression to tackle non-linearity in variables

library(mgcv)
gam_model <- gam(Concrete_compressive_strength ~ Cement + s(Superplasticizer) + s(Water) + s(Age) ,
                 data = data, method = 'REML')
summary(gam_model)

plot(gam_model, pages = 1, shade = TRUE, seWithMean = TRUE)


#Logistic regression


model_logiatic_1 <- glm( Concrete_Category ~ Coarse_Aggregate + Fine_Aggregate,
                        data = data, family = "binomial")

summary(model_logiatic_1)

Imp <- varImp(model_logiatic_1, scale = FALSE)

# Dividing our DV into 2 ranges and making a binary column


data <- data %>% 
  mutate(Concrete_Distribution = case_when(
    data$Concrete_compressive_strength >= 36 ~ 'High',
    data$Concrete_compressive_strength <36  ~ 'Low'
  ))

data$Concrete_Distribution <- as.factor(data$Concrete_Distribution)

#model 1

model_logiatic_1 <- glm(Concrete_Distribution ~ Cement + Blast_Furnace_Slag +Fly_Ash
                         + Water + Superplasticizer + Coarse_Aggregate + Fine_Aggregate + Age,
                         data = data, family = "binomial")
summary(model_logiatic_1)

vif(model_logiatic_1)




#model 2

model_logiatic_2 <- glm( Concrete_Distribution ~ Cement + Blast_Furnace_Slag
                         + Water + Superplasticizer + Age,
                         data = data, family = "binomial")
summary(model_logiatic_2)

vif(model_logiatic_2)


Imp <- varImp(model_logiatic_1, scale = FALSE)
Imp

#model 3

model_logiatic_3 <- glm( Concrete_Distribution ~ Blast_Furnace_Slag + Superplasticizer + Age +
                          Fly_Ash + Cement + Water +Fine_Aggregate ,
                         data = data, family = "binomial")

summary(model_logiatic_3)

#checking assumptions

probs <- predict(model_logiatic_3, data=data,type="response")
data$probs <- probs


logits <- log(probs/(1-probs))
data$logits <- logits

pairs(data[,c(16,1,2,3,4,5,7,8)], lower.panel = NULL, upper.panel = panel.smooth, 
      pch = 19,cex = 0.2)


plot(model_logiatic_3, which = 4, id.n = 3)

vif(model_logiatic_3)

table(data$Concrete_Distribution)
sum(is.na(data$Concrete_Distribution))









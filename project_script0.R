library(data.table)
library(ggplot2)
library(scales)
library(tidyverse)
library(dplyr)
library(skimr)
library(caret)

getwd()
data.wd <-"C:/Users/hp/final-proposal"
setwd(data.wd);

# Read CSV file----
institution <- fread("Institutions.csv")
prelevement <- fread("prelevements.csv")
resultat <- fread("resultats.csv")

# Merging data----
dada <- merge(institution, prelevement, by = "Inst1_1", all = FALSE)
setnames(dada, "Age_prel_c0", "Age_prel_c0_lab")
dada <- merge(resultat,dada, by = "Numero_1", all = FALSE, suffixes = c("", ""))  


# Factors creation----
dada[, `:=`(
  
  # Test result
  result = factor(
    fcase(
      Hb_S == "HbS", 1L,
      Hb_S == "Autre_Hb", 0L,
      default = NA_integer_
    ), 
    levels = c(0L, 1L), 
    labels = c("hb_AA", "Hb_S")
  ),
  
  # Sex of the child
  gender = factor(
    fcase(
      Sexe == "M", 1L,
      Sexe == "F", 0L,
      default = NA_integer_
    ), 
    levels = c(0L, 1L), 
    labels = c("F", "M")
  ),
  
  # Year of blood collection
  Age = factor(
    fcase(
      Age_prel_c0 == "0-28 days", 0L,
      Age_prel_c0 == "1-5 years", 2L,
      Age_prel_c0 == "29-364 days", 1L,
      default = NA_integer_
    ), 
    levels = c(0L, 1L, 2L), 
    labels = c("0-28 days", "29-364 days", "1-5 years")
  ),
  
  Year = factor( ifelse(annee_prel_1 > 2013, NA_integer_, annee_prel_1-2006),
                levels = c(0L, 1L, 2L, 3L, 4L, 5L, 6L),
                labels = c("2006", "2007", "2008","2009", "2010", "2011", "2012")),
  
  # Is the hospital that send sample blood Private or public
  Owner = factor(
    fcase(
      Nature_1 == "Publique", 1L,
      (Nature_1 == "Privée" |Nature_1 == "Priv\xe9e"), 0L,
      default = NA_integer_
    ), 
    levels = c(0L, 1L), 
    labels = c("Private", "Public")
  ),
  
  # province of mother
  province = factor(
    fcase(
      Prov_mere_1 == "BDD", 1L,
      Prov_mere_1 %in% c("AFR", "EUROPE", "INCONNUE", "KINSH"), 2L,
      Prov_mere_1 == "EQUATEUR", 3L,
      Prov_mere_1 == "KASAI", 4L,
      Prov_mere_1 == "KATANGA", 5L,
      Prov_mere_1 == "KIVU", 6L,
      Prov_mere_1 == "KONG-C", 7L,
      Prov_mere_1 == "PROV OR", 8L,
      default = NA_integer_
    ), 
    levels = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L), 
    labels = c("Bandundu", "Kinshasa", "Equateur", "Kasai", "Katanga", "Kivu", "Kongo-Central", "Prov-Oriental")
  ),
  
  # Route of sample
  Route_sample = factor(
    fcase(
      Endoit_prelev_1 == "Talon", 0L,
      Endoit_prelev_1 == "Cordon", 1L,
      Endoit_prelev_1 == "Autres", 2L,
      default = NA_integer_
    ), 
    levels = c(0L, 1L, 2L), 
    labels = c("Knee", "Cord", "Other")
  )
)]


# Descriptive data----

# Number of children by province of mother
prov_mother <- dada[, .N, by = province][order(-N)][
  , `:=`(
    Cumul_N = cumsum(N),
    pct_N = 100 * N / sum(N)
  )
]

# Age distribution of children
age_distribution <- dada[, .N, by = Age][order(-N)][
  , `:=`(
    pct_N = 100 * N / sum(N),
    Cumul_N = cumsum(N)
  )]

# Age distribution of children
age_owner <- dada[, .N, by = Owner][order(-N)][
  , `:=`(
    pct_N = 100 * N / sum(N),
    Cumul_N = cumsum(N)
  )]

# Age distribution of children
age_Year <- dada[, .N, by = Year][
  , `:=`(
    pct_N = 100 * N / sum(N),
    Cumul_N = cumsum(N)
  )][order(Year)]


# Distribution of the screening result
Result_grouped <- dada[, .N, by = result][order(-N)][
  , `:=`(
    Cumul_N = cumsum(N),
    pct_N = 100 * N / sum(N)
  )][order(-N)]

# Distribution of Hemoglobin by child's age
hb_prop_by_age <- dada[, .N, by = .(Age, result)][
  , `:=`(
    pct_N = 100 * N / sum(N), 
    Age_group_N = sum(N)
  ), by = Age][order(Age,result)]


# Proportion of Hb S by child mother's province
hb_S_prop_by_prov <- dada[, .N, by = .(province, result)][
  , `:=`(
    pct_N = 100 * N / sum(N)
  ), by = province][order(-pct_N)][
    result == "Hb_S"]


# Plot for Proportion of Hb_gene by age at prelevement
ggplot(na.omit(hb_prop_by_age), aes(pct_N, Age)) +  
  geom_segment(aes(xend = 0, yend = Age), color = "#1f78b4", size  = 1) +
  geom_text(aes(label = round(pct_N, 1)), size = 4, hjust = -0.9) +
  geom_point(aes(size = Age_group_N), color = "#1f78b4") +
  scale_size(label = dollar_format(scale = 1e-3, prefix = "", suffix = "K")) +
  theme(legend.justification = "top") +
  facet_wrap(~ result) +
  expand_limits(y = 0) +
  labs(y = "Age at prelevement", x = "Proportion of Hb genes") +
  theme_minimal()


# Modeling----
model <- glm(result ~ gender + Age+Year + Route_sample, data =dada , family = binomial)

# Get a summary of the model
summary(model)

# Predict probabilities
dada[, predicted_probs := predict(model, dada, type = "response")]

# Plot fit (you may need to adjust this function for binary outcomes)
#plot_fit(dada)

# Assuming 'result' is a binary outcome (0/1)
dada$predicted_probs <- predict(model, dada, type = "response")






fp <- dada %>% drop_na(gender, Owner, Age, province, result, Year, Route_sample)

# Split Data into Training and Testing Sets
set.seed(123)

# Create a training set (70% of the data) and a test set (30% of the data)
train_indices <- sample(1:nrow(fp), size = 0.7 * nrow(fp))
train_data <- fp[train_indices, ]
test_data <- fp[-train_indices, ]

# model
model <- glm(result ~ Owner+gender + Age + Year + Route_sample, data = train_data, family = binomial)
summary(model) 

# Make predictions
test_data$predicted_probs <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to binary outcomes (0/1) based on a threshold (e.g., 0.5)
test_data$predicted_classes <- ifelse(test_data$predicted_probs > 0.5, 1, 0)

unique(test_data$result)               # Actual classes
unique(test_data$predicted_classes)     # Predicted classes

# Example of changing the threshold
threshold <- 0.3  # Adjust based on your analysis
test_data$predicted_classes <- ifelse(test_data$predicted_probs > threshold, 1, 0)

confusionMatrix(as.factor(test_data$predicted_classes), as.factor(test_data$result))


# Model evaluation
confusionMatrix(as.factor(test_data$predicted_classes), as.factor(test_data$result))


# Load the caret package
library(caret)
train_control <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)


logistic_model <- train(
  result ~ gender + Owner + Age + province + Year + Route_sample,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = train_control,
  metric = "ROC"
)

# Print Model Summary
summary(logistic_model$finalModel)

# Evaluate Model on Test Data
# Predict probabilities on the test set
test_predictions <- predict(logistic_model, newdata = test_data, type = "prob")

# Convert probabilities to binary outcomes (threshold = 0.5)
test_data$Predicted_Hb_S <- ifelse(test_predictions[, "Yes"] > 0.5, "Yes", "No")

# Confusion Matrix
conf_matrix <- confusionMatrix(factor(test_data$Predicted_Hb_S), factor(test_data$Hb_S))
print(conf_matrix)

# Visualize Results /ROC Curve

roc_data <- roc(response = test_data$Hb_S, predictor = test_predictions[, "Yes"])
plot(roc_data, main = "ROC Curve for Logistic Regression", col = "blue")

# Save Results
write.csv(test_data, "predicted_results.csv")


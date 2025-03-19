#### Install necessary libraries if not installed####
list.of.packages <- c("tidyverse", "caret", "randomForest", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
install.packages("ggcorrplot")
# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)

####Load dataset####
data <- read.csv("E:\\Kaggle\\Dataset\\archive\\student_depression_dataset.csv")

# View dataset structure
str(data)

# Summary statistics
summary(data)

# Check for missing values
sum(is.na(data))

# Handle missing values (if any)
data <- na.omit(data)




####Load necessary libraries####
library(ggplot2)
library(dplyr)
library(ggcorrplot)

# Convert categorical variables to factors
data$Gender <- factor(data$Gender, levels = c("Male", "Female"))
data$Sleep.Duration <- factor(data$Sleep.Duration, levels = c("'Less than 5 hours'", "'5-6 hours'", "'7-8 hours'"))
data$Dietary.Habits <- factor(data$Dietary.Habits, levels = c("Healthy", "Moderate"))
data$Degree <- factor(data$Degree)
data$Have.you.ever.had.suicidal.thoughts.. <- factor(data$Have.you.ever.had.suicidal.thoughts.., levels = c("No", "Yes"))
data$Family.History.of.Mental.Illness <- factor(data$Family.History.of.Mental.Illness, levels = c("No", "Yes"))

# Convert 'Financial.Stress' to numeric if it is in character format
data$Financial.Stress <- as.numeric(data$Financial.Stress)




####1. Enhanced Distribution of Depression by Gender:#####
# Distribution of Depression by Gender with enhanced visuals
ggplot(data, aes(x = Gender, fill = factor(Depression))) +
  geom_bar(position = "fill", width = 0.7) +
  labs(
    title = "Distribution of Depression by Gender", 
    x = "Gender", 
    y = "Proportion of Individuals"
  ) +
  scale_fill_manual(values = c("lightblue", "salmon"), labels = c("No", "Yes")) +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )






####2. Enhanced Correlation Heatmap:####
# Correlation heatmap with improved aesthetics and italicized x-axis labels
ggcorrplot(correlation_matrix, 
           lab = TRUE, 
           type = "lower", 
           colors = c("red", "white", "blue"),
           lab_size = 4, 
           title = "Correlation Heatmap of Numerical Variables") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    axis.text.y = element_text(face = "italic"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )




####3. Boxplot of Age vs. Depression:####
# Boxplot of Age vs Depression with enhanced visuals
# Boxplot of Age vs Sleep Duration with enhanced visuals
ggplot(data, aes(x = Sleep.Duration, y = Age, fill = Sleep.Duration)) +
  geom_boxplot() +
  labs(
    title = "Age Distribution by Sleep Duration", 
    x = "Sleep Duration", 
    y = "Age"
  ) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink")) +  # Customize colors as needed
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )





####4. Depression vs Sleep Duration:####
# Depression vs Sleep Duration with enhanced visuals
ggplot(data, aes(x = Sleep.Duration, fill = factor(Depression))) +
  geom_bar(position = "fill", width = 0.7) +
  labs(
    title = "Depression vs Sleep Duration", 
    x = "Sleep Duration", 
    y = "Proportion of Individuals"
  ) +
  scale_fill_manual(values = c("lightblue", "salmon"), labels = c("No", "Yes")) +
  theme_minimal(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )















#### Regression####
# Convert Depression to a factor
data$Depression <- factor(data$Depression, levels = c(0, 1), labels = c("No", "Yes"))



####Convert Categorical Variables to Factors:####
# Convert categorical variables to factors
data$Gender <- factor(data$Gender)
data$Sleep.Duration <- factor(data$Sleep.Duration)
data$Dietary.Habits <- factor(data$Dietary.Habits)
data$Have.you.ever.had.suicidal.thoughts.. <- factor(data$Have.you.ever.had.suicidal.thoughts..)
data$Family.History.of.Mental.Illness <- factor(data$Family.History.of.Mental.Illness)
data$Financial.Stress <- factor(data$Financial.Stress)


####2. Fit the Logistic Regression Model####
# Fit logistic regression model
logistic_model <- glm(Depression ~ Age + Gender + Sleep.Duration + Financial.Stress + CGPA, 
                      data = data, 
                      family = binomial(link = "logit"))

# View model summary
summary(logistic_model)



####3. Evaluate the Model####
# Predict probabilities for each observation
predicted_probabilities <- predict(logistic_model, type = "response")

# View predicted probabilities for the first few observations
head(predicted_probabilities)


####3.3. Model Evaluation (Confusion Matrix)####

# Create binary predictions based on threshold of 0.5
predicted_class <- ifelse(predicted_probabilities > 0.5, "Yes", "No")

# Confusion matrix
table(Predicted = predicted_class, Actual = data$Depression)



####4. Model Performance Metrics####
library(pROC)

# Create ROC curve and calculate AUC
roc_curve <- roc(data$Depression, predicted_probabilities)
auc(roc_curve)



####Visualize####
library(pROC)
# Create ROC curve
roc_curve <- roc(data$Depression, predicted_probabilities)

# Plot ROC curve with enhanced features
plot(roc_curve, 
     col = "blue", 
     lwd = 2, 
     main = "ROC Curve for Logistic Regression Model", 
     xlab = "False Positive Rate (FPR)", 
     ylab = "True Positive Rate (TPR)", 
     cex.main = 1.5, 
     cex.lab = 1.2)
grid()
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 3)), 
       col = "blue", lwd = 2, cex = 1.2)



####2. Confusion Matrix###

# View the confusion matrix table structure
str(cm$table)

# Convert confusion matrix into a data frame
cm_data <- as.data.frame(as.table(cm$table))

# Plot confusion matrix as heatmap
ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black", size = 5) +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16))



####3. Customized Coefficient Plot for Logistic Regression####
library(broom)
library(ggplot2)

# Get model coefficients
model_coefs <- tidy(logistic_model)

# Calculate confidence intervals for the coefficients
conf_ints <- confint(logistic_model)

# Add confidence intervals to the model coefficients
model_coefs$conf.low <- conf_ints[, 1]
model_coefs$conf.high <- conf_ints[, 2]

# Filter out the intercept
model_coefs <- model_coefs[model_coefs$term != "(Intercept)", ]

# Plot coefficients with confidence intervals
ggplot(model_coefs, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "blue") +
  coord_flip() +
  labs(title = "Logistic Regression Coefficients", x = "Predictor", y = "Coefficient Estimate") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(size = 16))







install.packages("officer")
install.packages("flextable")

library(officer)
library(flextable)





library(flextable)

# Convert the gt table to flextable
model_coefs_flextable <- model_coefs_table %>%
  flextable() %>%
  compose(
    j = "estimate",
    value = as_paragraph(estimate)
  ) %>%
  compose(
    j = c("conf.low", "conf.high"),
    value = as_paragraph(conf.low, " - ", conf.high)
  )

# Create a Word document and add the table
library(officer)

doc <- read_docx() %>%
  body_add_flextable(model_coefs_flextable) %>%
  body_add_par("Logistic Regression Coefficients with Confidence Intervals", style = "heading 1")

# Save the Word document
print(doc, target = "model_coefs_table.docx")


























install.packages("knitr")
library(knitr)
# Create a table for model coefficients with confidence intervals
model_coefs_table <- model_coefs[, c("term", "estimate", "conf.low", "conf.high")]

# Use kable to display the table
kable(model_coefs_table, caption = "Logistic Regression Coefficients with Confidence Intervals")

install.packages("gtsummary")
library(gtsummary)
install.packages("gt")
library(gt)


# Create a nice formatted table using gt
model_coefs_table %>%
  gt() %>%
  tab_header(
    title = "Logistic Regression Coefficients with Confidence Intervals"
  ) %>%
  cols_label(
    term = "Predictor",
    estimate = "Coefficient Estimate",
    conf.low = "Lower 95% CI",
    conf.high = "Upper 95% CI"
  ) %>%
  tab_spanner(
    label = "Confidence Interval",
    columns = c(conf.low, conf.high)
  ) %>%
  tab_spanner(
    label = "Estimate",
    columns = estimate
  )




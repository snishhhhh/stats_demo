
# ONLINE SHOPPING PURCHASING INTENTION

library(tidyverse)

# Import dataset
data <- read.csv(file.choose())

# 1. BASIC DATASET INFORMATION


# Number of rows and columns
dim(data)

# Number of observations
nrow(data)

# Number of variables
ncol(data)

# Variable names
names(data)

# Structure of dataset
str(data)

# First 6 rows
head(data)

# Last 6 rows
tail(data)

# Check missing values in every variable
colSums(is.na(data))


revenue_summary <- data %>%
  count(Revenue) %>%
  mutate(Percentage = n / sum(n) * 100)
revenue_summary

prop.table(table(data$Revenue)) * 100


#DUPLICATES
sum(duplicated(data))
sum(duplicated(data)) / nrow(data) * 100
#Therefore:125 observations, representing approximately 1.01% of the dataset, are duplicate rows 

# 1. Show all duplicate patterns
print(duplicate_counts, n = Inf)
duplicate_counts <- data %>%
  group_by(across(everything())) %>%
  summarise(
    Frequency = n(),
    .groups = "drop"
  ) %>%
  filter(Frequency > 1)

# 2. Frequency of duplicate patterns
table(duplicate_counts$Frequency)

# 3. Maximum frequency
max(duplicate_counts$Frequency)

# 4. Most repeated pattern(s)
duplicate_counts %>%
  filter(Frequency == max(Frequency))

# 5. Revenue distribution among duplicated rows
duplicate_rows <- data[duplicated(data), ]

table(duplicate_rows$Revenue)

prop.table(table(duplicate_rows$Revenue)) * 100

duplicate_counts %>%
  filter(Frequency == 14) %>%
  as.data.frame()

# Remove exact duplicate observations

data_clean <- data[!duplicated(data), ]

# Compare dimensions before and after removing duplicates

dim(data)
dim(data_clean)

# Number of observations removed

nrow(data) - nrow(data_clean)

# Percentage of observations removed

(nrow(data) - nrow(data_clean)) / nrow(data) * 100

# Confirm that no duplicates remain

sum(duplicated(data_clean))

revenue_clean <- data_clean %>%
  count(Revenue) %>%
  mutate(
    Percentage = n / sum(n) * 100
  )

revenue_clean

#Variables relationship
summary(data_clean$ProductRelated)
summary(data_clean$ProductRelated_Duration)
summary(data_clean$BounceRates)
summary(data_clean$ExitRates)
summary(data_clean$PageValues)

### Grouping the values by there means and medians to their purchase outcomes
#1. ProductRelated_Mean/median
data_clean %>%
  group_by(Revenue) %>%
  summarise(
    n = n(),
    
    ProductRelated_Mean = mean(ProductRelated),
    ProductRelated_Median = median(ProductRelated),
  )
ggplot(data_clean, aes(
  x = factor(
    Revenue,
    levels = c(FALSE, TRUE),
    labels = c("No Purchase", "Purchase")
  ),
  y = ProductRelated
)) +
  geom_boxplot() +
  labs(
    title = "Product-Related Page Views by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Number of Product-Related Pages Viewed"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )
#2. ProductRelated_Duration_Mean/median

data_clean %>%
  group_by(Revenue) %>%
  summarise(
    n = n(),
    ProductRelated_Duration_Mean = mean(ProductRelated_Duration),
    ProductRelated_Duration_Median = median(ProductRelated_Duration),
  )
ggplot(data_clean, aes(
  x = factor(
    Revenue,
    levels = c(FALSE, TRUE),
    labels = c("No Purchase", "Purchase")
  ),
  y = ProductRelated_Duration
)) +
      geom_boxplot() +
      labs(
        title = "Product-Related Page Duration by Purchase Outcome",
        x = "Purchase Outcome",
        y = "Time Spent on Product-Related Pages"
      ) +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 12)
        )


#3. BounceRates_Mean/median
data_clean %>%
  group_by(Revenue) %>%
  summarise(
    n = n(),  
    BounceRates_Mean = mean(BounceRates),
    BounceRates_Median = median(BounceRates),
  )

ggplot(data_clean, aes(
  x = factor(
    Revenue,
    levels = c(FALSE, TRUE),
    labels = c("No Purchase", "Purchase")
  ),
  y = BounceRates
)) +
  geom_boxplot() +
  labs(
    title = "Bounce Rate by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Bounce Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
     axis.title = element_text(size = 12)
    )
    
 #4. ExitRates_Mean/median  
data_clean %>%
  group_by(Revenue) %>%
  summarise(
    n = n(),
    ExitRates_Mean = mean(ExitRates),
    ExitRates_Median = median(ExitRates),
  )

ggplot(data_clean, aes(
  x = factor(
    Revenue,
    levels = c(FALSE, TRUE),
    labels = c("No Purchase", "Purchase")
  ),
  y = ExitRates
)) +
   geom_boxplot() +
  labs(
    title = "Exit Rate by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Exit Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )




### correlation between predicting numerical variables by purchase results

#1. Administrative_Mean/median
engagement_summary <- data_clean %>%
  group_by(Revenue) %>%
  summarise(
    Administrative_Mean = mean(Administrative),
    Administrative_Median = median(Administrative),
  )
engagement_summary

ggplot(data_clean, 
       aes(x = factor(Revenue), 
           y = Administrative, 
           fill = factor(Revenue))) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c("FALSE" = "No Purchase",
               "TRUE" = "Purchase")
  ) +
  labs(
    title = "Administrative Page Visits by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Administrative Page Visits"
  ) +
  theme_minimal() +
  guides(fill = "none")+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )
    
#2.dministrative_Duration_Mean/median

engagement_summary <- data_clean %>%
  group_by(Revenue) %>%
  summarise(    
    Administrative_Duration_Mean = mean(Administrative_Duration),
    Administrative_Duration_Median = median(Administrative_Duration),
  )
engagement_summary

ggplot(data_clean, 
       aes(x = factor(Revenue), 
           y = Administrative_Duration, 
           fill = factor(Revenue))) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c("FALSE" = "No Purchase",
               "TRUE" = "Purchase")
  ) +
  labs(
    title = "Administrative Duration by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Administrative Duration"
  ) +
  theme_minimal() +
  guides(fill = "none")+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )

#3. Informational_Mean/median 
engagement_summary <- data_clean %>%
  group_by(Revenue) %>%
  summarise(  
        
    Informational_Mean = mean(Informational),
    Informational_Median = median(Informational),
  )
engagement_summary

ggplot(data_clean, 
       aes(x = factor(Revenue), 
           y = Informational, 
           fill = factor(Revenue))) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c("FALSE" = "No Purchase",
               "TRUE" = "Purchase")
  ) +
  labs(
    title = "Informational Page Visits by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Informational Page Visits"
  ) +
  theme_minimal() +
  guides(fill = "none")+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )


#4. Informational_Duration_Mean/median
    
engagement_summary <- data_clean %>%
  group_by(Revenue) %>%
  summarise(    
        
    Informational_Duration_Mean = mean(Informational_Duration),
    Informational_Duration_Median = median(Informational_Duration),
  )
engagement_summary

ggplot(data_clean, 
       aes(x = factor(Revenue), 
           y = Informational_Duration, 
           fill = factor(Revenue))) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c("FALSE" = "No Purchase",
               "TRUE" = "Purchase")
  ) +
  labs(
    title = "Informational Duration by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Informational Duration"
  ) +
  theme_minimal() +
  guides(fill = "none")+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )

#5.ProductRelated_Mean/median
    
engagement_summary <- data_clean %>%
  group_by(Revenue) %>%
  summarise(    
        
    ProductRelated_Mean = mean(ProductRelated),
    ProductRelated_Median = median(ProductRelated),
  )
engagement_summary

ggplot(
  data_clean,
  aes(
    x = factor(Revenue,
               levels = c(FALSE, TRUE),
               labels = c("No Purchase", "Purchase")),
    y = ProductRelated,
    fill = Revenue
  )
) +
  geom_boxplot() +
  labs(
    title = "Product Related Page Visits by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Product Related Page Visits"
  ) +
  scale_fill_manual(
    values = c("FALSE" = "#F8766D", "TRUE" = "#00BFC4"),
    guide = "none"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )

#6. ProductRelated_Duration_Mean/median
    
engagement_summary <- data_clean %>%
  group_by(Revenue) %>%
  summarise(        
    ProductRelated_Duration_Mean = mean(ProductRelated_Duration),
    ProductRelated_Duration_Median = median(ProductRelated_Duration)

  )
engagement_summary

ggplot(
  data_clean,
  aes(
    x = ifelse(Revenue == TRUE, "Purchase", "No Purchase"),
    y = ProductRelated_Duration,
    fill = ifelse(Revenue == TRUE, "Purchase", "No Purchase")
  )
) +
  geom_boxplot() +
  labs(
    title = "Product Related Duration by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Product Related Duration",
    fill = "Purchase Outcome"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )

#7. Page value_mean/median

data_clean %>%
  group_by(Revenue) %>%
  summarise(
    n=n(),
    PageValues_Mean = mean(PageValues),
    PageValues_Median = median(PageValues)
  )

pagevalue_summary <- data_clean %>%
  group_by(Revenue) %>%
  summarise(
    n = n(),
    Mean = mean(PageValues),
    Median = median(PageValues),
    Q1 = quantile(PageValues, 0.25),
    Q3 = quantile(PageValues, 0.75),
    Maximum = max(PageValues),
    Positive_PageValues = sum(PageValues > 0),
    Percentage_Positive = mean(PageValues > 0) * 100
  )
pagevalue_summary


ggplot(data_clean, aes(
  x = factor(
    Revenue,
    levels = c(FALSE, TRUE),
    labels = c("No Purchase", "Purchase")
  ),
  y = PageValues
)) +
  geom_boxplot() +
  labs(
    title = "Page Values by Purchase Outcome",
    x = "Purchase Outcome",
    y = "Page Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
  )
#Positive frequency of PageValue
data_clean %>%
  group_by(Revenue) %>%
  summarise(
    n = n(),
    Positive_PageValues = sum(PageValues > 0),
    Percentage_Positive = mean(PageValues > 0) * 100
  )

# Relationship between revenue and PageValue
data_clean %>%
  mutate(
    PageValue_Group = ifelse(PageValues > 0, "Positive", "Zero")
  ) %>%
  group_by(PageValue_Group) %>%
  summarise(
    Sessions = n(),
    Purchases = sum(Revenue),
    Purchase_Rate = mean(Revenue) * 100
  )

#Calculating correlation matrix
numeric_vars <- data_clean %>%
  select(
    Administrative,
    Administrative_Duration,
    Informational,
    Informational_Duration,
    ProductRelated,
    ProductRelated_Duration,
    BounceRates,
    ExitRates,
    PageValues,
    SpecialDay
  )

correlation_matrix <- cor(
  numeric_vars,
  use = "complete.obs",
  method = "spearman"
)

round(correlation_matrix, 2)


library(corrplot)

corrplot(
  correlation_matrix,
  method = "color",
  type = "lower",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.cex = 0.9,
  tl.col = "black",
  diag = FALSE
)

# correlation of catrgoriel variables in-terms of purchase results

#1.vistor type
visitor_purchase <- data_clean %>%
  count(VisitorType, Revenue) %>%
  group_by(VisitorType) %>%
  mutate(
    Percentage = n / sum(n) * 100
  )

visitor_purchase

ggplot(visitor_purchase,
      aes(
        x = VisitorType,
        y = Percentage,
        fill = factor(
          Revenue,
          levels = c(FALSE, TRUE),
          labels = c("No Purchase", "Purchase")
  )
))+
  geom_col(position = "dodge") +
  labs(
    title = "Purchase Outcome by Visitor Type",
    x = "Visitor Type",
    y = "Percentage of Sessions",
    fill = "Purchase Outcome"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )

#2. weekend
weekend_purchase <- data_clean %>%
  count(Weekend, Revenue) %>%
  group_by(Weekend) %>%
  mutate(
    Percentage = n / sum(n) * 100
  )

weekend_purchase

ggplot(weekend_purchase,
       aes(
         x = factor(Weekend,
                    levels = c(FALSE, TRUE),
                    labels = c("Not Weekend", "Weekend")),
         y = Percentage,
         fill = factor(
           Revenue,
           levels = c(FALSE, TRUE),
           labels = c("No Purchase", "Purchase")
         )
       )) +
  geom_col(position = "dodge") +
  labs(
    title = "Purchase Outcome by Weekend",
    x = "Day Type",
    y = "Percentage of Sessions",
    fill = "Purchase Outcome"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )
#3.month
month_purchase <- data_clean %>%
  count(Month, Revenue) %>%
  group_by(Month) %>%
  mutate(
    Percentage = n / sum(n) * 100
  )

month_purchase

ggplot(month_purchase,
       aes(
         x = Month,
         y = Percentage,
         fill = factor(
           Revenue,
           levels = c(FALSE, TRUE),
           labels = c("No Purchase", "Purchase")
         )
       )) +
  geom_col(position = "dodge") +
  labs(
    title = "Purchase Outcome by Month",
    x = "Month",
    y = "Percentage of Sessions",
    fill = "Purchase Outcome"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )

#4.special day

specialday_purchase <- data_clean %>%
  group_by(SpecialDay, Revenue) %>%
  summarise(
    Sessions = n(),
    .groups = "drop"
  ) %>%
  group_by(SpecialDay) %>%
  mutate(
    Percentage = Sessions / sum(Sessions) * 100
  )

specialday_purchase

ggplot(specialday_purchase,
       aes(
         x = factor(SpecialDay),
         y = Percentage,
         fill = factor(
           Revenue,
           levels = c(FALSE, TRUE),
           labels = c("No Purchase", "Purchase")
         )
       )) +
  geom_col(position = "dodge") +
  labs(
    title = "Purchase Outcome by Special Day",
    x = "Special Day",
    y = "Percentage of Sessions",
    fill = "Purchase Outcome"
  ) +
  theme_minimal()+theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )

#5. operating system

operating_system_purchase <- data_clean %>%
  count(OperatingSystems, Revenue) %>%
  group_by(OperatingSystems) %>%
  mutate(
    Percentage = n / sum(n) * 100
  )

operating_system_purchase
ggplot(operating_system_purchase,
       aes(
         x = factor(OperatingSystems),
         y = Percentage,
         fill = factor(
           Revenue,
           levels = c(FALSE, TRUE),
           labels = c("No Purchase", "Purchase")
         )
       )) +
  geom_col(position = "dodge") +
  labs(
    title = "Purchase Outcome by Operating System",
    x = "Operating System",
    y = "Percentage of Sessions",
    fill = "Purchase Outcome"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )

#6. Browser

browser_purchase <- data_clean %>%
  count(Browser, Revenue) %>%
  group_by(Browser) %>%
  mutate(
    Percentage = n / sum(n) * 100,
    Revenue_Label = ifelse(Revenue == TRUE, "Purchase", "No Purchase")
  )

browser_purchase

ggplot(
  browser_purchase,
  aes(
    x = factor(Browser),
    y = Percentage,
    fill = Revenue_Label
  )
)+
  geom_col(position = "dodge") +
  labs(
    title = "Purchase Outcome by Browser",
    x = "Browser",
    y = "Percentage of Sessions",
    fill = "Purchase Outcome"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )

#7. region

region_purchase <- data_clean %>%
  count(Region, Revenue) %>%
  group_by(Region) %>%
  mutate(
    Percentage = n / sum(n) * 100,
    Revenue_Label = ifelse(
      Revenue == TRUE,
      "Purchase",
      "No Purchase"
    )
  )

region_purchase

ggplot(
  region_purchase,
  aes(
    x = factor(Region),
    y = Percentage,
    fill = Revenue_Label
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Purchase Outcome by Region",
    x = "Region",
    y = "Percentage of Sessions",
    fill = "Purchase Outcome"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#8. traffic type

traffic_purchase <- data_clean %>%
  count(TrafficType, Revenue) %>%
  group_by(TrafficType) %>%
  mutate(
    Percentage = n / sum(n) * 100,
    Revenue_Label = ifelse(
      Revenue == TRUE,
      "Purchase",
      "No Purchase"
    )
  )

traffic_purchase

ggplot(
  traffic_purchase,
  aes(
    x = factor(TrafficType),
    y = Percentage,
    fill = Revenue_Label
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Purchase Outcome by Traffic Type",
    x = "Traffic Type",
    y = "Percentage of Sessions",
    fill = "Purchase Outcome"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


#Inferential analysis- wilcoxon test


wilcox_results <- data_clean %>%
  summarise(
    
    Administrative_p = wilcox.test(
      Administrative ~ Revenue
    )$p.value,
    
    Administrative_Duration_p = wilcox.test(
      Administrative_Duration ~ Revenue
    )$p.value,
    
    Informational_p = wilcox.test(
      Informational ~ Revenue
    )$p.value,
    
    Informational_Duration_p = wilcox.test(
      Informational_Duration ~ Revenue
    )$p.value,
    
    ProductRelated_p = wilcox.test(
      ProductRelated ~ Revenue
    )$p.value,
    
    ProductRelated_Duration_p = wilcox.test(
      ProductRelated_Duration ~ Revenue
    )$p.value,
    
    BounceRates_p = wilcox.test(
      BounceRates ~ Revenue
    )$p.value,
    
    ExitRates_p = wilcox.test(
      ExitRates ~ Revenue
    )$p.value,
    
    PageValues_p = wilcox.test(
      PageValues ~ Revenue
    )$p.value
  )

wilcox_results

wilcox_results_long <- wilcox_results %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "P_Value"
  ) %>%
  mutate(
    Variable = gsub("_p$", "", Variable),
    Significance = ifelse(
      P_Value < 0.05,
      "Significant",
      "Not Significant"
    )
  )

wilcox_results_long


## Chi-Square Tests for Categorical Variables

visitor_chi <- chisq.test(table(data_clean$VisitorType, data_clean$Revenue))
weekend_chi <- chisq.test(table(data_clean$Weekend, data_clean$Revenue))
month_chi <- chisq.test(table(data_clean$Month, data_clean$Revenue))
specialday_chi <- chisq.test(table(data_clean$SpecialDay, data_clean$Revenue))
os_chi <- chisq.test(table(data_clean$OperatingSystems, data_clean$Revenue))
browser_chi <- chisq.test(table(data_clean$Browser, data_clean$Revenue))
region_chi <- chisq.test(table(data_clean$Region, data_clean$Revenue))
traffic_chi <- chisq.test(table(data_clean$TrafficType, data_clean$Revenue))

visitor_chi
weekend_chi
month_chi
specialday_chi
os_chi
browser_chi
region_chi
traffic_chi
## as I got warning I will Check expected frequencies for variables with Chi-square warnings

os_chi$expected
browser_chi$expected
traffic_chi$expected

## Number of expected cells below 5

sum(os_chi$expected < 5)
sum(browser_chi$expected < 5)
sum(traffic_chi$expected < 5)

## Percentage of expected cells below 5

mean(os_chi$expected < 5) * 100
mean(browser_chi$expected < 5) * 100
mean(traffic_chi$expected < 5) * 100


os_chi_sim <- chisq.test(
  table(data_clean$OperatingSystems, data_clean$Revenue),
  simulate.p.value = TRUE,
  B = 10000
)

browser_chi_sim <- chisq.test(
  table(data_clean$Browser, data_clean$Revenue),
  simulate.p.value = TRUE,
  B = 10000
)

traffic_chi_sim <- chisq.test(
  table(data_clean$TrafficType, data_clean$Revenue),
  simulate.p.value = TRUE,
  B = 10000
)

os_chi_sim
browser_chi_sim
traffic_chi_sim

##final results for chi-square

chi_results <- data.frame(
  Variable = c(
    "Visitor Type",
    "Weekend",
    "Month",
    "Special Day",
    "Operating System",
    "Browser",
    "Region",
    "Traffic Type"
  ),
  
  P_Value = c(
    visitor_chi$p.value,
    weekend_chi$p.value,
    month_chi$p.value,
    specialday_chi$p.value,
    os_chi_sim$p.value,
    browser_chi_sim$p.value,
    region_chi$p.value,
    traffic_chi_sim$p.value
  )
) %>%
  mutate(
    Result = ifelse(
      P_Value < 0.05,
      "Significant",
      "Not Significant"
    )
  )

chi_results

#data preration for categorical variables 
model_data <- data_clean %>%
  mutate(
    Revenue = factor(
      Revenue,
      levels = c(FALSE, TRUE),
      labels = c("No Purchase", "Purchase")
    ),
    
    VisitorType = factor(VisitorType),
    Month = factor(Month),
    OperatingSystems = factor(OperatingSystems),
    Browser = factor(Browser),
    Region = factor(Region),
    TrafficType = factor(TrafficType),
    Weekend = factor(Weekend)
  )

str(model_data)

#Logistic regression model

logistic_model <- glm(
  Revenue ~ Administrative + Administrative_Duration +
    Informational + Informational_Duration +
    ProductRelated + ProductRelated_Duration +
    BounceRates + ExitRates + PageValues +
    SpecialDay + Month + Browser +
    Region + TrafficType + VisitorType + Weekend,
  family = binomial,
  data = model_data
)
summary(logistic_model)


#ODDS RATIOS

logistic_results <- as.data.frame(
  summary(logistic_model)$coefficients
)

logistic_results$Variable <- rownames(logistic_results)

logistic_results <- logistic_results %>%
  select(
    Variable,
    Estimate,
    `Pr(>|z|)`
  ) %>%
  rename(
    P_Value = `Pr(>|z|)`
  ) %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    Significance = ifelse(
      P_Value < 0.05,
      "Significant",
      "Not Significant"
    )
  )

logistic_results

# Odds Ratio Plot

ggplot(
  logistic_significant,
  aes(
    x = reorder(Variable, Odds_Ratio),
    y = Odds_Ratio
  )
) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = CI_Lower,
      ymax = CI_Upper
    ),
    width = 0.2
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title = "Significant Predictors of Purchase",
    x = "Variable",
    y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )


###cleaning the regression model
summary(logistic_model)

model_summary <- summary(logistic_model)

logistic_CI_results <- data.frame(
  Variable = rownames(model_summary$coefficients),
  Estimate = model_summary$coefficients[, "Estimate"],
  Std_Error = model_summary$coefficients[, "Std. Error"],
  P_Value = model_summary$coefficients[, "Pr(>|z|)"]
)

logistic_CI_results <- logistic_CI_results %>%
  mutate(
    Odds_Ratio = exp(Estimate),
    CI_Lower = exp(Estimate - 1.96 * Std_Error),
    CI_Upper = exp(Estimate + 1.96 * Std_Error),
    Significance = ifelse(
      P_Value < 0.05,
      "Significant",
      "Not Significant"
    )
  )

logistic_CI_results

# final linear regression result

logistic_final <- logistic_results %>%
  filter(
    Variable != "(Intercept)",
    !is.na(Odds_Ratio),
    !is.na(P_Value)
  ) %>%
  mutate(
    Odds_Ratio = round(Odds_Ratio, 3),
    P_Value = signif(P_Value, 3)
  ) %>%
  select(
    Variable,
    Odds_Ratio,
    P_Value,
    Significance
  )

logistic_final

alias(logistic_model)
model_data$Browser <- droplevels(model_data$Browser)
table(model_data$Browser, model_data$Revenue)
###multicollinearity

library(car)

vif_values <- vif(logistic_model)

vif_values

## Linearity of continuous predictors with the logit

library(ggplot2)

continuous_vars <- c(
  "Administrative",
  "Administrative_Duration",
  "Informational",
  "Informational_Duration",
  "ProductRelated",
  "ProductRelated_Duration",
  "BounceRates",
  "ExitRates",
  "PageValues",
  "SpecialDay"
)

for (var in continuous_vars) {
  
  plot_data <- data.frame(
    x = model_data[[var]],
    Revenue = model_data$Revenue
  )
  
  ggplot(plot_data, aes(x = x, fill = Revenue)) +
    geom_density(alpha = 0.4) +
    labs(
      title = paste("Distribution of", var, "by Revenue"),
      x = var,
      y = "Density"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Significant Logistic Regression Results

logistic_significant <- logistic_CI_results %>%
  filter(
    Variable != "(Intercept)",
    P_Value < 0.05
  ) %>%
  select(
    Variable,
    Odds_Ratio,
    CI_Lower,
    CI_Upper,
    P_Value,
    Significance
  )

logistic_significant

# Round-offs

logistic_significant_display <- logistic_significant %>%
  mutate(
    Odds_Ratio = round(Odds_Ratio, 4),
    CI_Lower = round(CI_Lower, 4),
    CI_Upper = round(CI_Upper, 4),
    P_Value = signif(P_Value, 3)
  )

logistic_significant_display

#overall model fit

null_deviance <- logistic_model$null.deviance
residual_deviance <- logistic_model$deviance

pseudo_R2 <- 1 - (residual_deviance / null_deviance)

model_fit <- data.frame(
  Null_Deviance = null_deviance,
  Residual_Deviance = residual_deviance,
  AIC = AIC(logistic_model),
  Pseudo_R2 = pseudo_R2
)

model_fit

#significance

model_comparison <- anova(
  logistic_model,
  test = "Chisq"
)

model_comparison

# Checking for Singular Coefficients

coef(logistic_model)[is.na(coef(logistic_model))]

#overall summary combining all three analyses 

overall_results <- data.frame(
  Analysis = c(
    "Wilcoxon",
    "Wilcoxon",
    "Chi-square",
    "Chi-square",
    "Logistic Regression",
    "Logistic Regression"
  ),
  
  Variable = c(
    "PageValues",
    "ProductRelated_Duration",
    "Month",
    "Region",
    "PageValues",
    "ExitRates"
  ),
  
  P_Value = c(
    wilcox_results$PageValues_p,
    wilcox_results$ProductRelated_Duration_p,
    chi_results$P_Value[chi_results$Variable == "Month"],
    chi_results$P_Value[chi_results$Variable == "Region"],
    logistic_CI_results$P_Value[
      logistic_CI_results$Variable == "PageValues"
    ],
    logistic_CI_results$P_Value[
      logistic_CI_results$Variable == "ExitRates"
    ]
  )
)

overall_results

#Interpretation

logistic_interpretation <- logistic_CI_results %>%
  mutate(
    Effect = case_when(
      Odds_Ratio > 1 & Odds_Ratio < 1.01 ~ "Slightly higher odds",
      Odds_Ratio >= 1.01 ~ "Higher odds",
      Odds_Ratio < 1 ~ "Lower odds",
      TRUE ~ "No clear change in odds"
    )
  ) %>%
  select(
    Variable,
    Odds_Ratio,
    CI_Lower,
    CI_Upper,
    P_Value,
    Effect
  )

logistic_interpretation

#Logistic Regression Model Fit

null_deviance <- logistic_model$null.deviance
residual_deviance <- logistic_model$deviance

likelihood_ratio_chi <- null_deviance - residual_deviance

df_difference <- logistic_model$df.null - logistic_model$df.residual

likelihood_ratio_p <- pchisq(
  likelihood_ratio_chi,
  df = df_difference,
  lower.tail = FALSE
)

mcfadden_r2 <- 1 - (residual_deviance / null_deviance)

aic_value <- AIC(logistic_model)

logistic_fit_results <- data.frame(
  Measure = c(
    "Null Deviance",
    "Residual Deviance",
    "Likelihood Ratio Chi-square",
    "Likelihood Ratio p-value",
    "McFadden Pseudo R-squared",
    "AIC"
  ),
  Value = c(
    null_deviance,
    residual_deviance,
    likelihood_ratio_chi,
    likelihood_ratio_p,
    mcfadden_r2,
    aic_value
  )
)

logistic_fit_results

# Rounded-up Model Fit Results

logistic_fit_results_display <- logistic_fit_results %>%
  mutate(
    Value = round(Value, 4)
  )

logistic_fit_results_display

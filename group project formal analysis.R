library(ggplot2)
library(dplyr)
library(tidyr)
data <- read.csv("DAProject3.csv")
obesity_yearly <- data %>%
  group_by(Year) %>%
  summarise(ObesityRate = mean(Obese == "Yes"))
ggplot(obesity_yearly, aes(x = Year, y = ObesityRate)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Prevalence of Obesity Over the Years",
       x = "Year",
       y = "Obesity Rate")


obesity_by_age <- data %>%
  group_by(AgeGroup) %>%
  summarise(ObesityRate = mean(Obese == "Yes"))
ggplot(obesity_by_age, aes(x = AgeGroup, y = ObesityRate, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Obesity Rates by Age Group",
       x = "Age Group",
       y = "Obesity Rate") +
  theme(legend.position = "none")
table_age_obesity <- table(data$AgeGroup, data$Obese)
chisq.test(table_age_obesity)



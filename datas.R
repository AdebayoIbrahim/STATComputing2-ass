#require haven library
library(foreign)
library(dplyr)
library(ggplot2)
data <- read.spss("/home/setup/Downloads/Data_1.sav", to.data.frame=TRUE)

#example_analyzed_data_using EDA from tutor
#example <- read.spss("/home/setup/Downloads/EDA_1.spv", to.data.frame=TRUE)

admission_summary <- data %>%
  group_by(AdMode) %>%
  summarise(Count = n())

ggplot(admission_summary, aes(x = "", y = Count, fill = AdMode)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Proportion of DE vs. UTME Entries") +
  theme_void()

summary(admission_summary)

# Create a frequency table with counts and percentages
frequency_table <- admission_summary %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 1))

# View the frequency table
print(frequency_table)


##-------------MArital Stautus of Student


marital_status <- data %>%
  group_by(MS) %>%
  summarise(Count = n())


# Create the bar chart
ggplot(data, aes(x = MS, fill = MS)) +
  geom_bar() +
  labs(title = "Distribution of Marital Status Among Admitted Students",
       x = "Marital Status",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Single" = "steelblue", "Married" = "tomato"))

##-----------student -department
department <- data %>%
  group_by(Dept) %>%
  summarise(Count = n())

print(department)

ggplot(department, aes(x = "", y = Count, fill = Dept)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Proportion of Student department.") +
  theme_void()

#summary(department)

## --- students Gender
gender <- data %>%
  group_by(Gender) %>%
  summarise(Count = n())

print(department)

ggplot(gender, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Proportion of Student Gender.") +
  theme_void()

print(gender)

##stat as first Choice
statop <- data %>%
  group_by(StatOp) %>%
  summarise(Count = n())


ggplot(statop, aes(x = "", y = Count, fill = StatOp)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Proportion of Student with Stat as first Option") +
  theme_void()

print(statop)

## accomodation status

accstatus <- data %>%
  group_by(AS) %>%
  summarise(Count = n())


ggplot(accstatus, aes(x = "", y = Count, fill = AS)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Proportion of Student Accomodation Status") +
  theme_void()

print(accstatus)

## types of school attended

tsa <- data %>%
  group_by(TSA) %>%
  summarise(Count = n())


ggplot(tsa, aes(x = "", y = Count, fill = TSA)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Proportion of Student with type of School Attended") +
  theme_void()

print(tsa)

## State of origin

sto <- data %>%
  group_by(SOR) %>%
  summarise(Count = n())


ggplot(sto, aes(x = "", y = Count, fill = SOR)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Proportion of Student From OYO and Others") +
  theme_void()

print(sto)
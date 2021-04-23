# Yuhaniz Aly
# Assignment 1

# Load libraries
library(dplyr)
library(ggplot2)
library(patchwork)

# Load data
deaths_data <- read.csv("data/deaths.csv")
ylds_data <- read.csv("data/ylds.csv")
life_expectancy_data <- read.csv("data/life_expectancy.csv")

# Replace NA as 0 for deaths, ylls, ylds
deaths_data[is.na(deaths_data)] <- 0
ylds_data[is.na(ylds_data)] <- 0
life_expectancy_data[is.na(life_expectancy_data)] <- 0

### Computing metrics ###

# Merge data
merge_data <- deaths_data %>%
  merge(life_expectancy_data) %>%
  merge(ylds_data) %>%
  mutate(used_age = NA)

# Fix the age group to use in formula (probably not needed)
i <- 1
while (i < nrow(merge_data) + 1) {
  if (merge_data$age[i] == "Under 5") {
    merge_data$used_age[i] <- 0
  }
  else if (merge_data$age[i] == "5 to 9") {
    merge_data$used_age[i] <- 5
  }
  else {
    merge_data$used_age[i] <- substring(merge_data$age[i], 1, 2)
  }
  merge_data$used_age[i] <- as.numeric(merge_data$used_age[i])
  i <- i + 1
}

merge_data$used_age <- as.numeric(merge_data$used_age)

# New column for YLLs and compute YLLs
merge_data$ylls <- merge_data$life.expectancy * merge_data$deaths

# New column for DALYs and compute DALYs
merge_data$dalys <- merge_data$ylls + merge_data$ylds

### Burden by Cause ###
# Compute top 10 highest cause of deaths
top_10_cause_deaths <- merge_data %>%
  group_by(cause_name) %>%
  summarise(total_deaths = sum(deaths)) %>%
  arrange(-total_deaths) %>%
  top_n(10, total_deaths)

# Barchart for top 10 highest causes of deaths
top_10_cause_deaths_plot <- ggplot(top_10_cause_deaths) +
  geom_col(mapping = aes(x = total_deaths, y = reorder(cause_name, total_deaths)
                         ), fill = "skyblue") +
  labs(
    title = "Top 10 Causes of Deaths",
    x = "Number of Deaths",
    y = "Causes"
  )

# Compute top 10 highest causes of YLLs
top_10_cause_ylls <- merge_data %>%
  group_by(cause_name) %>%
  summarise(total_ylls = sum(ylls)) %>%
  arrange(-total_ylls) %>%
  top_n(10, total_ylls)

# Barchart for top 10 highest causes of ylls
top_10_cause_ylls_plot <- ggplot(top_10_cause_ylls) +
  geom_col(mapping = aes(x = total_ylls, y = reorder(cause_name, total_ylls)),
           fill = "lightpink") +
  labs(
    title = "Top 10 Causes of Years of Life Lost (YLLs)",
    x = "Number of YLLs",
    y = "Causes"
  )

# Compute top 10 highest causes of YLDs
top_10_cause_ylds <- merge_data %>%
  group_by(cause_name) %>%
  summarise(total_ylds = sum(ylds)) %>%
  arrange(-total_ylds) %>%
  top_n(10, total_ylds)

# Barchart for top 10 highest causes of ylds
top_10_cause_ylds_plot <- ggplot(top_10_cause_ylds) +
  geom_col(mapping = aes(x = total_ylds, y = reorder(cause_name, total_ylds)),
           fill = "lightyellow") +
  # geom_bar(stat = "identity", fill = "yellow") +
  labs(
    title = "Top 10 Causes of Years Lived with Disability (YLDs)",
    x = "Number of YLDs",
    y = "Causes"
  )

# Compute top 10 highest causes of DALYs
top_10_cause_dalys <- merge_data %>%
  group_by(cause_name) %>%
  summarise(total_dalys = sum(dalys)) %>%
  arrange(-total_dalys) %>%
  top_n(10, total_dalys)

# Barchart for top 10 highest causes of DALYs
top_10_cause_dalys_plot <- ggplot(top_10_cause_dalys) +
  geom_col(mapping = aes(x = total_dalys, y = reorder(cause_name, total_dalys)),
           fill = "lightgreen") +
  # geom_bar(stat = "identity", fill = "yellow") +
  labs(
    title = "Top 10 Causes of Disability Adjusted Life Years (DALYs)",
    x = "Number of DALYs",
    y = "Causes"
  )

# Patchwork for burden by cause
cause_patchwork <- (top_10_cause_deaths_plot | top_10_cause_ylls_plot) /
  (top_10_cause_ylds_plot | top_10_cause_dalys_plot)

### Burden by Age ###
# Compute age vs number of deaths
age_deaths <- merge_data %>%
  group_by(used_age) %>%
  summarise(total_deaths = sum(deaths))

# Barchart for age vs number of deaths
age_deaths_plot <- ggplot(age_deaths) +
  geom_col(mapping = aes(x = used_age, y = total_deaths), fill = "lightblue") +
  labs(
    title = "Number of Deaths by Age",
    x = "Age",
    y = "Number of Deaths"
  )

# Compute age vs ylls
age_ylls <- merge_data %>%
  group_by(used_age) %>%
  summarise(total_ylls = sum(ylls))

# Barchart for age vs ylls
age_ylls_plot <- ggplot(age_ylls) +
  geom_col(mapping = aes(x = used_age, y = total_ylls), fill = "lightpink") +
  labs(
    title = "Years of Life Lost (YLLs) by Age",
    x = "Age",
    y = "Number of YLLs"
  )

# Compute age vs ylds
age_ylds <- merge_data %>%
  group_by(used_age) %>%
  summarise(total_ylds = sum(ylds))

# Barchart for age vs ylds
age_ylds_plot <- ggplot(age_ylds) +
  geom_col(mapping = aes(x = used_age, y = total_ylds), fill = "lightyellow") +
  labs(
    title = "Years Lives with Disability (YLDs) by Age",
    x = "Age",
    y = "Number of YLDs"
  )

# Compute age vs dalys
age_dalys <- merge_data %>%
  group_by(used_age) %>%
  summarise(total_dalys = sum(dalys))

# Barchart for age vs dalys
age_dalys_plot <- ggplot(age_dalys) +
  geom_col(mapping = aes(x = used_age, y = total_dalys), fill = "lightgreen") +
  labs(
    title = "Disability Adjusted Life Years (DALYs) by Age",
    x = "Age",
    y = "Number of DALYs"
  )

# Patchwork for burden by age
age_patchwork <- (age_deaths_plot | age_ylls_plot) /
  (age_ylds_plot | age_dalys_plot)

### Burden by Sex ###
# Compute sex vs number of deaths
sex_deaths <- merge_data %>%
  group_by(sex) %>%
  summarise(total_deaths = sum(deaths))

# Barchart for sex vs number of deaths
sex_deaths_plot <- ggplot(sex_deaths) +
  geom_col(mapping = aes(x = sex, y = total_deaths), fill = "lightblue") +
  labs(
    title = "Number of Deaths VS Sex",
    x = "Sex",
    y = "Number of Deaths"
  )

# Compute sex vs ylls
sex_ylls <- merge_data %>%
  group_by(sex) %>%
  summarise(total_ylls = sum(ylls))

# Barchart for sex vs ylls
sex_ylls_plot <- ggplot(sex_ylls) +
  geom_col(mapping = aes(x = sex, y = total_ylls), fill = "lightpink") +
  labs(
    title = "Number of Years of Life Lost (YLLs) VS Sex",
    x = "Sex",
    y = "Number of YLLs"
  )

# Compute sex vs ylds
sex_ylds <- merge_data %>%
  group_by(sex) %>%
  summarise(total_ylds = sum(ylds))

# Barchart for sex vs ylds
sex_ylds_plot <- ggplot(sex_ylds) +
  geom_col(mapping = aes(x = sex, y = total_ylds), fill = "lightyellow") +
  labs(
    title = "Number of Years Lives with Disability (YLDs) VS Sex",
    x = "Sex",
    y = "Number of YLDs"
  )

# Compute sex vs dalys
sex_dalys <- merge_data %>%
  group_by(sex) %>%
  summarise(total_dalys = sum(dalys))

# Barchart for sex vs ylds
sex_dalys_plot <- ggplot(sex_dalys) +
  geom_col(mapping = aes(x = sex, y = total_dalys), fill = "lightgreen") +
  labs(
    title = "Number of Disability Adjusted Life Years (DALYs) VS Sex",
    x = "Sex",
    y = "Number of DALYs"
  )

# Patchwork for burden by sex
sex_patchwork <- (sex_deaths_plot | sex_ylls_plot) /
  (sex_ylds_plot | sex_dalys_plot)

library(tidyverse)
library(data.table)
library(readxl)
library(ggpubr)


clean_age <- function(df) {
  dt <- data.table::data.table(df)

  pat <- "(\\d)+"
  pat <- "(\\d)+"

  transform_to_years <- function(x) {
    as.numeric(str_extract(x, pat)) / 12
  }

  dt[stringr::str_detect(age_at_diagnosis_ckd, "month"), age_at_diagnosis_ckd := transform_to_years(age_at_diagnosis_ckd)]
  dt[, age_at_diagnosis_ckd := round(as.numeric(age_at_diagnosis_ckd), 2)]
}
clean_death <- function(df) {
  dt <- data.table::data.table(df)
  dt[, dead := FALSE]
  dt[outcome %>%
    tolower() %>%
    stringr::str_detect("dead"), dead := TRUE]
  dt[outcome %>%
    tolower() %>%
    stringr::str_detect("pts"), dead := TRUE]
  dt[outcome %>%
    tolower() %>%
    stringr::str_detect("euthanasia"), dead := TRUE]
  dt
}
clean_ckd <- function(df) {
  dt <- data.table::data.table(df)

  dt[ckd == "Y", ckd := TRUE]
  dt[ckd == "N", ckd := FALSE]
  dt
}

raw_data <-
  readxl::read_excel("data/raw_data.xlsx") %>%
  janitor::clean_names() %>%
  clean_age() %>%
  clean_death() %>%
  clean_ckd()


# Death vs Kidney Disease vs Proteinuria ----------------------------------

raw_data %>%
  dplyr::select(dead, upcr1, ckd) %>%
  na.omit() %>%
  ggplot(aes(x = dead, y = upcr1, colour = dead)) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.2) +
  facet_wrap(ckd ~ .)

raw_data %>%
  dplyr::select(dead, upcr1, ckd) %>%
  na.omit() %>%
  ggplot(aes(x = upcr1, fill = dead)) +
  geom_histogram(alpha = 1, position = "dodge") +
  facet_wrap(ckd ~ .)



# Proteinuria | Healthy vs Ill --------------------------------------------

# Yes, UPCR1 is higher on Ill patiens
raw_data %>%
  select(healthy, upcr1) %>%
  na.omit() %>%
  filter(upcr1 < 5) %>%
  ggplot(aes(x = healthy, y = upcr1, colour = healthy)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  stat_compare_means(method = "t.test") +
  gameofthrones::scale_colour_got_d(labels = c("Sickly", "Healthy"), option = "margaery", guide = FALSE) +
  theme_pubr() +
  xlab("") +
  ylab("UPCR") +
  scale_x_discrete(labels = c("Sickly", "Healthy")) +
  ggtitle("Difference in proteinuria levels between Healthy and Sickly patients") +
  labs(subtitle = "There is enough statistical evidence to confirm such difference")


# Proteinuria vs Mortality on CKD positives -------------------------------

raw_data %>%
  select(ckd, upcr1, dead) %>%
  filter(ckd == "TRUE") %>%
  filter(upcr1 < 5) %>%
  ggplot(aes(x = dead, y = upcr1, colour = dead)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  stat_compare_means(method = "t.test") +
  gameofthrones::scale_colour_got_d(labels = c("Alive", "Dead"), option = "margaery", guide = FALSE) +
  theme_pubr() +
  xlab("") +
  ylab("UPCR") +
  scale_x_discrete(labels = c("Alive", "Dead")) +
  ggtitle("Relationship between proteinuria levels and survival on CKD positive patients") +
  labs(subtitle = "There is not enough evidence to observe difference in proteinuria levels")


# Weight vs Proteinuria ---------------------------------------------------

healthy_labels <- c(
  "No" = "Sick",
  "Yes" = "Healthy"
)

health_labeller <- function(variable, value) healthy_labels[value]


raw_data %>% 
  select(weight_1, upcr1, healthy) %>% 
  mutate(weight_1 = as.numeric(weight_1)) %>% 
  na.omit() %>%
  filter(upcr1 < 5) %>%
  ggplot(aes(x = weight_1, y = upcr1)) +
  facet_wrap(healthy ~., scales = "free", labeller = health_labeller) +
  geom_point(size = 5, alpha = 0.5, colour = "#1F2430") +
  xlab("Weight (Kg)") +
  ylab("UPCR") +
  ggtitle("Relationship between UPCR levels and Weight") +
  labs(subtitle = "Groups separated based on wether they presented clinical signs")
  

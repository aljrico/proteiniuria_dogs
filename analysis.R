library(tidyverse)
library(data.table)
library(readxl)
library(ggpubr)
library(stringr)

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
create_variables <- function(df){
  df %>% 
    mutate(diagnostic_work_up1 = ifelse(diagnostic_work_up1 == "ND", NA, diagnostic_work_up1)) %>% 
    mutate(ultrasound_abnormalities = (is.na(diagnostic_work_up1)))
}
clean_clinical_signs <- function(df){
  df %>% 
    mutate(clinical_signs1 = tolower(clinical_signs1)) %>% 
    mutate(
      vomiting    = (clinical_signs1 %>% str_detect("vomit") | clinical_signs1 %>% str_detect("chronic v")),
      weight_loss = (clinical_signs1 %>% str_detect('weight loss')),
      lethargy    = (clinical_signs1 %>% str_detect("lethargy")),
      diarrhea    = (clinical_signs1 %>% str_detect("diarrhea")),
      pu_pd       = (clinical_signs1 %>% str_detect("pu-pd") | clinical_signs1 %>% str_detect('polyuria') | clinical_signs1 %>% str_detect("polydipsia") | clinical_signs1 %>% str_detect("drinking more")),
      anorexia    = (clinical_signs1 %>% str_detect("anorexia")),
      hyporexia   = (clinical_signs1 %>% str_detect("hyporexia"))
    )
}
clean_breed <- function(df){
  df %>% 
    mutate(breed = breed %>% tolower()) %>% 
    mutate(breed = breed %>% stringr::str_replace_all(" ", "_")) %>% 
    mutate(breed = ifelse(breed == "dhs", "dsh", breed))
}

raw_data <-
  readxl::read_excel("data/datos_bp.xlsx") %>%
  janitor::clean_names() %>%
  clean_age() %>%
  clean_death() %>%
  clean_ckd() %>% 
  create_variables() %>% 
  clean_clinical_signs() %>% 
  clean_breed()


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

# Test non linear relationshipts
test_nonlinear <- function(df, var1, var2){
  
  variables <- c("healthy", var1, var2)
  
  df <- df[, variables]
  df[[var1]] <- as.numeric(df[[var1]])
  df[[var2]] <- as.numeric(df[[var2]])
  df <- df %>% na.omit()
  df$pvalue <- ""
  
  y = df %>% filter(healthy == "No") %>% .[[var1]]
  x = df %>% filter(healthy == "No") %>% .[[var2]]
  n = length(x)
  r = cor(x,y, method = "spearman") # Rank Correlation
  t = r * sqrt((n - 2) / (1 - r^2))
  nh_pvalue <- dt(t, df =  n - 2) %>% round(3)
  nh_message <- paste0("Spearman Correlation:", round(r, 2), "\nP-Value: ", nh_pvalue)
  
  y = df %>% filter(healthy == "Yes") %>% .[[var1]]%>% as.numeric()
  x = df %>% filter(healthy == "Yes") %>% .[[var2]]%>% as.numeric()
  n = length(x)
  r = cor(x,y, method = "spearman") # Rank Correlation
  t = r * sqrt((n - 2) / (1 - r^2))
  h_pvalue <- dt(t, df = n - 2) %>% round(3)
  h_message <- paste0("Spearman Correlation: ", round(r, 2), "\nP-Value: ", h_pvalue)
  
  df[df$healthy == "No", ]$pvalue <- nh_message
  df[df$healthy == "Yes", ]$pvalue <- h_message
  
  clean_var_name <- . %>% 
    stringr::str_remove_all("_.*") %>% 
    # stringr::str_remove_all("_") %>% 
    stringr::str_remove_all("[1-9]+") %>% 
    stringr::str_to_title()
  
  title_label <- paste0("Relationship between UPCR levels and ", clean_var_name(var2))
  x_label <- clean_var_name(var2)
  
  x_pos <- (min(df[[var2]]) + max(df[[var2]])) / 2
  y_pos <- max(df[[var1]]) * 0.9
  
  df %>% 
    filter(upcr1 < 5) %>%
    ggplot(aes_string(x = var2, y = var1, colour = "healthy")) +
    facet_wrap(healthy ~., labeller = health_labeller) +
    geom_point(size = 5, alpha = 0.5) +
    geom_text(aes_string(label = "pvalue", x = x_pos, y = y_pos), alpha = 4 / nrow(df)) +
    xlab(x_label) +
    ylab("UPCR") +
    ggtitle(title_label) +
    labs(subtitle = "Groups separated based on wether they presented clinical signs") +
    gameofthrones::scale_colour_got_d(option = "margaery", guide = FALSE, direction = -1) +
    theme_bw()
}
# The relationshipt between the two continuous variables has been studied by the non-parametric approach of calculating the Spearman Rank correlation coefficient and tested its significance using t = r * sqrt((n - 2) / (1 - r^2)). which is distributed approximately as Student's t-distribution with n − 2 degrees of freedom under the null hypothesis. [Press; Vettering; Teukolsky; Flannery (1992). Numerical Recipes in C: The Art of Scientific Computing] A justification for this result relies on a permutation argument.[Kendall, M. G.; Stuart, A. (1973). "Sections 31.19, 31.21". The Advanced Theory of Statistics, Volume 2: Inference and Relationship]  

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "weight_1")

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "age_at_diagnosis_ckd")

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "bp1")

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "crea_bq1")

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "alb_1")

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "k1")

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "urea1")

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "usg1")

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "tp1")

raw_data %>% 
  test_nonlinear(var1 = "upcr1", var2 = "phos1")

# y = test_data %>% filter(healthy == "No") %>% .$upcr1
# x = test_data %>% filter(healthy == "No") %>% .$weight_1 %>% as.numeric()
# mgcv::gam(y ~ s(x)) %>% summary() # Generalized Additive Model
# lm(y ~ x) %>% summary() # Linear Model
# energy::dcor.test(x, y, R = 1e3) # Correlation Distances test
# r = cor(x,y, method = "spearman") # Rank Correlation
# 
# reference <- 'Kendall, M. G.; Stuart, A. (1973). "Sections 31.19, 31.21". The Advanced Theory of Statistics, Volume 2: Inference and Relationship'
# t = r * sqrt((n - 2) / (1 - r^2))
# dt(t, df = 1)
# 
# y = test_data %>% filter(healthy == "Yes") %>% .$upcr1
# x = test_data %>% filter(healthy == "Yes") %>% .$weight_1 %>% as.numeric()
# mgcv::gam(y ~ s(x)) %>% summary() # Generalized Additive Model
# lm(y ~ x) %>% summary() # Linear Model
# energy::dcor.test(x, y, R = 1e3) # Correlation Distances test
# r = cor(x,y, method = "spearman") # Rank Correlation
# 
# reference <- 'Kendall, M. G.; Stuart, A. (1973). "Sections 31.19, 31.21". The Advanced Theory of Statistics, Volume 2: Inference and Relationship'
# t = r * sqrt((n - 2) / (1 - r^2))
# dt(t, df = 1)

#



# UPCR vs Ultrasound Abnormalities ----------------------------------------

raw_data %>% 
  select(upcr1, ultrasound_abnormalities) %>% 
  na.omit() %>%
  filter(upcr1 < 5) %>% 
  ggplot(aes(x = ultrasound_abnormalities, y = upcr1, colour = ultrasound_abnormalities)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  stat_compare_means(method = "t.test") +
  gameofthrones::scale_colour_got_d(labels = c("No", "Yes"), option = "margaery", guide = FALSE, name = "Ultrasound Abnormalities") +
  theme_pubr() +
  xlab("Ultrasound Abnormalities") +
  ylab("UPCR") +
  scale_x_discrete(labels = c("No", "Yes")) +
  ggtitle("Relationship between Proteinuria levels and Ultrasound abdnormalities") +
  labs(subtitle = "There is not enough statistical evidence to confirm such difference")


# UPCR vs Clinical Signs --------------------------------------------------

raw_data %>% 
  select(
    vomiting    
    , weight_loss 
    , lethargy    
    , diarrhea    
    , pu_pd       
    , anorexia    
    , hyporexia   
    , upcr1
  ) %>% 
  pivot_longer(cols = -upcr1) %>% 
  na.omit() %>%
  filter(!is.na(value)) %>% 
  filter(upcr1 < 5) %>%
  ggplot(aes(x = value, y = upcr1, colour = value)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  facet_wrap(name ~., scales = "free") +
  stat_compare_means(method = "wilcox.test") +
  gameofthrones::scale_colour_got_d(labels = c("No", "Yes"), option = "margaery", guide = FALSE) +
  theme_pubr() +
  xlab("") +
  ylab("UPCR") +
  scale_x_discrete(labels = c("No", "Yes")) +
  ggtitle("Relationship between proteinuria levels and clinical signs") +
  labs(subtitle = "There is not enough statistical evidence to confirm such difference")


# UPCR vs Blood Pressure --------------------------------------------------

raw_data %>% 
  mutate(bp1 = as.numeric(bp1)) %>% 
  mutate(ht = bp1 >= 160) %>% 
  select(upcr1, ht) %>% 
  na.omit() %>% 
  filter(upcr1 < 5) %>%
  ggplot(aes(x = ht, y = upcr1, colour = ht)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  stat_compare_means(method = "wilcox.test") +
  gameofthrones::scale_colour_got_d(labels = c("No", "Yes"), option = "margaery", guide = FALSE) +
  theme_pubr() +
  xlab("Hypertension") +
  ylab("UPCR") +
  scale_x_discrete(labels = c("No", "Yes")) +
  ggtitle("Relationship between proteinuria levels and Hypertension") +
  labs(subtitle = "There is enough statistical evidence to confirm such difference")



# UPCR vs CKD -------------------------------------------------------------

raw_data %>% 
  select(upcr1, ckd) %>% 
  na.omit() %>% 
  filter(upcr1 < 5) %>%
  ggplot(aes(x = ckd, y = upcr1, colour = ckd)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  stat_compare_means(method = "wilcox.test") +
  gameofthrones::scale_colour_got_d(labels = c("No", "Yes"), option = "margaery", guide = FALSE) +
  theme_pubr() +
  xlab("CKD") +
  ylab("UPCR") +
  scale_x_discrete(labels = c("No", "Yes")) +
  ggtitle("Relationship between proteinuria levels and CKD diagnosis") +
  labs(subtitle = "There is enough statistical evidence to confirm such difference")


# UPCR vs IRIS stages -----------------------------------------------------

iris_data <- 
  raw_data %>% 
  select(upcr1, ckd_stage, ckd) %>% 
  mutate(ckd_stage = ifelse(ckd == FALSE, "Healthy", ckd_stage)) %>% 
  mutate(ckd_stage = ifelse(ckd_stage == "NA", NA, ckd_stage)) %>% 
  ungroup() %>% 
  na.omit() %>% 
  filter(upcr1 < 5)

study_groups <- iris_data %>% filter(ckd_stage != control) %>% .$ckd_stage %>% unique() %>% sort()
control <- "Healthy"
x = iris_data %>% filter(ckd_stage == control) %>% .$upcr1
y = list()
results = list()
for(group in study_groups) y[[group]] = iris_data %>% filter(ckd_stage == group) %>% .$upcr1
for(group in study_groups) results[[group]] = wilcox.test(x, y[[group]]) %>% .$p.value

g <- 
  iris_data %>% 
  ggplot(aes(x = ckd_stage, y = upcr1, colour = ckd_stage)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  gameofthrones::scale_colour_got_d(option = "margaery", guide = FALSE) +
  theme_pubr() +
  xlab("CKD Stages") +
  ylab("UPCR") +
  ggtitle("Relationship between proteinuria levels and CKD diagnosis") +
  labs(subtitle = "Using the Wilcoxon Test, we observe significant differences in groups 3 and 4")

n <- 1
for(group in study_groups){
  n <- n + 1
  g <- 
    g +
    annotate("text", x = n, y = 2.5, label = paste0("P-value: ", round(results[[group]], 4))) +
    annotate("rect", xmin = n + -0.45, xmax = n + 0.45, ymin = 2.25, ymax = 2.75, fill = gameofthrones::got(n = 5, option = "margaery")[[n]], alpha = 0.5, size = 2)
}
g



# UPCR vs Breed  ------------------------------------------------------------
raw_data %>%
  select(healthy, upcr1, breed) %>%
  na.omit() %>%
  filter(upcr1 < 5) %>%
  mutate(healthy = ifelse(healthy == "No", "Sick", "Healthy")) %>% 
  ggplot(aes(x = reorder(breed, (upcr1)), y = upcr1, colour = breed)) +
  facet_wrap(healthy~., scales = 'free_x') +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  gameofthrones::scale_colour_got_d(option = "margaery", guide = FALSE) +
  coord_flip() +
  xlab('') +
  ylab("UPCR") +
  theme_pubr() +
  labs(
    title = "UPCR related to animal's breed"
  )


# UPCR vs Sex -------------------------------------------------------------
raw_data %>%
  select(sex, upcr1) %>%
  na.omit() %>%
  filter(upcr1 < 5) %>%
  ggplot(aes(x = sex, y = upcr1, colour = sex)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  stat_compare_means(method = "wilcox.test") +
  gameofthrones::scale_colour_got_d(labels = c("Female", "Male"), option = "margaery", guide = FALSE, direction = -1) +
  theme_pubr() +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels = c("Female", "Male")) +
  ggtitle("Relationship between proteinuria levels and Sex") +
  labs(subtitle = "There is NOT enough statistical evidence to confirm such difference")

# Other metrics vs Mortality ----------------------------------------------


metric_labels <- c(
  "bp1" = "Blood Pressure",
  "pcv1" = "PCV",
  "upcr1" = "UPC",
  "urea1" = "Urea"
)

metric_labeller <- function(variable, value) metric_labels[value]

raw_data %>% 
  select(bp1, upcr1, urea1, pcv1, dead) %>% 
  mutate(bp1 = as.numeric(bp1), urea1 = as.numeric(urea1), pcv1 = as.numeric(pcv1)) %>% 
  filter(upcr1 < 5) %>%
  pivot_longer(-dead) %>% 
  na.omit() %>% 
  ggplot(aes(x = dead, y = value, colour = dead)) +
  geom_boxplot(size = 1, outlier.shape = NA) +
  geom_jitter(size = 5, width = 0.1, alpha = 0.25) +
  stat_compare_means(method = "wilcox.test") +
  facet_wrap(name~., scales = 'free', labeller = metric_labeller) +
  gameofthrones::scale_colour_got_d(labels = c("No", "Yes"), option = "margaery", guide = FALSE) +
  theme_pubr() +
  xlab("Dead") +
  ylab("") +
  scale_x_discrete(labels = c("No", "Yes")) +
  ggtitle("Relationship between proteinuria levels and CKD diagnosis") +
  labs(subtitle = "There is enough statistical evidence to confirm such difference")

  

# -----------------------------
# 1. Load packages
# -----------------------------
packages <- c("dplyr", "readr", "stringr", "lubridate", "janitor", "purrr", "tibble")
installed <- rownames(installed.packages())

for (p in packages) {
  if (!p %in% installed) install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

# -----------------------------
# 2. File path
# -----------------------------
file_path <- "clients.csv"

# -----------------------------
# 3. Read raw lines manually
#    (important because this dataset has malformed rows)
# -----------------------------
raw_lines <- readLines(file_path, encoding = "UTF-8")

# Parse each line as CSV safely
parse_csv_line <- function(x) {
  read.csv(text = x, header = FALSE, stringsAsFactors = FALSE,
           na.strings = c("", "NA"))
}

parsed_list <- lapply(raw_lines, parse_csv_line)

# Header
header <- as.character(parsed_list[[1]][1, ])
n_cols <- length(header)

# Data rows as character vectors
rows_list <- lapply(parsed_list[-1], function(df) as.character(df[1, ]))

# -----------------------------
# 4. Helper functions
# -----------------------------
is_date_ddmmyyyy <- function(x) {
  !is.na(x) && str_detect(x, "^\\d{2}-\\d{2}-\\d{4}$")
}

is_numeric_like <- function(x) {
  !is.na(x) && str_detect(x, "^-?\\d+(\\.\\d+)?$")
}

# -----------------------------
# 5. Fix malformed rows
#    Pattern A: "2n Cycle" split into 2 columns
#    Pattern B: missing Teenhome -> row shifted left
# -----------------------------
fix_row <- function(x, header) {
  # Ensure character
  x <- as.character(x)
  
  # Pad/truncate if needed
  if (length(x) < length(header)) {
    x <- c(x, rep(NA, length(header) - length(x)))
  }
  if (length(x) > length(header)) {
    x <- x[1:length(header)]
  }
  
  names(x) <- header
  
  # ----- Pattern A -----
  # Education = "2n", Marital_Status = "Cycle"
  # True value should be Education = "2n Cycle"
  if (!is.na(x["Education"]) &&
      !is.na(x["Marital_Status"]) &&
      x["Education"] == "2n" &&
      x["Marital_Status"] == "Cycle") {
    
    original <- unname(x)
    
    corrected <- c(
      original[1:3],                 # unnamed, ID, Year_Birth
      "2n Cycle",                    # Education corrected
      original[6:30],                # shift left from old column 6 onward
      NA                             # Response missing after correction
    )
    
    corrected <- corrected[1:30]
    names(corrected) <- header
    x <- corrected
  }
  
  # ----- Pattern B -----
  # Teenhome contains a date -> means Teenhome is missing
  if (!is.na(x["Teenhome"]) && is_date_ddmmyyyy(x["Teenhome"])) {
    
    original <- unname(x)
    
    corrected <- c(
      original[1:7],   # up to Kidhome
      NA,              # Teenhome missing
      original[8:29]   # shift right from old Teenhome to old Z_Revenue/Response
    )
    
    corrected <- corrected[1:30]
    names(corrected) <- header
    x <- corrected
  }
  
  return(x)
}

fixed_rows <- lapply(rows_list, fix_row, header = header)

# Convert back to data frame
raw_df <- bind_rows(lapply(fixed_rows, function(x) as.data.frame(as.list(x), stringsAsFactors = FALSE)))

# -----------------------------
# 6. Basic column cleaning
# -----------------------------
df <- raw_df %>%
  janitor::clean_names()

# Rename first column clearly
if ("x" %in% names(df)) {
  df <- df %>% rename(row_id = x)
} else if ("unnamed_0" %in% names(df)) {
  df <- df %>% rename(row_id = unnamed_0)
}

# -----------------------------
# 7. Trim spaces
# -----------------------------
df <- df %>%
  mutate(across(where(is.character), ~ str_trim(.)))

# -----------------------------
# 8. Convert variable types
# -----------------------------
numeric_cols <- c(
  "row_id", "id", "year_birth", "income", "kidhome", "teenhome", "recency",
  "mnt_wines", "mnt_fruits", "mnt_meat_products", "mnt_fish_products",
  "mnt_sweet_products", "mnt_gold_prods", "num_deals_purchases",
  "num_web_purchases", "num_catalog_purchases", "num_store_purchases",
  "num_web_visits_month", "accepted_cmp3", "accepted_cmp4", "accepted_cmp5",
  "accepted_cmp1", "accepted_cmp2", "complain", "z_cost_contact",
  "z_revenue", "response"
)

numeric_cols <- intersect(numeric_cols, names(df))

df <- df %>%
  mutate(across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.))))

# Date column
if ("dt_customer" %in% names(df)) {
  df <- df %>%
    mutate(dt_customer = suppressWarnings(dmy(dt_customer)))
}

# -----------------------------
# 9. Standardize categorical values
# -----------------------------
df <- df %>%
  mutate(
    education = case_when(
      education %in% c("Graduation", "graduation") ~ "Graduation",
      education %in% c("PhD", "phd") ~ "PhD",
      education %in% c("Master", "master") ~ "Master",
      education %in% c("Basic", "basic") ~ "Basic",
      education %in% c("2n Cycle", "2n cycle", "2N Cycle") ~ "2n Cycle",
      TRUE ~ education
    ),
    marital_status = case_when(
      marital_status %in% c("Single", "single") ~ "Single",
      marital_status %in% c("Married", "married") ~ "Married",
      marital_status %in% c("Together", "together") ~ "Together",
      marital_status %in% c("Divorced", "divorced") ~ "Divorced",
      marital_status %in% c("Widow", "widow") ~ "Widow",
      marital_status %in% c("Alone", "alone") ~ "Alone",
      marital_status %in% c("Absurd", "absurd") ~ "Absurd",
      marital_status %in% c("YOLO", "yolo") ~ "YOLO",
      TRUE ~ marital_status
    )
  )

# Convert to factor
factor_cols <- intersect(c("education", "marital_status"), names(df))
df <- df %>%
  mutate(across(all_of(factor_cols), as.factor))

# -----------------------------
# 10. Remove duplicated rows
# -----------------------------
df <- df %>%
  distinct()

# -----------------------------
# 11. Handle impossible / suspicious values
# -----------------------------
# Year_Birth: keep plausible range
df <- df %>%
  mutate(
    year_birth = ifelse(year_birth < 1900 | year_birth > year(Sys.Date()), NA, year_birth)
  )

# Binary variables: enforce 0/1 where expected
binary_cols <- intersect(c(
  "accepted_cmp1", "accepted_cmp2", "accepted_cmp3",
  "accepted_cmp4", "accepted_cmp5", "complain", "response"
), names(df))

for (col in binary_cols) {
  df[[col]] <- ifelse(df[[col]] %in% c(0, 1), df[[col]], NA)
}

# Teenhome / Kidhome usually 0,1,2
home_cols <- intersect(c("kidhome", "teenhome"), names(df))
for (col in home_cols) {
  df[[col]] <- ifelse(df[[col]] %in% c(0, 1, 2), df[[col]], NA)
}

# Recency usually 0-99
if ("recency" %in% names(df)) {
  df <- df %>%
    mutate(recency = ifelse(recency < 0 | recency > 100, NA, recency))
}

# Income should be non-negative
if ("income" %in% names(df)) {
  df <- df %>%
    mutate(income = ifelse(income < 0, NA, income))
}

# -----------------------------
# 12. Optional: create useful derived variables
# -----------------------------
df <- df %>%
  mutate(
    age = ifelse(!is.na(year_birth), year(Sys.Date()) - year_birth, NA),
    total_children = rowSums(across(any_of(c("kidhome", "teenhome"))), na.rm = TRUE),
    total_spending = rowSums(across(any_of(c(
      "mnt_wines", "mnt_fruits", "mnt_meat_products",
      "mnt_fish_products", "mnt_sweet_products", "mnt_gold_prods"
    ))), na.rm = TRUE),
    total_purchases = rowSums(across(any_of(c(
      "num_web_purchases", "num_catalog_purchases", "num_store_purchases"
    ))), na.rm = TRUE),
    accepted_total = rowSums(across(any_of(c(
      "accepted_cmp1", "accepted_cmp2", "accepted_cmp3",
      "accepted_cmp4", "accepted_cmp5"
    ))), na.rm = TRUE)
  )

# -----------------------------
# 13. Missing value report
# -----------------------------
na_report <- data.frame(
  variable = names(df),
  missing_n = sapply(df, function(x) sum(is.na(x))),
  missing_pct = round(sapply(df, function(x) mean(is.na(x)) * 100), 2)
) %>%
  arrange(desc(missing_n))

print("===== MISSING VALUE REPORT =====")
print(na_report)

# -----------------------------
# 14. Optional: outlier check for income
#     (do not delete automatically unless needed)
# -----------------------------
if ("income" %in% names(df)) {
  Q1 <- quantile(df$income, 0.25, na.rm = TRUE)
  Q3 <- quantile(df$income, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  df <- df %>%
    mutate(income_outlier = ifelse(!is.na(income) &
                                     (income < lower_bound | income > upper_bound), 1, 0))
}

# -----------------------------
# 15. Final sanity checks
# -----------------------------
print("===== STRUCTURE =====")
str(df)

print("===== SUMMARY =====")
print(summary(df))

print("===== UNIQUE EDUCATION =====")
if ("education" %in% names(df)) print(unique(df$education))

print("===== UNIQUE MARITAL STATUS =====")
if ("marital_status" %in% names(df)) print(unique(df$marital_status))

# -----------------------------
# 16. Save cleaned data
# -----------------------------
write_csv(df, "clients_clean.csv")
write_csv(na_report, "clients_missing_report.csv")

print("Data cleaning completed.")
print("Files created:")
print("- clients_clean.csv")
print("- clients_missing_report.csv")
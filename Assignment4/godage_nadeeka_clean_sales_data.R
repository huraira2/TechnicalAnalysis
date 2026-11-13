# =============================================================================
# godage_nadeeka_clean_sales_data.R
# BDA400 - Data Science Tools and Techniques
# Assignment 4: Ask AI - Clean This Dataset with R and dplyr
# Author : Nadeeka Godage (Student ID: 664430944)
# Date   : November 13, 2026        R 4.6.1, dplyr 1.2.1
#
# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5) for suggesting
# dplyr cleaning steps, explaining code and debugging errors. Prompts used: see
# the AI PROMPT LOG below (P1-P6). I verified outputs using before/after
# summary() and nrow() checks, sum(is.na()) counts, table() of every category,
# duplicate checks, stopifnot() sanity checks, and an independent base-R
# recalculation of total revenue. All final calculations are done by myself.
# I am responsible for the accuracy and originality of this work.
#
# Input : sales_data_dirty.csv  (610 rows: OrderID, Product, Quantity, Price,
#         Customer - synthetic course dataset)
# Output: sales_data_clean.csv
# Run   : source("<this file>", echo = TRUE)  from the Assignment4 folder
# =============================================================================
#
# AI PROMPT LOG
# -----------------------------------------------------------------------------
# P1 (seed)       "I have a sales dataset in R with missing customer IDs,
#                  inconsistent region names, and outliers in revenue. What are
#                  common data cleaning steps I should perform using dplyr?"
# P2 (refinement) "Here is a sample of my data (first 10 rows). Suggest R code
#                  using dplyr to fix these issues."  [pasted head(sales, 10)]
# P3 (validation) "Please explain what each line of your suggested R code does,
#                  and how I can confirm it worked correctly."
# P4 (debug)      "This AI-generated code gives an error: 'object not found'.
#                  How can I debug or rewrite it in R?"  [pasted the error:
#                  object 'CustomerID' not found]
# P5 (refinement) "Simplify this mutate() chain for readability."
# P6 (validation) "Does this cleaning pipeline follow tidyverse best practices?
#                  Suggest improvements."
#
# WHAT THE AI SUGGESTED AND WHAT I CHANGED
# -----------------------------------------------------------------------------
# v1 (answer to P1) used columns CustomerID, Region and Revenue. My file has
#    none of these, so it failed:  Error: object 'CustomerID' not found.
#    -> The prompt described the brief, not my data. Fixed by sending the real
#       column names (P2/P4).
# v2 (answer to P2) ran, but testing showed three logic problems:
#    a) filter(!is.na(Product)) kept 35 blank products, because read.csv()
#       reads empty cells in text columns as "" not NA.
#       -> I added na.strings = c("", "NA") to read.csv().
#    b) It replaced missing Quantity with the median (15). That invents 94
#       sales and inflates revenue.
#       -> I remove rows with unknown Quantity instead (revenue cannot be known).
#    c) It cut outliers with Quantity < quantile(Quantity, 0.99). That only
#       worked by luck (the 99th percentile happens to be exactly 1000).
#       -> I use an explicit business rule (see Step 5) and check it.
#    d) distinct() alone left OrderID 265 twice (Quantity 1000 and 25).
#       -> I also de-duplicate on OrderID after removing invalid rows.
# v3 (this file) = v2 + my fixes + P5/P6 suggestions (one mutate(), across()
#    not needed, named constants, stopifnot() checks).
# =============================================================================

library(dplyr)

# ---- Settings ----------------------------------------------------------------
input_file  <- "sales_data_dirty.csv"
output_file <- "sales_data_clean.csv"
max_valid_quantity <- 100     # largest believable order size (see Step 5)


# =============================================================================
# STEP 1 - LOAD AND EXPLORE THE RAW DATA
# =============================================================================
# na.strings turns empty cells ("") into NA so they can be detected
sales_raw <- read.csv(input_file, na.strings = c("", "NA"))

glimpse(sales_raw)                                  # column types and examples
summary(sales_raw)                                  # ranges, NA counts
nrow(sales_raw)                                     # 610 rows

colSums(is.na(sales_raw))                           # missing values per column
table(sales_raw$Product, useNA = "ifany")           # product labels
table(sales_raw$Customer, useNA = "ifany")          # customer labels
table(sales_raw$Quantity, useNA = "ifany")          # quantity values

sum(duplicated(sales_raw))                          # exact duplicate rows
sum(duplicated(sales_raw$OrderID))                  # repeated OrderIDs
sales_raw %>%                                       # the OrderID that repeats
  group_by(OrderID) %>%                             # with different values
  filter(n() > 1, n_distinct(Quantity) > 1) %>%
  ungroup()

# Issues found (details in the data-quality document):
#  1. Product missing in 49 rows
#  2. Quantity missing in 95 rows
#  3. Invalid quantities: -5 (37 rows) and 0 (44 rows)
#  4. Extreme quantity 1000 (20 rows) - 10x the next largest value
#  5. 9 exact duplicate rows + OrderID 265 entered twice (1000 vs 25)
#  6. Customer names all lower case ("alice") - not report-ready


# =============================================================================
# STEP 2-6 - CLEANING PIPELINE (final version, v3)
# =============================================================================
sales_clean <- sales_raw %>%
  # Step 2: remove exact duplicate rows (610 -> 601)
  distinct() %>%
  # Step 3: keep only rows with a known, valid quantity
  #   - NA: revenue cannot be calculated, and guessing would invent sales
  #   - <= 0: -5 and 0 are not real sales (returns/cancellations or errors)
  #   - > 100: 1000 is a data-entry error (OrderID 265 shows 1000 vs 25)
  filter(!is.na(Quantity),
         Quantity > 0,
         Quantity <= max_valid_quantity) %>%
  # Step 4: one row per OrderID (after the 1000 row for 265 is gone)
  distinct(OrderID, .keep_all = TRUE) %>%
  # Step 5: standardise text and add revenue in one mutate()
  mutate(
    Product  = coalesce(trimws(Product), "Unknown"),     # blank -> "Unknown"
    Customer = tools::toTitleCase(trimws(Customer)),     # "alice" -> "Alice"
    Revenue  = round(Quantity * Price, 2)                # sales value
  ) %>%
  # Step 6: sort by order number for reporting
  arrange(OrderID)


# =============================================================================
# STEP 7 - VERIFY: BEFORE vs AFTER
# =============================================================================
cat("\n===== BEFORE =====\n")
cat("Rows:", nrow(sales_raw), "\n")
cat("Missing values:", sum(is.na(sales_raw)), "\n")
print(summary(sales_raw))

cat("\n===== AFTER =====\n")
cat("Rows:", nrow(sales_clean), "\n")
cat("Missing values:", sum(is.na(sales_clean)), "\n")
print(summary(sales_clean))
table(sales_clean$Product)
table(sales_clean$Customer)
table(sales_clean$Quantity)

# Where did the rows go? (row accounting)
step_counts <- c(
  raw                    = nrow(sales_raw),
  after_exact_duplicates = nrow(distinct(sales_raw)),
  after_quantity_rules   = nrow(filter(distinct(sales_raw), !is.na(Quantity),
                                       Quantity > 0, Quantity <= max_valid_quantity)),
  final                  = nrow(sales_clean)
)
step_counts

# Sanity checks - the script stops if any of these is FALSE
stopifnot(
  sum(is.na(sales_clean)) == 0,                          # no missing values
  !any(duplicated(sales_clean$OrderID)),                 # one row per order
  all(sales_clean$Quantity > 0),                         # no zero/negative
  all(sales_clean$Quantity <= max_valid_quantity),       # no extreme outliers
  all(sales_clean$Product %in% c("Gadget", "Thingamajig", "Widget", "Unknown")),
  all(sales_clean$Customer == tools::toTitleCase(sales_clean$Customer)),
  all(sales_clean$Price > 0)
)

# Independent cross-check of revenue with base R (no dplyr)
base_check <- sales_raw[!duplicated(sales_raw), ]
base_check <- base_check[!is.na(base_check$Quantity) & base_check$Quantity > 0 &
                         base_check$Quantity <= max_valid_quantity, ]
base_check <- base_check[!duplicated(base_check$OrderID), ]
stopifnot(nrow(base_check) == nrow(sales_clean),
          isTRUE(all.equal(sum(round(base_check$Quantity * base_check$Price, 2)),
                           sum(sales_clean$Revenue))))
cat("Total revenue (dplyr):  ", format(sum(sales_clean$Revenue), big.mark = ","), "\n")
cat("Total revenue (base R): ", format(sum(round(base_check$Quantity * base_check$Price, 2)),
                                       big.mark = ","), "\n")

# Revenue by product - the report this data is being prepared for
sales_clean %>%
  group_by(Product) %>%
  summarise(orders = n(), units = sum(Quantity), revenue = sum(Revenue)) %>%
  arrange(desc(revenue))

# ---- Save --------------------------------------------------------------------
write.csv(sales_clean, output_file, row.names = FALSE)
cat("Saved", nrow(sales_clean), "clean rows to", output_file, "\n")
cat("All checks passed.\n")

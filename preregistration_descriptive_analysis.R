###############################################################################
#  Qualitative Preregistration Analysis – Quantitative Components
#  ----------------------------------------------------------------
#  This script analyzes the coded preregistration-publication comparison data.
#  
#  Main analyses:
#  (1) Frequency tables for all *_changed columns (Table 1 in paper)
#  (2) Build axial-to-general-category mapping tables for each component
#  (3) Identify studies with zero deviations from preregistration
#  (4) Top-10 most frequent general category codes across all components
#  (5) Open-access and publication-type frequencies
#  (6) Optional: Discipline distribution plot & PRISMA flow diagram
###############################################################################

## ── Load required packages ──────────────────────────────────────────────────
# Note: Install packages first if needed using install.packages("package_name")
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(writexl)
})

## ── Read the dataset ─────────────────────────────────────────────────────────
# Update this path to match your local file location
file_path <- "Coding_Dataset_Preregistration-Publication_Comparison__N_214_.xlsx"
df <- read_excel(file_path)

###############################################################################
# PART A: Frequencies for *_changed Columns (Table 1)
###############################################################################

# Identify all columns ending with "_changed"
changed_cols <- names(df)[str_detect(names(df), "_changed$")]

# Helper function to remove the "_changed" suffix from column names
strip_suffix <- function(x) str_remove(x, "_changed$")

# Function to count yes/no/NI values for a single *_changed column
count_changed <- function(col) {
  df %>% 
    mutate(code = str_to_lower(str_trim(.data[[col]]))) %>% 
    filter(code %in% c("yes", "no", "ni")) %>% 
    count(code) %>% 
    pivot_wider(names_from  = code,
                values_from = n,
                values_fill = 0) %>% 
    
    # Add any absent columns before calculating percentages
    { if (!"yes" %in% names(.)) .$yes <- 0L ; . } %>% 
    { if (!"no"  %in% names(.)) .$no  <- 0L ; . } %>% 
    { if (!"ni"  %in% names(.)) .$ni  <- 0L ; . } %>% 
    
    # Calculate summary statistics
    mutate(
      element     = strip_suffix(col),
      total_rows  = yes + no + ni,
      pct_yes     = round(yes / total_rows * 100, 1),
      pct_no      = round(no  / total_rows * 100, 1),
      pct_ni      = round(ni  / total_rows * 100, 1),
      # Percentage of changed among yes+no (excluding NI)
      pct_yes_no  = if_else(
        (yes + no) > 0,
        round(yes / (yes + no) * 100, 1),
        NA_real_)
    )
}

# Apply the function to all *_changed columns and combine results
changed_summary <- map_dfr(changed_cols, count_changed) %>% 
  arrange(desc(pct_yes))

print(changed_summary)

# Optional: Export to Excel for further use
# write_xlsx(changed_summary, "changed_columns_summary.xlsx")

###############################################################################
# PART B: Axial and General Category Code Tables
###############################################################################

# Identify all *_how_axial columns to determine which components have coding
axial_cols <- names(df)[str_detect(names(df), "_how_axial$")]
general_cols_exist <- names(df)[str_detect(names(df), "_how_general$")]

# Only keep elements that have BOTH axial and general category columns
elements_with_axial <- str_remove(axial_cols, "_how_axial$")
elements_with_general <- str_remove(general_cols_exist, "_how_general$")
elements <- intersect(elements_with_axial, elements_with_general)

# Function to create frequency tables for both axial and general category codes
make_tables <- function(prefix) {
  axial   <- paste0(prefix, "_how_axial")
  general <- paste0(prefix, "_how_general")

  # Filter to rows with non-empty codes in both columns
  tmp <- df %>%
    mutate(across(all_of(c(axial, general)), str_trim)) %>%
    filter(.data[[axial]]   != "" & !is.na(.data[[axial]]),
           .data[[general]] != "" & !is.na(.data[[general]]))

  # Return three tables: axial counts, general category counts, and their mapping
  list(
    axial_counts   = tmp %>% count(.data[[axial]],   name = "n") %>% arrange(-n),
    general_counts = tmp %>% count(.data[[general]], name = "n") %>% arrange(-n),
    map_table      = tmp %>% count(.data[[general]], .data[[axial]], name = "n")
  )
}

# Generate all code tables for each component that has both columns
code_tables <- set_names(elements) %>% map(make_tables)

# Example access patterns:
# code_tables$rq$map_table      # Research questions axial-to-general mapping
# code_tables$sam$axial_counts  # Sampling strategy axial codes

# Optional: Export all tables to a multi-sheet Excel workbook
# out_sheets <- imap(code_tables, \(lst, nm) {
#   list(
#     paste0(nm, "_axial")   = lst$axial_counts,
#     paste0(nm, "_general") = lst$general_counts,
#     paste0(nm, "_map")     = lst$map_table
#   )
# }) |> flatten()
# write_xlsx(out_sheets, "all_code_tables.xlsx")

###############################################################################
# PART C: Studies with Zero Deviations
###############################################################################

# Identify rows where all *_changed columns are coded as "no"
df_no_dev <- df %>%
  mutate(across(all_of(changed_cols), str_to_lower)) %>%
  filter(if_all(all_of(changed_cols), \(x) x == "no"))

cat("Studies with NO deviations from preregistration:", nrow(df_no_dev), "\n")

# Optional: Export these studies for closer inspection
# write_xlsx(df_no_dev, "studies_no_deviation.xlsx")

###############################################################################
# PART D: Top-10 Most Frequent General Category Codes
###############################################################################

# Identify all *_how_general columns
general_cols <- names(df)[str_detect(names(df), "_how_general$")]

# Count occurrences of each general category code across all components
top10_general <- df %>%
  select(all_of(general_cols)) %>%
  pivot_longer(everything(),
               names_to  = "source_col",
               values_to = "code",
               values_drop_na = TRUE) %>%
  filter(code != "") %>%
  count(code, source_col, name = "n") %>%
  group_by(code) %>%
  summarise(
    total   = sum(n),
    columns = paste(sort(unique(source_col)), collapse = ", ")
  ) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10)

print(top10_general)

###############################################################################
# PART E: Open Access and Publication Type Frequencies
###############################################################################

# Open access status
oa_counts <- df %>%
  mutate(oa = str_trim(str_to_lower(oa))) %>%
  filter(oa %in% c("yes", "no")) %>%
  count(oa, name = "n") %>%
  mutate(pct = round(n / sum(n) * 100, 1))
print(oa_counts)

# Publication type distribution
pubtype_counts <- df %>%
  mutate(pub_type = str_trim(pub_type)) %>%
  filter(pub_type != "" & !is.na(pub_type)) %>%
  count(pub_type, name = "n") %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))
print(pubtype_counts)

###############################################################################
# PART F: General Category Code Frequencies by Component
###############################################################################

# Detailed breakdown of general category codes for each component
count_general <- function(col) {
  element <- str_remove(col, "_how_general$")
  
  df %>% 
    mutate(code = str_trim(.data[[col]])) %>% 
    filter(!is.na(code) & code != "") %>%
    count(code, name = "n") %>%
    mutate(element = element) %>%
    select(element, general_code = code, n)
}

# Build comprehensive table of all general category codes
general_summary <- map_dfr(general_cols, count_general) %>% 
  arrange(element, desc(n))

print(general_summary, n = Inf, width = Inf)

# Optional: Export for further analysis
# write_xlsx(general_summary, "general_code_summary.xlsx")

###############################################################################
# PART G (Optional): Visualizations
###############################################################################

# ── PRISMA Flow Diagram ──────────────────────────────────────────────────────
# Uncomment the section below to generate the PRISMA flowchart

# library(DiagrammeR)
# 
# prisma_dot <- "
# digraph prisma {
#   rankdir = TB
#   nodesep = 0.45
#   ranksep = 0.6
#   node  [shape = box style = rounded fontname = Helvetica fontsize = 10]
#   edge  [arrowhead = normal arrowsize = 0.8]
# 
#   n0 [label = 'Records via OSF preregistrations\\n(Qual template, ≤ 09 Jan 2025): 1806'];
#   n1 [label = 'Removed before screening:\\nRegistered 2024-25 (n = 587)'];
#   n2 [label = 'Records screened (n = 1219)'];
#   n3 [label = 'Excluded: Not qualitative (n = 493)'];
#   n4 [label = 'Reports sought (n = 726)'];
#   n5 [label = 'Reports not retrieved:\\nNo public output (n = 483)'];
#   n6 [label = 'Reports assessed (n = 243)'];
#   n8 [label = 'Excluded after eligibility:\\nNo match / inaccessible (n = 29)'];
#   n7 [label = 'Studies in final analysis (n = 214)',
#       style = 'rounded,filled', fillcolor = '#d5e8ff'];
# 
#   n0 -> n1
#   n0 -> n2
#   n2 -> n3
#   n2 -> n4
#   n4 -> n5
#   n4 -> n6
#   n6 -> n8
#   n6 -> n7
# }
# "
# 
# grViz(prisma_dot)
# 
# # Optional: Save as image
# # prisma_graph <- grViz(prisma_dot)
# # DiagrammeRsvg::export_svg(prisma_graph) %>%
# #   charToRaw() %>%
# #   rsvg::rsvg_png("prisma_flowchart.png")

# ── Discipline Distribution Plot ─────────────────────────────────────────────
# Uncomment to create a bar plot of disciplines
# Note: Requires the 'ggplot2' package and a column with discipline information

# library(ggplot2)
# library(forcats)
# 
# disc_plot <- df %>%
#   count(Domain, name = "n") %>%                    # Adjust column name as needed
#   mutate(Domain = fct_reorder(Domain, n, .desc = TRUE)) %>%
#   ggplot(aes(x = n, y = Domain)) +
#   geom_col(fill = "grey50") +
#   geom_text(aes(label = n), hjust = -0.15) +
#   labs(title = "Preregistrations by Discipline",
#        x = "Count", y = NULL) +
#   theme_minimal()
# 
# print(disc_plot)
# ggsave("discipline_barplot.png", disc_plot, width = 8, height = 5)

###############################################################################
cat("✓ Analysis complete\n")
cat("✓ All results printed to console\n")
cat("✓ Uncomment export lines to save tables and figures\n")
###############################################################################

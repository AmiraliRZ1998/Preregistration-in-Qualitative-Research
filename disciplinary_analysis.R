# ============================================================================
# Simplified Disciplinary Analysis Visualizations - BLACK AND WHITE VERSION
# Works with base R and minimal dependencies - APA Compliant
# ============================================================================
setwd("C:/Users/amira/OneDrive/Desktop/FYP OSF Material")

# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data (UPDATE THIS PATH to your file location)
data <- read_excel("QualPlusReview_RECODED_WITH_JUSTIFICATIONS.xlsx")

# Filter for three main disciplines
disciplines <- c("Health Sciences", "Economic & Social Sciences", "Arts & Humanities")
data_filtered <- data %>% 
  filter(Domain %in% disciplines)

# Define component mapping
components <- c(
  ra_changed = "Research Aims",
  rq_changed = "Research Questions",
  sam_changed = "Sampling Strategy",
  sd_changed = "Study Design",
  ds_changed = "Data Sources",
  dcm_changed = "Data Collection Methods",
  dct_changed = "Data Collection Timeline",
  daa_changed = "Data Analysis Approach",
  dap_changed = "Data Analysis Process",
  cs_changed = "Credibility Strategies",
  rcs_changed = "Credibility Rationale",
  rop_changed = "Reflexivity/Positionality"
)

# APA theme
theme_apa <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11),
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 11),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      panel.border = element_blank()
    )
}

# Calculate change rates
results_list <- list()
for (discipline in disciplines) {
  disc_data <- data_filtered %>% filter(Domain == discipline)
  n_total <- nrow(disc_data)
  
  for (col_name in names(components)) {
    if (col_name %in% names(disc_data)) {
      changed_count <- sum(tolower(as.character(disc_data[[col_name]])) == "yes", na.rm = TRUE)
      rate <- (changed_count / n_total) * 100
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Discipline = discipline,
        Component = components[col_name],
        Changed = changed_count,
        Total = n_total,
        Rate = rate,
        stringsAsFactors = FALSE
      )
    }
  }
}

results_df <- bind_rows(results_list)

# Grayscale colors for black and white printing (APA compliant)
disc_colors <- c(
  "Health Sciences" = "#000000",              # Black
  "Economic & Social Sciences" = "#525252",   # Dark gray
  "Arts & Humanities" = "#969696"             # Light gray
)

# ============================================================================
# FIGURE 1: Top 5 Most Changed - Separate plots
# ============================================================================

for (disc in disciplines) {
  disc_data <- results_df %>%
    filter(Discipline == disc) %>%
    arrange(desc(Rate)) %>%
    slice_head(n = 5) %>%
    mutate(Component = factor(Component, levels = Component))
  
  n_total <- disc_data$Total[1]
  color <- disc_colors[disc]
  
  p <- ggplot(disc_data, aes(x = Rate, y = Component)) +
    geom_col(fill = color, alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%% (n=%d)", Rate, Changed)), 
              hjust = -0.1, size = 3.5) +
    scale_x_continuous(limits = c(0, max(disc_data$Rate) * 1.25), expand = c(0, 0)) +
    labs(
      title = paste0("Most Frequently Changed Components\n", disc, " (N = ", n_total, ")"),
      x = "Change Rate (%)",
      y = NULL
    ) +
    theme_apa()
  
  filename <- paste0("fig1_", gsub(" |&", "_", tolower(disc)), "_most_changed.png")
  ggsave(filename, p, width = 10, height = 6, dpi = 300, bg = "white")
}

# ============================================================================
# FIGURE 2: Top 5 Most Stable - Separate plots
# ============================================================================

for (disc in disciplines) {
  disc_data <- results_df %>%
    filter(Discipline == disc) %>%
    arrange(Rate) %>%
    slice_head(n = 5) %>%
    mutate(
      Stability_Rate = 100 - Rate,
      Unchanged = Total - Changed,
      Component = factor(Component, levels = Component)
    )
  
  n_total <- disc_data$Total[1]
  color <- disc_colors[disc]
  
  p <- ggplot(disc_data, aes(x = Stability_Rate, y = Component)) +
    geom_col(fill = color, alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%% (n=%d)", Stability_Rate, Unchanged)), 
              hjust = -0.1, size = 3.5) +
    scale_x_continuous(limits = c(0, max(disc_data$Stability_Rate) * 1.25), expand = c(0, 0)) +
    labs(
      title = paste0("Most Stable Components\n", disc, " (N = ", n_total, ")"),
      x = "Stability Rate (%)",
      y = NULL
    ) +
    theme_apa()
  
  filename <- paste0("fig2_", gsub(" |&", "_", tolower(disc)), "_most_stable.png")
  ggsave(filename, p, width = 10, height = 6, dpi = 300, bg = "white")
}

# ============================================================================
# FIGURE 3: Heatmap (Black and White)
# ============================================================================

heatmap_data <- results_df %>%
  pivot_wider(names_from = Discipline, values_from = Rate, id_cols = Component) %>%
  mutate(Average = rowMeans(select(., -Component), na.rm = TRUE)) %>%
  arrange(desc(Average)) %>%
  select(-Average) %>%
  pivot_longer(-Component, names_to = "Discipline", values_to = "Rate") %>%
  mutate(
    Discipline = factor(Discipline, levels = disciplines),
    Component = factor(Component, levels = unique(Component))
  )

fig3 <- ggplot(heatmap_data, aes(x = Discipline, y = Component, fill = Rate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", Rate)), size = 3) +
  scale_fill_gradient(low = "white", high = "black", 
                      limits = c(0, 100), 
                      name = "Change\nRate (%)") +
  labs(
    title = "Change Rates Across Research Components and Disciplines",
    x = NULL,
    y = NULL
  ) +
  theme_apa() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave("fig3_heatmap.png", fig3, width = 10, height = 8, dpi = 300, bg = "white")

# ============================================================================
# FIGURE 4: Key Components Comparison (Black and White)
# ============================================================================

key_components <- c("Research Questions", "Sampling Strategy", "Data Sources",
                    "Data Collection Methods", "Data Collection Timeline")

key_comparison <- results_df %>%
  filter(Component %in% key_components) %>%
  mutate(
    Discipline = factor(Discipline, levels = disciplines),
    Component = factor(Component, levels = key_components)
  )

fig4 <- ggplot(key_comparison, aes(x = Component, y = Rate, fill = Discipline)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.75) +
  geom_text(aes(label = sprintf("%.1f%%", Rate)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = disc_colors) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Disciplinary Comparison of Key Component Changes",
    x = "Research Component",
    y = "Change Rate (%)",
    fill = "Discipline"
  ) +
  theme_apa() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave("fig4_key_comparison.png", fig4, width = 12, height = 7, dpi = 300, bg = "white")

# ============================================================================
# FIGURE 5: Sampling Strategy Gradient (Black and White)
# ============================================================================

sampling_data <- results_df %>%
  filter(Component == "Sampling Strategy") %>%
  arrange(Rate) %>%
  mutate(
    Discipline = factor(Discipline, levels = Discipline),
    Label = sprintf("%.1f%% (n=%d/%d)", Rate, Changed, Total)
  )

fig5 <- ggplot(sampling_data, aes(x = Rate, y = Discipline, fill = Discipline)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = Label), hjust = -0.1, size = 4, fontface = "bold") +
  scale_fill_manual(values = disc_colors) +
  scale_x_continuous(limits = c(0, 80), expand = c(0, 0)) +
  labs(
    title = "Sampling Strategy Change Rates Across Disciplines",
    x = "Change Rate (%)",
    y = "Discipline"
  ) +
  theme_apa() +
  theme(legend.position = "none")

ggsave("fig5_sampling_gradient.png", fig5, width = 10, height = 6, dpi = 300, bg = "white")

# ============================================================================
# Summary Statistics
# ============================================================================

cat("\n=== SUMMARY STATISTICS ===\n\n")
cat("Sample sizes:\n")
print(table(data_filtered$Domain))

cat("\n\nTop 3 most changed per discipline:\n")
top_changed <- results_df %>%
  group_by(Discipline) %>%
  arrange(desc(Rate)) %>%
  slice_head(n = 3)
print(as.data.frame(top_changed))

cat("\n\nTop 3 most stable per discipline:\n")
top_stable <- results_df %>%
  group_by(Discipline) %>%
  arrange(Rate) %>%
  slice_head(n = 3)
print(as.data.frame(top_stable))

cat("\n\n=== ALL FIGURES SAVED (BLACK AND WHITE) ===\n")
cat("Figure 1: Individual plots for each discipline (most changed)\n")
cat("Figure 2: Individual plots for each discipline (most stable)\n")
cat("Figure 3: Heatmap of all change rates (white to black gradient)\n")
cat("Figure 4: Key components comparison (grouped bar chart, grayscale)\n")
cat("Figure 5: Sampling strategy disciplinary gradient (grayscale)\n")
cat("\nAll figures use grayscale colors suitable for black and white printing.\n")

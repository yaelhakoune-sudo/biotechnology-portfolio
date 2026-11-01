getwd()
setwd("/Users/yaelhakoune/Downloads")

# ================================
# PDB Source Organism Analysis
# ================================

# Install packages if needed
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggplot2")

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# 1. Read the Excel file
file_name <- "pdb_entries_min-2.xlsx"

pdb_data <- read_excel(file_name)

# 2. Report file information
cat("File read:", file_name, "\n")
cat("Number of rows:", nrow(pdb_data), "\n")
cat("Number of columns:", ncol(pdb_data), "\n")
cat("Column names:\n")
print(colnames(pdb_data))

# 3. Check missing SOURCE values
missing_source <- sum(is.na(pdb_data$SOURCE) | pdb_data$SOURCE == "")

cat("Rows with missing SOURCE:", missing_source, "\n")
cat("Rows with non-empty SOURCE:", nrow(pdb_data) - missing_source, "\n")

# 4. Clean and count source organisms
# Some rows contain more than one source separated by ;
organism_counts <- pdb_data %>%
  filter(!is.na(SOURCE), SOURCE != "") %>%
  separate_rows(SOURCE, sep = ";") %>%
  mutate(
    SOURCE = str_trim(SOURCE),
    SOURCE = str_squish(SOURCE),
    SOURCE = str_to_sentence(SOURCE)
  ) %>%
  filter(SOURCE != "") %>%
  filter(SOURCE != "Synthetic construct") %>%   # not a living organism
  count(SOURCE, name = "structure_count") %>%
  arrange(desc(structure_count)) %>%
  mutate(
    percent_of_all_structures = structure_count / nrow(pdb_data) * 100
  )

# 5. Show top organisms
top10 <- organism_counts %>%
  slice_head(n = 10)

print(top10)

# 6. Calculate concentration
top1_share <- sum(organism_counts$structure_count[1:1]) / nrow(pdb_data) * 100
top3_share <- sum(organism_counts$structure_count[1:3]) / nrow(pdb_data) * 100
top5_share <- sum(organism_counts$structure_count[1:5]) / nrow(pdb_data) * 100
top10_share <- sum(organism_counts$structure_count[1:10]) / nrow(pdb_data) * 100
top20_share <- sum(organism_counts$structure_count[1:20]) / nrow(pdb_data) * 100

concentration_table <- data.frame(
  group = c("Top 1", "Top 3", "Top 5", "Top 10", "Top 20"),
  share_percent = c(top1_share, top3_share, top5_share, top10_share, top20_share)
)
print(concentration_table)


# 7. Save the organism count table
write.csv(
  organism_counts,
  "pdb_source_organism_counts_R.csv",
  row.names = FALSE
)

# 8. Make a bar chart
bar_chart <- ggplot(top10, aes(x = reorder(SOURCE, structure_count), y = structure_count)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Source Organisms in the PDB Dataset",
    x = "Source organism",
    y = "Number of structures"
  ) +
  theme_minimal()

print(bar_chart)

# 9. Save the bar chart
ggsave(
  filename = "pdb_top_source_organisms_bar_chart_R.png",
  plot = bar_chart,
  width = 10,
  height = 6,
  dpi = 300
)

# ================================
# Second Research Question:
# Has the dominance of the top source organisms changed over time?
# ================================

# Install packages only if you do not have them yet
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("lubridate")

# Load packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

# ----------------
# 1. Read the file
# ----------------

file_name <- "pdb_entries_min-2.xlsx"

pdb_data <- read_excel(file_name)

cat("File read:", file_name, "\n")
cat("Number of rows:", nrow(pdb_data), "\n")
cat("Number of columns:", ncol(pdb_data), "\n")
cat("Column names:\n")
print(colnames(pdb_data))

# ----------------
# 2. Extract year from DATE column
# ----------------

# This works if DATE is read as a real date
# If DATE is not a real date, it tries to extract a 4-digit year from the text

# ----------------
# 2. Extract year from DATE column - safer version
# ----------------

# Add row ID first
pdb_data <- pdb_data %>%
  mutate(row_id = row_number())

# Check how R reads the DATE column
cat("Class of DATE column:\n")
print(class(pdb_data$DATE))

cat("First DATE values:\n")
print(head(pdb_data$DATE, 10))

# Convert DATE to year safely
if (inherits(pdb_data$DATE, "Date")) {
  
  pdb_data$year <- as.integer(format(pdb_data$DATE, "%Y"))
  
} else if (inherits(pdb_data$DATE, "POSIXct") | inherits(pdb_data$DATE, "POSIXt")) {
  
  pdb_data$year <- as.integer(format(pdb_data$DATE, "%Y"))
  
} else if (is.numeric(pdb_data$DATE)) {
  
  # Excel sometimes stores dates as numbers
  pdb_data$year <- as.integer(format(as.Date(pdb_data$DATE, origin = "1899-12-30"), "%Y"))
  
} else {
  
  # If DATE is text, extract the 4-digit year
  pdb_data$DATE_text <- as.character(pdb_data$DATE)
  pdb_data$year <- as.integer(stringr::str_extract(pdb_data$DATE_text, "\\d{4}"))
  
}

# Check result
cat("Year range:\n")
print(range(pdb_data$year, na.rm = TRUE))

cat("First extracted years:\n")
print(head(pdb_data[, c("DATE", "year")], 10))

# Check if years were extracted correctly
cat("Year range:\n")
print(range(pdb_data$year, na.rm = TRUE))

# ----------------
# 3. Clean SOURCE column
# ----------------

source_long <- pdb_data %>%
  filter(!is.na(SOURCE), SOURCE != "", !is.na(year)) %>%
  separate_rows(SOURCE, sep = ";") %>%
  mutate(
    SOURCE_clean = str_trim(SOURCE),
    SOURCE_clean = str_squish(SOURCE_clean),
    SOURCE_clean = str_to_sentence(SOURCE_clean)
  ) %>%
  filter(SOURCE_clean != "") %>%
  filter(SOURCE_clean != "Synthetic construct")

# ----------------
# 4. Find the top 10 source organisms
# ----------------

top_n <- 10

top_organisms <- source_long %>%
  count(SOURCE_clean, name = "structure_count") %>%
  arrange(desc(structure_count)) %>%
  slice_head(n = top_n)

print(top_organisms)

top_organism_names <- top_organisms$SOURCE_clean

# ----------------
# 5. For each PDB structure, check if it contains one of the top organisms
# ----------------

structure_top_status <- source_long %>%
  mutate(is_top_organism = SOURCE_clean %in% top_organism_names) %>%
  group_by(row_id, year) %>%
  summarise(
    has_top_organism = any(is_top_organism),
    .groups = "drop"
  )

# ----------------
# 6. Calculate yearly percentage
# ----------------

yearly_share <- structure_top_status %>%
  group_by(year) %>%
  summarise(
    total_structures_with_source = n(),
    structures_from_top_organisms = sum(has_top_organism),
    percent_from_top_organisms =
      structures_from_top_organisms / total_structures_with_source * 100,
    .groups = "drop"
  ) %>%
  arrange(year)

print(yearly_share)

# Save the yearly table
write.csv(
  yearly_share,
  "pdb_top_organisms_over_time_R.csv",
  row.names = FALSE
)

# ----------------
# 7. Make the line graph
# ----------------

line_graph <- ggplot(
  yearly_share,
  aes(x = year, y = percent_from_top_organisms)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Share of PDB Structures from Top 10 Source Organisms Over Time",
    x = "Year",
    y = "Structures from top 10 organisms (%)"
  ) +
  theme_minimal()

print(line_graph)

# Save the graph
ggsave(
  filename = "pdb_top_organisms_over_time_line_graph_R.png",
  plot = line_graph,
  width = 10,
  height = 6,
  dpi = 300
)
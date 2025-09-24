# Name: NAJJINDA GLORIA
# Access Number: B30304
# Registration Number: S24B38/013
# Assignment 1: Descriptive Analytics on IDA Credits to Uganda
# Date: September 23, 2025

# ----------------------------
# Load and Install Packages
# ----------------------------
if (!require(readr)) install.packages("readr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(ggplot2)) install.packages("ggplot2")

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# ----------------------------
# Load Dataset
# ----------------------------
df <- read_csv(file.choose(), col_types = cols(.default = "c"))

# ----------------------------
# Preprocessing
# ----------------------------
# Clean column names
names(df) <- make.names(names(df))
print(names(df))

# Convert date columns
df$Board.Approval.Date <- as.Date(df$Board.Approval.Date, format = "%m/%d/%Y")
df$Effective.Date..Most.Recent. <- as.Date(df$Effective.Date..Most.Recent., format = "%m/%d/%Y")

# Fill missing Board Approval Dates
df$Board.Approval.Date[is.na(df$Board.Approval.Date)] <- 
  df$Effective.Date..Most.Recent.[is.na(df$Board.Approval.Date)]

# Remove rows with missing dates
df <- df[!is.na(df$Board.Approval.Date), ]

# Clean numeric columns
numeric_cols <- c(
  "Disbursed.Amount..US..", "Original.Principal.Amount..US..",
  "Cancelled.Amount..US..", "Undisbursed.Amount..US..",
  "Repaid.to.IDA..US..", "Due.to.IDA..US..",
  "Exchange.Adjustment..US..", "Borrowers.Obligation..US..",
  "Sold.3rd.Party..US..", "Repaid.3rd.Party..US..",
  "Due.3rd.Party..US..", "Credits.Held..US.."
)

for (col in numeric_cols) {
  if (col %in% names(df)) {
    df[[col]] <- as.numeric(gsub("[^0-9.-]", "", df[[col]]))
    df[[col]][is.na(df[[col]])] <- 0
  }
}

# Set 0 for cancelled/terminated credits
df$Disbursed.Amount..US..[df$Credit.Status %in% c("Fully Cancelled", "Terminated")] <- 0
df$Original.Principal.Amount..US..[df$Credit.Status %in% c("Fully Cancelled", "Terminated")] <- 0

# Convert Credit.Status to factor
df$Credit.Status <- as.factor(df$Credit.Status)

# Extract year
df$Year <- year(df$Board.Approval.Date)

# Save cleaned dataset
write.csv(df, "ida_credits_cleaned.csv", row.names = FALSE)

# ----------------------------
# Task 1: Time-Series Analysis of Disbursed Amount
# ----------------------------
annual_disbursed <- df %>%
  filter(!Credit.Status %in% c("Fully Cancelled", "Terminated")) %>%
  group_by(Year) %>%
  summarise(Total.Disbursed = sum(Disbursed.Amount..US.., na.rm = TRUE) / 1e6, .groups = "drop")

p1 <- ggplot(annual_disbursed, aes(x = Year, y = Total.Disbursed)) +
  geom_line(color = "#1f77b4", linewidth = 1) +
  geom_point(color = "#ff7f0e", size = 2) +
  theme_minimal() +
  labs(title = "Trend of World Bank Fund Disbursements to Uganda (1967-2025)",
       x = "Year", y = "Total Disbursed Amount (US$ Millions)") +
  theme(plot.title = element_text(hjust = 0.5))
print(p1)
ggsave("disbursement_trend.png", plot = p1, width = 10, height = 6, dpi = 300)

# ----------------------------
# Task 2: Credit Status Summary
# ----------------------------
status_summary <- df %>%
  count(Credit.Status, sort = TRUE) %>%
  mutate(Percentage = n / sum(n) * 100)
print(status_summary)

p2 <- ggplot(status_summary, aes(x = reorder(Credit.Status, -n), y = n, fill = Credit.Status)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Distribution of Credit Status for Uganda's IDA Credits",
       x = "Credit Status", y = "Number of Credits") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(p2)
ggsave("credit_status_bar.png", plot = p2, width = 10, height = 6, dpi = 300)

# ----------------------------
# Task 3: Original Principal Amount Analysis
# ----------------------------
annual_principal <- df %>%
  filter(!Credit.Status %in% c("Fully Cancelled", "Terminated")) %>%
  group_by(Year) %>%
  summarise(Total.Principal = sum(Original.Principal.Amount..US.., na.rm = TRUE) / 1e6, .groups = "drop")

p3 <- ggplot(annual_principal, aes(x = Year, y = Total.Principal)) +
  geom_bar(stat = "identity", fill = "#2ca02c") +
  theme_minimal() +
  labs(title = "Original Principal Amounts Borrowed by Uganda (1967-2025)",
       x = "Year", y = "Total Original Principal (US$ Millions)") +
  theme(plot.title = element_text(hjust = 0.5))
print(p3)
ggsave("principal_amount_bar.png", plot = p3, width = 10, height = 6, dpi = 300)

# ----------------------------
# Total Borrowed
# ----------------------------
cat("Total Original Principal Borrowed: $", 
    format(sum(df$Original.Principal.Amount..US.., na.rm = TRUE), big.mark = ","), "\\n")

# Get working directory
getwd()

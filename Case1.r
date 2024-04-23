library(tidyverse)
library(dplyr)
library(tidyr)
data <- read.csv("supermarket.csv")
sampled_data <- data[1:50, ]


num_columns <- ncol(sampled_data)  # Get the number of columns
column_names <- paste0("Item_", 1:num_columns)  # Generate column names

# Assign column names to the original dataset
colnames(sampled_data) <- column_names

head(sampled_data)


# Add Customer_ID as a new column
sampled_data <- mutate(Customer_ID = row_number(),sampled_data)

# Reorder the column
sampled_data <- sampled_data %>%
  select(Customer_ID, Item_1:Item_32)
head(sampled_data)


# Count the number of items for each customer
sampled_data <- sampled_data %>%
  mutate(num_items = rowSums(!is.na(select(., starts_with("Item")))))

# Reshape the data from wide to long format
sampled_data <- sampled_data %>%
  pivot_longer(cols = starts_with("Item"),       
               names_to = "Item",                # Specify name for resulting column holding item number
               values_to = "Scan") %>%          # Store scan values in the 'Scan' column
  arrange(Customer_ID) %>%                      # Arrange data by Customer_ID
  group_by(Customer_ID)                         # Group by Customer_ID

# Remove unwanted columns
sampled_data <- select(sampled_data, -num_items)

# Remove rows with no values in the 'Scan' column
sampled_data <- sampled_data %>%
  filter(!is.na(Scan) & Scan != "")

head(sampled_data)


sampled_data <- sampled_data %>%
  mutate(Department = substring(Scan, 1,3),
         Time = as.integer(substring(Scan, 4,6)),
         Price = as.numeric(substring(Scan, 7)))

head(sampled_data)

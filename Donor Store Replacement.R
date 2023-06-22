# Load required packages
if (!require("cluster")) {install.packages("cluster")}+library(cluster)
if (!require("dplyr")) {install.packages("dplyr")}+library(dplyr)
if (!require("svDialogs")) {  install.packages("svDialogs")}+library(svDialogs)


Data <- read.csv(file.choose())

#creating similarity matrix
similarity_matrix <- as.matrix(dist(Data$ACV))

# Find the top replacement
find_best_replacements <- function(lost_donor_store, num_replacements) {
  donor_store_index <- which(Data$store_id == lost_donor_store)
  donor_store_similarity <- similarity_matrix[donor_store_index, ]
  donor_store <- Data[donor_store_index, ]
  
  # Find rows with highest similarity (excluding the donor store)
  potential_replacements <- Data[Data$store_id != lost_donor_store, ]
  potential_replacements$similarity <-  donor_store_similarity[Data$store_id != lost_donor_store]
  
  potential_replacements <- potential_replacements %>%
    filter(
      StoreType == donor_store$StoreType,
      Region == donor_store$Region,
      Channel == donor_store$Channel
    ) %>%
    arrange(abs(ACV - donor_store$ACV))  # Sort by the absolute difference in ACV
  
  # Return the lost store and top replacement rows
  return(list(
    LostStore = donor_store,
    TopReplacements = head(potential_replacements, num_replacements)
  ))
}

# Example usage: Find the best replacement store for a lost donor store and output lost store and top 5 replacements
lost_donor_store <- as.integer(dlg_list(Data$store_id, title = "Select lost store id")$res)
replacement_results <- find_best_replacements(lost_donor_store, num_replacements = as.integer(dlg_input(message = "Enter number of top replacements")$res))

# Print the lost store
cat("Lost Donor Store:\n")
print(replacement_results$LostStore)

# Print the top 5 replacement rows
cat("\nTop 5 Replacement Rows:\n")
print(replacement_results$TopReplacements)

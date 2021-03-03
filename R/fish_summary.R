# Fish summary function
# Input: Vector of fish names
# Parameter: To return or not return a histogram of the fish types
# Output: The most common fish, the rarest fish, the total number of fish
##

library(checkmate)

fish_summary <- function(fish_list, fishtogram = FALSE) {
  # Validation checking
  # is the input an atomic vector?
  assert_atomic_vector(fish_list)
  # is the vector mode character? We should warn the user if what they are
  # feeding into the function is a vector of numbers, they may have grabbed the
  # wrong data since we normally call fish by name not number.
  check_character(fish_list)
  
  # Change the input vector to a factor data type
  factorized_fish_list <- as.factor(fish_list)
  
  # output the summary
  fish_summary <- summary(factorized_fish_list)
  
  # Most common fish from the input
  most_common_fish <- names(which.max(fish_summary))
  # Rarest fish from the input
  rarest_fish <- names(which.min(fish_summary))
  # Total number of fish from the input
  total_fish <- sum(fish_summary)
  
  # Logic to control the output of a histogram
  if (fishtogram == TRUE) {
    # Build the plot title based on total fish
    plottitle = sprintf("We caught %d fish today!", total_fish)
    
    # Create the fishtogram
    fishtogram <-
      ggplot(data.frame(fish = factorized_fish_list),
             aes(fish, fill = fish)) +
      geom_histogram(stat = "count") +
      labs(title = plottitle)
    
    # Return all the outputs in a list
    return(
      list(
        most_common_fish = most_common_fish,
        rarest_fish = rarest_fish,
        total_fish = total_fish,
        fishtogram = fishtogram
      )
    )
  } else {
    
    # User didn't want the fishtogram...
    return(
      list(
        most_common_fish = most_common_fish,
        rarest_fish = rarest_fish,
        total_fish = total_fish
      )
    )
  }
}

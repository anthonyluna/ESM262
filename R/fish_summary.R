# Fish summary function
# input: vector of fish names
# parameter: to return or not return a histogram of the fish types
# output: The most common fish, the rarest fish, the total number of fish
##

library(checkmate)

fish_summary <- function(fish_list, histogram = FALSE) {
  # Validation checking
  # is the input an atomic vector?
  assert_atomic_vector(fish_list)
  # is the vector mode character? We should warn the user if what they are
  # feeding into the function is a vector of numbers, they may have grabbed the
  # wrong data since we normally call fish by name not number.
  check_character(fish_list)
  
  factorized_fish_list <- as.factor(fish_list)
  fish_summary <- summary(factorized_fish_list)
  
  most_common_fish <- names(which.max(fish_summary))
  rarest_fish <- names(which.min(fish_summary))
  total_fish <- sum(fish_summary)
  
  if (histogram == TRUE) {
    plottitle = sprintf("We caught %d fish today!", total_fish)
    
    fishtogram <-
      ggplot(data.frame(fish = factorized_fish_list),
             aes(fish, fill = fish)) +
      geom_histogram(stat = "count") +
      labs(title = plottitle)
    return(
      list(
        most_common_fish = most_common_fish,
        rarest_fish = rarest_fish,
        total_fish = total_fish,
        fishtogram = fishtogram
      )
    )
  } else {
    return(
      list(
        most_common_fish = most_common_fish,
        rarest_fish = rarest_fish,
        total_fish = total_fish
      )
    )
  }
  # you can also add numbers to the string
  
}

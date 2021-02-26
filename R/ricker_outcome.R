ricker_outcome <- name <- function(competition_data) {
  outcome <-
    unique(round(tail(competition_data, n = 20) %>% select(-i), 0))
  
  outcome_title <-
    case_when(
      nrow(outcome) > 1 ~ "Periodic Population Cycle",
      nrow(outcome) == 1 &
        outcome$xn == 0 ~ "Species Y Outcompetes Species X",
      nrow(outcome) == 1 &
        outcome$yn == 0 ~ "Species X Outcompetes Species Y"
    )
  
  
  change_over_time <-
    ggplot(data = head(competition_data, n = 100), aes(x = i)) +
    geom_line(aes(y = xn), color = "red") +
    geom_line(aes(y = yn), color = "blue") +
    geom_point(aes(y = xn), color = "red") +
    geom_point(aes(y = yn), color = "blue") +
    theme_minimal()
  
  population_point <- ggplot(data = competition_data) +
    geom_point(aes(x = xn, y = yn, alpha = i)) +
    theme_minimal()
  
  return((change_over_time / population_point) +
           plot_annotation(title = paste("Outcome:", unique(outcome_title))))
}

# Outcome of the Adult/Juvenile Ricker Model
#
# Goal: To provide two plots that describes the population dynamics of the 
# two species of interest given the inputs to the ricker function (see ricker.R)
#
# Input: A dataframe with 3 columns that match the output of the ricker function
#
# parameters: Name of species x and species y
#
# output: two plots with a title describing what happened.
#
# Modules:
#   - Find the outcome
#   - Build the names
#   - Choose the name based on the outcome
#   - Plot the change over time
#   - Plot the population phase plot
#   - Combine the plots and title into one image and return this


library(patchwork)
library(checkmate)

ricker_outcome <-
  function(competition_data,
           species_x_name = "X",
           species_y_name = "Y") {
    
    # Error checking, make sure that the dataframe is the expected format
    # and that the names given as arguments are strings
    checkmate::assert_data_frame(competition_data, ncols = 3)
    checkmate::assert_true(identical(names(competition_data),c("i","xn","yn")))
    checkmate::assert_string(species_x_name)
    checkmate::assert_string(species_y_name)
    
    # Generate the outcome. This is a heuristic that looks at the last 20
    # timesteps from the competition_data, rounds to the 4th decimal place for
    # each (this is where the heuristic is most vulnerable, but for most cases,
    # systems will reach equilibrium after 1000 iterations), and gathers the
    # unique values.
    
    outcome <-
      unique(round(tail(competition_data, n = 20) %>% select(-i), 4))
    
    # Defining the different ploit titles based on outcomes
    outcome_periodic <- "Periodic Population Cycle"
    
    outcome_x_extinct <-
      paste(species_y_name,
             "Survives,",
             species_x_name,
             "Goes Extinct")
    
    outcome_y_extinct <-
      paste(species_x_name,
             "Survives,",
             species_y_name,
             "Goes Extinct")
    
    outcome_x_dom <-
      paste("Both species survive,",
             species_x_name,
             "with higher population.")
    
    outcome_y_dom <-
      paste("Both species survive,",
             species_y_name,
             "with higher population.")
    
    # Testing outcomes
    outcome_title <-
      case_when(
        # if there is more than 1 row aver the hueristic is applied, this is 
        # likely a periodic outcome. 
      nrow(outcome) > 1 ~ outcome_periodic,
      # x extinction scenario
      nrow(outcome) == 1 &
        outcome$xn == 0 ~ outcome_x_extinct,
      
      # y extinction scenario
      nrow(outcome) == 1 &
        outcome$yn == 0 ~ outcome_y_extinct,
      
      # cohabitation with x dominating
      nrow(outcome) == 1 &
        outcome$yn != 0 &
        outcome$xn != 0 &
        outcome$yn < outcome$xn ~ outcome_x_dom,
      
      # cohabitation with y dominating
      nrow(outcome) == 1 &
        outcome$yn != 0 &
        outcome$xn != 0 &
        outcome$yn > outcome$xn ~ outcome_y_dom
    )
  
  
    # Here is the plot over time. Only take the first 100 rows, its pretty
    # boring after that normally. Need to pivot data to make the legends play
    # nicely.
    change_over_time <-
      ggplot(
        data = head(competition_data, n = 100) %>%
          pivot_longer(
            cols = c("xn", "yn"),
            names_to = "Species",
            values_to = "Population"
          ),
        aes(x = i)
      ) +
      geom_line(aes(y = Population, color = Species) )+
      geom_point(aes(y = Population, color = Species)) +
      scale_x_continuous(name = "Population") +
      scale_y_continuous(name = "Timestep") +
      scale_color_discrete(labels = c(species_x_name, species_y_name)) +
      theme_minimal()
  
  # Population phase plot here. This may be the not so great way to do this, but
  # it works for now
  population_point <- ggplot(data = competition_data %>% filter(i%%10==0)) +
    geom_point(aes(x = xn, y = yn, alpha = i)) +
    scale_x_continuous(name=species_x_name)+
    scale_y_continuous(name=species_y_name)+
    theme_minimal()
  
  # Formatting the output plot with the outcomes.
  return((change_over_time / population_point) +
           plot_annotation(title = paste("Outcome:", unique(outcome_title))))
}

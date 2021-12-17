# import some libraries
library(abind)
library(dplyr)

# Some global parameters for the problem
generations <- 100; 

# initial population parameters
min                  <- -50;
max                  <- 50;
population_size      <- 1000;
number_of_timestamps <- 10; 

# mutation parameters
location <- 0; 
spread   <- 0.0007;

# crossover parameters
number_of_offspring <- 0.3;

# start end parameters
start <- c(-10, -10);
end   <- c( 10,  10);

# fitness function parameters 
A           <- array(c(1, 0, 0, 1), dim = c(2, 2));
time_bounds <- c(0, 1);

# selection parameters
number_to_remove <- 0.3;
descending <- TRUE;

# generate the initial population of genotypes
initial_population_random <- function(number_of_timestamps, number_of_genotypes){
  # return the matrix. 
  # Change 'runif' if a different distribution is wanted
  return(
    array(
      runif(
        prod(number_of_timestamps, number_of_genotypes, 2), 
        min = min,
        max = max
      ),
      dim = c(number_of_genotypes, number_of_timestamps, 2)
    )
  )
}

# perform cauchy distribution mutations on the genotype
mutation_cauchy <- function(population, location, spread){
  change <- array(
    rcauchy(prod(dim(population)), location = location, scale = spread),
    dim = dim(population)
  );
  return(
    # add on a random number to each element from a cauchy distribution
    array(as.numeric(population) + as.numeric(change), dim = dim(population))
  )
}

# bread two genotypes
crossover_bread <- function(gen_1, gen_2){
  pivot_point <- sample(2:(dim(gen_1)[1] - 1), size = 1);
  return(rbind(gen_1[1:pivot_point, ], gen_2[(pivot_point + 1):dim(gen_2)[1], ]))
}

# apply the crossover algorithm
crossover_basic <- function(population, number_of_offspring){
  # Get the people chosen for crossover
  chosen_people <- population[sample(1:dim(population)[1], size = number_of_offspring * 2, replace = F), , ];
  
  offspring <- array(dim = c(0, dim(population)[2], 2));
  # for each child needed: 
  for (genotype in 1:(dim(chosen_people)[1] / 2)){
    # figure out who their parents will be (already randomly sorted)
    gen1 <- chosen_people[genotype, , ];
    gen2 <- chosen_people[dim(chosen_people)[1] + 1 - genotype, , ];
    
    # create the child
    current_offspring <- crossover_bread(gen1, gen2);
    
    offspring <- abind(offspring, current_offspring, along = 1);
  }
  return(abind(population, offspring, along = 1));
}

# add the start and end c
add_start_end <- function(genotype){
  return(
    rbind(
      array(start, dim = c(1, 2)), genotype, 
      array(end, dim = c(1, 2)))
  )
}

# fitness function (alternate length)
fitness_function <- function(genotype){
  genotype <- add_start_end(genotype); 
  curve_length <- curve_length(curve = genotype, A = A, bounds = time_bounds); 
  return(curve_length)
}

# calculate fitnesses for population
fitness_function_all <- function(population){
  fitnesses <- apply(population, MARGIN = 1, FUN = function(genotype){
    return(fitness_function(genotype))
  })
  return(fitnesses)
}

# perform selection
selection <- function(population, number_to_remove){
  fitnesses <- fitness_function_all(population);
  fitness_order <- order(fitnesses, decreasing = descending);
  new_population <- population[fitness_order, , ];
  new_population <- new_population[ (number_to_remove + 1):(dim(new_population)[1]), , ];
  return(new_population)
}

# function to approximately differentiate a paired vector array
differentiate <- function(curve){
  gradients <- c();
  for (segment_index in 1:(dim(curve)[1] - 1)){
    first_coordinate  <- curve[segment_index    , ];
    second_coordinate <- curve[segment_index + 1, ];
    
    y_diff <- (second_coordinate[2] - first_coordinate[2]);
    x_diff <- (second_coordinate[1] - first_coordinate[1]);
    
    gradient <- y_diff / x_diff; 
    gradients <- append(gradients, gradient);
  }
  gradients <- append(gradients, gradient); 
  return(gradients);
}

# calculate the length of a parametrically defined curve from a 
# paired vector array. 
curve_length <- function(curve, A = NULL, bounds = c(0, 1)){
  if (is.null(A)){
    A <- array(c(1, 0, 0, 1), dim = c(2, 2));
  };
  
  differential <- differentiate(curve);
  
  time_interval <- (bounds[2] - bounds[1]) / (dim(curve)[1] - 1);
  time_stamps   <- ((1:dim(curve)[1]) - 1) * time_interval;
  
  differential <- array(append(time_stamps, differential), dim = dim(curve));
  
  total_area <- 0;
  for (time_index in 1:(dim(curve)[1] - 1)){
    current_gradient <- differential[time_index, ];
    A_component <- current_gradient %*% A; 
    magnitude   <- sqrt(sum(A_component * current_gradient));
    
    local_area <- magnitude * time_interval;
    total_area <- total_area + local_area; 
  }
  return(total_area)
}

initial_population <- initial_population_random(number_of_timestamps = number_of_timestamps, number_of_genotypes = population_size);
current_population <- initial_population; 

fitnesses <- c();

for (i in 1:generations){
  print(paste0("Current Generation: ", i, "/", generations))
  current_population <- current_population %>% 
    mutation_cauchy(location = location, spread = spread) %>%
    crossover_basic(number_of_offspring = as.integer(number_of_offspring * population_size)) %>%
    selection(number_to_remove = as.integer(number_to_remove * population_size)); 
  
  fitest <- current_population[1, , ];
  fitest_fitness <- fitness_function(fitest); 
  fitnesses <-  append(fitnesses, fitest_fitness);
  
  if (i == 1){
    initial_fitness <- as.numeric(fitest_fitness); 
    initial_member <- add_start_end(fitest); 
  }
  
  print(paste0("Current Fitness: ", fitest_fitness));
}

fitest <- add_start_end(fitest)

plot(initial_member[, 1], initial_member[, 2], type = "l", main = "Fitest Member of First Generation")
plot(fitest[, 1], fitest[, 2], type = "l", main = "Fitest Member of Final Generation")
plot(1:length(fitnesses), fitnesses, main = "Fitness of Best Member in Each Generation")

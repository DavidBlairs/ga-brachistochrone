library(dplyr)

# Some global parameters for the problem
seed   <- 15; 
A_y    <- 100;
B_x    <- 500;
R      <- 5;
g      <- 9.81;
pop    <- 100;
remove <- 0.1;
generations <- 100;
mutation_size <- 5;

set.seed(seed)

# generate a population from the uniform distribution
initial_population_random <- function(A_y, genotype_length, number_of_genotypes){
  # return the matrix. 
  # Change 'runif' if a different distribution is wanted
  return(
    matrix(
      runif(
        genotype_length * number_of_genotypes, 
        min = 0,
        max = A_y
      ),
      nrow = genotype_length, 
      ncol = number_of_genotypes
    )
  )
}

# apply the cauchy mutation operator
mutation_cauchy <- function(population, location, spread){
  change <- matrix(
    rcauchy(nrow(population) * ncol(population), location = location, scale = spread),
    ncol = ncol(population),
    nrow = nrow(population)
  );
  
  return(
    # add on a random number to each element from a cauchy distribution
    matrix(as.numeric(population) + as.numeric(change), ncol = ncol(population), nrow = nrow(population))
  )
}

# bread two genotypes
crossover_bread <- function(gen_1, gen_2){
  pivot_point <- sample(2:(length(gen_1) - 1), size = 1);
  return(
    c(gen_1[1:pivot_point], gen_2[(pivot_point + 1):length(gen_2)])
  )
}

# apply the crossover algorithm
crossover_basic <- function(population, number_of_offspring){
  # Get the people chosen for crossover
  chosen_people <- population[,sample(1:ncol(population), size = number_of_offspring * 2, replace = F)];
  
  offspring <- matrix(ncol = 0, nrow = nrow(population));
  # for each child needed: 
  for (genotype in 1:(ncol(chosen_people) / 2)){
    # figure out who their parents will be (already randomly sorted)
    gen1 <- chosen_people[, genotype];
    gen2 <- chosen_people[, ncol(chosen_people) + 1 - genotype];
    
    # create the child
    current_offspring <- crossover_bread(gen1, gen2) %>% matrix(ncol = 1, nrow = nrow(population));
    offspring <- cbind(offspring, current_offspring);
  }
  # return the complete list of children created
  return(cbind(population, offspring))
}

# apply fitness function to a single genotype (eom derived)
fitness_function_eom <- function(genotype){
  genotype <- as.vector(genotype); 
  
  coordinates <- matrix(c(0, A_y), nrow = 2, ncol = 1);
  velocities <- c(0);
  
  total_time <- 0; 
  
  for (segment_index in 1:(length(genotype) + 1)){
    first_coordinate  <- coordinates[, segment_index];
    second_coordinate <- matrix(c(segment_index * (B_x / (R + 1)),genotype[segment_index]));
    
    if (segment_index == (length(genotype) + 1)){
      second_coordinate[2, 1] <- 0;
    }
    
    coordinates <- cbind(coordinates, second_coordinate);
    
    distance <- sqrt(((first_coordinate[2] - second_coordinate[2])**2) + ((first_coordinate[1] - second_coordinate[1])**2));
    theta <- atan((second_coordinate[1] - first_coordinate[1]) / (first_coordinate[2] - second_coordinate[2]));
    
    acceleration <- g * cos(theta);
    
    initial_velocity <- velocities[segment_index] * theta; 
    final_velocity <- sqrt((initial_velocity**2) + (2 * acceleration * distance));
    
    velocities <- append(velocities, final_velocity); 
    
    time_taken <- (final_velocity - initial_velocity) / acceleration; 
    total_time <- total_time + time_taken; 
  }
  
  velocities <<- velocities; 
  
  return(total_time)
}

fitness_function_euc <- function(genotype){
  genotype_full  <- c(A_y, genotype, 0);
  resolution_gap <- B_x / (length(genotype_full) - 1); 
  
  total_distance <- 0;
  
  # for each segment of the approximate curve
  for (height_index in 1:(length(genotype_full) - 1)){
    #get the coordinates of the start and end of the segment
    first_coordinate  <- c(resolution_gap * (height_index - 1), genotype_full[height_index]); 
    second_coordinate <- c(resolution_gap * height_index, genotype_full[height_index + 1]); 
    
    # calculate the length of the segment
    distance <- ((first_coordinate[1] - second_coordinate[1])**2) +
      ((first_coordinate[2] + second_coordinate[2])**2);
    
    # and add to the total distance
    total_distance <- total_distance + distance;
  }
  return(total_distance)
}

# calculate fitness euclidean
fitness_function <- function(population){
  fitnesses <- apply(population, MARGIN = 2, FUN = function(genotype){
    return(fitness_function_euc(genotype))
  })
  return(fitnesses)
}

# perform selection
selection <- function(population, number_to_remove){
  fitnesses <- fitness_function(population);
  fitness_order <- order(fitnesses, decreasing = TRUE);
  new_population <- population[, fitness_order][, (number_to_remove + 1):(ncol(population))]
  return(new_population)
}

initial_population <- initial_population_random(A_y = A_y, genotype_length = R, number_of_genotypes = pop);
current_population <- initial_population; 

fitnesses <- c();
for (i in 1:generations){
  print(paste0("Current Generation: ", i, "/", generations))
  current_population <- current_population %>% 
    mutation_cauchy(location = 0, spread = mutation_size) %>%
    crossover_basic(number_of_offspring = as.integer(remove * pop)) %>%
    selection(number_to_remove = as.integer(remove * pop)); 
  
  fitest <- as.vector(current_population[, 1]);
  fitnesses <-  append(fitnesses, fitness_function_euc(fitest));
  
  print(paste0("Current Fitness: ", fitness_function_euc(fitest)));
  plot(1:length(fitest), fitest)
}
plot(1:length(fitest), fitest)
plot(1:length(fitnesses), fitnesses)



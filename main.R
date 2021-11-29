library(dplyr)

# Some global parameters for the problem

A_y    <- 10;
B_x    <- 40;
R      <- 20;
g      <- 9.81;
pop    <- 500;
remove <- 0.5;
generations <- 1000;
mutation_size <- 0.00007;
descending <- TRUE; 


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
  # add the first and last height to the genotype
  complete_genotype <- c(A_y, genotype, 0);
  
  # calculate the drop in height from A_y to each height
  height_drop <- abs(A_y - complete_genotype);
  
  # determine the velocities (loss in GPE is gain in KE)
  velocities <- sqrt(2 * height_drop * g);
  total_time <- 0;
  
  # this is the length of the gap between heights
  x_diff <- abs(B_x / (length(genotype) + 1));
  
  # for each edge in our curve
  for (segment_index in 1:(length(genotype) + 1)){
    # calculate the difference in height between the two heights of the segment
    y_diff <- abs(complete_genotype[segment_index] - complete_genotype[segment_index + 1]);
    
    # get the coordinates of the segment
    first_coordinate  <- c(x_diff * (segment_index - 1),  complete_genotype[segment_index]);
    second_coordinate <- c(x_diff * (segment_index),  complete_genotype[segment_index + 1]);
    
    # calculate the distance between the coordinates
    distance <- sqrt(sum(abs(first_coordinate - second_coordinate)**2));
    
    # calculate acceleration in the direction of the segment 
    acceleration <- g * (y_diff / x_diff); 
    
    # determine the time according to the equations of motion
    total_time <- total_time + ((2 * distance) / (velocities[segment_index] + velocities[segment_index + 1]));

  }
  return(total_time)
}

# calculate fitness euclidean
fitness_function <- function(population){
  fitnesses <- apply(population, MARGIN = 2, FUN = function(genotype){
    return(fitness_function_eom(genotype))
  })
  return(fitnesses)
}

# perform selection
selection <- function(population, number_to_remove){
  fitnesses <- fitness_function(population);
  fitness_order <- order(fitnesses, decreasing = descending);
  new_population <- population[, fitness_order];
  new_population <- new_population[ , (number_to_remove + 1):(ncol(new_population))];
  return(new_population)
}

initial_population <- initial_population_random(A_y = A_y, genotype_length = R, number_of_genotypes = pop);
current_population <- initial_population; 

fitnesses <- c();

start.time <- Sys.time();

for (i in 1:generations){
  print(paste0("Current Generation: ", i, "/", generations))
  current_population <- current_population %>% 
    mutation_cauchy(location = 0, spread = mutation_size) %>%
    crossover_basic(number_of_offspring = as.integer(remove * pop)) %>%
    selection(number_to_remove = as.integer(remove * pop)); 
  
  fitest <- as.vector(current_population[, 1]);
  fitest_fitness <- fitness_function_eom(fitest); 
  fitnesses <-  append(fitnesses, fitest_fitness);
  
  if (i == 1){
    initial_fitness <- as.numeric(fitest_fitness); 
    initial_member <- fitest; 
  }
  
  print(paste0("Current Fitness: ", fitest_fitness));
}

plot(1:length(initial_member), initial_member, main = "Fitest Member of First Generation")
plot(1:length(fitest), fitest, type = "l", main = "Fitest Member of Final Generation")
plot(1:length(fitnesses), fitnesses, main = "Fitness of Best Member in Each Generation")
print("====================================")
print(paste0("Elapsed Time (seconds): ", Sys.time() - start.time))
print("====================================")
print(paste0("Inital Best Fitness: ", initial_fitness))
print(paste0("Final Best Fitness : ", fitest_fitness))
print("====================================")


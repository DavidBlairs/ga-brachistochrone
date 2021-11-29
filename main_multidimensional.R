library(abind)
library(dplyr)

# Some global parameters for the problem


A_y    <- 10;
B_x    <- 40;
R      <- 10;
g      <- 9.81;
pop    <- 100;
remove <- 0.5;
generations <- 10000;
mutation_size <- 0.007;
descending <- TRUE; 

initial_population_random <- function(A_y, genotype_length, number_of_genotypes){
  # return the matrix. 
  # Change 'runif' if a different distribution is wanted
  return(
    array(
      runif(
        genotype_length * number_of_genotypes * 2, 
        min = 0,
        max = A_y
      ),
      dim = c(genotype_length, 2, number_of_genotypes)
    )
  )
}

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
  chosen_people <- population[ , , sample(1:dim(population)[3], size = number_of_offspring * 2, replace = F)];
  
  offspring <- array(dim = c(dim(population)[1], 2, 0));
  # for each child needed: 
  for (genotype in 1:(dim(chosen_people)[3] / 2)){
    # figure out who their parents will be (already randomly sorted)
    gen1 <- chosen_people[, , genotype];
    gen2 <- chosen_people[, , dim(chosen_people)[3] + 1 - genotype];
    
    # create the child
    current_offspring <- crossover_bread(gen1, gen2);
    
    offspring <- abind(offspring, current_offspring, along = 3);
  }
  return(abind(population, offspring, along = 3));
}

# add the start and end c
add_start_end <- function(genotype){
  return(
    rbind(
      array(c(0, A_y), dim = c(1, 2)), genotype, 
      array(c(B_x, 0), dim = c(1, 2)))
    )
}

# apply fitness function to a single genotype (eom derived)
fitness_function_eom <- function(genotype){
  genotype <- add_start_end(genotype);
  
  # calculate the drop in height from A_y to each height
  height_drop <- abs(A_y - genotype[,2]);
  
  # determine the velocities (loss in GPE is gain in KE)
  velocities <- sqrt(2 * height_drop * g);
  total_time <- 0;
  
  # for each edge in our curve
  for (segment_index in 1:(dim(genotype)[1] - 1)){
    # calculate the difference in height between the two heights of the segment
    y_diff <- abs(genotype[segment_index, 2] - genotype[segment_index + 1, 2]);
    x_diff <- abs(genotype[segment_index, 1] - genotype[segment_index + 1, 1]);
    
    # get the coordinates of the segment
    first_coordinate  <- c(genotype[segment_index, 1],  genotype[segment_index, 2]);
    second_coordinate <- c(genotype[segment_index + 1, 1],  genotype[segment_index + 1, 2]);
    
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
  fitnesses <- apply(population, MARGIN = 3, FUN = function(genotype){
    return(fitness_function_eom(genotype))
  })
  return(fitnesses)
}

# perform selection
selection <- function(population, number_to_remove){
  fitnesses <- fitness_function(population);
  fitness_order <- order(fitnesses, decreasing = descending);
  new_population <- population[, ,fitness_order];
  new_population <- new_population[ , , (number_to_remove + 1):(dim(new_population)[3])];
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
  
  fitest <- current_population[, ,1];
  fitest_fitness <- fitness_function_eom(fitest); 
  fitnesses <-  append(fitnesses, fitest_fitness);
  
  if (i == 1){
    initial_fitness <- as.numeric(fitest_fitness); 
    initial_member <- add_start_end(fitest); 
  }
  
  if (i %% 10 == 0){
    jpeg(file = paste0("gif/generation_", i, ".jpg"))
    fitest <- add_start_end(fitest);
    
    plot(fitest[,1], fitest[,2], type = "l", main = paste0("Fitest Member of Generation ", i))
    dev.off()
  }
  
  print(paste0("Current Fitness: ", fitest_fitness));
}

fitest <- add_start_end(fitest)

plot(initial_member[,1], initial_member[,2], main = "Fitest Member of First Generation")
plot(fitest[,1], fitest[,2], type = "l", main = "Fitest Member of Final Generation")
plot(1:length(fitnesses), fitnesses, main = "Fitness of Best Member in Each Generation")
print("====================================")
print(paste0("Elapsed Time (seconds): ", Sys.time() - start.time))
print("====================================")
print(paste0("Inital Best Fitness: ", initial_fitness))
print(paste0("Final Best Fitness : ", fitest_fitness))
print("====================================")

library(abind)

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

x <- initial_population_random(A_y = 10, genotype_length = 4, number_of_genotypes = 12);

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

x <- mutation_cauchy(population = x, location = 0, spread = 1);

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

x <- crossover_basic(population = x, number_of_offspring = 1); 


# apply fitness function to a single genotype (eom derived)

fitness_function_eom <- function(genotype){
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

fitness_function(population = x)
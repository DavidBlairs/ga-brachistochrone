library(tidyverse)
library(ggforce)
library(palmerpenguins)
library(abind)
library(geometry)

# fitness function
g <- 9.81;
G <- 6.67408;

start <- c(50, 50);
end   <- c(150, 150);

mass <- 1;

# initial population
min <- 0;
max <- 200;

population_size <- 100;
time_iterations <- 100; 

# mutations
mutation_size <- 0.000000007;

# selection
remove <- 0.5;
descending <- TRUE;

# objects
objects <- list(
  Earth = list(
    mass  = 10,
    x     = 50, 
    y     = 50
  ),
  Mars  = list(
    mass  = 10, 
    x     = 150, 
    y     = 150
  ),
  Sun   = list(
    mass  = 50, 
    x     = 100,
    y     = 100
  )
)

generations <- 1000;

initial_population_random <- function(min, max, time_iterations, number_of_genotypes){
  # return the matrix. 
  # Change 'runif' if a different distribution is wanted
  return(
    array(
      runif(
        time_iterations * number_of_genotypes * 2, 
        min = min,
        max = max
      ),
      dim = c(time_iterations, 2, number_of_genotypes)
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
      array(start, dim = c(1, 2)), genotype, 
      array(end  , dim = c(1, 2)))
  )
}

# calculate the force acting on an object at a specific point
calculate_force <- function(coordinate){
  net_force <- c(0, 0);
  
  for (object in names(objects)){
    name <- as.character(object);

    distance <- sqrt((objects[[name]]$x - coordinate[1])**2 +
                       (objects[[name]]$y - coordinate[2])**2);
    unit_vector <- (coordinate - c(objects[[name]]$x, objects[[name]]$y)) / distance;
    
    scalar <- (G * mass * objects[[name]]$mass) / distance**2;
    
    acting_force <- scalar * unit_vector; 
    
    net_force <- net_force - ifelse(is.nan(acting_force), 0, acting_force);
  }
  
  return(net_force)
}

# apply the fitness function
fitness_function_eom <- function(genotype){
  complete_genotype <- add_start_end(genotype);
  
  total_work <- 0;
  for (coordinate in 1:(nrow(genotype) - 1)){
    start_coordinate <- genotype[coordinate, ];
    end_coordinate   <- genotype[coordinate + 1, ];
    
    distance_vector   <- end_coordinate - start_coordinate;
    distance <- sqrt(distance_vector[1]**2 + distance_vector[2]**2);
      
    middle_coordinate <- (start_coordinate + end_coordinate) / 2; 
    
    net_force <- calculate_force(middle_coordinate);
    work_done <- dot(net_force, distance_vector);
    
    total_work <- total_work + work_done; 
  }
  return(abs(total_work))
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


initial_population <- initial_population_random(min = min, max = max, time_iterations = time_iterations, number_of_genotypes = population_size);
current_population <- initial_population; 

fitnesses <- c();

for (i in 1:generations){
  print(paste0("Current Generation: ", i, "/", generations))
  current_population <- current_population %>% 
    mutation_cauchy(location = 0, spread = mutation_size) %>%
    crossover_basic(number_of_offspring = as.integer(remove * population_size)) %>%
    selection(number_to_remove = as.integer(remove * population_size)); 
  
  fitest <- current_population[, ,1];
  fitest_fitness <- fitness_function_eom(fitest); 
  fitnesses <-  append(fitnesses, fitest_fitness);
  
  if (i == 1){
    initial_fitness <- as.numeric(fitest_fitness); 
    initial_member <- add_start_end(fitest); 
  }
  
  if (i %% 10 == 0){
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
print(paste0("Inital Best Fitness: ", initial_fitness))
print(paste0("Final Best Fitness : ", fitest_fitness))
print("====================================")


#
#theme_set(theme_bw(16))
#print(penguins %>%
#  ggplot(aes(x=bill_length_mm, 
#             y=bill_depth_mm,
#             color=species))+
#  geom_point() +
#  geom_circle(aes(x0 = 38.5, y0 = 18, r = 3),
#              inherit.aes = FALSE))



library(dplyr)
#' this program solves the brachistochrone problem but uses a 2D representation
#' of each of the points on the curve (i.e. an x and y coordinate instead of a height)

# generate a population of 2D coordinates from the uniform distribution 
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
      dim = c(genotype_length, number_of_genotypes, 2)
    )
  )
}

x <- initial_population_random(A_y = 10, genotype_length = 5, number_of_genotypes = 5)

# apply the cauchy mutation operator
mutation_cauchy <- function(population, location, spread){
  change <- array(
    rcauchy(nrow(population) * ncol(population) *  2, location = location, scale = spread),
    dim = c(nrow(population), ncol(population), 2)
  );
  
  return(
    # add on a random number to each element from a cauchy distribution
    array(as.numeric(population) + as.numeric(change), dim = c(nrow(population), ncol(population), 2))
  )
}

x <- mutation_cauchy(population = x, location = 0, spread = 1);

# bread two genotypes
crossover_bread <- function(gen_1, gen_2){
  pivot_point <- sample(2:(nrow(gen_1) - 1), size = 1);
  return(rbind(gen_1[1:pivot_point, ], gen_2[(pivot_point + 1):nrow(gen_2), ]))
}

# apply the crossover algorithm
crossover_basic <- function(population, number_of_offspring){
  # Get the people chosen for crossover
  chosen_people <- population[ ,sample(1:ncol(population), size = number_of_offspring * 2, replace = F), ];
  
  offspring <- array(dim = c(nrow(population), 1, 2));
  # for each child needed: 
  for (genotype in 1:(ncol(chosen_people) / 2)){
    # figure out who their parents will be (already randomly sorted)
    gen1 <- chosen_people[, genotype, ];
    gen2 <- chosen_people[, ncol(chosen_people) + 1 - genotype, ];
    
    # create the child
    current_offspring <- crossover_bread(gen1, gen2);
    
    print(offspring)
    print(current_offspring)
    
    offspring[, ncol(offspring) + 1, ] <- current_offspring;
  }
  print(offspring)
  
  # return the complete list of children created
  # return(cbind(population, offspring))
}

crossover_basic(population = x, number_of_offspring = 2);


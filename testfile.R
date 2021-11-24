fitness_single_genotype <- function(genotype){
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
    
    initial_velocity <- velocities[segment_index]; 
    final_velocity <- sqrt((initial_velocity**2) + (2 * acceleration * distance));
    
    velocities <- append(velocities, final_velocity); 
    
    time_taken <- (final_velocity - initial_velocity) / acceleration; 
    total_time <- total_time + time_taken; 
  }
  return(total_time)
}
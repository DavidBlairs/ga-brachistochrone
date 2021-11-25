solution <- c(80, 60, 40, 20);
A_y <- 100;
B_x <- 100;
g <- 9.81; 

fitness_function_eom <- function(genotype){
  height_drop <- abs(A_y - c(A_y, genotype, 0));
  velocities <- sqrt(2 * height_drop * g);
  total_time <- 0;
  
  x_diff <- abs(B_x / (length(genotype) + 1));
  for (segment_index in 1:(length(genotype) + 1)){
    y_diff <- abs(height_drop[segment_index] - height_drop[segment_index + 1]);
    
    acceleration <- g * abs(y_diff / x_diff); 
    total_time <- total_time + abs((velocities[segment_index + 1] - velocities[segment_index]) / acceleration);
  }
  return(total_time)
}

time <- fitness(solution);
print(time)
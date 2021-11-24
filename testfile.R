fitness_function_euc <- function(genotype){
  genotype_full  <- c(A_y, genotype, 0);
  resolution_gap <- B_x / (length(genotype_full) - 1); 
  
  total_distance <- 0;
  
  # for each segment of the approximate curve
  for (height_index in 1:(length(genotype_full) - 1)){
    #get the coordinates of the start and end of the segment
    first_coordinate  <- c(resolution_gap * (height_index - 1), genotype_full[height_index]); 
    second_coordinate <- c(resolution_gap * height_index, genotype_full[height_index + 1]); 
    
    print("========")
    print(first_coordinate)
    print(second_coordinate)
    
    # calculate the length of the segment
    distance <- sqrt(((first_coordinate[1] - second_coordinate[1])**2) +
                       ((first_coordinate[2] - second_coordinate[2])**2));
    
    print(distance)
    
    # and add to the total distance
    total_distance <- total_distance + distance;

  }
  return(total_distance)
}
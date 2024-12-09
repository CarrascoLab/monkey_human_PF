fig_2_A <- function(human, monkey) {
  
  # Add error bars for monkeys
  sem_monkey = std.error(monkey$dprime_normalized)
  sem_human = std.error(human$dprime_normalized)
  
  # Define the angles
  angle_pos <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
  angles_rad <- angle_pos * pi / 180
  
  # assign subplots:
  par(mfrow = c(2, 2))
  # Plot for monkeys
  polar.plot(c(monkey$full_visual_field_dprime, monkey$full_visual_field_dprime[1]),angle_pos, lwd = 3, lty = 1, line.col = 'red',
             point.col = 'red', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 0, radial.lim=c(0,1.5), show.legend = FALSE)
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkey$full_visual_field_dprime, monkey$full_visual_field_dprime[1])[i] * cos(angles_rad[i])
    y <- c(monkey$full_visual_field_dprime, monkey$full_visual_field_dprime[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey[i] * cos(angles_rad[i])
    dy <- sem_monkey[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = 'red', lwd = 4)
  }
  
  polar.plot(c(human$full_visual_field_dprime, human$full_visual_field_dprime[1]),angle_pos, lwd = 3, lty = 1, line.col = 'black',
             point.col = 'black', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 1, radial.lim=c(0,1.5))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(human$full_visual_field_dprime, human$full_visual_field_dprime[1])[i] * cos(angles_rad[i])
    y <- c(human$full_visual_field_dprime, human$full_visual_field_dprime[1])[i] * sin(angles_rad[i])
    dx <- sem_human[i] * cos(angles_rad[i])
    dy <- sem_human[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = 'black', lwd = 4)
  }
  
  #### Plot reaction time:
  human$reaction_time <- human$reaction_time + 0.5; # add the stim duration
  monkey_RT_for_err <- apply(monkey$reaction_time, c(2, 3, 4), mean)/1000
  sem_monkey = std.error(monkey_RT_for_err[1,,])
  sem_human = std.error(human$reaction_time)
  
  # Plot for monkeys
  polar.plot(c(monkey$full_visual_field_RT, monkey$full_visual_field_RT[1]),angle_pos, lwd = 3, lty = 1, line.col = 'red',
             point.col = 'red', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 0, radial.lim=c(0,1))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkey$full_visual_field_RT, monkey$full_visual_field_RT[1])[i] * cos(angles_rad[i])
    y <- c(monkey$full_visual_field_RT, monkey$full_visual_field_RT[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey[i] * cos(angles_rad[i])
    dy <- sem_monkey[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = 'red', lwd = 4)
  }
  
  # Plot for monkeys
  polar.plot(c(human$full_visual_field_RT+0.5, human$full_visual_field_RT[1]+0.5),angle_pos, lwd = 3, lty = 1, line.col = 'black',
             point.col = 'black', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 1, radial.lim=c(0,1))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(human$full_visual_field_RT+0.5, human$full_visual_field_RT[1]+0.5)[i] * cos(angles_rad[i])
    y <- c(human$full_visual_field_RT+0.5, human$full_visual_field_RT[1]+0.5)[i] * sin(angles_rad[i])
    dx <- sem_human[i] * cos(angles_rad[i])
    dy <- sem_human[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = 'black', lwd = 4)
  }
  
  
}



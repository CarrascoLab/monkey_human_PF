fig_4 <- function(monkey) {
  
  # part 2: look at the attention effect
  monkey_cued <- apply(monkey$dprime_attention_conds[,2,,], c(2, 3), mean)
  monkey_neutral <- apply(monkey$dprime_attention_conds[,1,,], c(2, 3), mean)
  
  monkeycued_avg = apply(monkey_cued, 2, mean)
  monkey_neutral_avg = apply(monkey_neutral, 2, mean)
  
  sem_monkey_cued = std.error(monkey_cued);
  sem_monkey_neutral = std.error(monkey_neutral);
  
  # Define the angles
  angle_pos <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
  angles_rad <- angle_pos * pi / 180
  
  par(mfrow = c(2, 1))
  
  # Plot for monkeys
  polar.plot(c(monkeycued_avg, monkeycued_avg[1]),angle_pos, lwd = 3, lty = 1, line.col = '#4A7056',
             point.col = '#4A7056', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 0, radial.lim=c(0,3.5))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkeycued_avg, monkeycued_avg[1])[i] * cos(angles_rad[i])
    y <- c(monkeycued_avg, monkeycued_avg[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey_cued[i] * cos(angles_rad[i])
    dy <- sem_monkey_cued[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = '#4A7056', lwd = 4)
  }
  par(new=T)
  
  # Plot for monkeys
  polar.plot(c(monkey_neutral_avg, monkey_neutral_avg[1]),angle_pos, lwd = 3, lty = 1, line.col = '#807DBA',
             point.col = '#807DBA', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 1, radial.lim=c(0,3.5))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkey_neutral_avg, monkey_neutral_avg[1])[i] * cos(angles_rad[i])
    y <- c(monkey_neutral_avg, monkey_neutral_avg[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey_neutral[i] * cos(angles_rad[i])
    dy <- sem_monkey_neutral[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = '#807DBA', lwd = 4)
  }
  
  # RT:
  
  # part 2: look at the attention effect
  monkey_cued <- apply(monkey$reaction_time[,2,,], c(2, 3), mean)
  monkey_neutral <- apply(monkey$reaction_time[,1,,], c(2, 3), mean)
  
  monkeycued_avg = apply(monkey_cued, 2, mean)
  monkey_neutral_avg = apply(monkey_neutral, 2, mean)
  
  sem_monkey_cued = std.error(monkey_cued);
  sem_monkey_neutral = std.error(monkey_neutral);
  
  # Plot for monkeys
  polar.plot(c(monkeycued_avg, monkeycued_avg[1]),angle_pos, lwd = 3, lty = 1, line.col = '#4A7056',
             point.col = '#4A7056', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 0, radial.lim=c(0,1000))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkeycued_avg, monkeycued_avg[1])[i] * cos(angles_rad[i])
    y <- c(monkeycued_avg, monkeycued_avg[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey_cued[i] * cos(angles_rad[i])
    dy <- sem_monkey_cued[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = '#4A7056', lwd = 4)
  }
  par(new=T)
  
  # Plot for monkeys
  polar.plot(c(monkey_neutral_avg, monkey_neutral_avg[1]),angle_pos, lwd = 3, lty = 1, line.col = '#807DBA',
             point.col = '#807DBA', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 1, radial.lim=c(0,1000))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkey_neutral_avg, monkey_neutral_avg[1])[i] * cos(angles_rad[i])
    y <- c(monkey_neutral_avg, monkey_neutral_avg[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey_neutral[i] * cos(angles_rad[i])
    dy <- sem_monkey_neutral[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = '#807DBA', lwd = 4)
  }
  
  #### Bar plots ####
  # part 2: look at the attention effect
  monkey_cued <- apply(monkey$dprime_attention_conds[,2,,], c(2, 3), mean)
  monkey_neutral <- apply(monkey$dprime_attention_conds[,1,,], c(2, 3), mean)
  
  # Convert the matrix to a data frame
  monkey_cued_data_df <- as.data.frame(monkey_cued)
  monkey_neutral_data_df <- as.data.frame(monkey_neutral)
  
  # Name the columns
  colnames(monkey_cued_data_df) <- c("RHM","IC_NE","UVM","IC_NW","LHM","IC_SW", "LVM","IC_SE")
  colnames(monkey_neutral_data_df) <- c("RHM","IC_NE","UVM","IC_NW","LHM","IC_SW", "LVM","IC_SE")
  
  # Select the columns to plot
  columns_to_plot <- c("RHM","LHM","UVM","LVM")
  
  # Reshape the data for plotting
  monkey_cued_data_melt <- melt(monkey_cued_data_df[, columns_to_plot], variable.name = "Condition", value.name = "Value")
  monkey_neutral_data_melt <- melt(monkey_neutral_data_df[, columns_to_plot], variable.name = "Condition", value.name = "Value")
  
  # Set the order of the levels
  order_levels <- c("UVM", "LVM", "LHM", "RHM")
  monkey_cued_data_melt$Condition <- factor(monkey_cued_data_melt$Condition, levels = order_levels)
  monkey_neutral_data_melt$Condition <- factor(monkey_neutral_data_melt$Condition, levels = order_levels)
  
  # Calculate the mean and SEM for each condition
  monkey_cued_summary <- monkey_cued_data_melt %>%
    group_by(Condition) %>%
    dplyr::summarize(Mean = mean(Value), SEM = sd(Value) / sqrt(length(Value)))
  
  monkey_neutral_summary <- monkey_neutral_data_melt %>%
    group_by(Condition) %>%
    dplyr::summarize(Mean = mean(Value), SEM = sd(Value) / sqrt(length(Value)))
  
  # Create bar plots with error bars
  plot_cued <- ggplot(monkey_cued_summary, aes(x=Condition, y=Mean, fill=Condition)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), width=0.2) +
    labs(title="Monkey cued Data", x="Condition", y="Mean Value") +
    ylim(0, 3.6) +
    theme_minimal() +
    theme(axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  plot_neutral <- ggplot(monkey_neutral_summary, aes(x=Condition, y=Mean, fill=Condition)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), width=0.2) +
    labs(title="Monkey neutral Data", x="Condition", y="Mean Value") +
    ylim(0, 3.6) +
    theme_minimal() +
    theme(axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  # Combine plots into one figure
  combined_plot <- plot_grid(
    plot_cued + ggtitle("Panel A: Monkey cued Data"),
    plot_neutral + ggtitle("Panel B: Monkey neutral Data"),
    labels = c("A", "B"),
    ncol = 2
  )
  
  # Print the combined plot
  print(combined_plot)

}


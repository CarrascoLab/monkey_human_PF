fig_3 <- function(monkey) {
  
  monkey_fellow_data <- monkey$dprime[1,,]
  monkey_fellow_data <- monkey_fellow_data[-2,] # removes the control animal
  monkey_amb_data <- monkey$dprime[2,,]
  monkey_amb_data <- monkey_amb_data[-2,]
  
  monkey.amb.norm <- monkey_amb_data 
  monkey.fellow.norm <- monkey_fellow_data
  
  monkeyamb = apply(monkey.amb.norm, 2, mean)
  monkeyfellow = apply(monkey.fellow.norm, 2, mean)
  
  # Add error bars for monkeys
  sem_monkey_amb = std.error(monkey_amb_data);
  sem_monkey_fellow = std.error(monkey_fellow_data);
  
  # Define the angles
  angle_pos <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
  angles_rad <- angle_pos * pi / 180
  
  par(mfrow = c(2, 1))
  
  # Plot for monkeys
  polar.plot(c(monkeyamb, monkeyamb[1]),angle_pos, lwd = 3, lty = 1, line.col = '#4A7056',
             point.col = '#4A7056', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 0, radial.lim=c(0,3))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkeyamb, monkeyamb[1])[i] * cos(angles_rad[i])
    y <- c(monkeyamb, monkeyamb[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey_amb[i] * cos(angles_rad[i])
    dy <- sem_monkey_amb[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = '#4A7056', lwd = 4)
  }
  par(new=T)
  polar.plot(c(monkeyfellow, monkeyfellow[1]),angle_pos, lwd = 3, lty = 1, line.col = '#807DBA',
             point.col = '#807DBA', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 1, radial.lim=c(0,3))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkeyfellow, monkeyfellow[1])[i] * cos(angles_rad[i])
    y <- c(monkeyfellow, monkeyfellow[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey_fellow[i] * cos(angles_rad[i])
    dy <- sem_monkey_fellow[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = '#807DBA', lwd = 4)
  }
  
  
  #graph2ppt(file="supp_1_A_monkeytwoeyes_dprime.pptx", width=5, height=5) 
  
  # look at the RT data
  monkeyrt_across_att <- apply(monkey$reaction_time, c(1, 3, 4), mean)
  monkey_fellow_data <- monkeyrt_across_att[1,,]
  monkey_fellow_data <- monkey_fellow_data[-2,]
  monkey_amb_data <- monkeyrt_across_att[2,,]
  monkey_amb_data <- monkey_amb_data[-2,]
  
  
  monkey.amb.norm <- monkey_amb_data 
  monkey.fellow.norm <- monkey_fellow_data 
  
  monkeyamb = apply(monkey.amb.norm, 2, mean)
  monkeyfellow = apply(monkey.fellow.norm, 2, mean)
  
  # Add error bars for monkeys
  sem_monkey_amb = std.error(monkey_amb_data);
  sem_monkey_fellow = std.error(monkey_fellow_data);
  
  # Define the angles
  angle_pos <- c(0, 45, 90, 135, 180, 225, 270, 315, 360)
  angles_rad <- angle_pos * pi / 180
  
  # Plot for monkeys
  polar.plot(c(monkeyamb, monkeyamb[1]),angle_pos, lwd = 3, lty = 1, line.col = '#4A7056',
             point.col = '#4A7056', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 0, radial.lim=c(0,900))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkeyamb, monkeyamb[1])[i] * cos(angles_rad[i])
    y <- c(monkeyamb, monkeyamb[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey_amb[i] * cos(angles_rad[i])
    dy <- sem_monkey_amb[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = '#4A7056', lwd = 4)
  }
  par(new=T)
  polar.plot(c(monkeyfellow, monkeyfellow[1]),angle_pos, lwd = 3, lty = 1, line.col = '#807DBA',
             point.col = '#807DBA', 
             point.symbols = 1, cex = 1, show.grid = TRUE, rp.type = "spline", 
             labels = c(0, 45, 90, 135, 180, 225, 270, 315), show.grid.labels = 1, radial.lim=c(0,900))
  
  
  for (i in seq_along(angle_pos)) {
    x <- c(monkeyfellow, monkeyfellow[1])[i] * cos(angles_rad[i])
    y <- c(monkeyfellow, monkeyfellow[1])[i] * sin(angles_rad[i])
    dx <- sem_monkey_fellow[i] * cos(angles_rad[i])
    dy <- sem_monkey_fellow[i] * sin(angles_rad[i])
    
    # Offset starting and ending points of error bars
    x_start <- x - dx/2
    y_start <- y - dy/2
    x_end <- x + dx/2
    y_end <- y + dy/2
    
    arrows(x_start, y_start, x_end, y_end, angle = 90, code = 3, length = 0, col = '#807DBA', lwd = 4)
  }
  
  #graph2ppt(file="savingDraft.pptx", width=5, height=5) 
  
  # Fig 3 A: Bar plots
  # Convert the matrix to a data frame
  monkey_fellow_data <- monkey$dprime[1,,]
  monkey_amb_data <- monkey$dprime[2,,]
  monkey_fellow_data <- monkey_fellow_data[-2,]
  monkey_amb_data <- monkey_amb_data[-2,]
  monkey_amb_data_df <- as.data.frame(monkey_amb_data)
  monkey_fellow_data_df <- as.data.frame(monkey_fellow_data)
  
  # Name the columns
  colnames(monkey_amb_data_df) <- c("RHM","IC_NE","UVM","IC_NW","LHM","IC_SW", "LVM","IC_SE")
  colnames(monkey_fellow_data_df) <- c("RHM","IC_NE","UVM","IC_NW","LHM","IC_SW", "LVM","IC_SE")
  
  # Select the columns to plot
  columns_to_plot <- c("RHM","LHM","UVM","LVM")
  
  # Reshape the data for plotting
  monkey_amb_data_melt <- melt(monkey_amb_data_df[, columns_to_plot], variable.name = "Condition", value.name = "Value")
  monkey_fellow_data_melt <- melt(monkey_fellow_data_df[, columns_to_plot], variable.name = "Condition", value.name = "Value")
  
  # Set the order of the levels
  order_levels <- c("UVM", "LVM", "LHM", "RHM")
  monkey_amb_data_melt$Condition <- factor(monkey_amb_data_melt$Condition, levels = order_levels)
  monkey_fellow_data_melt$Condition <- factor(monkey_fellow_data_melt$Condition, levels = order_levels)
  
  # Calculate the mean and SEM for each condition
  monkey_amb_summary <- monkey_amb_data_melt %>%
    group_by(Condition) %>%
    dplyr::summarize(Mean = mean(Value), SEM = sd(Value) / sqrt(length(Value)))
  
  monkey_fellow_summary <- monkey_fellow_data_melt %>%
    group_by(Condition) %>%
    dplyr::summarize(Mean = mean(Value), SEM = sd(Value) / sqrt(length(Value)))
  
  # Create bar plots with error bars
  plot_amb <- ggplot(monkey_amb_summary, aes(x=Condition, y=Mean, fill=Condition)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), width=0.2) +
    labs(title="Monkey Amb Data", x="Condition", y="Mean Value") +
    ylim(0, 3) +
    theme_minimal() +
    theme(axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  plot_fellow <- ggplot(monkey_fellow_summary, aes(x=Condition, y=Mean, fill=Condition)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), width=0.2) +
    labs(title="Monkey Fellow Data", x="Condition", y="Mean Value") +
    ylim(0, 3) +
    theme_minimal() +
    theme(axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  # Combine plots into one figure
  combined_plot <- plot_grid(
    plot_amb + ggtitle("Panel A: Monkey Amb Data"),
    plot_fellow + ggtitle("Panel B: Monkey Fellow Data"),
    labels = c("A", "B"),
    ncol = 2
  )
  
  # Print the combined plot
  print(combined_plot)
}

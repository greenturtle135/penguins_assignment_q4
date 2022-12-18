
# Plot the ANCOVA figure 
plot_ANCOVA_plot <- function(penguins_mass_flipper) {
  penguins_mass_flipper %>%
     ggplot(aes(x = body_mass_g, y = flipper_length_mm, colour = species)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm") +
      labs(title= "ANCOVA Model Plot", x = 'Body mass (g)',  
           y = 'Flipper length (mm)') +
      guides(color = guide_legend(title = "Species"))+
      scale_color_manual(labels = c("Adelie", "Chinstrap", "Gentoo"), values = c("#009E73", "#E69F00", "#0072B2"))+
      theme_bw()+ 
    theme(plot.title = element_text(hjust = 0.5)) +
      stat_poly_line() +
      stat_poly_eq(aes(label = paste(after_stat(eq.label), sep = "*\", \"*")))}
    

# Save ANCOVA figure as png and define the size, resolution, and scaling
save_ANCOVA_plot_png <-function(penguins_mass_flipper, 
                                filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  penguins_ANCOVA_plot <- plot_ANCOVA_plot(penguins_mass_flipper)
  print(penguins_ANCOVA_plot)
  dev.off()
}

# Save ANCOVA figure as a svg and define the size and scaling
save_ANCOVA_plot_svg <- function(penguins_mass_flipper, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  penguins_ANCOVA_plot <- plot_ANCOVA_plot(penguins_mass_flipper)
  print(penguins_ANCOVA_plot)
  dev.off()
}

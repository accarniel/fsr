#library('ggspatial')
#library('ggplot2')

# first_plot 


# teste
plot(spo_test@supp)


# spo_point_df <- spo_as_tibble(spo_test)
# 
# spo_line_df <- spo_as_tibble(spo_line)


plot_spo(spo_test)

plot_spo(spo_line)













# fline <- ggplot(spo_as_tibble(spo_line)) + 
#   geom_sf(aes(colour = md, geometry=geometry), size = 2) + theme_classic() +
#   scale_colour_gradient(name="Membership Degree", limits = c(0, 1), low = "white", high = "black")
# 
# 
# 
# 
# 
# 
# 
# 
# colnames(fuzzy_df) <- c('md', 'geometry')
# colnames(fuzzy_df_lines) <- c('md', 'geometry')
# 
# 
# ggplot() +
#   layer_spatial(fuzzy_df, aes(col= md)) +
#   labs(title = "Spatial Plateau Algebra", x = 'x', y = 'y', color = "Membership Degree") +
#   scale_colour_gradient(name="", limits = c(0, 1), low = "white", high = "black")
#   
# 
# 
# fpoint <- ggplot(fuzzy_df) + 
#         geom_sf(aes(colour = md, geometry=geom), size = 2) + theme_classic() +
#         scale_colour_gradient(name="Membership Degree", limits = c(0, 1), low = "white", high = "black")
# 
# fline <- ggplot(spo_as_tibble(spo_line)) + 
#   geom_sf(aes(colour = md, geometry=geometry), size = 2) + theme_classic() +
#   scale_colour_gradient(name="Membership Degree", limits = c(0, 1), low = "white", high = "black")
# 
# 
# 
# 
# 
# ggplot() +
#   layer_spatial(fuzzy_df_lines, aes(col= md)) +
#   labs(title = "Spatial Plateau Algebra", x = 'x', y = 'y', color = "Membership Degree") +
#   scale_fill_continuous(low = "#132B43", high = "#56B1F7")
# 
# 
# 
# 





library('ggspatial')
library('ggplot2')
library(comprehenr)
# first_plot 



plot(spo_test@supp)

points <- to_list(for(point in spo_test@component) point@obj)




fuzzy_df <- data.frame(
  md <- to_vec(for(comp in spo_test@component) comp@md),
  points <- st_sfc(to_list(for(point in spo_test@component) point@obj))
)


fuzzy_df_lines <- data.frame(
  md <- to_vec(for(comp in spo_line@component) comp@md),
  points <- st_sfc(to_list(for(point in spo_line@component) point@obj))
)




###

# 1. Sempre usar tibble ao invés de DF. 
# 2. Nome da legenda (md) = Membership Degree
# 3. Os ticks da legendas (0 a 1)
# 4. Params função :
#### 4.1   spo
#### 4.2  colour (opcional) <- default (grayscale)
#### ## Reparar na escala de cor (mais escura + maior md)

### Return (plot)

colnames(fuzzy_df) <- c('md', 'geometry')
colnames(fuzzy_df_lines) <- c('md', 'geometry')
ggplot() +
  layer_spatial(fuzzy_df, aes(col= md))

ggplot() +
  layer_spatial(fuzzy_df_lines, aes(col= md))



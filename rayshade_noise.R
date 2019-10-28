library(tidyverse)
library(rayshader)
library(ambient)

grid <-
  long_grid(x = seq(0, 10, length.out = 100), 
            y = seq(0, 10, length.out = 100)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 3) /2, 
    y1 = y + gen_perlin(x = x, y = y, frequency = 2) /3,
    simplex_warp = fracture(gen_worley, ridged, octaves = 8, x = x1, y = y1, value = "distance2sub", distance = "euclidean",
                            distance_ind = c(1, 2), jitter = 0.4, frequency = 0.5),
    billow_warp = fracture(gen_perlin, billow, octaves = 7, x = x1, y = y1)
  )

grid_mat <- as.matrix(grid, x, value = billow_warp)

grid <-
  long_grid(x = seq(0, 10, length.out = 100), 
            y = seq(0, 10, length.out = 100)) %>% 
  mutate(noise = fracture(gen_simplex, billow, x = x, y = y, octaves = 8))

plot(as.raster(grid, normalise(noise)))

grid_mat <- as.matrix(grid, x, value = noise)

ambmat = ambient_shade(grid_mat)

grid_mat %>%
  sphere_shade(texture = "imhof1") %>%
  #add_water(detect_water(grid_mat), color = "desert") %>%
  add_shadow(ray_shade(grid_mat, zscale = 0.5, maxsearch = 300), 0.5) %>%
  add_shadow(ambmat, 0.5) %>%
  plot_3d(grid_mat, zscale = 0.3, theta = 135, zoom = 0.75, phi = 45)

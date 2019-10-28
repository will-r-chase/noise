library(tidyverse)
library(ambient)

grid <- long_grid(1, seq(1, 10, length.out = 1000))
grid$noise <- fracture(gen_perlin, fbm, octaves = 5, x = grid$x, y = grid$y)

line <- data.frame(x = seq(1:1000), y = grid$noise[1:1000])

ggplot(line) +
  geom_line(aes(x = x, y = y), size = 2) +
  ylim(c(-100, 100)) + 
  theme_void()

ggsave("test1.png")


#trying to draw lines
a <- c(2, 3)
b <- c(4, 4)
v <- b - a
df <- data.frame(x = 1:9, y = 1:9)
for (i in 1:9) {
  p <- a + v * i / 9L
  df$x[i] <- p[1]
  df$y[i] <- p[2]
}

ggplot(df) +
  geom_point(aes(x, y))

#worley noise with rotation applied
grid2 <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))
grid2$rotate <- trans_affine(grid2$x, grid2$y, rotate(0.05))
grid2$noise <- gen_worley(grid2$rotate$x, grid2$rotate$y, value = 'distance')
plot(as.raster(grid2, normalise(noise)))

ggplot() +
  geom_raster(data = grid2, aes(x = x, y = y, fill = noise)) +
  scale_fill_gradient2(low = "white", high = "pink") +
  theme_void()

ggsave("worley.png", width = 10, height = 10)

#simplex noise rounded to 0, -1, 1
grid <- long_grid(seq(1, 10, length.out = 3000), seq(1, 10, length.out = 6000))
grid$noise <- gen_simplex(grid$x, grid$y)
grid$threshold <- round(grid$noise, digits = 0)
plot(as.raster(grid, normalize(threshold)))

ggplot() +
  geom_raster(data = grid, aes(x = x, y = y, fill = normalize(threshold))) +
  #geom_raster(data = grid2, aes(x = x, y = y, fill = noise)) +
  scale_fill_gradientn(colors = c("#DC1F24", "#EDE8E8","#4BC4CB"), guide = "none") +
  #scale_fill_gradient2(low = "white", high = "pink") +
  theme_void()

ggsave("threshold.png", width = 10, height = 10)

#some more worley testing
grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(noise = gen_worley(x, y, value = "distance2sub", distance = "euclidean", distance_ind = c(1, 2), jitter = 0.45),
         fracture = fracture(gen_worley, ridged, octaves = 8, x = x, y = y, value = "distance2div", distance = "euclidean",
                             distance_ind = c(1, 2), jitter = 0.4))

grid <- noise_worley(c(1000,1000), frequency = 0.01, distance = "euclidean",
             fractal = "none", octaves = 3, lacunarity = 2, gain = 0.5,
             value = "distance2add", distance_ind = c(1, 10), jitter = 0.4,
             pertubation = "normal", pertubation_amplitude = 50)
plot(as.raster(normalize(grid)))

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = fracture)) +
  #scale_fill_gradientn(colors = c("black", "#0E6D6C"), guide = "none") +
  theme_void() +
  theme(legend.position = "none")

ggsave("worley_d2d_eu_1-2_04_frac_ridged_oc8.png", width = 10, height = 10)


#testing diff fractals on simplex noise
grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(noise = gen_simplex(x, y),
         fracture = fracture(gen_simplex, ridged, octaves = 3, x = x, y = y))

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = fracture)) +
  #scale_fill_gradientn(colors = c("black", "#0E6D6C"), guide = "none") +
  theme_void() +
  theme(legend.position = "none")

##testing blending
grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
          y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_simplex(x, y) / 2, 
    y1 = y + gen_simplex(x, y) * 2,
    worley = gen_worley(x, y, value = 'distance2mul', jitter = 0.5),
    simplex_frac = fracture(gen_simplex, ridged, octaves = 10, x = x, y = y),
    full = blend(normalise(worley), normalise(simplex_frac), gen_spheres(x1, y1))
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = full)) +
  scale_fill_gradientn(colors = c("black", "#DC1F24", "#EDE8E8","#4BC4CB"), guide = "none") +
  theme_void() +
  theme(legend.position = "none", plot.background = element_blank(), panel.background = element_blank())

ggsave("stranger_things.png", width = 10, height = 10)

#more blending
grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + fracture(gen_perlin, fbm, octaves = 5, x= x, y = y) / 1.2, 
    y1 = y + fracture(gen_perlin, fbm, octaves = 5, x= x, y = y) * 1.2,
    worley = gen_worley(x, y, value = 'distance2mul', jitter = 0.5),
    simplex_frac = fracture(gen_simplex, billow, octaves = 10, x = x, y = y),
    full = blend(normalise(worley), normalise(simplex_frac), gen_waves(x1, y1))
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = full)) +
  scale_fill_gradientn(colors = c("black", "#DC1F24", "#EDE8E8","#4BC4CB"), guide = "none") +
  theme_void() +
  theme(legend.position = "none", plot.background = element_blank(), panel.background = element_blank())

ggsave("stranger_things3.png", width = 10, height = 10)

##clouds
grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(noise = gen_simplex(x, y),
         fracture = fracture(gen_perlin, fbm, octaves = 7, gain = ~./2, frequency = ~.*2, x = x, y = y))

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = fracture)) +
  #scale_fill_gradientn(colors = c("black", "#0E6D6C"), guide = "none") +
  theme_void() +
  theme(legend.position = "none")

##warping
grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 3) /2, 
    y1 = y + gen_perlin(x = x, y = y, frequency = 2) /3,
    simplex_warp = fracture(gen_worley, ridged, octaves = 8, x = x1, y = y1, value = "distance2sub", distance = "euclidean",
                            distance_ind = c(1, 2), jitter = 0.4, frequency = 0.5),
    billow_warp = fracture(gen_perlin, billow, octaves = 7, x = x1, y = y1)
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = simplex_warp)) +
  scale_fill_gradientn(colors = c('#253852', '#51222f', '#b53435', '#ecbb51', "#eeccc2"), guide = "none") +
  theme_void() +
  theme(legend.position = "none")

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = billow_warp)) +
  scale_fill_gradient2(low = "#1EB925", high = "#ABE2A6", guide = "none") +
  theme_void() +
  theme(legend.position = "none")

ggsave("minty.png", width = 10, height = 10)

##super warp
grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 1), 
    y1 = y + gen_perlin(x = x, y = y, frequency = 2),
    x2 = x1 + gen_simplex(x = x1, y = y1, frequency = 1),
    y2 = y1 + gen_simplex(x = x1, y = y1, frequency = 3),
    simplex_warp = gen_simplex(x = x1, y = y2)
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = simplex_warp)) +
  scale_fill_gradientn(colors = c('#253852', '#51222f', '#b53435', '#ecbb51', "#eeccc2"), guide = "none") +
  theme_void() +
  theme(legend.position = "none")

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = simplex_warp)) +
  scale_fill_gradientn(colors = c("#c71313", "#f0c029", "#148727", "#398cab", "#131078"), guide = "none") +
  theme_void() +
  theme(legend.position = "none")

ggsave("melted_gold.png", width = 10, height = 10)

#melted crayons
crayons <- c("#EE204D", "#1F75FE", "#1CAC78", "#926EAE", "#FCE883", "#FF7538", "#B4674D", "#232323")
grid <-
  long_grid(x = seq(0, 10, length.out = 1000), 
            y = seq(0, 10, length.out = 1000)) %>% 
  mutate(
    x1 = x + gen_perlin(x = x, y = y, frequency = 1.5), 
    y1 = y + gen_perlin(x = x, y = y, frequency = 10),
    simplex_warp = gen_simplex(x = x1, y = y1),
    threshold = round(simplex_warp, digits = 1)
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = threshold)) +
  scale_fill_gradientn(colors = crayons, guide = "none") +
  theme_void() +
  theme(legend.position = "none")


#bubbles??
grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(
         x1 = x + fracture(gen_worley, ridged, octaves = 8, x = x, y = y, value = "distance2div", distance = "euclidean",
                           distance_ind = c(1, 2), jitter = 0.4),
         y1 = y + fracture(gen_worley, ridged, octaves = 8, x = x, y = y, value = "distance2div", distance = "euclidean",
                           distance_ind = c(1, 2), jitter = 0.4),
         simplex_warp = gen_worley(x = x1, y = y1, value = "distance")
         )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = simplex_warp)) +
  theme_void() +
  theme(legend.position = "none")

ggsave("bubbles_1.png", width = 10, height = 10)


grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000)) %>%
  mutate(
    x1 = x + fracture(gen_worley, ridged, octaves = 8, x = x, y = y, value = "distance2div", distance = "euclidean",
                      distance_ind = c(1, 2), jitter = 0.5),
    y1 = y + fracture(gen_worley, ridged, octaves = 8, x = x, y = y, value = "distance2div", distance = "euclidean",
                      distance_ind = c(1, 3), jitter = 0.4),
    worley_warp = gen_worley(x = x1, y = y1, value = "distance", jitter = 0.4, distance = "manhattan"),
    worley_warp2 = fracture(gen_worley, ridged, octaves = 8, x = x1, y = y1, value = "distance2div", distance = "euclidean",
                            distance_ind = c(1, 2), jitter = 0.5),
    cubic = gen_cubic(x = x * 3, y = y / 3),
    blend = blend(normalize(cubic), worley_warp, worley_warp2)
  )

ggplot() + 
  geom_raster(data = grid, aes(x, y, fill = blend)) +
  scale_fill_gradientn(colors = c('#f0efe2', '#363d4a', '#7b8a56', '#ff9369', '#f4c172'), guide = "none") +
  theme_void() +
  theme(legend.position = "none")

ggsave("bubbles_2.png", width = 10, height = 10)




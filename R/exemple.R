require(raster)
require(ggplot2)
require(slideR)

# Costanza with slideR
# 10x10
f1 <- "~/Downloads/costanza.tif"
f2 <- "~/Downloads/costanza2.tif"
f2 <- f1
# 4800x4800
f1 = "~/Downloads/cerrado_h13v10_t01_h-all_v-all.tif"
f2 = "~/Downloads/cerrado_h13v10_t02_h-all_v-all.tif"

r1 <- raster::raster(f1)
r2 <- raster::raster(f2)

plot(r1)
plot(r2)

m1 <- matrix(r1, nrow = nrow(r1))
m2 <- matrix(r2, nrow = nrow(r2))


a <- costanza(m1, m2, resolution = 1:10)
win2(1:25, dim = c(5,5), from = c(2,2), to = c(4, 4))

ggplot(a) + 
    geom_line(aes(resolution, 
                  class_acc, 
                  colour = as.factor(classes), 
                  group = classes)) +
    geom_line(aes(resolution,
                  global_acc))

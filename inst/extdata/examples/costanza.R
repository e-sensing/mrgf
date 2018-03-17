require(raster)
require(ggplot2)
require(mrgf)

# Costanza with 10x10 images
f1 <- system.file("extdata/tif/costanza1.tif", package = "mrgf")
f2 <- system.file("extdata/tif/costanza2.tif", package = "mrgf")

r1 <- raster::raster(f1)
r2 <- raster::raster(f2)

plot(r1)
plot(r2)

m1 <- matrix(r1, nrow = nrow(r1))
m2 <- matrix(r2, nrow = nrow(r2))


a <- costanza(m1, m2, resolution = 1:10)

ggplot(a) + 
    geom_line(aes(resolution, 
                  class_acc, 
                  colour = as.factor(classes), 
                  group = classes)) +
    geom_line(aes(resolution,
                  global_acc))

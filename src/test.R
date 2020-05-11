r <- raster(ncol=10, nrow=10)
# assign values to cells
values(r) <- 1:ncell(r)
plot(r)
s <- r + 10
s <- sqrt(s)
plot(s)
s <- s * r + 5
r[] <- runif(ncell(r))
r <- round(r)
r <- r == 1

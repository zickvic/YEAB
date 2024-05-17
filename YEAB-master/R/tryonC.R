tryon <- function(datos) {
  diferencia <- sum((datos[1:(length(datos) - 1)] - datos[2:length(datos)])^2)
  diferencia2 <- 2 * (sum((datos - mean(datos))^2))

  c <- 1 - (diferencia / diferencia2)

  sc <- sqrt((length(datos) - 2) / ((length(datos) - 1) * (length(datos) + 1)))

  z <- c / sc

  if (z < 0) p <- pnorm(z) else p <- 1 - pnorm(z)

  return(list(z = z, p.value = p))
}


# tryon(rn[7:12])

# ## tryon data

# d = c(28,46,39,45,24,20,35,37,36,40,
#       24,16,37,45,18,19,18,18,13.12,15,13,15,16,11,14,14,12,13,14,17,16,
#       15,21,16,23,20,26,26,22,15,24)
# plot(d, type = 'l')

# cpm = cpt.meanvar(as.integer(d), test.stat = 'Poisson', method = 'BinSeg')

# abline(v = cpm@cpts, col = 2)



# ss = seq(10, length(d), 10)
# cps_tryon = numeric(length(ss))

# for (ii in 1:length(ss)) {
#   i = ss[ii]
#   dtmp = d[1:i]
#   z = tryon(dtmp)
#   if (z[[2]] <= 0.01) {
#     print(i)
#     cps_tryon[ii] = i
#   }
# }
# plot(d, type = 'l')
# abline(v = cps_tryon)

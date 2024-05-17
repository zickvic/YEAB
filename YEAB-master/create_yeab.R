library(usethis)
library(devtools)
# setwd("../") # fijar wd un directorio antes
use_description(
  fields = list(
    Title = "Package To Analyze Data From Analysis Of Behavior Experiments",
    Description = "This is a colletion of functions aimed to analyze data from behavioral
                   experiments from MED output and others. It also have functions to fit exponential or
                   hyperbolic models from delay discounting tasks, exponential mixtures to IRTs, Gaussian
                  plus ramp model for peak procedures data, etc.",
    `Authors@R` = c(
      person(
        "Emmanuel", "Alcala",
        email = "jealcala@gmail.com",
        role = c("aut", "cre")
      ),
      person(
        "Rodrigo", "Sosa",
        email = "rsosas@up.edu.mx",
        role = "aut"
      )
    ),
    License = "CC0"
  )
)


pck_import <- c(
  "dplyr", "zoo", "minpack.lm", "scales", "magrittr",
  "ggplot2", "VGAM", "grid", "gridExtra", "cluster",
  "ks", "rmi", "infotheo", "Polychrome", "foreach",
  "doParallel", "sfsmisc"
)

sapply(pck_import, use_package)

# peak data
r_times <- c(
  28.1, 40.7, 44.2, 44.4, 44.7, 45, 45.4, 47.9, 48.1, 48.3, 48.6, 48.8,
  49.8, 50.2, 50.7, 51.2, 51.4, 51.7, 51.9, 52.7, 53, 53.5, 53.7, 53.9,
  54.1, 54.3, 54.9, 55.3, 55.5, 55.7, 55.8, 57.2, 57.4, 57.7, 58.3, 58.5,
  58.7, 60.4, 60.6, 60.7, 61.1, 61.6, 61.8, 62.6, 62.8, 63.1, 63.3, 63.5,
  63.8, 64.4, 64.8, 64.9, 65.1, 66.1, 66.4, 67, 68.7, 68.9, 69.5, 69.6,
  70.1, 70.9, 71, 71.3, 71.6, 71.8, 73.9, 74.1, 74.4, 74.6, 75.2, 76.4,
  76.6, 77.4, 77.6, 77.8, 78.2, 79.3, 79.9, 80.5, 80.7, 81.3, 82.2, 82.4,
  82.6, 82.9, 83, 83.1, 83.7, 84.4, 84.4, 84.8, 85, 85.6, 86.6, 87, 87.1,
  87.3, 87.4, 87.8, 88.1, 88.2, 89.4, 99.1, 99.3, 99.6, 99.8, 100.2,
  133.1, 133.1, 133.6, 134.9, 135.2, 135.3, 135.4, 135.7, 136.5, 173.8,
  174.1, 174.3, 174.7, 175.9, 176.3, 176.6, 177.4, 177.5, 177.7, 178.1,
  178.2, 178.4, 178.5, 178.8, 179.4
)

use_data(r_times)

# raw FI interval data --
# fileName <- "path_to_/2018-04-30_12h28m_Subject M333.txt"
# connection <- file(fileName,open = "r")
# fi60_raw_from_med <-readLines(connection)
# use_data(fi60_raw_from_med)

## creación de dataset para ajuste hiperbólico

fn_hyp <- function(delay, k, v0) {
  v0 / (1 + k * delay)
}

v0 <- 900
k <- 0.5
delay_real <- seq(0, 100, len = 2000)

real_sv <- fn_hyp(delay_real, k, v0)
delay <- seq(0, 100, len = 20)

sv_with_noise <- fn_hyp(delay, k, v0) + rnorm(length(delay), 0, 40)
# normalizar
real_sv_norm <- real_sv / max(real_sv)
sv_with_noise_norm <- unity_normalization(sv_with_noise)
plot(delay_real, real_sv_norm, type = "l", ylim = c(0, 1))
points(delay, sv_with_noise_norm)

hyp_data_list <- list(
  sv = sv_with_noise_norm,
  delay = delay,
  real_k = k
)

usethis::use_data(hyp_data_list)

DD_data <- data.frame(
  norm_sv = c(0.934, 0.746, 0.746, 0.488, 0.684, 0.441),
  Delay = c(1, 6, 12, 36, 60, 120)
)

usethis::use_data(DD_data)

# install again

devtools::document()
devtools::install_github("jealcalat/YEAB", force = TRUE)

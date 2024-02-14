
library(devtools)
library(tidyverse)
library(boot)

load_all()
data(ar_data)
ar_data |>
  prox_matrix(legislature = 15)

ar_data |>
  prox_matrix()

ar_data %>% count(partido)
#---------------------------------------------------------------------#





#-------------------------------------------------------------
# Bootstrap by those proximity matrices ####


library(devtools)
library(tidyverse)
library(boot)

load_all()

data("ar_data")

df_nest <- ar_data |>
  filter(legis == 15) %>%
  prox_by_bill(type = "tibble")

df_nest %>% boot_prox(indices = 1:11)

res <- boot::boot(data = df_nest,
                  statistic = boot_prox,
                  R = 1000,
                  stype = "i",
                  sim = "ordinary" # I must explore what this means
)

# standard errors are They are calculated by print.boot and not stored in the boot object.
# I can calulate them my self: ( for the case of only one:)
# the original:
res$t0

# comparing to this, we see the output is from top to bottom, left to right, on the lower triangular matrix
ar_data %>%
  prox_matrix(legislature = 15)

# bias:
mean(res$t)-res$t0

#se:
sd(res$t)

# build a matrix back
list_ci <- 1:length(res$t0) %>%
  map(~{
    a <- boot.ci(res, type = "bca", index = .x)
    a$bca[4:5] #return
  })

# list_ci has the CI for each combination.
# I must turn it into 2 matrices.
ci_vec <- list_ci %>% unlist()
low_ci <- ci_vec[seq(1, length(res$t0)*2, by = 2)]
high_ci <- ci_vec[seq(2, length(res$t0)*2, by = 2)]

# (n*n-n)/2 = length(res$t0)
# using the quadratic formula:
quad <- function(a, b, c)
{
  a <- as.complex(a)
  answer <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
              (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
  if(all(Im(answer) == 0)) answer <- Re(answer)
  if(answer[1] == answer[2]) return(answer[1])
  answer
}

n <- quad(a = 1, b = -1, c = -2*length(res$t0))
# keep only the positive answer
n <- n[n > 0]

# create a matrix to fill
prox_mat_low <- matrix(0, nrow = n, ncol = n)
prox_mat_high <- matrix(0, nrow = n, ncol = n)

prox_mat_low[lower.tri(prox_mat_low, diag = F)] <- low_ci
prox_mat_high[lower.tri(prox_mat_high, diag = F)] <- high_ci
prox_mat_low
prox_mat_high
# is this correct?




#---------------------------------------------#
# full sample!!
load_all()

df <- read_ar_data("df_1st_vote", path = "../Project_data_AR")

df %>%
  prox_by_bill()











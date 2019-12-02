library(dplyr)

set.seed(1)

media_gp_fla = 48/23
media_gc_fla = 20/23
media_gp_gre = 37/23
media_gc_gre = 25/23
lambda_fla = mean(c(media_gp_fla, media_gc_gre))
lambda_gre = mean(c(media_gp_gre, media_gc_fla))

N = 1000000
fla = rpois(N, lambda_fla)
gre = rpois(N, lambda_gre)

res = tibble(Flamengo = fla, Gremio = gre) %>%
  count(Flamengo, Gremio) %>%
  mutate(p = round(100*n/N, 0)) %>%
  select(-n) %>%
  arrange(desc(p))

I = 2

res = res %>%
  mutate(palette = p %/% 2 + 1)

# https://imagecolorpicker.com/
palette = tibble(palette = c(1,2,3,4,5,7),
                 rgb = c("255,255,197",
                         "254,233,141",
                         "255,210,93",
                         "254,162,59",
                         "252,120,41",
                         "248,51,42"))

res = res %>%
  inner_join(palette)

pfla = round(100*sum(fla > gre)/N)

pemp = round(100*sum(fla == gre)/N)

pgre = round(100*sum(fla < gre)/N)

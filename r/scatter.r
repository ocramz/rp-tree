library(tidyverse)

fname <- "scatter_data.csv"
plot_path <- "scatter.png"

df <- read_csv( fname , col_names=c( 'x', 'y', 'i'), col_types=c( col_double(), col_double(), col_integer()))

df

df %>%
    ggplot( aes(x, y) ) +
    geom_point(
        mapping = aes( x = x, y = y, color = i )
    )


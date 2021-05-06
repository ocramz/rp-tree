library(tidyverse)

fname <- "scatter_data.csv"
plot_path <- "scatter.png"


scatter <- function( fin, fout){
    df <- read_csv( fin , col_names=c( 'x', 'y', 'i'), col_types=c( col_double(), col_double(), col_integer()))

    df %>%
        ggplot( aes(x, y) ) +
        geom_point(
            mapping = aes( x = x, y = y, color = i),
            alpha = 1.0,
            size = 0.2
        ) +
        theme_bw()

    ggsave( fout )
}

scatter( "scatter_data.csv", "scatter0.pdf" )
scatter( "scatter_data_rt2.csv", "scatter1.pdf" )


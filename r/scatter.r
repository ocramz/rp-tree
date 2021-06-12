library(tidyverse)

theme_set(theme_bw())  # pre-set the bw theme.

scatter <- function( fin, fout){
    df <- read_csv( fin , col_names=c( 'x', 'y', 'i'), col_types=c( col_double(), col_double(), col_integer()))

    df %>%
        ggplot( aes(x, y) ) +
        geom_point(
            mapping = aes( x = x, y = y, color = i),
            alpha = 1.0,
            size = 0.2
        )

    ggsave( fout )
}

scatter( "scatter_data_2.csv", "scatter0_2.pdf" )
## scatter( "scatter_data_rt2.csv", "scatter1.pdf" )



scatter_knn <- function(fin, fout){
    df <- read_csv( fin, col_names=c('x', 'y', 'i'), col_types=c( col_double(), col_double(), col_integer()) )

    df_knn <- df[ i == -1 ]

    splot <- ggplot( aes(x, y) )

    df %>%
      ( splot +
        geom_point( mapping = aes(x = x, y = y, color = i), alpha = 1.0, size = 0.2 ) +
        scale_colour_grey()
      )

    df_knn %>%
       splot +
       geom_point( mapping = aes(x = x, y = y, color = 'red'), alpha = 1.0, size = 0.2 )
      

    ggsave(fout)
}

scatter( "scatter_knn.csv" , "scatter_knn.pdf")
scatter( "scatter_knnH.csv" , "scatter_knnH.pdf")

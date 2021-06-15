library(tidyverse)

theme_set(
  theme_bw())  # pre-set the bw theme.

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

    ## df_knn <- subset(df, i == -1)
    df_knn <- df %>%
              filter(i == - 1 )

    ggplot( df, aes(x, y) ) +
      geom_point( mapping = aes(x = x, y = y, colour = factor(i)), alpha = 0.5, size = 0.1 ) +
      # #scale_colour_grey() +
      geom_point( data = df_knn, mapping = aes(x = x, y = y , colour="red"), alpha = 1.0, size = 1.0 ) +
      theme(legend.position = "none")

    ggsave(fout)
}

scatter_knn( "scatter_knn.csv" , "scatter_knn.pdf")
scatter_knn( "scatter_knnH.csv" , "scatter_knnH.pdf")

my_df <- data.frame(lets = c('A','A','C','C','B','D','A','E','E','B','A','C'), colors = c("#FFFFBF", "#FFFFBF", "#2C7BB6", "#2C7BB6", "#D7191C", "#ABD9E9", "#FFFFBF", "#FDAE61", "#FDAE61", "#D7191C", "#FFFFBF", "#2C7BB6"))

How can I pass the actual colors in my_df$colors to my plot, something akin to this (which doesn't work properly)?:

ggplot(my_df, aes(x = lets, fill = colors)) + geom_bar()


+ scale_fill_identity()

geom_link_lab <- function(data = NULL, mapping = NULL, position = 'identity', ...){
    structure(list(data = data, 
                   mapping = mapping,
                   position = position,
                   ...
                   ),
               class = "link_lab"
    )

}

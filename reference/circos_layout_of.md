# The layout carried by a plot

Layers read the layout through this function and never modify it.

## Usage

``` r
circos_layout_of(plot)
```

## Arguments

- plot:

  A ggplot, a data frame, or a `circos_layout`.

## Value

A `circos_layout`, or `NULL` when there is none.

## Examples

``` r
p <- ggcircos(cor(mtcars))
circos_layout_of(p)
#> <ggcircos layout>
#>   nodes:   11
#>   links:   55
#>   sectors: 1
#>   tracks:  <none>
```

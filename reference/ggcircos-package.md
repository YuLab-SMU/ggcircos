# Circular layouts, chord diagrams and tracks for ggplot2

The layout of a circular plot – node arcs, sectors and the angular slot
reserved for every link – is computed once by
[`circos_layout()`](https://yulab-smu.github.io/ggcircos/reference/circos_layout.md)
and kept as data. Layers then behave like any other ggplot2 layer: they
read the layout and add their own data, and nothing accumulates on the
plot.

## See also

Useful links:

- <https://yulab-smu.github.io/ggcircos>

- Report bugs at <https://github.com/YuLab-SMU/ggcircos/issues>

## Author

**Maintainer**: Shuangbin Xu <xshuangbin@163.com>
([ORCID](https://orcid.org/0000-0003-3513-5362))

Authors:

- Shuangbin Xu <xshuangbin@163.com>
  ([ORCID](https://orcid.org/0000-0003-3513-5362))

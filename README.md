# ggqualpal

Generate perceptually distinct qualitative colour palettes and use them easily with **ggplot2**.

`ggqualpal` provides simple wrappers around the palette optimisation algorithm implemented in [`qualpalr`](https://github.com/jolars/qualpalr). The package makes it easy to:

- generate qualitative colour palettes
- map categories to colours
- apply qualitative palettes to **ggplot2** scales

The main goal is to simplify the use of `qualpalr` when working with categorical data and visualisation.

---

# Installation

You can install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Fuschi/ggqualpal")
```

---

# Generate qualitative palettes

Use `pal_qualpal()` to generate perceptually distinct colours.

```r
library(ggqualpal)

pal <- pal_qualpal()

pal(6)
```

Preview the palette:

```r
show_qualpal(pal(6))
```

---

# Map categories to colours

`map_qualpal()` maps discrete categories directly to colours.

```r
x <- c("A", "B", "C", "A")

pal <- map_qualpal(x)

pal
```

Apply the palette to the data:

```r
pal[x]
```

---

# Fix colours for selected categories

You can reserve specific colours using `fixed`.  
These colours are taken into account when generating the rest of the palette.

```r
x <- c("A", "B", "Other", "Smaller")

pal <- map_qualpal(
  x,
  fixed = c(
    Other = "transparent",
    Smaller = "#F5F5DC"
  )
)

show_qualpal(pal)
```

---

# Replace colours after palette generation

Use `override` to replace colours after the palette has been generated.

```r
map_qualpal(
  x,
  override = c(
    Other = "#000000"
  )
)
```

---

# Use with ggplot2

`ggqualpal` provides discrete ggplot2 scales based on `qualpalr`.

```r
library(ggplot2)

df <- data.frame(
  x = 1:6,
  y = 1:6,
  g = factor(c("A", "B", "C", "A", "B", "C"))
)

ggplot(df, aes(x, y, colour = g)) +
  geom_point(size = 3) +
  scale_color_qualpal()
```

Fill scale:

```r
ggplot(df, aes(x, y, fill = g)) +
  geom_point(shape = 21, size = 4) +
  scale_fill_qualpal()
```

---

# Why ggqualpal?

`ggqualpal` is designed for situations where many distinct colours are needed,
such as visualising categorical data with a large number of groups.

The package builds on the palette optimisation algorithm implemented in
`qualpalr`, which generates colours that are maximally distinguishable in
perceptual colour space.

Compared with using `qualpalr` directly, `ggqualpal` provides:

- simple generation of large qualitative palettes
- direct mapping from categories to colours
- seamless integration with ggplot2 scales
- utilities for inspecting generated palettes

---

# Related packages

- `qualpalr` – perceptually optimised qualitative palettes  
- `scales` – scale tools for ggplot2  
- `colorspace` – advanced colour utilities  

---

# License

MIT © Alessandro Fuschi

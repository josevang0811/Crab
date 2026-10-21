# Análisis estadístico de la edad de cangrejos

Proyecto de Métodos Estadísticos (Universidad del Norte) desarrollado en `bookdown`
con la base *Regression with a Crab Age Dataset* de Kaggle.

Autores: Jairo Hernández Carvajal y Jose Van Strahlen García.

Libro en línea: https://josevang0811.github.io/Crab/

Las versiones en PDF y Word están en la carpeta `entregables`.

Para compilar el libro:

```r
source("instalar_paquetes.R")   # solo la primera vez
bookdown::render_book("index.Rmd")                          # HTML (carpeta docs)
bookdown::render_book("index.Rmd", "bookdown::pdf_book",
                      output_dir = "entregables")           # PDF
bookdown::render_book("index.Rmd", "bookdown::word_document2",
                      output_dir = "entregables")           # Word
```

El libro HTML se genera en la carpeta `docs`, que es la que publica GitHub Pages.

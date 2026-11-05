# emplikDTA

Empirical likelihood inference for a continuous diagnostic marker in two or
three independent ordered groups. Larger values indicate higher diagnostic
classes. The current interfaces do not model clusters or covariates.

emplikDTA is licensed under GNU GPL version 2 or any later version.

## Main functions

| Task | Two groups | Three groups |
| --- | --- | --- |
| Overall accuracy and inference | `auc()` | `vus()` |
| Empirical ROC estimation | `ROC_emp_curve()` | `ROC_emp_surface()` |
| Optimal thresholds | `threshold2()` | `threshold3()` |
| Middle-class accuracy at fixed outer fractions | — | `tcf2()` |

`geom_operating_point()` and `geom_confidence_region()` add layers to ROC
curve plots. `CR_tcfs()` adds a joint confidence region at a fixed pair of
thresholds to a ROC surface plot.
`empi_llike_proj()` is an exported low-level likelihood helper.

## Install from source

From the package parent directory, run `R CMD INSTALL emplikDTA` with the
package dependencies and a suitable C++ toolchain installed. Alternatively,
install the development repository using:

```r
remotes::install_github("toduckhanh/emplikDTA")
```

## Example

```r
library(emplikDTA)
set.seed(123)
x <- rnorm(60)
y <- rnorm(60, 1)
z <- rnorm(60, 2)
auc(x, y, B = 100, seed = 34)
vus(x, y, z, B = 100, seed = 34)
roc <- ROC_emp_curve(x, y, plot = FALSE)
plot(roc)
threshold2(x, y, method = "all", ci = FALSE)
```

Formula methods use `marker ~ group`. Supply `diag_levels` explicitly or
set the factor levels in the required order; sample means do not reorder
the groups. Handle missing values before calling vector methods. Bootstrap
inference defaults to seed 34 and changes the global RNG state.

## Documentation and development

Use `help(package = "emplikDTA")` for the function index. Each public topic
lists arguments, return values and implementation limitations. The
[function review](docs/function-review.md) records unresolved code issues;
the [helper guide](docs/function-reference.md) maps internal functions to
source files and explains their contracts. Worked examples are in `vignettes/`.


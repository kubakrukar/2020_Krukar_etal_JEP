# The effect of orientation instructions on the recall and reuse of route and survey elements in wayfinding descriptions

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/kubakrukar/sketchmaptypes/HEAD?urlpath=rstudio----UPDATE)

This repository contains data and code that reproduces statistical results, tables and figures from this paper:

Krukar, J., Anacta, V. J., & Schwering, A. (2020). The effect of orientation instructions on the recall and reuse of route and survey elements in wayfinding descriptions. *Journal of Environmental Psychology, 68, 101407.* doi: 10.1016/j.jenvp.2020.101407

# Run this code

There are 3 main ways in which you can use this repository:

1. You can open 'Krukar-etal-2018.md-UPDATE' by clicking it on the file list above. The file shows what code was used to achieve results from the paper.

2. You can click on the 'lunch binder' button above. This will open an interactive session of RStudio in your web browser. You are able to change the code there by selecting the file 'Krukar-etal-2018.Rmd-UPDATE' and re-generating the analysis with the 'Knit' button. Rememeber any changes will be lost as soon as you close the browser window.

3. You can download the code and data and and try to run it locally. This is likely to fail in the future when software versions change. The analysis does work on the session listed below.-UPDATE




```
> sessionInfo()
R version 4.0.2 (2020-06-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS  10.16

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] here_1.0.1            irr_0.84.1            lpSolve_5.6.15        psych_2.0.12          GPArotation_2014.11-1 semPlot_1.1.2        
 [7] knitr_1.30            readxl_1.3.1          lavaan_0.6-7          forcats_0.5.0         stringr_1.4.0         dplyr_1.0.3          
[13] purrr_0.3.4           readr_1.4.0           tidyr_1.1.2           tibble_3.0.5          ggplot2_3.3.3         tidyverse_1.3.0      

loaded via a namespace (and not attached):
  [1] minqa_1.2.4         colorspace_2.0-0    ellipsis_0.3.1      rprojroot_2.0.2     htmlTable_2.2.1     corpcor_1.6.10     
  [7] base64enc_0.1-3     fs_1.5.0            rstudioapi_0.13     lubridate_1.7.9.2   xml2_1.3.2          splines_4.0.2      
 [13] mnormt_2.0.2        glasso_1.11         Formula_1.2-4       jsonlite_1.7.2      nloptr_1.2.2.2      broom_0.7.3        
 [19] cluster_2.1.0       dbplyr_2.0.0        png_0.1-7           regsem_1.8.0        compiler_4.0.2      httr_1.4.2         
 [25] backports_1.2.1     assertthat_0.2.1    Matrix_1.3-2        cli_2.5.0           htmltools_0.5.1.1   tools_4.0.2        
 [31] OpenMx_2.19.8       igraph_1.2.6        coda_0.19-4         gtable_0.3.0        glue_1.4.2          reshape2_1.4.4     
 [37] Rcpp_1.0.7          carData_3.0-4       cellranger_1.1.0    vctrs_0.3.6         nlme_3.1-151        lisrelToR_0.1.4    
 [43] xfun_0.20           openxlsx_4.2.3      lme4_1.1-26         rvest_0.3.6         lifecycle_0.2.0     gtools_3.8.2       
 [49] XML_3.99-0.6        statmod_1.4.35      MASS_7.3-53         scales_1.1.1        hms_1.0.0           kutils_1.70        
 [55] parallel_4.0.2      RColorBrewer_1.1-2  yaml_2.2.1          pbapply_1.5-0       gridExtra_2.3       rpart_4.1-15       
 [61] latticeExtra_0.6-29 stringi_1.5.3       sem_3.1-11          checkmate_2.0.0     boot_1.3-26         zip_2.1.1          
 [67] truncnorm_1.0-8     rlang_0.4.10        pkgconfig_2.0.3     Rsolnp_1.16         arm_1.11-2          evaluate_0.14      
 [73] lattice_0.20-41     htmlwidgets_1.5.3   tidyselect_1.1.0    plyr_1.8.6          magrittr_2.0.1      R6_2.5.0           
 [79] generics_0.1.0      Hmisc_4.5-0         DBI_1.1.1           pillar_1.4.7        haven_2.3.1         foreign_0.8-81     
 [85] withr_2.4.0         rockchalk_1.8.144   survival_3.2-7      abind_1.4-5         nnet_7.3-15         modelr_0.1.8       
 [91] crayon_1.3.4        fdrtool_1.2.16      tmvnsim_1.0-2       rmarkdown_2.6       jpeg_0.1-8.1        qgraph_1.6.9       
 [97] grid_4.0.2          data.table_1.13.6   pbivnorm_0.6.0      matrixcalc_1.0-5    reprex_0.3.0        digest_0.6.27      
[103] xtable_1.8-4        mi_1.0              RcppParallel_5.0.2  stats4_4.0.2        munsell_0.5.0      
```

Jakub Krukar

krukar@uni-muenster.de

http://krukar.staff.ifgi.de


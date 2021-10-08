# The effect of orientation instructions on the recall and reuse of route and survey elements in wayfinding descriptions

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/kubakrukar/2020_Krukar_etal_JEP/HEAD?urlpath=rstudio)

This repository contains data and code that reproduces statistical results, tables and figures from the following paper:

Krukar, J., Anacta, V. J., & Schwering, A. (2020). The effect of orientation instructions on the recall and reuse of route and survey elements in wayfinding descriptions. *Journal of Environmental Psychology, 68, 101407.* doi: 10.1016/j.jenvp.2020.101407

# Run this code

There are 3 main ways in which you can use this repository:

1. You can open 'results/run_this.md' by navigating to it on the file list above. The file will open in your browser. It demonstrates what code was used to achieve results from the paper.

2. You can click on the 'launch binder' button above. This will open an interactive session of RStudio in your web browser. You are able to change the code there by selecting the file 'results/run_this.Rmd' and re-generating the analysis with the 'Knit' button. Note that any changes will be lost as soon as you close the browser window. Launching Binder might take between few minutes and one hour.

3. You can download the code and data and try to run it locally. This is likely to fail in the future when software versions change. The analysis does work on the session listed below.




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
 [1] dunn.test_1.3.5    ordinal_2019.12-10 sjPlot_2.8.7       lmerTest_3.1-3     lme4_1.1-26        Matrix_1.3-2      
 [7] irr_0.84.1         lpSolve_5.6.15     here_1.0.1         readxl_1.3.1       kableExtra_1.3.1   forcats_0.5.0     
[13] stringr_1.4.0      dplyr_1.0.3        purrr_0.3.4        readr_1.4.0        tidyr_1.1.2        tibble_3.0.5      
[19] ggplot2_3.3.3      tidyverse_1.3.0   

loaded via a namespace (and not attached):
 [1] nlme_3.1-151        fs_1.5.0            lubridate_1.7.9.2   insight_0.13.2      webshot_0.5.2      
 [6] httr_1.4.2          rprojroot_2.0.2     numDeriv_2016.8-1.1 tools_4.0.2         backports_1.2.1    
[11] sjlabelled_1.1.7    R6_2.5.0            DBI_1.1.1           colorspace_2.0-0    withr_2.4.0        
[16] tidyselect_1.1.0    emmeans_1.5.3       compiler_4.0.2      performance_0.7.0   cli_2.5.0          
[21] rvest_0.3.6         xml2_1.3.2          sandwich_3.0-0      bayestestR_0.8.2    scales_1.1.1       
[26] mvtnorm_1.1-1       digest_0.6.27       minqa_1.2.4         rmarkdown_2.6       pkgconfig_2.0.3    
[31] htmltools_0.5.1.1   dbplyr_2.0.0        rlang_0.4.10        rstudioapi_0.13     generics_0.1.0     
[36] zoo_1.8-8           jsonlite_1.7.2      magrittr_2.0.1      parameters_0.12.0   Rcpp_1.0.7         
[41] munsell_0.5.0       ucminf_1.1-4        lifecycle_0.2.0     stringi_1.5.3       multcomp_1.4-15    
[46] yaml_2.2.1          MASS_7.3-53         grid_4.0.2          sjmisc_2.8.6        crayon_1.3.4       
[51] lattice_0.20-41     ggeffects_1.0.1     haven_2.3.1         splines_4.0.2       sjstats_0.18.1     
[56] hms_1.0.0           knitr_1.30          pillar_1.4.7        boot_1.3-26         estimability_1.3   
[61] effectsize_0.4.4-1  codetools_0.2-18    reprex_0.3.0        glue_1.4.2          evaluate_0.14      
[66] modelr_0.1.8        vctrs_0.3.6         nloptr_1.2.2.2      cellranger_1.1.0    gtable_0.3.0       
[71] assertthat_0.2.1    xfun_0.26           xtable_1.8-4        broom_0.7.3         coda_0.19-4        
[76] survival_3.2-7      viridisLite_0.3.0   statmod_1.4.35      TH.data_1.0-10      ellipsis_0.3.1  
```

Jakub Krukar

krukar@uni-muenster.de

http://krukar.staff.ifgi.de


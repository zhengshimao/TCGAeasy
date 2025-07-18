usethis::create_package("G:/my_github/TCGAeasy/")
# 协议

usethis::use_mit_license()
# 修改DESCRIPTION

# 添加管道符
usethis::use_pipe(export = TRUE)
devtools::document() # 将其导入到命名空间

# 添加R包
# usethis::use_package("TCGAbiolinks",min_version = "2.24.3")
usethis::use_package("xml2")
usethis::use_package("dplyr")
usethis::use_package("stringr",type = "Suggests")
# usethis::use_package("tibble")


# 写R函数

# 添加函数注释
devtools::document()

# 检查
devtools::check()

# build R包 # "G:/my_github/TCGAeasy_0.0.0.9000.tar.gz"
devtools::build()

# vignette
usethis::use_vignette("TCGAeasy.qmd", "Easy Access and Analysis Tools for TCGA Data")
usethis::use_vignette("Read_TCGA_XML.qmd",  "Read TCGA Clinical XML files Quickly.")
usethis::use_vignette("Read_TCGA_XML2.qmd", "Read TCGA Clinical XML files Simply.")
##  构建 vignette
devtools::build_vignettes()

# 安装本地包
install.packages("./",repos = NULL, type = "source")





installed_pkgs=installed.packages()

installed_pkgs=installed_pkgs[,c("Package","Version")]

installed_pkgs=data.frame(installed_pkgs)

pkg=c(  "data.table" ,
        "openxlsx",   
        "writexl" ,   
        "logr"    ,   
        "common"  ,   
        "glue"   ,    
        "rlang"  ,    
        "haven"  ,    
        "stringdist" ,
        "readxl" ,    
        "tidyr"  ,    
        "purrr"      ,
        "lubridate"  ,
        "stringr",    
        "dplyr" ,     
        "phonics"    
)

version=c( "1.16.2",
           "4.2.7.1",
           "1.5.1",
           "1.3.8",
           "1.1.3",
           "1.8.0",
           "1.1.4",
           "2.5.4",
           "0.9.12",
           "1.4.3",
           "1.3.1",
           "1.0.2",
           "1.9.3",
           "1.5.1",
           "1.1.4",
           "1.3.10"
)

needed_pkgs=data.frame( pkg,version )
colnames(needed_pkgs)=c("pkg","version")
#Added a comment for testing

for (items in pkg_versions) {
  installed.packages()
}
  

x=installed.packages()
xx=x[,1]

print(rownames(installed.packages()))

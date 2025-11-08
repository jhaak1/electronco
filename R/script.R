library('readr')

#icd9 = read_csv('icd9.csv')
#icd10 = read_csv('icd10.csv')
#cpt = read_csv('cpt.csv')

usethis::use_data(icd9, icd10, cpt, internal = T)
usethis::use_data_raw()
dir.create("inst/extdata", recursive = TRUE)
use_r('codeversions')
load_all()
codeversions()
use_r('codemetadata')
codeversions()
codemetadata(dataset = 'cpt')
check()
use_package('tibble')
use_package('yaml')
document()

profvis({

})

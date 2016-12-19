
# load header function into top of search list
while("oxydebt_funs" %in% search()) detach("oxydebt_funs")
sys.source("scripts/OxygenDebt/zz_header.R", envir = attach(NULL, name = "oxydebt_funs"))

# get scripts to run
files <- dir("scripts/OxygenDebt/", pattern = "^(data|input|model|output)_.*[.]R$")

# run scripts in correct order
for (file in sort(files)) {
  source(file, echo = TRUE, keep.source = TRUE)
}


# load header function into top of search list
while("tools:oxydebt_funs" %in% search()) detach("tools:oxydebt_funs")
sys.source("scripts/OxygenDebt/zz_header.R", envir = attach(NULL, name = "tools:oxydebt_funs"))

# get scripts to run
files <- paste0("scripts/OxygenDebt/", dir("scripts/OxygenDebt/", pattern = "^(data|input|model|output)_.*[.]R$"))
files <- files[-1]

# run scripts in correct order
for (file in sort(files)) {
  source(file, echo = TRUE, keep.source = TRUE)
}


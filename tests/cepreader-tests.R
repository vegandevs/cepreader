## test that we can read data

require(cepreader)
cep <- file.path(path.package("cepreader"), "testdata", "dune.spe")
readCEP(cep)

#demo

# path = "C:/Users/Alaia/Desktop/Postdoc/R script/atlantis2ls/data"
# prefix = "AMPS"
# fg.file = "C:/Users/Alaia/Desktop/Postdoc/R script/atlantis2ls/data/PugetSoundAtlantisFunctionalGroups_2024.csv"
# bio.prm = "C:/Users/Alaia/Desktop/Postdoc/R script/atlantis2ls/data/AMPSbioparam_mv1_2024_V4.prm"
# bio.prm2 = "C:/Users/Alaia/Desktop/Postdoc/R script/atlantis2ls/data/AMPSbioparam_mv1_20222.prm"
# txt.file = "AMPS_OUTBiomIndx.txt"
# bio.lines <- readLines(bio.prm)
# a <- read_atlantis(path = path, prefix = prefix, fg.file = fg.file, N_only = F, txt.file = txt.file)
# outputs.nc <- ncdf4::nc_open("C:/Users/Alaia/Desktop/Postdoc/R script/atlantis2ls/data/AMPS_OUT.nc")
#
# atlantis_bgm <- read_bgm(paste(path,"/PugetSound_89b_070116.bgm", sep = ""))

# for (i in 1:10){
#   edit_param_mum_age(bio.prm=bio.prm,
#                      new.mum = get_param_mum_age(bio.prm, write.output = F, output.dir, out.name, vector.size = i)*2,
#                      overwrite = T,
#                      new.file.name = bio.prm2,
#                      single.group = F,
#                      vector.size = i)
# }
# a <-edit_param_mq_sp(readLines(bio.prm), factor = 10, species = "CHY")
# a <- edit_param_sp(bio.prm, param = "bhbeta", factor = 2, species = "HEP")
# ratio
# package fusem --> helpful to create a package
#
# harvest.prm = "data/AMPS_atharvest_2024_calib.prm"
# harvest.lines = readLines(harvest.prm)
# factor = 2
# species = "CHY"
# harvest.lines <- edit_param_mfc(harvest.lines, factor, species)
#
# writeLines(harvest.lines, "data/harvest.prm")

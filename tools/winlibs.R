# Build against MS Visual Studio builds of APPL
if(!file.exists("../windows/appl-0.96")){
  if(getRversion() < "3.3.0") setInternet2()
  download.file("https://github.com/rwinlib/appl/archive/v0.96.zip", "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}

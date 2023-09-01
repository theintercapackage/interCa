  interShiny<- function(...) {
    appDir = system.file("interShiny", package = "interCa")

    shiny::runApp(appDir, display.mode = "normal")
  }


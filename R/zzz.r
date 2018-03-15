.onLoad = function(libname, pkgname)
{
  data("gmh_directions", package="gmhelper", envir=parent.env(environment()))
  data("gmh_henchmen", package="gmhelper", envir=parent.env(environment()))
  data("gmh_racenames", package="gmhelper", envir=parent.env(environment()))
  data("gmh_services", package="gmhelper", envir=parent.env(environment()))
  
  invisible(NULL)
}

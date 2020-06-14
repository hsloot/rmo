require_R_version <- function( # nolint
    version_string = paste0(major, minor, sep="."),
    major = R.version$major,
    minor = R.version$minor) {
  R_version_string <- paste(R.version$major, R.version$minor, sep=".") # nolint
  return(1 == compareVersion(R_version_string, version_string))
}

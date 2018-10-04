
write.gadget.file_old<-write.gadget.file

write.gadget.file<-function (obj, path, recursive = TRUE) 
{
  file_name <- attr(obj, "file_name")
  file_config <- attr(obj, "file_config")
  mainfile <- attr(path, "mainfile")
  if (!isTRUE(nzchar(mainfile))) {
    mainfile <- "main"
  }
  variant_dir <- attr(path, "variant_dir")
  if (isTRUE(nzchar(variant_dir))) {
    file_name <- variant_full_path(variant_dir, file_name)
  }
  dir.create(dirname(file.path(path, file_name)), recursive = TRUE, 
             showWarnings = FALSE)
  write_comp_subfiles <- function(comp) {
    if (!is.list(comp)) 
      return()
    for (field in comp) {
      if ("gadget_file" %in% class(field)) {
        if (is.null(field$data)) {
          field <- gadgetfile(field$filename, file_type = "generic", 
                              field$components)
        }
        else {
          field <- gadgetfile(field$filename, file_type = "generic", 
                              c(field$components, list(data = field$data)))
        }
      }
      if ("gadgetfile" %in% class(field)) {
        if (isTRUE(nzchar(variant_dir))) {
          attr(field, "file_name") <- variant_full_path(variant_dir, 
                                                        attr(field, "file_name"))
        }
        write.gadget.file(field, path)
      }
      else {
        write_comp_subfiles(field)
      }
    }
  }
  if (recursive) 
    write_comp_subfiles(obj)
  fh = file(file.path(path, file_name), "w")
  tryCatch(capture.output(print(obj, path = path), file = fh), 
           finally = close(fh))
  if (!is.null(mainfile) && !is.na(file_config$mainfile_section)) {
    do.call(gadget_mainfile_update, c(list(path, mainfile), 
                                      structure(list(file_name), names = c(file_config$mainfile_section)), 
                                      structure(list(file_config$mainfile_overwrite), names = "overwrite"), 
                                      NULL))
  }
}
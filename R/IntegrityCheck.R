IntegrityCheck <- function(FileName) {
  
  assertive.base::dont_stop(assert_all_are_existing_files(FileName))
  
}
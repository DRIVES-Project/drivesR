httr_dry_run <- function(r) {
  require(httr)
  old_cb <- httr::get_callback("request")
  on.exit(httr::set_callback("request", old_cb))
  
  httr::set_callback("request", function(req) req)
  r
}
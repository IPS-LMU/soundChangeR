is_cache_valid <- function(agent, cacheName) {
  agent$cache[name == cacheName, valid]
}

update_cache <- function(agent, cacheName, method, ...) {
  agent$cache[name == cacheName, `:=`(value = list(method(agent, ...)), valid = TRUE)]
}

set_cache_value <- function(agent, cacheName, cacheValue) {
  agent$cache[name == cacheName, `:=`(value = list(cacheValue), valid = TRUE)]
}

get_cache_value <- function(agent, cacheName) {
  agent$cache[name == cacheName, value][[1]]
}

invalidate_cache <- function(agent, cacheName) {
  agent$cache[name == cacheName, valid := FALSE]
}

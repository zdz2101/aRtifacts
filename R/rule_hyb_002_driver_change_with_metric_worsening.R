#' HYB-002: Driver change with metric worsening
#'
#' @param driver_domain Driver domain name.
#' @param metric_domain Metric domain name.
#' @param entity_var Entity identifier variable.
#' @param driver_var Numeric driver variable.
#' @param metric_var Numeric metric variable.
#' @param driver_delta_min Minimum driver increase.
#' @param metric_delta_min Minimum metric worsening increase.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_hyb_002_driver_change_with_metric_worsening <- function(driver_domain = "DRIVER",
                                                             metric_domain = "METRIC",
                                                             entity_var = "entity_id",
                                                             driver_var = "driver_value",
                                                             metric_var = "metric_value",
                                                             driver_delta_min = 0,
                                                             metric_delta_min = 0,
                                                             owner_team = "Data Operations") {
  new_rule(
    id = "HYB-002",
    title = "Driver change with metric worsening",
    topology = "A7",
    scope = "record",
    intent = "ops_anomaly",
    domains = c(driver_domain, metric_domain),
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      drv_prev <- .topology_domain_df(previous, driver_domain)
      drv_cur <- .topology_domain_df(current, driver_domain)
      met_prev <- .topology_domain_df(previous, metric_domain)
      met_cur <- .topology_domain_df(current, metric_domain)
      if (is.null(drv_prev) || is.null(drv_cur) || is.null(met_prev) || is.null(met_cur)) {
        return(data.frame())
      }

      needed_drv <- c(entity_var, driver_var)
      needed_met <- c(entity_var, metric_var)
      if (!all(needed_drv %in% names(drv_prev)) || !all(needed_drv %in% names(drv_cur)) ||
          !all(needed_met %in% names(met_prev)) || !all(needed_met %in% names(met_cur))) {
        return(data.frame())
      }

      summarize_mean <- function(df, id_var, value_var, out_name) {
        vals <- suppressWarnings(as.numeric(df[[value_var]]))
        agg <- stats::aggregate(vals, by = list(entity = as.character(df[[id_var]])), FUN = function(x) mean(x, na.rm = TRUE))
        names(agg) <- c(id_var, out_name)
        agg
      }

      d_prev <- summarize_mean(drv_prev, entity_var, driver_var, "driver_prev")
      d_cur <- summarize_mean(drv_cur, entity_var, driver_var, "driver_cur")
      m_prev <- summarize_mean(met_prev, entity_var, metric_var, "metric_prev")
      m_cur <- summarize_mean(met_cur, entity_var, metric_var, "metric_cur")

      merged <- Reduce(function(x, y) merge(x, y, by = entity_var, all = FALSE), list(d_prev, d_cur, m_prev, m_cur))
      if (!nrow(merged)) {
        return(data.frame())
      }

      merged$driver_delta <- merged$driver_cur - merged$driver_prev
      merged$metric_delta <- merged$metric_cur - merged$metric_prev

      bad <- !is.na(merged$driver_delta) & !is.na(merged$metric_delta) &
        merged$driver_delta >= driver_delta_min & merged$metric_delta >= metric_delta_min
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c(entity_var, "driver_prev", "driver_cur", "driver_delta", "metric_prev", "metric_cur", "metric_delta"), drop = FALSE]
      out$driver_threshold <- as.numeric(driver_delta_min)
      out$metric_threshold <- as.numeric(metric_delta_min)
      out$reason <- "Driver increase is accompanied by metric worsening"
      out
    }
  )
}

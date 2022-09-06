## Public interface ########

#' SQL select on data frames
#'
#' sqldf is an R package for running SQL statements on R data frames, optimized for convenience.
#' The user simply specifies an SQL statement in R using data frame names in place of table
#' names and a database with appropriate table layouts/schema is automatically created,
#' the data frames are automatically loaded into the database, the specified SQL statement
#' is performed, the result is read back into R and the database is deleted all
#' automatically behind the scenes making the database's existence transparent to the user
#' who only specifies the SQL statement. Surprisingly this can at times be even faster
#' than the corresponding pure R calculation (although the purpose of the project is
#' convenience and not speed).
#'
#' All `DBI` drivers are supported, although features may be limited by driver support. \link[RSQLite:RSQLite-package]{RSQLite} and
#' \link[duckdb:duckdb-package]{duckdb} are recommended.
#'
#' `DBIsqldf` is free software published under the GNU General Public License that can be downloaded from CRAN.
#'
#' ## Options
#'
#' `sqldf.driver`: A [DBIDriver-class] object for a transient database connection when `conn` is omitted,
#' or an expression or function that yields one. The default is `quote(RSQLite::SQLite)`
#'
#' ## FAQ
#'
#' 1. How does DBIsqldf handle classes and factors?
#'
#'     It doesn't. The exact behaviour will depend on the database you choose to use. This includes dates.
#'
#' 2. Why does DBIsqldf seem to mangle certain variable names?
#'
#'     If a data frame contains variables with names that are not valid in SQL, the
#'     behavior is left up to the database driver you choose to use. Notably, periods
#'     can cause issues - you may need to double quote the affected variables in
#'     your SQL statement. Also please be aware that SQL is **not case sensitive**, while R is.
#'
#' 3. Why does sqldf("select var(x) from DF") not work?
#'
#'    Functions are provided by the database you choose to use, not by R.
#'
#' 4. Why am I having problems with update?
#'
#'    Be sure to set `tx` to commit to persist any changes you have made to the database.
#'
#' 5. Why do certain calculations come out as integer rather than double?
#'
#'    Calculations performed within the database will follow the database's rules.
#'
#'
#' @param statement A SQL statement. Use `?` for parameterized queries.
#' @param ... parameters to be interpolated into `statement`.
#' @param conn optional, a [DBIConnection-class] object.
#' @param tx   optional, one of `'none'` (default), `'commit'`, or `'rollback'`.
#'
#' @examples
#'
#' library(DBIsqldf)
#'
#' if(!requireNamespace("RSQLite")) {
#'   o <- options(sqldf.driver=quote(DBI::ANSI))
#'   on.exit(options(o))
#' }
#'
#' data(iris, envir=environment())
#'
#' # head
#' sqldf("select * from iris limit 5")
#'
#' # Filter
#' sqldf("select * from iris where species = 'virginica' limit 5 ")
#'
#' # Parameterized query
#' sqldf("select * from iris where species = ? limit 5", "versicolor")
#'
#' # Aggregate, quoting
#' sqldf('select species, avg("Petal.Width") from iris group by 1 limit 5')
#'
#' # Compare with aggregate(Petal.Width~Species, iris, FUN = var)
#' # CTE, back join
#' sqldf('
#'   with tbl_width as (
#'       select species, avg("Petal.Width") as xbar, count(1) as n
#'       from iris
#'       group by 1
#'   )
#'   select Species,
#'   1.0/ (n-1) *sum(("Petal.Width" - xbar)*("Petal.Width" - xbar)) as var
#'   from iris
#'   join tbl_width using (species)
#'   group by 1
#' ')
#'
#'
#'
#' # Manually managing the DB connection, and writing to a database
#' if(requireNamespace("RSQLite")) {
#'
#'   conn <- DBI::dbConnect(RSQLite::SQLite(), tmp <- tempfile())
#'   on.exit(DBI::dbDisconnect(conn), add=TRUE)
#'   on.exit(file.remove(tmp), add=TRUE)
#'
#'   data(mtcars, envir=environment())
#'
#'   sqldf("create table usacars as select * from mtcars where am = 1", conn=conn, tx='commit')
#'
#'   #Now compare
#'   DBI::dbListTables(conn)
#'
#'   # vs persisted tables
#'   sqldf("SELECT type, name FROM sqlite_schema", conn=conn)
#'
#'   # hang up and reconnect
#'   DBI::dbDisconnect(conn)
#'   conn <- DBI::dbConnect(conn)
#'
#'   DBI::dbListTables(conn)
#'
#'   sqldf("SELECT cyl, count(1) as n from usacars group by 1", conn=conn)
#' }
#'
#'
sqldf <- function(statement, ..., conn, tx) {

  stopifnot(length(statement) == 1)

  tx <- if(missing(tx)) "none" else match.arg(tx, c('none', 'commit', 'rollback'))

  ### Init conn if need be
  if(missing(conn)) {
    stopifnot("Can't set `tx` to `commit` with transient db."=!tx %in% "commit")

    drv <- getOption("sqldf.driver", quote(RSQLite::SQLite))
    if(is.expression(drv) || is.call(drv))
      drv <- eval(drv)
    if(is.function(drv))
      drv <- drv()

    conn <- dbConnect(drv, ":memory:")
    on.exit(dbDisconnect(conn))
  }

  ## Find identifiers in statement
  ## * SQL identifiers begin with a letter, and then are alnum + underscore
  ## * SQL identifiers are not case sensitive
  ## * Capture by \\b so that #temptables and schema.tables can be matched

  m <- gregexpr("\\b[A-Za-z][A-Za-z0-9_]*\\b", statement, ignore.case=TRUE)
  words <- regmatches(statement, m)[[1]]

  ## Drop SQL keywords and preexisting tables
  words <- setdiff_ignore_case(words, SQLKeywords(conn))
  words <- setdiff_ignore_case(words, dbListTables(conn))

  words <- mget(words, parent.frame(), ifnotfound = list(NULL))
  words <- Filter(is.data.frame, words)

  # Begin a transaction, in the case that a transaction is requested
  if(tx %in% "commit") {
    dbBegin(conn)
    on.exit(dbCommit(conn), add=TRUE, after=FALSE)
  } else if (tx %in% "rollback") {
    # Can be used as fallback if temp tables not supported
    dbBegin(conn)
    on.exit(dbRollback(conn), add=TRUE, after=FALSE)
  }

  for(tbl in names(words)) {
    dbWriteTable(conn, tbl, words[[tbl]], temporary=TRUE)
  }

  res <- dbSendStatement(conn, statement)
  on.exit(dbClearResult(res), add=TRUE, after=FALSE)

  dots <- list(...)
  if(length(dots) > 0) {
    dbBind(res, dots)
  }

  if(dbHasCompleted(res)) invisible(NULL) else dbFetch(res)
}

## Helpers #####

setdiff_ignore_case <- function(x, y) {
  u <- tolower(x)
  v <- tolower(y)
  x[!duplicated(u) & (match(u, v, 0L) == 0L)]
}

## Startup / Shutdown ########

# .onAttach <- function(libname, pkgname) {}

# .onUnload <- function(libpath) {}


## Package Documentation ########

#' DBIsqldf package overview
#'
#' Provides an easy way to perform SQL selects on R data frames.
#'
#' The package contains a single function [sqldf()] whose help file
#' contains more information and examples.
#'
#' This package is a fork from the [original sqldf-package](https://github.com/ggrothendieck/sqldf).
#' The primary differences are:
#'
#' * DBI interface only. This increases compatibility with database drivers, but at the expense
#'   of database-specific convenience features.
#' * DBI native parameterized query support.
#' * More conservative defaults for writing tables - data frames are imported as temporary tables
#'   and should be committed explicitly is they need to be persisted.
#' * No legacy (transitive) dependencies.
#'
#' @name DBIsqldf-package
#' @docType package
NULL

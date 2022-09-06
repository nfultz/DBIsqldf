library(DBIsqldf)

stopifnot(requireNamespace("RSQLite"))
options(sqldf.driver=quote(RSQLite::SQLite))

data(iris, envir=environment())

# head
sqldf("select * from iris limit 5")

# Filter
sqldf("select * from iris where species = 'virginica' limit 5 ")

# Parameterized query
sqldf("select * from iris where species = ? limit 5", "versicolor")

# Aggregate, quoting
sqldf('select species, avg("Petal.Width") from iris group by 1 limit 5')

# Compare with aggregate(Petal.Width~Species, iris, FUN = var)
# CTE, back join
sqldf('
  with tbl_width as (
      select species, avg("Petal.Width") as xbar, count(1) as n
      from iris
      group by 1
  )
  select Species,
  1.0/ (n-1) *sum(("Petal.Width" - xbar)*("Petal.Width" - xbar)) as var
  from iris
  join tbl_width using (species)
  group by 1
')

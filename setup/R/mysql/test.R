# Test connection to MySQL DB from lamp-rm docker service.
# See: https://github.com/gdancik/lamp-rm

library(RMariaDB)

# Connect to employee database using a configuration file,
# which should be in the working directory
con <- dbConnect(MariaDB(), group = "Employees", 
                 default.file = '.my.test.cnf')

# connect to database without configuration file 
# con <- dbConnect(MariaDB(), user = 'root', password = 'password',
#                  host = '0.0.0.0', port = '3000', 
#                  dbname = 'employees')

# test query
employees <- dbGetQuery(con, 'SELECT * from employees LIMIT 10 ')

# print output
print(employees)

# disconnect from database
dbDisconnect(con)

# Load the package
library(RODBC)

# Connect to MySQL (my credentials are mysql_server_64/root/root)
db = odbcConnect("mysql_server_64", uid="root", pwd="")
sqlQuery(db, "USE ma_charity_small")

# Extract data from database
query = "SELECT contact_id,
                DATEDIFF(20180626, MIN(act_date)) / 365 AS 'T.cal',
                DATEDIFF(MAX(act_date), MIN(act_date)) / 365 AS 't.x',
                COUNT(amount) - 1 AS 'x'
         FROM acts
         WHERE act_type_id LIKE 'DO'
         GROUP BY 1
         LIMIT 20000;"
data = sqlQuery(db, query)
print(head(data))

# Buy-til-you-die library
library(BTYD)

# Estimate parameters for the Pareto-NBD process
params = pnbd.EstimateParameters(data)
print(params)

# Plot heterogeneity in drop-out and purchase processes
pnbd.PlotDropoutRateHeterogeneity(params)
pnbd.PlotTransactionRateHeterogeneity(params)

# Plot goodness of fit (7 purchases)
pnbd.PlotFrequencyInCalibration(params, data, 7)

# P-NBD expectations after 10 years
ltv = vector(length = 10)
ltv_delta = vector(length = 10)
for (y in 1:10) {
   ltv[y] = pnbd.Expectation(params, t=y);
   if (y == 1) {
      ltv_delta[y] = ltv[y]
   } else {
      ltv_delta[y] = ltv[y] - ltv[y - 1]
   }
}
print(ltv)
barplot(ltv)
barplot(ltv_delta)

# Get individual estimates
for (i in 1:10) {
   data$ltv[i]    = pnbd.ConditionalExpectedTransactions(params, T.star = 10,
                                                         data$x[i], data$t.x[i], data$T.cal[i])
   data$palive[i] = pnbd.PAlive(params, data$x[i], data$t.x[i], data$T.cal[i])
}
print(data[1:10, ])

#' @name setup_shinyapps.R

# This script will help us get 
# setup for deploying ShinyApps!
install.packages("rsconnect")


# Authorize your account so this R Session
# can connect to your ShinyApps.io account
# Go to https://www.shinyapps.io/admin/#/dashboard
# and get the appropriate name, token, and secret there.
rsconnect::setAccountInfo(
  name='YOUR_ACCOUNT_NAME_GOES_HERE',
  token='YOUR_TOKEN_GOES_HERE',
  secret='YOUR_SECRET_GOES_HERE')



# Once you are connected,
# You can deploy an app by connecting us to the relevant path!
library(rsconnect)
# Let's try and deploy the NYC flights app.
# Make sure your desired app code is named app.R
# I've taken app7.R from our training 
# 6C_app_nycflights/ and renamed it app.R

# Let's see the contents of my app folder
dir("workshops/23C_dashboards/app_nycflights")
# Got all our data? Got our app script?
# Okay! Let's roll!

# Deploy app to ShinyApps.io
rsconnect::deployApp('workshops/23C_dashboards/app_nycflights')

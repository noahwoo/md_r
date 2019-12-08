# r-comp-fin
library(devtools)
# Install the latest version of the Webinar package from GitHub
install_github("rhochreiter/webinar-cpom/package")
library(webinar.cpom)

# Choose the version of the Webinar
version <- 1

# Run the setup package, which installs all necessary packages
source(paste0(path.package("webinar.cpom"), "/cpom.setup.", version, ".R"))

# Open the Webinar script in the editor pane
file.edit(paste0(path.package("webinar.cpom"), "/cpom.webinar.", version, ".R"))


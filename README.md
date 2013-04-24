# TimetrackR
A timetracker web service using the Haskell Web Framework [Yesod](http://www.yesodweb.com)

## Installation
    # Generate the tar archive "timetracker.tar.gz"
    rake pack_init
    
    # Move it where it suits you best
    mv timetracker.tar.gz /path/to/timetracker

    # Extract the archive
    cd /path/to/timetracker
    tar xf timetracker.tar.gz

    # Initial setup
    vi config/settings.yml

    # Start the service
    ./timetracker Production --port 80

## Screenshot
![](http://home.in.tum.de/~strittma/timetracker/timetracker_screenshot.png)
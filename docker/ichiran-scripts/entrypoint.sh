#!/bin/bash

echo "Starting main container init."

# First run init-sbcl if core doesn't exist
if [ ! -f /root/ichiran.core ]; then
    init-sbcl
fi

# Then start the web server
init-web

echo "All set, awaiting commands."
sleep infinity;
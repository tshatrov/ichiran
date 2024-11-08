#!/bin/bash

echo "Starting main container init."

# First run init-sbcl if core doesn't exist
if [ ! -f /root/ichiran.core ]; then
    echo "Core file not found, initializing..."
    init-sbcl
    if [ ! -f /root/ichiran.core ]; then
        echo "Failed to create core file!"
        exit 1
    fi
fi

# Then start the web server
echo "Starting web server with existing core..."
exec init-web

echo "All set, awaiting commands."
sleep infinity;
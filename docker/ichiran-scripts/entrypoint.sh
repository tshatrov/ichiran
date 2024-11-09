#!/bin/bash

echo "Starting main container init."

init-all;

# Start the web server
echo "Starting web server..."
exec init-web
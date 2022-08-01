# !/bin/bash/

# This will copy a files, appending the date and time
# to the end of the filename.

date_formatted=$(date +%m_%d_%y-%H.%M.%S)

cp -iv $1 $1.$date_formatted

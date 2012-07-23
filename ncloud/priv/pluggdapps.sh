netscale_path = $(dirname $(dirname $(dirname $0)))
source $netscale_path/pluggdapps/pa-env/bin/activate;
python3.2 $netscale_path/pluggdapps/pluggdapps/erlport.py $*;

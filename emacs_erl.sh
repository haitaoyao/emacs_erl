#!/bin/bash
set -u -e
##################################################################
# 
# written by haitao.yao @ 2012-06-03.22:14:03
# 
# 
# 
##################################################################
current_dir="$(cd $(dirname $0);pwd)"
node_name="emacs@haitao"
pa="-pa $current_dir"

if [ -d "ebin/" ]
then
	pa=" $pa -pa ebin" 
fi

if [ -d "apps" ]
then
	for dir in $(ls apps)
	do
		dir="apps/$dir/ebin" 
		if [ -d "$dir" ]
		then
			pa=" $pa -pa $dir"
			echo "app folder: $dir added"
		fi
	done
fi

if [ -d './deps' ]
then
	for dep_folder in $(find deps -type d -depth 1)
	do
		if [ -d "./$dep_folder/ebin" ]
		then
			echo "deps: $dep_folder added"
			pa="$pa -pa $dep_folder/ebin"
		else
			echo "no ebin folder for deps: $dep_folder"
		fi
	done
fi
function compile_eunit()
{
	pid=$1
	for i in $(seq 1 10000000)
	do
		if [ -z "$(pgrep bash | grep $pid)" ]
		then
			break
		fi
		erlc -DTEST -o ebin src/*.erl >> compile.log 2>&1 
		sleep 1
	done
}
#compile_eunit $$ &
erl -name "$node_name" $pa -s emacs_erl_util

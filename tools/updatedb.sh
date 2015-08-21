#!/bin/bash

ROOT_PATH=$1
DB=/data/${USER}/$(basename ${ROOT_PATH} 2>/dev/null).db
ALIAS=locate_$(basename ${ROOT_PATH} 2>/dev/null)

usage() {
    echo "usage: $0 <root_path>"
    exit 1
}

error() {
    echo "error: $@"
    exit 2
}

[ "x${ROOT_PATH}" = "x" ] && usage

[ ! -e ${ROOT_PATH} ] && error "${ROOT_PATH} does not exists"
[ ! -d ${ROOT_PATH} ] && error "${ROOT_PATH} is not a directory"

echo "Initializing db into ${DB}..." 
updatedb -U ${ROOT_PATH} -o ${DB} --prunenames ".bzr .hg .git .svn" -l 0
[ $? -ne  0 ] && error "fail to initialize ${DB}"
echo "done!"

echo "You can now search the database using locate command"
echo "Example: locate -d ${DB} [regexp]"
echo "For simplicity you can use: alias '${ALIAS}=locate -d ${DB}'"

#!/bin/bash

declare -a modules=($(git submodule | perl -ne 'print "$1 " if /[+ ]\w+ (.+) \(.+/'))

function pushd {
    command pushd "$@" > /dev/null
}

function popd {
    command popd > /dev/null
}

function main {
    for module in "${modules[@]}"; do
        pushd "${module}"
        pwd

        # check local changes
        if ! git diff-index --quiet HEAD; then
            echo "Local changes detected"
            popd
            continue
        fi

        # check fork project
        url=$(git remote get-url origin)
        if [[ "${url}" =~ "JulienMasson" ]] || [[ "${url}" =~ "massonju" ]]; then
            echo "Fork or project owner detected"
            popd
            continue
        fi

        # update branch
        found=$(git ls-remote --heads origin master)
        if [ -n "${found}" ]; then
            branch="master"
        else
            branch="main"
        fi

        echo "Update ${branch} branch"
        git fetch --quiet origin
        if git show-ref --quiet "${branch}"; then
            git checkout --quiet --detach
            git branch --quiet -D "${branch}"
        fi
        git checkout --quiet "origin/${branch}" -b "${branch}"

        popd
    done
}

main "$@"

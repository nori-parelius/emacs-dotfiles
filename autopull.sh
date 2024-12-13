#!/bin/bash

# Function to pull changes for a given repository
function pull_changes {
    local repo_path=$1

    if [[ -z "$repo_path" ]]; then
        echo "Repository path is required."
        return 1
    fi

    if [[ ! -d "$repo_path" ]]; then
        echo "Directory $repo_path does not exist."
        return 1
    fi

    # Navigate to the Git repository
    cd "$repo_path" || return 1

    # Get the current branch
    currentBranch=$(git symbolic-ref --short HEAD)

    if [[ -z "$currentBranch" ]]; then
        echo "Failed to determine the current branch in $repo_path."
        return 1
    fi

    echo "Current branch in $repo_path: $currentBranch"

    # Pull changes from the remote repository
    echo "Pulling changes for branch $currentBranch..."
    git pull origin "$currentBranch"

}

# List of repositories
repos=(
    "/home/nori/Documents/AllWritings/noriparelius"
    "/home/nori/.emacs.d"
)

# Iterate over the list and call the function for each repository
for repo in "${repos[@]}"; do
    pull_changes "$repo"
done
read -p "Press enter to continue"

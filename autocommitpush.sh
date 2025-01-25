#!/bin/bash

# Function to commit and push changes for a given repository
function commit_and_push {
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

    # Check for changes
    if [[ -n $(git status --porcelain) ]]; then
        echo "Changes detected in $repo_path, staging changes..."
        git add -A

        # Get current date and time
        currentDate=$(date +'%Y-%m-%d')
        currentTime=$(date +'%H-%M')

        # Commit changes with a message
        echo "Committing changes..."
        git commit -m "Automated commit on $currentDate at $currentTime"

    else
        echo "No changes to commit in $repo_path."
    fi
    # Push changes to the remote repository
    echo "Pushing changes to $currentBranch..."
    git push origin "$currentBranch"

}

# List of repositories
repos=(
    "/home/nori/Documents/noriparelius"

)

# Iterate over the list and call the function for each repository
for repo in "${repos[@]}"; do
    commit_and_push "$repo"
done

read -p "Press enter to continue"

# DATX02-15-29
Haskell on Erlang VM

## Git flow guide

For this repo there are some strict rules that should be followed, and some guidelines that are encouraged to follow.

### Strict rules
* No binary files in the repo. The key factor here to make sure not to stage compiled binaries when testing the product locally. If you notice that git wants to stage some files that you know should never be added (.o files for instance), add the fileending notation to .gitignore and commit that addition as a singe commit.
* No spaces in file/folder names
* Never push to master. Us the develop branch as the connection between your local repo and this shared repo.
* Always rebase your local branch with develop before pushing. By doing this, eventual merge conflicts will be dealt with locally, keeping this remote repo clean.
* DO NOT delete master or develop in the remote repo.

### Guidelines
The general workflow is:
* Pull from the master branch and the develop branch.
* Rebase the develop branch with master
* Create a new local branch. This branch will be your workbench. It is ok to work directly in the develop branch, but it can get tedious if you want to pull other new commits from origin develop to use other members work when your develop branch differs.
* When you think that your feature is done, merge your local branch with develop, pull from origin develop with (using rebase), solve the mergeconflicts if any, and push. Another way of doing this is checking out develop, pulling, rebasing your feature branch with develop, checking out develop, merge it with the feature and then pushing. When your commits are delivered to origin develop (and possibly when other members deem the commits ok), you can delete your local feature branch and start over by branching develop.
* Want to share some commits with other members without putting it into the origin developbranch? Just push your feature branch to origin <featureBranchName>. This will create the branch in origin. You can then tell other members that you have done so and your latest commits are available there.
* Are you done sharing some commits on a branch on origin that isnt develop and want to get rid of it? Simply type "git push origin :<branchName>". This means "push nothing into <branchName>" and git will delete the remote branch. NEVER do this with develop. If you have git 1.7.0 or newer, you can type "git push origin --delete <branchName>" for the same result.

### Don't agree with something? Got some suggestion?
Just comment the latest commit that changed this file!

# Autocommit
A simple script that commits and pushes all changes of git repositories inside a root path.

## Usage
Create an ssh key and link it to your Github account.

Create an empty file called `__autocommit__` in the .git folder of all the repositories you want to be autocommited.

Call the script when you want to autocommit. It scans all the tree of directories under a given root path, so the path of your workspace directory should be enough to commit most of your repos.
```
python autocommit.py -d [root_path]
```
push to an specific branch (-b defaults to master)
```
python autocommit.py -d [root_path] -b [branch_name]
```

Preferably set a [cronjob](http://askubuntu.com/questions/2368/how-do-i-set-up-a-cron-job) to do it for you

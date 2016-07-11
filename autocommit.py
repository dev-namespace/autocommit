import argparse, os
from datetime import datetime

def searchAndCommit(root_dir, branch='master'):
    """ 
    looks for git repositories in root_dir - subdirectories included - and performs a
    commit and a push if there's a file named __autocommit__ in the .git folder
    """
    for subdir, dirs, files in os.walk(root_dir):
        if '__autocommit__' in files:
            os.chdir(subdir+'/..')
            print ('Trying to commit %s' %(os.getcwd()))
            os.system("git add -A; git commit -m 'autocommit at %s'; git push origin %s" \
                  % (str(datetime.now()), branch))

def parseArgs():
    argparser = argparse.ArgumentParser()
    argparser.add_argument('-b', help='branch name to commit and push (defaults to master)')
    argparser.add_argument('-d', help='root directory of the tree search')
    return argparser.parse_args()

if __name__ == '__main__':
    args = parseArgs()

    if not args.d:
        print 'a root directory path is required. Use the -d argument'
    else:
        searchAndCommit(args.d, args.b or 'master')

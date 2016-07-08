""" 
script that looks for git repositories in a given path - subdirectories included - and 
performs a commit and a push if there's a file named __autocommit__ in the .git folder
"""
import argparse, os

def init(root_dir, branch='master'):
    for subdir, dirs, files in os.walk(root_dir):
        if '__autocommit__' in files:
            print (subdir, files)
            os.chdir(subdir+'/..')
            print os.listdir('.')
    

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
        init(args.d, args.b or 'master')

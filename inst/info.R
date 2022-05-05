# folders should be absolute or relative to the docssource folder
# do not use the trailing slash
## full path
MODULE_FOLDER="/path/to/folder"

## full path of project
PROJECT_FOLDER="/path/to/folder"

## where the source Rmd are, it is the folder where _site.yaml is.
SOURCE_FOLDER="docssource"

## target forlder. Cannot be the root of the project
STORE_FOLDER="docs"

## put `.` for organization pages, "docs" for standard github pages, or whatever folder
## you want the final web pages in. NULL for letting the files in the STORE_FOLDER
TARGET_FOLDER="."

### link to github data folder
DATALINK="https://github.com/owner/something.github.io/blob/master/data/"
# These handle the release notes from commits
MODULE_REPO="myrepo"
MODULE_REPO_OWNER="myowner"
# Mantainer email
MODULE_EMAIL=you@somewhere.com
## first version to list in the release notes
FIRST_VERSION="Version.0.0.0"
## commits you do not want in the release note
## if you do not need selection of commits, just put something absurd in them
BANNED_COMMITS_GREP=list("^#","^!","^Merge branch","spelling")
BANNED_COMMITS=list(".FuckIGotItAllWrong.")

# These handle the R vignettes

VIGNETTES_FOLDER=""

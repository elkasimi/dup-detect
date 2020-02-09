# dup-detect
Is a binary tool to show duplicate files in a given directory.

# How to build and run
1. get stack building tool [install link](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. cd dup-detect
3. stack run your_directory_where_you_suspect_duplicates
4. the duplicates if any will be displayed in console

# How does it work?
It lists all files inside directory and if it detects that a given size occurs more than once it does check with SHA256
if files with same size contain duplicates.

# References
I get the idea from this repo originally: [dup_files_detector](https://github.com/anas-aso/dup_files_detector)
with a Go implementation. I enjoyed creating a Haskell implementation with some optimisations ;)
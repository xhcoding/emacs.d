@echo off
emacs -Q --batch -l org --eval "(org-babel-tangle-file \"README.org\")"

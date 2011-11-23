#!/bin/bash

echo Sorgenti Clojure:
find ./ -name \*.clj -printf "\"%p\"\n" | xargs wc -l | sort -nr

echo -e "\nCSS:" 
find resources/public/css -name \*.css -printf "\"%p\"\n" | xargs wc -l | sort -nr


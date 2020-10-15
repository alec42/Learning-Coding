Functions have flags you can combine together for added functionality.

ls

- -a: For **a**ll files including hidden ones

- -l: for **l**ong **l**ist format of list
  
  - shows file permissions, number of links, owner name, owner group, file size, time stamp of last modification, and file / directory name

- -R: **r**ecursively lists files

- -r: **r**everse order

- -t: sort by (modification) **t**ime

cd 

- -: for previous directory

- .: for present directory

touch: peut créer un fichier

cat: peut visualiser le contenu d'un fichier

less: peut visualiser le contenu d'Un fichier d'une façon plus structurée avec des pages

- q: pour quitter et retourner à bash

- pageup/down, up & down: naviguer

- g: aller au début du fichier

- G: aller à la fin du fichier

- /search: équivalent du CMD-F il faut remplacer search par le mot

- h: pour de l'aide

file: information sur un fichier

history: liste des commandes éxecutées

ctrl-r: start typing a command and it will autofill with previous commands

cp \<file\> \<location\>: copy file

- -r: recursively copies

- -i: prompt before overwriting files if 2 files have the same name

mv: move and/or rename files

- mv \<old name\> \<new name\> to rename

- mv \<file\> \<file2 \> ... \<new location\> to move 1 or more files

- -b: backup file replacing

mkdir: create directory

- -p: creates subdirectory at the same time

rm: remove file

- -f: force remove protected or not

- -i: prompt whether to remove subdirectories & shit

- -r: recursive

- rmdir: remove directory alternatively to doing -r

find: find file

- -type: specifiy type of file

man: manuel

whatis: tells you what a command is

\>

+ rerout command to smt.

+ E.g. echo Hello World \> test.txt
  
  + Creates (overwrites) file with Hello World

+ \>\> would append the file instead of overwriting

+ \< redirects in

+ 2> redirects the error message
  
  + e.g. ls /fake/directory 2> test.txt would paste an error message (assuming directory doesn't exist) to the txt file





## Piping commands

+ e.g. if ls and there's alot of folders could do : ls -la /etc/ | less to have a scrollable view
# Getting local directories/files into Docker

Create a directory, navigate to it, and run:  

```
docker run --rm -it -v`pwd`:/data datascienceworkshops/data-science-at-the-command-line
```

to map that local directory to Docker. This will open Docker, and the local directory will be accessible in `/data` inside the container.  


To start Docker:  

```
docker run --rm -it datascienceworkshops/data-science-at-the-command-line
```

***  


# Chapter 2: Getting Started  

> Unix philosophy: each tool does one thing and does it well  

`seq` - generates a sequence of numbers  
`seq 3` returns 1 2 3  

pipes  `|`   

`grep` filters for pattern matches   

`wc` returns word count (default returns all three options below)  
* `-l` counts lines  
* `-w` counts words  
* `-c` counts characters 


`seq 3 | grep 2  returns 2`   

`seq 10 | grep 1 | wc -l`   returns 2. The `-l` flag tells `wc` to return only line count  

To rediret input and output: `>`  

`seq 10 > ten-numbers` saves the sequence as a file called `ten-numbers`

`>>` appends output to an existing file  

```
echo "Hello " > hello-world  
echo "World" >> hello-world
cat hello-world
rm hello-world
```

`cat` reads a file and prints it

`cat hello-world | wc -w` returns 2 and is identical to `< hello-world wc -w`, but in this new way you directly pass the file to the standard input of `wc` without running additional processes (i.e. - `cat`).  

Create files by redirecting output with `>` and `>>` for appending.  

Move files with `mv`.  

```
mkdir temp
mv hello-world temp
cd temp
ls
```

Can also rename files with `mv`  

```
mv hello-world bye
```
renames to `bye`  

to remove files use `rm`, and to remove an entire directory including its contents, use `rm -r` where `-r` stands for recursive.  


Copy files with `cp`. This is useful for making backups.  

```
cd /home/data/ch02/data
cp movies.txt movies_backup.txt
```

`-v` option means verbose and prints what's happening  

```
mkdir -v temp
rm -r -v temp
```


Access help with `man` for manual.  

`man cat` opens the help for `cat` and `q` exits the manual. To print some of the pages of the manual and stay in the terminal, pass the manual to `head` or `cat`:  

`man cat | cat` prints the entire manual for cat and `man cat | head -n 20` prints the first 20 lines of the manual for cat.  

`gzip` compresses and decompresses (`-d`) files:  

```
seq 10000 > file.txt
gzip file.txt
gzip -d file.txt
```

`top` gives the running processes, and `q` quits.  


use `help` for shell built-ins such as `cd`: `help cd`  

Also try `--help` after a command if `man` and `help` aren't helping.  

`Ctrl + L` clears the screen.  

`Ctrl + C` kills current process.  

`Ctrl + S` stops printing to screen, but doesn't stop process. `Ctrl + Q` resumes process

`Alt + -> and <-` moves back and forth one word  

`Ctrl + a` moves to beginning to line and `Ctrl + e` moves to end.  

`Ctrl + xx` toggle cursor between current position and start of line.  

`Ctrl + w` cut word before cursor and `Ctrl + y` paste  

`Ctrl + k` cuts everything after cursor  


***  


# Chapter 3: Obtaining Data

bas script to decompress files of various formats that does so by looking at extension  unpa 

```
#!/usr/bin/env bash
# unpack: Extract common file formats

# Display usage if no parameters given
if [[ -z "$@" ]]; then
    echo " ${0##*/} <archive> - extract common file formats)"
    exit
fi

# Required program(s)
req_progs=(7z unrar unzip)
for p in ${req_progs[@]}; do
    hash "$p" 2>&- || \
    { echo >&2 " Required program \"$p\" not installed."; exit 1; }
done

# Test if file exists
if [ ! -f "$@" ]; then
    echo "File "$@" doesn't exist"
    exit
fi

# Extract file by using extension as reference
case "$@" in
    *.7z ) 7z x "$@" ;;
    *.tar.bz2 ) tar xvjf "$@" ;;
    *.bz2 ) bunzip2 "$@" ;;
    *.deb ) ar vx "$@" ;;
    *.tar.gz ) tar xvf "$@" ;;
    *.gz ) gunzip "$@" ;;
    *.tar ) tar xvf "$@" ;;
    *.tbz2 ) tar xvjf "$@" ;;
    *.tar.xz ) tar xvf "$@" ;;
    *.tgz ) tar xvzf "$@" ;;
    *.rar ) unrar x "$@" ;;
    *.zip ) unzip "$@" ;;
    *.Z ) uncompress "$@" ;;
    * ) echo " Unsupported file format" ;;
esac
```

Compress a directory with `tar`: `tar -czvf name-of-archive.tar.gz /path/to/directory-or-file`  
* `-c` creates archive  
* `-z` uses gzip for compression  
* `-v` verbose  
* `-f` allows us to specify the filename of archive  

```
mkdir stuff
seq 1000 > stuff/my_seq.txt
tar -czvf archive.tar.gz stuff
ls
```

Now to decompress can use `unpack` cl tool from datascience toolbox.  

```
rm stuff
ls
unpack archive.tar.gz
```

Or `tar` with `-x` extract rather than `-c` create.  

```
rm stuff
ls
tar -xvzf archive.tar.gz
ls
```

`Csvkit` is a set of cl tools for working with CSV data.  

`in2csv` converts Excel spreadsheets to CSV files. `--sheet` specifies the sheet number to extract, otherwise, the first is taken.  

```
in2csv file.xlsx | head
in2csv file.xlsx > file.csv
```

`csvlook` formats the output to make it readable, and `csvcut` selects a given vector or comma-separted column names  

```
in2ccsv data.xlsx | head | csvcut -c Var1,Var2,Var3 | csvlook
```

`sql2csv` converts a relational database to csv  

```
sql2csv --db 'sqlite:///iris.db' --query 'SELECT * FROM iris WHERE sepal_length > 7.5'
```

To download data from the internet (URL), use **cURL** via `curl`. By default `curl` prints progress, which can be silenced with `-s`.  

For example, the adventures of huck finn:  

```
curl -s http://www.gutenberg.org/files/76/76-0.txt | head -n 10
```

Can save to a file with `> file.txt` or via the output option `-o file.txt`

```
curl http://www.gutenberg.org/files/76/76-0.txt > file.txt
curl -s http://www.gutenberg.org/files/76/76-0.txt -o file.txt
```

In addition to HTTP/HTTPS requests, `curl` can also download from an FTP server:  

```
curl -u username:password ftp://host/file
```

If using a shortened URL (e.g. - bitly) use the `-L`or `--location` option to automatically follow redirects: `curl -L short.bit.ly`  

`-I` or `--head` options give the HTTP header, useful for debugging. 



Web APIs that return JSON/XML can be accessed via `curl` and worked with `jq`  

`curl -s https://randomuser.me/api/1.2/ | jq`  



***  

# Chapter 4: Creating Re-usable command line tools 

Consider the following pipeline:  

```
$ curl -s http://www.gutenberg.org/files/76/76-0.txt |
> tr '[:upper:]' '[:lower:]' | 
> grep -oE '\w+' |             
> sort |                       
> uniq -c |                    
> sort -nr |                   
> head -n 10                   
   6441 and
   5082 the
   3666 i
   3258 a
   3022 to
   2567 it
   2086 t
   2044 was
   1847 he
   1778 of
```

Extracts the top ten words by count in a document. Let's turn into a cmd line tool. Can run every hour on a website, on all books, etc.  

Turn into a bash shell script. Steps:  

1. Copy and paste the one-liner into a file.  
2. Add execute permissions.  
3. Define a so-called shebang.  
4. Remove the fixed input part.  
5. Add a parameter.  
6. Optionally extend your PATH.  

Save as bash script `top-words-1.sh`:  

```
curl -s http://www.gutenberg.org/files/76/76-0.txt |
tr '[:upper:]' '[:lower:]' | grep -oE '\w+' | sort |
uniq -c | sort -nr | head -n 10
```

And use `bash` to run: `bash book/ch04/top-words-1.sh`.  

The file cannot be executed on its own, so it's not a "true" command-line tool. The reason it can't be executed directly is that we don't have the correct access permissions. In particular, you, as a user, need to have the permission to execute the file. To add permission use chmod (**change mode**).  

First copy file and rename: `cp top-words-{1,2}.sh`.  

`chmod u+x top-words-2.sh`  

The command-line argument u+x consists of three characters: (1) u indicates that we want to change the permissions for the user who owns the file, which is you, because you created the file; (2) + indicates that we want to add a permission; and (3) x, which indicates the permissions to execute. Let us now have a look at the access permissions of both files by listing the files `ls` in long format `-l`:  

```
$ ls -l top-words-{1,2}.sh
-rw-r--r--@ 1 richpauloo  staff  137 Jul  5 14:27 top-words-1.sh
-rwxr--r--@ 1 richpauloo  staff  137 Jul  5 14:30 top-words-2.sh
```

First character `-` means file and `d` means directory. These are files. Next are three sets of `rwx` which stand for reacd, write, and execute permissions. The first chunk of 3 is for you, the second 3 are for all users who share the file, and the last three are for ALL users.   

As you can see, the first chunk of `rwx` is complete for `top-words-2.sh` which we modified for the current user `u`. To set permissions for other groups, see [the docs](https://www.geeksforgeeks.org/chmod-command-linux/).  

Now you can execute the file without preceeding it with `bash`. Note that if you're in the same directory as an executable, you need to preceed it with `./`:   

```
$ ./top-words-2.sh 
6439 and
5077 the
3666 i
3258 a
3022 to
2567 it
2086 t
2044 was
1847 he
1777 of

```

Without the correct permission to execute you'll see:  

```
$ ./top-words-1.sh 
-bash: ./top-words-1.sh: Permission denied
```

Although we can already execute the file on its own, we should add a so-called **shebang** to the file. The shebang is a special line in the script, which instructs the system which executable should be used to interpret the commands.  

In our case we want to use bash to interpret our commands. Write a file `top-words-3.sh` with a shebang.  

```
#!/usr/bin/env bash
curl -s http://www.gutenberg.org/files/76/76-0.txt |
tr '[:upper:]' '[:lower:]' | grep -oE '\w+' | sort |
uniq -c | sort -nr | head -n 10
```

The name shebang comes from the first two characters: a hash (she) and an exclamation mark (bang). This command tells the OS to use `env` to run `bash` when calling this cmd line tool.  

In next step, make the code more useable by generalizing to any text. `cp top-words-{3,4}.sh`, `nano` to remove the first line with `curl`.  

Taking it another step further, you can add parameters to a shell script. For instance, consider the utility to passing a number `n` into the previous function so that `head` prints a user-defined amount of lines.  


`top-words-5.sh`: 

```
#!/usr/bin/env bash
NUM_WORDS="$1"                                        
tr '[:upper:]' '[:lower:]' | grep -oE '\w+' | sort |
uniq -c | sort -nr | head -n $NUM_WORDS 
```

* The variable `NUM_WORDS` is set to the value of `$1`, which is a special variable in Bash. It holds the value of the first command-line argument passed to our command-line tool. The table below lists the other special variables that Bash offers.  

* Note that in order to use the value of the `$NUM_WORDS` variable, you need to put a dollar sign in front of it. When you set it, you do not write a dollar sign.  

Now if we wanted to see the top 5 most-used words of our text, we would invoke our command-line tool as follows:  

```
$ cat /home/data/ch03/data/finn.txt | /home/data/ch04/top-words-5.sh 5
   6441 and
   5082 the
   3666 i
   3258 a
   3022 to
```


To make the cmd line tool accessible from everywhere, you need to add it to the `PATH`.  

Current paths are: `echo $PATH | fold`. Paths are separated by `:`, so let's transform `tr` those to new lines (`\n`):  

```
$ echo $PATH | tr ':' '\n'
/usr/local/sbin
/usr/local/bin
/usr/sbin
/usr/bin
/sbin
/bin
```


To change the `PATH` permanently, you'll need to edit the `.bashrc` or `.profile` file located in your home directory. If you put all your custom command-line tools into one directory, say, `~/tools`, then you only change the `PATH` once. As you can see, the Data Science Toolbox already has `/home/vagrant/.bin` in its `PATH`. Now, you no longer need to add the `./`, but you can just use the filename. Moreover, you do no longer need to remember where the command-line tool is located.  

Command-line tools in Python and R need to specify `python` and `Rscript`, respectively, as the interpreter after the shebang (`#!`).  

Equivalent Python and R code:  

```
#!/usr/bin/env python
import re
import sys
from collections import Counter
num_words = int(sys.argv[1])
text = sys.stdin.read().lower()
words = re.split('\W+', text)
cnt = Counter(words)
for word, count in cnt.most_common(num_words):
    print "%7d %s" % (count, word)
```

```
#!/usr/bin/env Rscript
n <- as.integer(commandArgs(trailingOnly = TRUE))
f <- file("stdin")
lines <- readLines(f)
words <- tolower(unlist(strsplit(lines, "\\W+")))
counts <- sort(table(words), decreasing = TRUE)
counts_n <- counts[1:n]
cat(sprintf("%7d %s\n", counts_n, names(counts_n)), sep = "")
close(f)
```

Processing "streaming data" is a bit different than "static data". Some cmd line tools like `sort` and `awk` require complete data before it's piped to another command, so with streaming data they become useless. However, in `R` and `Python` there are ways to deal with data coming from a non-stop stream.  

```
#!/usr/bin/env python
from sys import stdin, stdout
while True:
    line = stdin.readline()
    if not line:
        break
    stdout.write("%d\n" % int(line)**2)
    stdout.flush()
```

```
#!/usr/bin/env Rscript
f <- file("stdin")
open(f)
while(length(line <- readLines(f, n = 1)) > 0) {
        write(as.integer(line)^2, stdout())
}
close(f)
```



***  


# Chapter 5: Scrubbing Data  

`cut`, `sed`, `jq`, `csvgrep` and others.  

```
printf 'foo\nbar\nfoo' | sort | uniq -c | sort -nr
  2 foo
  1 bar
printf 'foo\nbar\nfoo' | sort | uniq -c | sort -nr | awk '{print $2","$1}' | header -a 'value, count'
  value,count
  foo,2
  bar,1
```

`uniq` only looks for different rows in sorted data, so things need to be sorted first.   


Filtering lines based on location.  

Create some dummy data with `seq`. `%{e,f,g}` options create different output. 

```
$ seq -f "Line %e" 2
  Line 1.000000e+00
  Line 2.000000e+00
$ seq -f "Line %f" 2
  Line 1.000000
  Line 2.000000
$ seq -f "Line %g" 2
  Line 1
  Line 2
```

`seq` is pretty cool. Make a bunch of files (`touch`) or directories (`mkdir`).    

```
mkdir temp
cd temp
touch $(seq -f "file%03g" 1 10)
ls
```

Let's make a dummy file and filter rows by position:  

```
seq -f "Line %g" 10 | tee lines
```

Print first 3 rows with `head`, `sed`, or `awk`. Can do the same for the last lines with `tail`.  

```
[/data]$ < lines head -n 3
Line 1
Line 2
Line 3
[/data]$ < lines sed -n '1,3p'
Line 1
Line 2
Line 3
[/data]$ < lines awk 'NR<=3'
Line 1
Line 2
Line 3
```

Removing lines with `head` and `tail`. Remove the first 3 lines. Note with `tail` you need to add one. With `head` you dont.  

```
< lines tail -n +4
< lines sed '1,3d'
< lines sed -n '1,3!p'
```


You can print (or extract) specific lines (4, 5, and 6 in this case) using a either `sed`, `awk`, or a combination of `head` and `tail`: 

```
< lines sed -n '4,6p'
< lines awk '(NR>=4)&&(NR<=6)'
< lines head -n 6 | tail -n 3
  Line 4
  Line 5
  Line 6
```  

Print odd lines with `sed` by specifying a start and a step, or with `awk` by using the modulo operator:  

```
< lines sed -n '1~2p'
< lines awk 'NR%2'
  Line 1
  Line 3
  Line 5
  Line 7
  Line 9
```

Printing even lines works in a similar manner:

```
< lines sed -n '0~2p'
< lines awk '(NR+1)%2'
  Line 2
  Line 4
  Line 6
  Line 8
  Line 10
```




































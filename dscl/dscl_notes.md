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













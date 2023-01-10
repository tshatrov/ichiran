# Ichiran

Ichiran is a collection of tools for working with text in Japanese language. It contains experimental segmenting and romanization algorithms and uses open source [JMdictDB](http://edrdg.org/~smg/) dictionary database to display meanings of words.

The web interface is under development right now. You can try it at [ichi.moe](http://ichi.moe).

## Installation

**!!!NEW!!!** There's now a [blog post](https://readevalprint.tumblr.com/post/639359547843215360/ichiranhome-2021-the-ultimate-guide) which contains detailed instructions how to get Ichiran running on Linux and Windows. It also describes how to use the new `ichiran-cli` command line interface!

1. Download JMDict data from [here](https://gitlab.com/yamagoya/jmdictdb/-/tree/master/jmdictdb/data). If you want to initialize database from scratch download [JMDict](ftp://ftp.monash.edu.au/pub/nihongo/JMdict.gz), and optionally [kanjidic2.xml](http://www.csse.monash.edu.au/~jwb/kanjidic2/kanjidic2.xml.gz) to use ichiran/kanji functionality.
2. Create a settings.lisp file based on the provided settings.lisp.template file with the correct paths to the abovementioned files and the database connection parameters.
3. The code can be loaded as a regular ASDF system. Use quicklisp to easily install all the dependencies.
4. - Easy mode: Use database dump from [the release page](https://github.com/tshatrov/ichiran/releases) to create a suitable database. Make sure `settings.lisp` contains the correct connection parameters. Use `(ichiran/maintenance:add-errata)` to make database up to date.
   - Hard mode: Use `(ichiran/maintenance:full-init)` to completely initialize the database. Use `(ichiran/maintenance:load-jmdict)` followed by `(ichiran/maintenance:load-best-readings)` to initialize only `ichiran/dict` and not `ichiran/kanji`. Either way, this will take a few hours or so.
5. Use `(ichiran/test:run-all-tests)` to check that the installation satisfies the tests.
6. Before using any word segmenting functionality, run `(ichiran/dict:init-suffixes t)` to create a suffix cache, which will improve the quality of segmentation.

## Dockerized version

Build (executed from the root of this repo):

```
docker compose build
```

Start containers (this will take longer for the first time, because the db will get imported from the dump here, and other ichiran initializations will also get done here):

```
docker compose up
```

If there were errors while importing db, or you want to import a new database you need to delete postgres data, so the postgres docker initdb scripts get called (if the folder is not empty it won't get called), and after this you can call `docker compose up` again:

```
sudo rm -rf docker/pgdata
```

Test suite:

```
$ docker exec -it ichiran-main-1 test-suite
This is SBCL 2.2.4, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
......................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
Unit Test Summary
 | 748 assertions total
 | 748 passed
 | 0 failed
 | 0 execution errors
 | 0 missing tests
```

Enter the sbcl interpreter (with ichiran already initialized):

```
$ docker exec -it ichiran-main-1 ichiran-sbcl
This is SBCL 2.2.4, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (romanize "一覧は最高だぞ" :with-info t)
"ichiran wa saikō da zo"
(("ichiran" . "一覧 【いちらん】
1. [n,vs] look; glance; sight; inspection
2. [n] summary; list; table; catalog; catalogue")
 ("wa" . "は
1. [prt] 《pronounced わ in modern Japanese》 indicates sentence topic
2. [prt] indicates contrast with another option (stated or unstated)
3. [prt] adds emphasis")
 ("saikō" . "最高 【さいこう】
1. [adj-no,adj-na,n] best; supreme; wonderful; finest
2. [n,adj-na,adj-no] highest; maximum; most; uppermost; supreme")
 ("da" . "だ
1. [cop,cop-da] 《plain copula》 be; is
2. [aux-v] 《た after certain verb forms; indicates past or completed action》 did; (have) done
3. [aux-v] 《indicates light imperative》 please; do")
 ("zo" . "ぞ
1. [prt] 《used at sentence end》 adds force or indicates command"))
* (ichiran:romanize "一覧は最高だぞ" :with-info t)
"ichiran wa saikō da zo"
(("ichiran" . "一覧 【いちらん】
1. [n,vs] look; glance; sight; inspection
2. [n] summary; list; table; catalog; catalogue")
 ("wa" . "は
1. [prt] 《pronounced わ in modern Japanese》 indicates sentence topic
2. [prt] indicates contrast with another option (stated or unstated)
3. [prt] adds emphasis")
 ("saikō" . "最高 【さいこう】
1. [adj-no,adj-na,n] best; supreme; wonderful; finest
2. [n,adj-na,adj-no] highest; maximum; most; uppermost; supreme")
 ("da" . "だ
1. [cop,cop-da] 《plain copula》 be; is
2. [aux-v] 《た after certain verb forms; indicates past or completed action》 did; (have) done
3. [aux-v] 《indicates light imperative》 please; do")
 ("zo" . "ぞ
1. [prt] 《used at sentence end》 adds force or indicates command"))
*
```

Ichiran cli:

```
$ docker exec -it ichiran-main-1 ichiran-cli -i "一覧は最高だぞ"
ichiran wa saikō da zo

* ichiran  一覧 【いちらん】
1. [n,vs] look; glance; sight; inspection
2. [n] summary; list; table; catalog; catalogue

* wa  は
1. [prt] 《pronounced わ in modern Japanese》 indicates sentence topic
2. [prt] indicates contrast with another option (stated or unstated)
3. [prt] adds emphasis

* saikō  最高 【さいこう】
1. [adj-no,adj-na,n] best; supreme; wonderful; finest
2. [n,adj-na,adj-no] highest; maximum; most; uppermost; supreme

* da  だ
1. [cop,cop-da] 《plain copula》 be; is
2. [aux-v] 《た after certain verb forms; indicates past or completed action》 did; (have) done
3. [aux-v] 《indicates light imperative》 please; do

* zo  ぞ
1. [prt] 《used at sentence end》 adds force or indicates command
```

## Documentation

There is no documentation yet. Any API is considered unstable at this point.

The basic functionality is `(ichiran:romanize "一覧は最高だぞ" :with-info t)`, but feel free to explore further.

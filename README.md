# Ichiran

Ichiran is a collection of tools for working with text in Japanese language. It contains experimental segmenting and romanization algorithms and uses open source [JMdictDB](http://edrdg.org/~smg/) dictionary database to display meanings of words.

The web interface is under development right now. You can try it at [ichi.moe](http://ichi.moe).

## Installation

**!!!NEW!!!** There's now a [blog post](https://readevalprint.tumblr.com/post/639359547843215360/ichiranhome-2021-the-ultimate-guide) which contains detailed instructions how to get Ichiran running on Linux and Windows. It also describes how to use the new `ichiran-cli` command line interface! 

1. Download JMDict data from [here](https://gitlab.com/yamagoya/jmdictdb/-/tree/master/jmdictdb/data). If you want to initialize database from scratch download [JMDict](ftp://ftp.monash.edu.au/pub/nihongo/JMdict.gz), and optionally [kanjidic2.xml](http://www.csse.monash.edu.au/~jwb/kanjidic2/kanjidic2.xml.gz) to use ichiran/kanji functionality.
2. Create a settings.lisp file based on the provided settings.lisp.template file with the correct paths to the abovementioned files and the database connection parameters.
3. The code can be loaded as a regular ASDF system. Use quicklisp to easily install all the dependencies. 
4.
   * Easy mode: Use database dump from [the release page](https://github.com/tshatrov/ichiran/releases) to create a suitable database. Make sure ```settings.lisp``` contains the correct connection parameters. Use ```(ichiran/maintenance:add-errata)``` to make database up to date.
   * Hard mode: Use ```(ichiran/maintenance:full-init)``` to completely initialize the database. Use ```(ichiran/maintenance:load-jmdict)``` followed by ```(ichiran/maintenance:load-best-readings)``` to initialize only `ichiran/dict` and not `ichiran/kanji`. Either way, this will take a few hours or so.
5. Use ```(ichiran/test:run-all-tests)``` to check that the installation satisfies the tests.
6. Before using any word segmenting functionality, run ```(ichiran/dict:init-suffixes t)``` to create a suffix cache, which will improve the quality of segmentation.

## Documentation

There is no documentation yet. Any API is considered unstable at this point.

The basic functionality is ```(ichiran:romanize "一覧は最高だぞ" :with-info t)```, but feel free to explore further.

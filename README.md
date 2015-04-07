# Ichiran

Ichiran is a collection of tools for working with text in Japanese language. It contains experimental segmenting and romanization algorithms and uses open source [JMdictDB](http://edrdg.org/~smg/) dictionary database to display meanings of words.

The web interface is under development right now. You can try it at [ichi.moe](http://ichi.moe).

## Notes

Currently incompatible with latest Postmodern due to [this issue](https://github.com/marijnh/Postmodern/issues/70). Please use ichiran branch of [my fork of Postmodern](https://github.com/tshatrov/Postmodern/tree/ichiran) until this is fixed. 

## Installation

1. Download [JMDict](ftp://ftp.monash.edu.au/pub/nihongo/JMdict.gz) and JMDict data from [here](http://edrdg.org/~smg/cgi-bin/hgweb-jmdictdb.cgi/file/9389981dcd33/pg/data?style=gitweb). Optionally also obtain [kanjidic2.xml](http://www.csse.monash.edu.au/~jwb/kanjidic2/kanjidic2.xml.gz) to use ichiran/kanji functionality.
2. Create a settings.lisp file based on the provided settings.lisp.template file with the correct paths to the abovementioned files and the database connection parameters.
3. The code can be loaded as a regular ASDF system. Use quicklisp to easily install all the dependencies. However note the Postmodern remark above.
4. Use ```(ichiran/maintenance:full-init)``` to completely initialize the database. Use ```(ichiran/maintenance:load-jmdict)``` followed by ```(ichiran/maintenance:load-best-readings)``` to initialize only ichiran/dict and not ichiran/kanji. Either way, this will take a few hours or so.
5. Before using any word segmenting functionality, run ```(ichiran/dict:init-suffixes t)``` to create a suffix cache, which will improve the quality of segmentation.

## Documentation

There is no documentation yet. Any API is considered unstable at this point.

The basic functionality is ```(ichiran:romanize "一覧は最高だぞ" :with-info t)```, but feel free to explore further.

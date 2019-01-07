# hsakamai

[![Hackage version](https://img.shields.io/hackage/v/hsakamai.svg?style=flat)](https://hackage.haskell.org/package/hsakamai)  [![Build Status](https://travis-ci.org/junjihashimoto/hsakamai.png?branch=master)](https://travis-ci.org/junjihashimoto/hsakamai)

Akamai API for Haskell.

# Install

```
$ stack install
```

# Usage for Netstorage

Put ```netstorage.yml``` in a local directory.
The format is below.

```
$ cat > netstorage.yml
hostname: hostname-of-netstorage
key: secret-key
keyname: keyname
cpcode: cpcode
ssl: false
```
Next use ```netstorage``` command.

```
$ netstorage --help
Usage: netstorage COMMAND

Available options:
-h,--help                Show this help text

Available commands:
download                 download
upload                   upload
dir                      dir
stat                     stat
delete                   delete
config                   config
```

# Usage for Fast-Purge

Put ```edgegrid.yml``` in a local directory.
The format is below.

```
$ cat > edgegrid.yml
clientsecret: xx
hostname: xx
accesstoken: xx
clienttoken: xx
```

Next use ```purge``` command.

```
$ purge --help
Usage: purge COMMAND

Available options:
-h,--help                Show this help text

Available commands:
invalidate-url           invalidate-url
invalidate-cpcode        invalidate-cpcode
invalidate-tag           invalidate-tag
delete-url               delete-url
delete-cpcode            delete-cpcode
delete-tag               delete-tag
config                   config
$ purge invalidate-url Production https://foo.com
```

# References

* https://learn.akamai.com/en-us/webhelp/netstorage/netstorage-http-api-developer-guide/GUID-22B017EE-DD73-4099-B96D-B5FD91E1ED98.html
* https://developer.akamai.com/legacy/introduction/Client_Auth.html

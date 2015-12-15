SerealFS
========

## Introduction
SerealFS is a (toy) project to map Sereal-encoded files to the _readonly_ FUSE-based file system.
Excluding the attractiveness factor of idea, the project itself can be helpful:
* when user wants to use familiar command lines tools to analyze Sereal files;
* due to the Haskell's support of lazy computaion the Sereal-files are _read_ and _decoded_ on-demand.

## Implementation
Mapping is mostly straighforward: hashes and arrays are mapped to the directory structures.
The hash keys are the filenames/dirnames and contents of the file/dir is contents of that hash entry. 
Arrays are also mapped to dirs, but entry name in this case is the index of the entry(0-based).

Let's see an example. The following Perl structure

```perl
{
    Alibaba => {
        sons => [
            Ali,
            Liba,
            Baba
        ],

        age => 1728
    }
}
```

will be mapped as:

```
Alibaba/
    sons/
          0 (File with `Ali` as contents)
          1 (File with `Liba` as contents)
          2 (File with `Baba` as contents) 
    age (File with `1728` as contents)
```

## Demo
![serealfs demo](/serealfs_demo.gif)

## Usage

### Building procedure
To install the project you need ghc and cabal installed on your system. Installation steps are simple:
* git clone https://github.com/manzur/serealfs.git 
* cd serealfs
* cabal install

### How to mount
SerealFS works as a "presentation layer" over the directory with Sereal encoded files.
So to use it you need to mount the `dir` with Sereal files in it you should execute:
(MacOS users don't need to specify `-o nonempty`):

```
serealfs dir -o nonempty
```

And that's it, now you can traverse the dir to see the contents of your files. 
Note: SerealFS will show then directory entries only for the top-level Sereal encoded files 
in the mounted directory, others will be just hidden.

### Examples

Find if the Sereal contains the given key:
```
find -name key .
```

Get the value of the key when the path is given:
```
cat ./path/to/key
```

List all the keys of the hash:
```
cd ./hash_or_array && ls
```

Get all the values for the given key(without separating files):
```
find -name key -exec cat '{}' \;
```

### Known bugs
The SerealFS works only with the Sereal files of the version3 and does not support the following tags:

* ALIAS
* COPY
* EXTEND
* LONG DOUBLE
* MANY
* OBJECT
* OBJECTV
* OBJECT_FREEZE
* OBJECTV_FREEZE
* PACKET_START
* PAD
* REFP
* REGEXP
* WEAKEN

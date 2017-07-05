
# What is this?

For the most part, this a `Haskell` project which creates a pile of images for a presentation.

The Haskell project creates an executable which:
- creates an `./images` directory and fills it full of images
- create a `slides.md` file which references those images
wherever the executable  is run.

# Usage

To build a slideshow, assuming you have `nix` and `pandoc` installed:
```
nix-shell
cabal configure && cabal build
./dist/build/waterflow/waterflow
./build-slides.sh # or whatever pandoc commands you want to run on slides.md
```

If you're happy with the above, you have `nix` installed, and you want to type fewer characters:
```
nix-build release.nix
```
will create a directory named `result` with the slideshow in it.


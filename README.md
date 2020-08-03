# Pythas Examples

This repository contains a collection of application examples for the [```Pythas```](https://github.com/pinselimo/Pythas) library.
It will **grow** with time and stay up to date with the master branch of ```Pythas```. 

## Examples

The examples are contained in Jupyter Notebooks in the ```notebooks``` directory. Its subdirectories contain the accompanying Haskell code.

0. [Appetizer using ```matplotlib``` to analyze a toy world-model implemented in Haskell](notebooks/0 - Wonderland.ipynb)
1. [Introduction to ```Pythas``` going over its accompanying ```Examples.hs``` module](notebooks/1 - Simple functions.ipynb)

## Install / Dependencies

Obviously one dependency is the ```Pythas``` library. It can be obtained from [GitHub](https://github.com/pinselimo/Pythas).
You will also have to obtain either [```Stack```](https://docs.haskellstack.org/en/stable/install_and_upgrade/) or [```GHC```](https://www.haskell.org/ghc/).

Although part of ```Pythas``` source is implemented in Haskell, you will not need to download any Haskell libraries. ```Pythas``` provides everything to compile its Haskell source except for the compiler itself. It will keep your global Haskell environment completely untouched as it is written with compatibility in mind.

## Contributing

I'd be very happy to add some more applied examples. So if you have a FOSS project that uses ```Pythas``` and you think it would be a good fit, let me know! Of course your example can be licensed differently than the rest of the notebooks.

If you find any mistakes, typos or have questions regarding any of the example notebooks, feel free to raise an issue on GitHub. 

## License

```Pythas``` is licensed under the LGPLv3 license. Some low level parts are useful enough to be available under the ```MIT``` license. This will be specified in their respective repositories.
This part is licensed under LGPLv3. Please refer the accompanying ```COPYING``` and ```COPYING.LESSER``` files for details.


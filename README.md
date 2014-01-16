Hugo Reyes
==========

[Hugo Reyes](http://en.wikipedia.org/wiki/Hugo_Reyes) is a character on the television series _Lost_.
It is also the name of a renderer, written in Scala, that uses the Reyes algorithm (or rather, something very similar to it), in order to generate imagery.

This renderer is partially based on the one presented in Alexander Boswell's [tutorial](http://www.steckles.com/reyes1.html).
The pipeline is basically the same (start with parametric surfaces, split them along UVs, dice, bust, and shade).
This version is intended to be simpler and easier to follow in learning how the Reyes algorithm works; as a consequent, it is often slower and unoptimized.
My code is largely written to be immutable; i.e. geometry and micropolygon grids are not modified as they pass through the pipeline; instead, new objects are generated and passed down.
One result of this is that everything after splitting is completely parallelizable.

None of the code has been translated directly, so if you're following Boswell's tutorial, you'll find that it doesn't match up. A lot of changes in the structure were due to designing the classes in a "Scala-esque" way, e.g. by using immutable objects instead of performing calculations in-place.

Shaders are passed as Scala lambdas; for example, a displacement shader can be written that takes the input vertex and UV, displacing the vertex based on the UV position.

Compiling
=========
I use IntelliJ IDEA to compile the project; it might take a bit of reorganizing and writing config files to get it to compile with another build tool, such as SBT.
The project should compile on a standard JVM+Scala install.

Samples
=======
Check out the Wiki for code to generate [sample renders](https://github.com/sdao/hugo-reyes/wiki/Cool-Test-Renders)!
![Spiky render](https://raw.github.com/wiki/sdao/hugo-reyes/spiky.png)

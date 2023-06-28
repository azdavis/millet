# Security

Millet performs I/O to read arbitrary user input and, in the case of formatting **only**, also overwrite user files.

When using smlfmt as the formatting engine, it searches for a binary called `smlfmt` in your PATH and executes it.

Millet should never access the network.

A user could probably "DOS" themselves by asking Millet to analyze a massive or complicated set of files, but we don't really consider this to be a security threat.

That being said, if there is a security issue with Millet, let us know at:

ariel DOT z DOT davis AT icloud DOT com

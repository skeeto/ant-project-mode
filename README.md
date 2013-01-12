# Emacs Java Support

See the header comments in each file for full documentation. The
recommended usage is in java-mode-plus.el.

To install drop these in your load-path somewhere and require them
(after enabling ido-mode, if you use ido-mode),

```el
(require 'java-mode-plus)
(require 'java-docs)
```

## Depreciation

**NOTICE**: The java-docs portion of this package is depreciated. It
has been replaced by the more powerful
[javadoc-lookup](https://github.com/skeeto/javadoc-lookup). Use that
instead.

To use java-docs you'll need to tell it where to find some
documentation. For example,

```el
(java-docs "/usr/share/doc/openjdk-6-jdk/api")
```

The snippets directory contains some YASnippets that hook into
java-docs class completing reads.

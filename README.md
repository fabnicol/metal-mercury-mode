# Metal Mercury Mode

[![MELPA](http://melpa.org/packages/metal-mercury-mode.svg)](http://melpa.org/#/metal-mercury-mode)

Major mode for editing mercury (http://mercurylang.org) files.

The default mercury-mode (derived from prolog-mode) seems to have
many issues (syntax and indentation both fail).

In this case, it seems easier to rewrite mercury-mode as an
independent mode that just handles exactly what it needs to.

## Installation
To install, clone the repository via:

```
cd ~/.emacs.d
git clone https://github.com/ahungry/metal-mercury-mode.git
```

Then, make sure to add the following to your ~/.emacs:

### Using require

```lisp
(add-to-list 'load-path "~/.emacs.d/metal-mercury-mode/")
(require 'metal-mercury-mode)
```

### Using metal-mercury-mode.el

Keybindings:

- `C-c C-c` : Compile your current file via `mmc --make file_name`
- `C-c C-r` : Compile and run your current file (no interactive input)

### Patches against original code (F. Nicol)

This emacs mode is to preferred over the Prolog-based one
that is available on ELPA.   
I have patched the original source code as follows:    
   
+ added the following commands:    
  `M-x metal-mercury-all-decls`     Find all declarations   
  `M-x-metal-mercury-funcs`         Find all function declarations   
  `M-x-metal-mercury-instances`     Find all instance declarations  
  `M-x-metal-mercury-pragmas`       Find all pragma declarations         
  `M-x-metal-mercury-preds`         Find all predicate declarations   
  `M-x-metal-mercury-types`         Find all type declarations    
  `M-x-metal-mercury-typeclasses`   Find all typeclass declarations   
  
+ Automatically save all buffers after 
one of the above commands is invoked.    
+ Automatically rename corresponding `*grep*` buffers 
into `*name*` where `name`is the name of the command.

## License
GPLv3

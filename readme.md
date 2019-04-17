[![img](http://melpa.org/packages/msvc-badge.svg)](http://melpa.org/#/msvc) [![img](http://stable.melpa.org/packages/msvc-badge.svg)](http://stable.melpa.org/#/msvc)  

# Main Document

[MSVC User Manual (English)](./doc/manual.en.md)  
[MSVC User Manual (Japanese)](./doc/manual.ja.md)  

# Notice

Preparing to support Emacs 26.x  
Because flymake has changed a lot with 26.x .  

# What's New?

## MSVC 2019-04-15 was released

### new feature

-   Supported Visual Studio 2019.
-   Can visit buffer to the related source file.  
    `msvc-mode-feature-visit-to-related-source-code-buffer`  
    This function visits the buffer associated with the current buffer.  
    foo.cpp -> foo.h -> foo.inl -> foo.cpp -> &#x2026;

## MSVC 2018-01-06 was released

### others

-   Renamed the symbol used in msvc-activate-projects-after-parse and msvc&#x2013;flymake-back-end and msvc&#x2013;flymake-manually-back-end.  
    'clang -> 'clang-server
-   Small refactoring.

## MSVC 2017-12-26 was released

### others

-   Small bug fixes.

## MSVC 2017-12-04 was released

### others

-   Small bug fixes.

## MSVC 2017-06-11 was released

### new feature

-   Supported Visual Studio 2017.

### others

-   Supported English Manual
-   Refactoring and optimization.
-   Small bug fixes.

## MSVC 2016-12-21 was released

### new feature

-   Supported Visual Studio 2017RC.

## MSVC 2015-04-21 was released

### new feature

-   Added solution build report display target.

## MSVC 2015-04-05 was released

### new feature

-   The database name generate by MD5.

## MSVC 2015-02-03 was released

### bug fix

-   Small bug fixes.

## MSVC 2014-06-09 was released

### new feature

-   Can jump to error line in the file from the project & solution build report buffer.

## MSVC 2014-05-31 was released

### new feature

-   Visual Studio of different versions became able to coexist.

### bug fix

-   Small bug fixes.

## MSVC 2013-09-30 was released

### new feature

First release

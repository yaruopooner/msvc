
# Table of Contents

1.  [Provided Functions](#orgf1c699d)
    1.  [Project Management](#org264424d)
    2.  [Coexistence and use of Visual Studio with different versions](#org6dd4aa0)
    3.  [IntelliSense-like Code Completion](#orgccdd5e3)
    4.  [Syntax check by flymake](#orgeac8d8d)
    5.  [Jump and return to definition/declaration](#org3a5e04d)
    6.  [Jump to include file & return](#org1a14b02)
    7.  [Build a project or solution](#orgca52240)
    8.  [Start Visual Studio from project or solution](#org110cc9d)
2.  [Essential environment](#org153e51c)
3.  [Required package](#org0b6774a)
4.  [Scope of correspondence](#org1172482)
5.  [Limitations](#orgd4d2329)
6.  [Installation](#org1a60dad)
    1.  [Emacs startup setting](#orgb5943d4)
    2.  [Emacs package settings](#org5ee155d)
    3.  [Initialization setting](#org6ad0e77)
7.  [how to use](#org74bab8d)
    1.  [Project parsing and registration 1](#orgb5dbebc)
        1.  [Registration sample](#org137a769)
        2.  [Required property](#orgcf5caca)
        3.  [Option property](#org7701017)
    2.  [Project parsing and registration 2](#orgf2635a5)
    3.  [Active project buffer](#orgda647c0)
    4.  [Code completion](#org65ba336)
    5.  [Syntax check](#org237669a)
    6.  [Jump and return to definition/declaration/include](#orgf79d7bd)
    7.  [Jump and return for include files](#org6824c17)
    8.  [Build project or solution](#orgbec1141)
    9.  [Jump to the error file from the build log of the project or solution](#org37e9564)
    10. [Re-parsing project or solution](#orgee9b6e7)
    11. [Launching Visual Studio](#org297c9c2)



<a id="orgf1c699d"></a>

# Provided Functions

By parsing the Visual Studio project file,the following functions become available.  
It is not necessary to start Visual Studio in order to use the function. It is OK if it is installed.  


<a id="org264424d"></a>

## Project Management

When a source file belonging to an activated project is opened, it is automatically registered in the management buffer.  
Also, msvc-mode is automatically turned on in the corresponding source file buffer.  
When you deactivate a project, msvc - mode of all belonging source code buffers is turned off.  

A management buffer is created for each project  
![img](./sample-pic-buffers.png)  

In management buffer  
![img](./sample-pic-project-buffers.png)  


<a id="org6dd4aa0"></a>

## Coexistence and use of Visual Studio with different versions

You can specify the product name of Visual Studio to be used for each solution / project handled  
by msvc even when multiple Visual Studio [2022/2019/2017|2015|2013|2012|2010] versions of different Visual Studio are installed.  
This makes the old version of Visual Studio available only for specific projects.  


<a id="orgccdd5e3"></a>

## IntelliSense-like Code Completion

We provide IntelliSense-like completion function using auto-complete and libclang.  
We will complement the project based on CFLAGS and include.  
It is possible to supplement using ac-source-semantic as the information source without using libclang.  

![img](./sample-pic-complete.png)  


<a id="orgeac8d8d"></a>

## Syntax check by flymake

We will check syntax on flymake based on the project CFLAGS and source code.  
flymake back end is MSBuild.  

![img](./sample-pic-flymake.png)  


<a id="org3a5e04d"></a>

## Jump and return to definition/declaration

In the buffer under project management,  
you can jump to the source file where class/method/function/enum etc is defined/declared using ac-clang function.  
I think that if you have used a tag jump function such as GTAGS, it is a familiar function.  


<a id="org1a14b02"></a>

## Jump to include file & return

In the buffer under project management, you can jump to the file written in include using CEDET function.  
Also, it is possible to return to the jump source by specific operation.  
The jump history is stacked and popped back when returning, so it is possible to jump multiple times.  
The difference with the tag file system is that it is not necessary to generate a tag file beforehand and it is advantageous that it can jump on the fly.  


<a id="orgca52240"></a>

## Build a project or solution

You can run builds on projects activated on Emacs.  
Build logs are displayed in the buffer.  
Jumping from the build log to the error file is also possible.  

![img](./sample-pic-build.png)  


<a id="org110cc9d"></a>

## Start Visual Studio from project or solution

Open Visual Studio and open the project activated on Emacs.  
Even if the same project is already open, start Visual Studio again.  
Visual check of process check etc is not done.  
I am testing whether it can be done via powershell.  


<a id="org153e51c"></a>

# Essential environment

-   GNU Emacs 24.1 or higher  
    Operation guaranteed only after 24.1  
    26.x is not yet supported
-   shell  
    One of the following  
    CYGWIN [64|32] bit (recommended)  
    MSYS  
    CMD
-   Microsoft Windows [64|32] bit
-   Microsoft Visual Studio [2022/2019/2017|2015|2013|2012|2010]  
    Use CL.exe/MSBuild.exe


<a id="org0b6774a"></a>

# Required package

Embedded with Emacs standard preinstalled & installable packages.  
If msvc is installed with M-x list-packages it will be installed automatically.  
Manual installation Only the following packages need to be installed.  

-   CEDET(built-in)
-   flymake(built-in)
-   auto-complete
-   yasnippet
-   ac-clang


<a id="org1172482"></a>

# Scope of correspondence

-   Emacs [64|32]bit  
    It works after CEDET becomes standard built-in version  
    
    Emacs tests below  
    <http://www.gnu.org/software/emacs/>  
    <https://github.com/yaruopooner/emacs-build-shells>  
    <https://github.com/chuntaro/NTEmacs64>

-   Shell  
    -   CYGWIN [64|32] bit  
        $ uname -r  
        1.7.29(0.272/5/3)  
        CYGWIN checked [64|32] bit operation
    -   MSYS2 [64|32] bit  
        OK
    -   CMD  
        cmdproxy, cmd checked operation

-   Microsoft Windows [64|32] bit  
    -   [10|8.x|7]  
        Operation test only with Professional 64 bit
    -   [Vista|XP]  
        Not supported

-   Microsoft Visual Studio [Community|Professional|Enterprise]  
    [2022/2019/2017|2015|2013|2012|2010] Operation checked

-   SDK  
    Tested in the sample project of SDK below.  
    Confirm that the API of the target SDK is supplemented by ac-clang.  
    -   Windows SDK 10.0.15.x
    -   Direct X SDK(June 2010)  
        Build test with some samples
    -   ISO C++ Standard [C++11|C++14|C++17]  
        Test library and template completion
    -   Boost  
        Test with Nuget Package


<a id="orgd4d2329"></a>

# Limitations

1.  Precompiled header (PCH) can not be used  
    PCH of Visual Studio can not be used.  
    It is ignored by MSVC even if it is set in the project to use PCH.


<a id="org1a60dad"></a>

# Installation


<a id="orgb5943d4"></a>

## Emacs startup setting

Make GNU Emacs bootable from CYGWIN or MSYS or CMD.  
If you launch the .emacs file in the HOME directory of each shell and start it,  
it is a good idea to write the following per .bashrc.  

Let's set the path where Emacs is located to  
c:/emacs/64/emacs-24.3-20130503  

-   CYGWIN  
    
        alias emacs64-243-1='/cygdrive/c/emacs/64/emacs-24.3-20130503/bin/emacs.exe --debug-init'
        alias emacs='emacs64-243-1'

-   MSYS  
    
        alias emacs64-243-1='c:/emacs/64/emacs-24.3-20130503/bin/emacs.exe --debug-init'
        alias emacs='emacs64-243-1'

-   CMD  
    There is no setting in particular.  
    There will be no problem if the path passes through  
    c:/emacs/64/emacs-24.3-20130503/bin


<a id="org5ee155d"></a>

## Emacs package settings

The following package settings are required.  
The order in which settings are executed is as follows.  

-   CEDET(built-in)
-   flymake(built-in)
-   auto-complete
-   yasnippet
-   ac-clang

Since a sample file that can set the necessary minimum setting is attached,  
those who do not have the self-setting regarding the package load the attached file and copy it.  
If you already use the above package and have your own settings,  
we recommend checking whether the settings conflict.  
See msvc/.minimal-config-sample/init.el.  
init.el is written to work when placed in ~/.emacs.d/ or below.  
Please extract the code as necessary.  

Caution  
Since ac-clang works with external programs, it can not execute with elisp package alone.  
You need to self-build the external program or download and install the prebuilt binaries.  
For details, refer to the ac-clang manual.  
<https://github.com/yaruopooner/ac-clang>  


<a id="org6ad0e77"></a>

## Initialization setting

After executing the initial settings including the loading of the above-mentioned required packages  
(CEDET/flymake/auto-complete/yasnippet/ac-clang), the following must be executed.  

Basically just writing the following settings in .emacs is fine.  
The following description assumes that the package directory is located in "~/.emacs.d",  
so please modify it according to your own environment.  

    (add-to-list 'load-path (expand-file-name "msvc/" "~/.emacs.d"))
    
    (require 'msvc)
    
    (setq w32-pipe-read-delay 0)
    (when (msvc-initialize)
      (msvc-flags-load-db :parsing-buffer-delete-p t)
      (add-hook 'c-mode-common-hook 'msvc-mode-on t))


<a id="org74bab8d"></a>

# how to use


<a id="orgb5dbebc"></a>

## Project parsing and registration 1

Execution of the function with the following parameters is executed asynchronously and the corresponding project database is created in msvc-db.  
Project management, completion, syntax check etc. are executed based on this database.  
The databaseized project refers to the date information and rebuilds the database only when the same project receives a re-parse request,  
if the project is newer than the date of the last database creation.  

Updating factors are changed  
project properties, SVN and other version control tools update project files, etc.  
When the project becomes active, a buffer with the corresponding project name is created.  
The project buffer name is based on the following format.  

**MSVC Project<\`db-name\`>**  

Also, msvc-mode is automatically applied when the source code belonging to the project is open or opened while active.  
Buffers with msvc-mode applied will be displayed in the mode line as **MSVC\`product-name\`[platform|configuration]** .  

We will parse & activate with the following function.  
`(msvc-activate-projects-after-parse &rest args)`  

Multiple projects can be activated simultaneously.  
There is no particular number restriction.  
Projects with different platforms and configurations on the same project can not be activated at the same time.  
In this case, the project that was activated first becomes effective.  
The reason is that there is only one target source buffer.  
Because the buffer itself maintains what project and what [Platform|Configuration] it will operate on.  
Because the buffer itself maintains what project, what 'platform | configuration' will work.  
The buffer itself holds which projects are active and what platforms and configurations to work with.  


<a id="org137a769"></a>

### Registration sample

    (msvc-activate-projects-after-parse :solution-file "d:/DirectXSamples/SubD11/SubD11_2010.sln"
                                        :project-file "d:/DirectXSamples/SubD11/SubD11_2010.vcxproj"
                                        :platform "x64"
                                        :configuration "Release" 
                                        :product-name "2013" 
                                        :toolset "x86_amd64"
                                        :md5-name-p nil
                                        :force-parse-p nil
                                        :allow-cedet-p t
                                        :allow-ac-clang-p t
                                        :allow-flymake-p t
                                        :cedet-root-path "d:/DirectXSamples/SubD11"
                                        :cedet-spp-table nil
                                        :flymake-back-end nil
                                        :flymake-manually-p nil
                                        :flymake-manually-back-end nil)


<a id="orgcf5caca"></a>

### Required property

-   `:solution-file` or `:project-file`  
    If either is set, it is OK.  
    `:solution-file`  
    When only this key is specified All projects included in the solution are parsed and activated.  
    The following functions are added.  
    It is possible to call build call of solution from activated project.  
    If the number of projects registered in the solution is small, it is better to write in this style.  
    `:project-file`  
    With this key alone Only the specified project is parsed and activated.  
    Function related to the solution can not be executed.  
    `:solution-file` & `:project-file` When both are specified.  
    It has the same effect as specifying only solution, but in case of solution only,  
    all belonging projects are parsed & activated, whereas only specified project is parsed & activated.  
    If you have a huge number of projects registered in the solution,  
    it is a good idea to describe only the projects you need in this style.
-   `:platform`  
    Parse Specify the platform to activate.  
    It must be a platform that exists in the project file.
-   `:configuration`  
    Parse Specify the configuration to activate.  
    It must be the configuration that exists in the project file.


<a id="org7701017"></a>

### Option property

-   `:version`  
    It has been discontinued because it has been renamed.  
    Use the following :product-name .
-   `:product-name`  
    Specify the product name of Visual Studio used for CFLAGS creation, syntax check, and solution build passed to project parse, ac-clang.  
    The designation is made with a character string.  
    Note that it is not an integer.  
    Designated as "2013".  
    If not specified, the value of msvc-env-default-use-product-name is set.  
    msvc-env-default-use-product-name is assigned the latest Visual Studio detected at startup.  
    You can change the product-name used in the standard by resetting the value of msvc-env-default-use-product-name after executing msvc-initialize.
-   `:toolset`  
    Specify the compiler platform.  
    The designation is made with a character string. Be careful as it is not a symbol.  
    If not specified, the value of msvc-env-default-use-toolset is set.
-   `:md5-name-p`  
    If no key is specified, it becomes nil. (recommended)  
    When t is set, treat the path as conflicting with the following restrictions as msvc,  
    convert the name to MD5 and treat it as a fixed-length name that does not conflict.  
    If the absolute path including the project name to be parsed or the absolute path including the database name after parsing exceeds MAX\_PATH (260 characters),  
    it can not be handled on the shell.  
    The UNICODE path of NTFS can be used up to 32 kB, but the value that can be handled on shell (cmd.exe) is limited to MAX\_PATH.
-   `:force-parse-p`  
    If no key is specified, it becomes nil. (recommended)  
    Even for projects that have already been parsed, they are forcibly parsed.  
    It is mainly for debugging purpose.
-   `:sync-p`  
    If no key is specified, it becomes nil. (recommended)  
    Synchronize parse.  
    So, if there are many project files, it will take time to return from the function.  
    I think that there is no need to use it for the first time to use it.  
    It is mainly for  
    debugging purpose.
-   `:allow-cedet-p`  
    t (recommended)  
    Use the CEDET feature.  
    It is registered in CEDET project management and semantic is activated.  
    If it is nil, the jump to the include file by semantic can not be used.
-   `:allow-ac-clang-p`  
    t (recommended)  
    Use the ac-clang feature.  
    Code completion by Clang and jump to declaration/definition become possible.  
    If it is nil, the jump by Clang becomes unusable, and completion will use semantic as the information source.
-   `:allow-flymake-p`  
    t (recommended)  
    Use the flymake feature.  
    We use syntax check by MSBuild.
-   `:cedet-root-path`  
    `:allow-cedet-p` It is referred to only when this value is t.  
    Specify CEDET ede project criteria directory.  
    \*.ede file is generated in the specified directory.  
    Usually it does not matter in the directory where the project file is located.  
    However, care should be taken when the placement of the source code is not the same level as the project file placement directory or descendant.  
    In this case, you need to specify a common parent directory that will be the same level or descendant.
-   `:cedet-spp-table`  
    If no key is specified, it becomes nil. (recommended)  
    `:allow-cedet-p` It is referred to only when this value is t.  
    Associative table of words that semantic wants to replace when parsing source.  
    It is a table that replaces define etc. which semantic can not interpret.  
    If semantic.cache can not be created successfully, setting is necessary.  
    Sample description below  
    
        :cedet-spp-table '(
                           ("ALIGN"              . "")
                           ("FORCE_INLINE"       . "")
                           ("NO_INLINE"          . "")
                           ("THREAD_LOCAL"       . "")
                           ("DLL_IMPORT"         . "")
                           ("DLL_EXPORT"         . "")
                           ("RESTRICT"           . ""))
    
    See the CEDET manual for details.
-   `:flymake-back-end`  
    If no key is specified, it becomes nil. (recommended)  
    `:allow-flymake-p` It is referred to only when this value is t.  
    Specify the back-end of flymake.  
    Specify it only when using other than MSBuild.  
    The following are possible symbols  
    'msbuild  
    'clang-server
-   `:flymake-manually-p`  
    If no key is specified, it becomes nil. (recommended)  
    `:allow-flymake-p` It is referred to only when this value is t.  
    Do not automatically start syntax check of flymake.  
    Valid only for manual check.
-   `:flymake-manually-back-end`  
    If no key is specified, it becomes nil. (recommended)  
    `:allow-flymake-p` It is referred to only when this value is t.  
    Specify the back-end of flymake-manually.  
    Specify it only when using other than MSBuild.  
    The following are possible symbols  
    'msbuild  
    'clang-server


<a id="orgf2635a5"></a>

## Project parsing and registration 2

If you create a file '.msvc' in '~/.emacs.d/', it will be executed at initialization.  
Using `msvc-activate-projects-after-parse` you can activate the project at emacs launch.  
It is a good idea to describe project settings to be used for a long time in this file.  


<a id="orgda647c0"></a>

## Active project buffer

Active projects are given the following buffer names:  
**MSVC Project<\`db-name\`>**  

When entering the buffer, the parameter specified by `msvc-activate-projects-after-parse` can be confirmed.  
Also, buffers belonging to the project are displayed in `:target-buffers` in the currently opened source code buffer.  
Hold the cursor to the buffer name and enter Enter or jump to the buffer if you mouse click.  

When this buffer is deleted, msvc - mode of all relevant source code buffers is turned off.  


<a id="org65ba336"></a>

## Code completion

Available places: msvc-mode on source code buffer  

`:allow-ac-clang-p` If this value is t, it can be complemented.  

-   operation  
    -   Completion  
        Key Bind    : . or -> or ::  
        Explanation : Completion starts automatically.


<a id="org237669a"></a>

## Syntax check

Available places: msvc-mode on source code buffer  

`:allow-flymake-p` If this value is t, it can be complemented.  
It automatically starts with buffer modified.  

Manual syntax check with "F5".  
If `:allow-flymake-p` is t, you can use both Auto and Manual even if the value of `:flymake-manually-p` is nil.  

The error display style can be changed by setting symbols in the following variables.  
`(setq msvc-flymake-error-display-style DISPLAY-STYLE-SYMBOL)`  

-   `DISPLAY-STYLE-SYMBOL`  
    -   'popup  
        It is the initial value.  
        It displays an error using the popup.el included in the auto-complete package.
    -   'mini-buffer  
        It displays an error in the minibuffer.
    -   nil  
        msvc does not display an error.  
        The error display method depends on the individual flymake setting.

-   operation  
    -   Manual syntax check.  
        Key Bind    : F5  
        Explanation : Execute flymake manually.
    -   Jump to error line(prev-error)  
        Key Bind    : M-[  
        Explanation : Jump to the error line and pop up the error contents.
    -   Jump to error line(next-error)  
        Key Bind    : M-]  
        Explanation : Jump to the error line and pop up the error contents.


<a id="orgf79d7bd"></a>

## Jump and return to definition/declaration/include

Available places: msvc-mode on source code buffer.  

Jump by pointing the cursor to the word you want to jump in the source code buffer.  
The jump history is stacked and it is possible to return to the first jump source after jumping multiple times.  
The jump function by msvc-mode is implemented by ac-clang,  
and the buffer which this function is effective is limited to C/C++ and the file in Visual Studio project management.  
So, if you are using GTAGS+CTAGS as well as other scripting languages such as lua, it is a good idea to use them together.  

-   operation  
    -   Jump to definition/declaration/include  
        Key Bind    : M-.  
        Explanation : If you point the cursor to the word you want to jump and perform key operation,  
                      open the source file which is defined/declared and point the cursor to the definition/declaration place of the corresponding buffer.
    
    -   Return to jump source  
        Key Bind    : M-,  
        Explanation : Return to the previous jump source.  
                      By repeating this operation you can go back to the jump history.


<a id="org6824c17"></a>

## Jump and return for include files

Available places: msvc-mode on source code buffer  

It is a jump using the function of semantic.  
Unless there is a particular reason, we recommend using the `Jump and return to definition/declaration/include` function in the previous section.  

"M-i" on the include line will jump to the target file.  
The jump history is stacked and it is possible to return to the first jump source after jumping multiple times.  
Restriction: If you jump to an include file not managed by the project, you can not return with "M-I".  
Please return it manually. For example standard libraries stdio.h, vectors and other SDK include.  

`semantic-decoration-include-visit: Point is not on an include tag`  
If the above message is displayed, "C-c , ,", let semantic reparse the corresponding buffer.  

-   operation  
    -   Jump to include file  
        Key Bind    : M-i  
        Explanation : If you point the cursor to the include file you want to jump and perform key operation, it opens the include file and jumps to the corresponding buffer.
    -   Return to jump source buffer  
        Key Bind    : M-I  
        Explanation : Return to the previous jump source.  
                      By repeating this operation you can go back to the jump history.


<a id="orgbec1141"></a>

## Build project or solution

Available places: active project buffer or msvc-mode on source code buffer.  

The build of the project or solution starts with C-f5.  
Call from the command  
`(msvc-mode-feature-build-solution)`  
`(msvc-mode-feature-build-project)`  

Functions provided only with commands  
`(msvc-mode-feature-rebuild-solution)`  
`(msvc-mode-feature-rebuild-project)`  
`(msvc-mode-feature-clean-solution)`  
`(msvc-mode-feature-clean-project)`  

-   operation  
    -   Build project or solution  
        Key Bind    : C-f5  
        Explanation : The build of the project or solution starts.

It is good to describe the following setting in `.msvc`.  

Configure build log report frame  
`(setq msvc-solution-build-report-display-target TARGET-SYMBOL)`  

-   `TARGET-SYMBOL`  
    Specify the window to display the build log buffer.  
    -   'other-frame  
        Generate and display a separate frame from the own frame.
    -   nil  
        Display on own frame

Can set build log report style.  
It can be changed by setting symbols in the following variables.  
`(setq msvc-solution-build-report-display-timing DISPLAY-TIMING-SYMBOL)`  

-   `DISPLAY-TIMING-SYMBOL`  
    Specify the window display timing of the build log buffer.  
    -   'before  
        When the build is started, windows are divided and displayed.
    -   'after  
        The window is split and displayed when the build is completed.
    -   nil  
        It creates a log buffer but does not make it foreground after completion of build.

Specify how to display in the build log buffer.  
`(setq msvc-solution-build-report-realtime-display-p BOOLEAN)`  

-   `BOOLEAN`  
    -   t  
        Real time display of build log.
    -   nil  
        Batch display after completion of build.


<a id="org37e9564"></a>

## Jump to the error file from the build log of the project or solution

Available places: Log buffer of build for project or solution.  

-   operation  
    -   Jump to previous error line  
        Key Bind    : [  
        Explanation : Jump to the previous error line in the build log.
    -   Jump to the next error line  
        Key Bind    : ]  
        Explanation : Jump to the first error line in the build log.
    -   Jump to the previous error line & display file line of the corresponding error to other window  
        Key Bind    : M-[  
        Explanation : Jump to the previous error line in the build log and display the error line of the corresponding file to another window.
    -   Jump to the next error line & display file line of the corresponding error to other window  
        Key Bind    : M-[  
        Explanation : It jumps to the error line one by one in the build log and displays the error line of the corresponding file to another window.
    -   Display error file line  
        Key Bind    : C-z  
        Explanation : Open other window and display error file line.
    -   Jump to error file  
        Key Bind    : RET or Mouse Click  
        Explanation : Open other window and jump to error file line


<a id="orgee9b6e7"></a>

## Re-parsing project or solution

Available Places: Anywhere  

Used when the project file is updated by editing the project file in Visual Studio or updating by version control while the msvc project is active.  
Re-parse and reactivate all projects currently active on Emacs.  
Re-parsing is done only for new projects with dates later than the previous analysis.  
`(msvc-reparse-active-projects)`  


<a id="org297c9c2"></a>

## Launching Visual Studio

Available places: active project buffer or msvc-mode on source code buffer.  

Launch the project solution file to which the corresponding buffer belongs in Visual Studio.  
Since we are only using the file association function of Windows, if more than one Visual Studio is installed follow the association setting.  

-   `(msvc-mode-feature-launch-msvs)`  
    Launch with solution if solution is associated with buffer.  
    If it is only project, it is started by project.
-   `(msvc-mode-feature-launch-msvs-by-project)`  
    Launched in the project.
-   `(msvc-mode-feature-launch-msvs-by-solution)`  
    Launched in the solution.


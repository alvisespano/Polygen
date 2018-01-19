
Polygen — a first effort towards satyre in computer science



-----

**Table of Contents**

<!-- MarkdownTOC autolink="true" bracket="round" autoanchor="false" lowercase="true" lowercase_only_ascii="true" uri_encoding="true" depth="3" -->

- [1.0 Prelude](#10-prelude)
- [1.1 Package](#11-package)
    - [1.1.1 Requirements](#111-requirements)
    - [1.1.2 Supported platforms](#112-supported-platforms)
    - [1.1.3 Installation](#113-installation)
        - [1.1.3.1 Windows Precompiled Binaries](#1131-windows-precompiled-binaries)
- [1.2 Usage](#12-usage)
    - [1.2.1 Defining grammars](#121-defining-grammars)
    - [1.2.2 Suggestions](#122-suggestions)

<!-- /MarkdownTOC -->

-----


# 1.0 Prelude

Polygen is a command line program for generating random sentences according to a grammar definition, that is following custom syntactical and lexical rules. It takes a text file as source program defining a grammar by means of BNF-like rules and executes it, eventually showing the result.

Here a source program is a grammar definition, the execution consists in the exploration of such grammar by selecting a random path and the result is the sentence built on the way.

![PolyGUIScreenshot][PolyGUI screenshot]

# 1.1 Package

## 1.1.1 Requirements

Polygen is fairly slim and does not need that powerful computer in order to work.

From a theoretical point of view, there could be unlucky cases making the program loop for a sensible amount of time over a certain recursive production; in the real world, though, it will never happen.

Everything you need is a shell or command line interpreter (such as Bash or Csh under UNIX/Linux or the DOS Command Prompt under Windows) and, only in case you wish to write/view a grammar source file, a plain text editor/viewer.


## 1.1.2 Supported platforms

Polygen packages come in a variety of flavours, some providing different executable files in a platform-dependent way. Be sure the one you downloaded suits your machine and operating system.

A package containing the full source code exists as well and is intended for either people who just wish to know how Polygen works or users whose platform is not directly supported by a package containing an executable file for their machine. The latter will be able to compile the program by themselves following the instructions in the README file located in the source directory.


## 1.1.3 Installation

Polygen needs no installation: it just consists of an executable file and a bunch of grammar sources.

Leave it as is in an own directory and '`cd`' there when you want to use it.

Refer to platform-dependant `README` file for additional hints.

### 1.1.3.1 Windows Precompiled Binaries

In the root of this project you'll find a Zip archive containing the precompiled binaries of Polygen and PolyGUI, along with the correct version of `cygwin1.dll` required for running Polygen under Windows 10:

- [`Polygen1.6.0-PolyGUI_Win10.zip`][Polygen Win Zip] (direct download link)

Just unpack its contents and you're ready to use it.

# 1.2 Usage

The executable file must be launched from a shell or command line interpreter, as stated in section 1.1.2. It takes a set of arguments and prints to standard output, which is the shell terminal itself by default. For the formal synopsis run the executable with no arguments.


## 1.2.1 Defining grammars

Refer to the tutorial hypertext file for a detailed guide to the grammar definition language interpreted by Polygen.


## 1.2.2 Suggestions

- Remember to provide an "`I`" non-terminal symbol in your own grammar files for the `-info` option.
- Try to avoid as many warnings as possible when developing your own grammar sources: your definitions will be more robust and won't lead to unexpected outputs.

[Polygen Win Zip]: https://github.com/alvisespano/Polygen/raw/master/Polygen1.6.0-PolyGUI_Win10.zip "Download Polygen for Windows and PolyGUI precompiled binaries"

[PolyGUI screenshot]: https://raw.githubusercontent.com/wiki/tajmone/Polygen/screenshot_PolyGUI.png "Screenshot of PolyGUI tool for creating and testing Polygen grammars"
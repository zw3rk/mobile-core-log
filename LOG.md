# A Christmas Story

10:46 AM So recap of my new knowledge gained over the weekend:
- iOS fairly trivial. Just build for macOS, and patch up the objects’ load commands from macOS -> iOS, to get around the linker being overly zealous.
- Android. Much more of a quagmire.
  - Multiple architectures. For now I’ll focus on aarch64 only
  - Can’t use staically compiled musl binaries, they don’t resolve and crash.
  - Can’t use glib copiled binaries, they have even more missing symbols
  - Can’t run bionic (android’s libc) compiled binaries, because they are by default dynamic, and linker64 isn’t readily available.
  - Building `linker64` from googles source is a pain. They currently use some soong build tool, that replaced their make files, and will be replaced by bazel.
  - Building `soong` from source is also near impossible; and barely documented. Yay!
  - Building `bionic` libc from source again requires their odd build tool. But maybe one can use the blueprint file to construct a Makefile (by hand).
  - Building static `libc`, `libm`, ... from bionic /should/ allow us to build android executables that just run on linux, as long as we don’t use any android APIs.
  - Building `linker64` should allow us to run android executables on linux as long as they don’t use any android apis.
  - Maybe we could use cross compiled rts, ... libraries, from an android cross compiler and swap them in for the musl/glibc libraries. Kinda frankensteining the thing together. This would allow us to have a native stage2 compiler (no TH or any other restriction really), and just fix up the libraries during linking on android.

10:46 To be fair, the most “interesting” options are the `linker64` and `bionic` libc builds.
10:47 The frankensteining thing might work if the cross compiler is technically a stage3 compiler so the ghc abis are stable. But it feels just “wrong”.

8:00 PM For clarification, we want to build fully native applications for iOS devices and Android devices. We’ll focus on aarch64, for now. That’s pretty much exclusively what Apples mobile hardware runs at this point. For Android we’d have to consider aarch64, aarch32, x86_64, mips, risc-v, and potentially a few more other platforms. We’ll restrict ourselves to aarch64 (and maybe x86_64) for now. A stretch goal is to maybe also produce some javascript web application from the same source code.

8:00 We’ll restrict ourselves to ghc 8.10.7 for now as well, as that’s what we basically have properly integrated.  

8:09 PM The general idea for the architecture is the following
```
mobile-core (haskell) - contains the business logic
|- mobile-ios (swift)
|- mobile-android (kotlin)
'- browser (maybe haskell (reflex?) / JavaScript)
```
The UI will be written using the native UI toolkit, and link the haskell library into the application. Building the UI in haskell by bridging it somehow is out of scope. We could probably also build a cli (brick?) app based on the core component, or a native application using the same approach.

8:14 I don’t have a specific application in mind so far, I’m mostly interested in the technical challenges along the way. Thus my current plan for milestones is as follows:
```
1. Build and link a simple haskell library that returns "Hello from Haskell" as a string, and display that string in the UI.
2. Extend this with some interactive functionality (e.g. let's compute fib, or something based on an input field and button)
3. Let's use sqlite-direct, and maybe beam, to lookup some values from a pre-populated database, and display them.
4. See if we can make the `network` package work. 
```

8:18 For technology, we’ll be using nix and hydra to build the mobile-core, and have it produce packages that contain static libraries of the core component. For iOS we’ll use Xcode, and for Android we’ll use Android Studio. We’ll not build iOS or Android in CI (for now at least).
Why use nix and hydra? Simply because it will allow us to have native access to aarch64-darwin, x86_64-darwin, aarch64-linux and x86_64-linux machines without having to set this up locally.  Nix also comes with farily useable cross compilation capabilities, so it will make dealing with SDKs much easier, and lets us focus on the technical challenges that haskell poses, instead of dealing with toolchain and other issues that would just take a lot of time, and are easy to get wrong.

8:19 This also allows us to leverage IOGs haskell.nix infrastructure, which has fairly good cross compilation support so far, but none really for mobile yet.

9:26 PM If you are wondering how does this relate to [Christmas of code haskell for mobile a 3000 grant](https://www.reddit.com/r/haskell/comments/r82ji7/christmas_of_code_haskell_for_mobile_a_3000_grant/), it doesn’t really. SimpleX did approach me a few month ago about helping them out; but I had to decline as I simply don’t have the time for this. I still don’t and the scope they want is beyond what I’m proposing here. But maybe the end result of what we do here, can help the person being awarded the work, and let them focus on the trickier high level bits, of writing the documentation and whatnot around it.
I **am** though helping the person who got selected in any Q/A capacity I can.

9:27 I don’t want to be the singular point of knowledge about all of this either. I did write down quite a bit about all this a few years back at [log.zw3rk.com](http://log.zw3rk.com/). Things have changed a little since then.

9:29 If you are reading this, you might be wondering why can’t we just build a haskell cross compiler, against the given platform SDK, and build a library? Actually we can. At least for Android. For iOS it’s a bit tricker, we can only do this on a mac due to licensing restrictions (same reason we can’t really run macOS builders on linux, or cross compile from Linux to macOS).

9:30 So yes, we can build x86_64-darwin -> aarch64-ios (also aarch64-darwin) ghc cross compiler that can produce aarch64-darwin libraries. Similarly we can do the same for x86_64-linux (or x86_64-darwin) to aarch64-android.

9:31 This is how most cross compilers work. You have a bunch of input files, and just compile them for the target architecture, let the toolchain take care of packaging (e.g. static library or dynamic library, static executable or dynamic executable), and deploy.
9:31 GHC has two rather annoying complications here:
- Template Haskell
- Plugins

9:34 Template Haskell is annoying because running a splice `$(fun arg1 arg2 ...)` tells the compiler to run the function `fun` with `arg1` and `arg2`, and replace `$(...)` with whatever that call returned. So how does GHC run `fun`? Simple, we just lookup the compiled machine code for fun (or in some instances we can lookup the bytecode object for `fun` (but that often isn’t bytecode all the way down into the libaries), and run it.

9:34 But wait, if we are looking up the machine code, that is for a different architecture! :scream:

9:35 This is the crux why cross compiling modern Haskell is so darn hard. TH has infested pretty much every production application, unless the team is rigorous in banning TH from the codebase outright. And this includes TH use in all dependencies.

9:35 Such a codebase can be cross compiled fairly easily.

9:38 Now Plugins, are another topic. There is no semantics around where plugins should run, on the compiler host or target? But for plugins the answer more naturally falls into compiler host. After all it’s a plugin you load into the compiler.  This today does not work.  We have a hack in GHCJS to make this work, and Sylvain is working diligently in trying to bring a generic proper solution for this into GHC. However this requires GHC to become multi-target aware, as it needs to be able to produce code for itself the compiler host, not just for the target. And it also needs to learn that the plugin isn’t just part of the package database we are compiling against, but should live in its own package database, with its dependencies.

9:40 So TH can be made to work? Yes. Luite pioneered this in GHCJS a long time ago, GHCJS is technically also a cross compiler as the compiler host is not the same as the compiler target. If we are compiling TH functions to JavaScript then, how can we actually execute them? The key to this is a javascript runtime, and `node` is pretty ubiquitous. So we can start a process in `node`, and evaluate the function in `node`, reading the result back and splicing it into what ever `$(...)` we had.

9:41 The same idea was later (slightly modified) ported to GHC in the form of an application called `iserv`. If you start `ghc` with `-fexternal-interpreter`, ghc will spawn an `iserv` process, and communicate with `iserv` for all interactive needs, this includes evaluating TH.

9:42 Thus, if we have an evaluation context where we can run the target code in, we can make TH work by spinning `iserv` up in that target context.

9:43 This is how we do windows cross compilation (we launch `iserv` in WINE), and compile all TH splices through the process via WINE.

9:44 Similarly we can cross compile x86_64-linux -> aarch64-linux this way, we use qemu in user-mode (it basically just translates one instruction set into another; rosetta2 on aarch64 mac’s works similarly to execute x86_64), and run iserv there. The performance penalty for emulating a different architecture is negligible as we usually do not spend the majority of time evaluating TH splices.

9:46 If we build static binaries for `x86_64`, we can build a full (cross compiled compiler), because a `musl` based GHC on x86_64-linux can just trivially execute on any x86_64-linux, as long as the kernel is sufficiently new enough to support the musl syscalls we use. So this technically isn’t cross compilation cross compilation.

9:49 This brings us to the topic of c standard libraries. As the above (static via musl) kind touched on that. I’ll go a bit more into details on that tomorrow to lay the foundation for some of what we are going to attempt.

11:23 PM I'm trying to explain from the basics as much as possible. But if something is unclear or makes no sense, do ask questions! (Just don't expect instant answers :sweat_smile:)

9:13 AM Alright, let’s continue with some more foundations, to build onto.  I’d like to go over what an executable actually is, machine code, container formats, a bit of linking terminology and some thoughts on the standard c library.  My hope is that this will provide us with the necessary basics to make the rest of what we are going to experiment with a bit easier to understand.
9:15 If we think about a simple program like say the following haskell example:
```haskell
main :: IO ()
main = putStrLn "Hello World"
```
or a somewhat equivalent C program
```c
#include <stdio.h>
int main(int argc, char ** argv) {
  printf("Hello World\n");
}
```
and compile this, what actually happens?

9:21 AM The compiler will first parse the code, turn it into some abstract syntax tree, likely do some transformations on it, and turn it into a lower level representation (also often referred to as “lowering”) a few times until we end up with effectively an assembly printer (or in some cases a direct instruction printer). For assembly we can feed that into an assembler (the `as` program), to get a byte stream of instructions. These instructions are specific to the CPU we are targeting, so most likely x86_64 or aarch64. The computer will load those instruction in chunks into the cache, and then churn through the instructions.  Again the CPU internally will likely decode the bytes that represent the instructions into some internal representation, but this part we will never see as it’s CPU specific.
So turn Text into Instructions and encode them into sequences of Bytes which the computer loads into the cache and the CPU then runs.

But wait, the CPU is already running our host operating system (Windows, Linux, macOS, ...), how can we have it execute our instructions?

9:24 If we are writing our own bare metal software/operating system this isn’t much of a problem, there will be some predefined offsets where the computer expects to read its first instructions off of, so we’d just need to work with the bootloader to make sure it puts our instructions into the right place. We can just throw the raw instructions there, and call it a day. But for a program running on a host operating system, we need to work with that operating system.

9:26 Side note, there is the compiler explorer (https://godbolt.org/), which is really helpful if you want to explore how compilers turn applications into assembly.

9:27 You can also ask your compiler to give you the assembly usually with the `-S` flag.

9:29 So how do we launch an application on our host operating system? This is basically the job of the kernel. Read the application file, put it into memory, and hand over execution to our entry point; and then our program can interact with the computer.

9:37 AM We have three major container formats for applications. Portable Executable (PE) on Windows, ELF dominates linux, solaris, bsds, and Mach-O pretty much exclusively on Apple platforms. These file formats are what the kernel reads, and then decides where and what executable code to place where into memory. Why do we need those, can we just have raw machine code in a file and tell the kernel to execute it? In principle this could work, but it’s nice to have some metadata and allow us to have different sections that describe actual machine code (called text sections), and data sections (e.g. the “Hello World” string, we likely want to lay that out as just an ascii string in memory, and then be able to refer to it). For this to actually work the kernel will after loading the application into memory, fixup references in the instructions, so that pointers actually point to e.g. the “Hello World” string.  This process is called relocation, and if you have an ELF file you can query the relocations with e.g. readelf, for mach-o, there is otool, and for PE’s theres probably some tool as well. I must admit I’m not that good with windows details.

9:39 Once we have slightly more complex programs that do not consist of a single file, we can’t go straight from `main.c` to `main`. (And even that isn’t completely correct, which we’ll touch on later).

9:43 So we have say a library `lib.c` and in our `main.c` we want to use a function (or multiple) of those provided in the library.  So we turn `lib.c` into `lib.o`, the object file. This is again in the container format for your operating system, thus this won’t be identical across different operating systems. We also turn `main.c` into `main.o`.  The `main.o` file will now have symbolic references (symbols) to something that’s not contained in `main.o`. Thus we can’t turn `main.o` straight into the `main` executable, as we’d run into a problem at runtime where we call the library function that can’t be found. The linker (e.g. ld) will complain about this. It can’t turn `main.o` on its own into an executable. (We can force it to ignore undefined symbols with -u and similar flags, but that’s mostly not advisable, unless you are certain you know what you are doing; e.g. you know that symbol will never be needed when actually running the program).

9:47 If we pass both `lib.o` and `main.o` to the linker, this will succeed as the linker is able to resolve symbols just fine. If you try this yourself, do note that most linkers are order sensitive. If you don’t provide the object file with the definition of the symbol prior to the object file that needs that symbol the linker will complain about a missing symbol. So how do you link mutually recursive objects? This again depends on the linker in question, the easiest is usually to just repeat the object files. `a.o b.o a.o b.o`.  Some linkers provide grouping functions as well in which they allow recursive linking.

9:48 Ok, so what we did now is we statically linked `lib.o` and `main.o` into `main`.  This is the most straight forward and least complex way of linking. It also allows the linker to potentially discard symbols that are not used from the final product.

9:50 But what, you may ask, if lots of people use `lib.o`, in their programs, we are linking that into each program, isn’t that wasteful? Shouldn’t we “share” that library across all executables and only have one instance of it? Well that is a good question and leads us to dynamic linking and so called Dynamic Shared Objects (DSOs).

9:54 These again come in flavours specific to each operating system. Windows calls them (Dynamic Link Library) `.dlls`, Linux (& friends) calls them (Shared Objects) `.so` , and maOS calls them (Dynamic Libraries) `.dylib`. There are some other details we’ll gloss over for now, but conceptually they follow the same idea.

9:56 Now we can turn our library lib.o into a shared lib.dll,lib.so, or lib.dylib. When we link our main executable now, we build a shared main executable, and tell the linker to link the shared library. So it won’t embed anything from the library in our main executable anymore, but rather reference the lib.dll, lib.so, or lib.dylib from the main executable instead.

9:58 Now we could teach the kernel to not only be aware of statically linked executables and how to run them but also how to load dynamic libraries. Whether or not the kernel you write for your own operating system does that is up to you, but what we usually end up with is that there is a loader program, that the kernel can delegate dynamic library resolution and linking to. Which program is to be used to do this can be encoded in the container (PE, ELF, Mach-O), and thus the kernel just needs to know which program to hand control over to for linking.

10:00 So, we’ve covered .o object files, containing machine code (instructions encoded as bytes), and some data, as well as some metadata packaged in a container format (PE, ELF, Mach-O), linked executables (again packaged into some container format), as well as dynamic libraries (also packaged in a container format).

10:01 What about static libraries, like those weird .a files? Ah! Very good (and important) question for what we are going to do.

10:05 As we saw earlier, we can compile lib.c into lib.o and then use that for linking, but in reality we’ll have lots of .c files we turn into .o files, and wouldn’t it be nice if we could just have an archive of all those .o files? Yes it would very much. That’s what .a files are, they are archives of .o files.  Not it would be too easy if we all agreed on one archive format, because, well, where is the fun in that? So we have GNU archives, and BSD archives. (This is where my windows knowledge leaves me and I have to look this up on a by-need basis.) macOS having a more BSD like userspace uses BSD archives as well. Luckily GNU and BSD archives don’t differ much, but enough to not be 1:1 compatible.

10:08 .a files have a a very simple format, the start with the following 8 bytes !<arch>\n, followed by archive entries for each file contained within the archive. Those entries contain an identifier (usually just the file name, e.g. lib.o), user id, group id, timestamp, size. So you can basically iterate over an archive, by checking for the magic 8 bytes at the beginning to see if it’s actually an archive, and then parsing the entry header (obtaining the metadata of the following payload), skip the payload read the next element, ...

10:10 On quirk is that archives can have multiple entries with the same name, so be careful if you think about extracting an archive. If there are multiple entries in the archive with the same identification (file name), it depends on your ar tool (and it’s version), what the behaviour will be, if you will get the first or last object only or what exactly happens :sigh:

10:11 Fundamentally you can encode any kind of data in archives. It’s not restricted to object files, it’s really just a list of files concatenated with some metadata and the magic 8 `!<arch>\n` bytes at the beginning.

10:13 So let’s say we have a bunch of library functions neatly grouped into string, memory, ... files and produce lib.a from string.o, memory.o, ... how how do we link our main executable with that? Rather simple. We just pass that lib.a to the linker, and it will mostly transparently deal with this for us.

10:17 So what are those -l and -L flags then? Let’s go back to my previous example of our library with string and memory and other functions. This seems like a rather useful library to have when working with c code. E.g. pritnf and malloc would be nice to have functions. What should we name our library? It’s really a fundamental c-library, and by convention we’ll call our static archive of object files libc.a, and the shared one libc.so, or libc.dylib.  (Though macOS calls their C library System, so we have libSystem.dylib). Similarly we might package mathematical functions into a library and usually call that libm.

10:20 If we provide our linker with `-l<name>` it will look for `lib<name>.<ext>` in all the path’s we passed via `-L/path/to/somewhere`.  If we ask the linker to produce a shared executable (via the -shared) flag, it will look for files with the <ext> being e.g. .so or .dylib first and fall back to .a if no dynamic lirbaries can be found. if we ask for -static, it will look for .a only, after all we told it that we don’t want a static executable.

10:24 You might be wondering, but hey, we didn’t provide -lc when we built our main.c into main, how can we use printf then, that doesn’t looke like a kernel function?  And you are absolutely right, it’s not a kernel function.  Your compiler will usually pass a few default libraries to the linker, unless you explicitly ask it not to do so via -nostdlibs.  The c compiler tries to be a bit smart here and provide you with some basic c library functionaly by default, so you’ll get libc and libm , and maybe libdl (for dynamic loading, so you could dlopen a shared library (e.g. a plugin)).

10:31 AM How does libc  then implement anything actually? How does printf or puts work? This is an abstraction layer to make writing c software easier.  And underneath libc will call so called system calls, that is it will tell the kernel to invoke a specific kernel function. System calls are neither necessarily stable across kernel versions, nor the same across operating system. Thus every operating system usually comes with its own libc. Can we have multiple libcs? Yes! Yes, we can! Now this is not very common on windows or macOS, but there are quite a few libc to choose from on linux. We have glibc The GNU libc, musl, diet, and a few others, including bionic, which is the libc for android. Android at its core is just a linux kernel with a custom libc, namely bionic. It’s called bionic because it’s basically a mash of various bsd libc’s some home grown google libc, and other features.

10:33 So how can we write software that works across multiple systems? We can recompile the above main.c on various platforms and have it work, because they all provide libcs that provide printf, with the same semantics. And this is where standards for those libraries arise. E.g. POSIX, if your libc is POSIX standard compliant, software written against POSIX functions should work on any operating system that comes with a kernel and a libc that implements those functions.

10:37 If your programming language doesn’t want to rely on any of this libc logic, it can provide its own low level system implementation, e.g. instead of calling printf and other functionality provided by the libc, it can just call into the kernel directly with system calls. This is what GO did (I’m not sure they still do this). This however requires that you can rely on the stability of syscalls. Linux has a fairly stable syscall api, (syscalls don’t really change much, you’ll get new ones, but old ones are kept around). This is very different on macOS. While you may get lucky and the syscalls are stable, there is no guarantee and the vendor only guarantees that the system library’s api is stable. E.g. don’t call syscalls, only call library calls from libc (libSystem on macOS).

10:38 Small recap:
if we build a dynamic executable, we (most likely) need a loader program to execute our executable as the kernel doesn’t provide the loader for DSOs.
if we build a fully static executable, we only need a kernel that supports the syscalls the libc (or our homegrown solution) we linked into our executable calls.

10:42 This has been quite a lot of text, so I’ll give this some time to digest. Can you come up with some ideas on how to solve the TH issue? We need to execute code for a (potentially different architecture and/or operating system) target.

12:47 PM One more note on system triples, as I might just end up referring to it without introducing them. To differentiate which system we are talking about it has become customary to use so called system triples. These have the following structure:
```<architecture>-<vendor>-<operating system>```
architecture is something like aarch64 (and it's alias arm64), x86_64 (and it's alias amd64), i386, ...
vendor can be pc, none, unknown, apple, ... can, and often is, omitted.
operatingsystem is effectively the kernel and what describes the operating system sufficiently enough. These can be macos, ios, android, windows, darwin, freebsd, linux, ... darwin is the umbrella across macos, ios, tvos, ... basically anything that's supposed to work on apples kernel with libSystem. It might be darwin12, ... to specify a specific operating system version.

12:49 Similarly freebsd12,... for linux we have linux-gnu, linux-musl, ... to indicate the libc.

12:52 If we write aarch64-darwin we thusly mean an apple operating system on a 64bit arm cpu.
aarch64-android and aarch64-linux-bionic would be synonymous.

12:53 A lot of effort goes into autotools configure to parse these triples correctly. For a human it's fairly obvious what's meant, but having vendor be optional makes parsing it a bit tricky.

7:59 PM Alright, enough theory and background, let’s try to get something working.

8:01 So you might be wondering, but wait, we said aarch64-ios and aarch64-macos, are both aarch64-darwin, right? So if we build a library on macOS on an aarch64 machine with a native stage2 (as opposed to a cross compiler), we won’t have TH restrictions, and it’s the same right?

8:03 Short detour, GHC stages. When we build GHC we start with a boostrap compiler, this is the compiler that we build GHC with. Sometimes also referred to as stage0 compiler. We use that compiler to build a GHC compiler from the sources. That resulting compiler is then our stage1 compiler. Depending on how large the difference between the boostrap (stage0) and first compiler (stage1) is, we might run int subtle bugs as the stage1 compiler does not necessarily follow the same codegen or calling conventions as the stage0 compiler we used to build it. To overcome this, we will build ghc from souce again, but this time use the stage1 compiler. This then yields our full stage2 compiler, which we usually package and ship as the binary distributions.

8:04 This poses a dilemma for cross compilers. If a cross compiler would compile itself, it would produce a compiler for the target, but not for the host where we want to run it on, thus cross compilers are limited to stage1 compilers. Ideally we therefore build cross compilers as virtual stage3 compierls. We build the cross compiler with a bootstrap compiler built from the identical sources.

8:05 Alright, so, let’s build a simple haskell library. And I’ll just ignore TH for now, we can focus on that later.

8:22 PM We are going to use the following fairly trivial Lib.hs, which exports a function chello  as hello.
```haskell
module Lib where

import Foreign.C (CString, newCString)

-- | export haskell function @chello@ as @hello@.
foreign export ccall "hello" chello :: IO CString

-- | Tiny wrapper to return a CString
chello = newCString hello

-- | Pristine haskell function.
hello = "Hello from Haskell"
```

This shouldn’t be too spectacular, but at least give us some fairly basic starting point.

8:23 We’ll also add a haskell executable to demonstrate how we’d use this from regular haskell. Remember our business logic is in hello, and we won’t need a c abi interface to our function, so we can just use hello.

```haskell
module Main where

import Lib

main :: IO ()
main = putStrLn hello
```

8:24 The more interesting thing is to call it from c. So we also add a main.c to illustrate basic usage:
```c
#include <stddef.h>
#include <stdio.h>

// #include "stubs/Lib_stub.h" // would contain the following:
extern void hs_init(int * argc, char ** argv[]);
extern char * hello();

int main(int argc, char ** argv) {
    // init GHCs runtime;
    hs_init(argc, argv); // or hs_init(NULL, NULL);
    // let's call our exported function.
    printf("%s\n", hello());
    return 0;
}
```

8:25 If you want to follow along the code is in github.com:zw3rk/mobile-core, and there is a cabal file with targets for the library, the haskell and the c main executables.

8:28 The idea is now to compile the library natively on macOS. We need a slightly modified compiler, that has --disable-large-address-space set during configure. This will prevent the runtime from trying to grab 1T of address space during initialisation. We know this will horribly fail on iOS, as iOS is not as permissible as macOS.

8:32 The easiest way (for me) to build this, is to simply throw it into nix, and let it build on my CI. For this I did add a flake.nix to the repository, and used haskell.nix to turn the cabal project into something nix can understand. I’ve also used the flake-utils to multiplex this across various targets, and added a bit of extra glue to have cross targets in there as well. On top of that I added some postInstall packaging hooks, to have hydra provide downloadable .zip files containing the library.

8:33 We can therefore trivially grab the lib:mobile-core:smallAddressSpace:static.aarch64-darwin from https://ci.zw3rk.com/eval/374#tabs-still-succeed.

8:34 After extracting the pkg.zip we’ll see:
```
├── include
│   └── stubs
│       └── Lib_stub.h
├── libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a
├── libffi.a
├── libgmp.a
└── libgmpxx.a
```

8:35 We can ignore the Lib_stub.h for now, while it would be nice to use that for #include, it depends on HsFFI.h, which we don’t have handy right now.

8:36 Further inspection of the archives yields rather little info:
```
libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a: current ar archive random library
libffi.a:                                                    current ar archive random library
libgmp.a:                                                    current ar archive random library
libgmpxx.a:                                                  current ar archive
```

8:38 Let’s create a temorary directory, and inspect the libHSmobile-core...a,
```
$ ar x ../libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a
$ ls
Adjustor.o		Desugar.o		HeapStackCheck.o	NonEmpty.o		RtsMain.o		TopHandler.o
...
.. Lib.o ..
...
Debug.o			Heap.o			NoOp.o			RtsFlags.o		Times.o			wrappers.o
```

whether or not that Lib.o in there is ours is hard to tell, as there could be multiple Lib.o in the archive, and we wouldn’t know which one we got.

8:40 If we use file on the object files we get the following:
```
$ file *.o
...
Latin1.o:             Mach-O 64-bit object arm64
Lazy.o:               Mach-O 64-bit object arm64
LdvProfile.o:         Mach-O 64-bit object arm64
Lex.o:                Mach-O 64-bit object arm64
Lib.o:                Mach-O 64-bit object arm64
Libdw.o:              Mach-O 64-bit object arm64
LibdwPool.o:          Mach-O 64-bit object arm64
...
```
ok. So that looks rather promising. We got aarch64 mach-o files. We should be able to link them into an iOS application. Let’s try this next.

8:46 PM We’ll start Xcode on a mac, and select “Create a new Xcode project”...
![Screenshot 2021-12-23 at 8.42.09 PM](assets/Screenshot%202021-12-23%20at%208.42.09%20PM.png) 


8:47 Next we’ll chose iOS > App as the template.
![Screenshot 2021-12-23 at 8.42.24 PM](assets/Screenshot%202021-12-23%20at%208.42.24%20PM.png) 


8:48 And then fill out the metadata. I’ve restricted it to the bare minimum, that includes leaving out tests. But we are going to use SwiftUI and Swift, because this is how modern apps are supposed to be developed for iOS (according to Apple).
![Screenshot 2021-12-23 at 8.42.56 PM](assets/Screenshot%202021-12-23%20at%208.42.56%20PM.png) 


8:48 And we’ve got ourself a scaffolded iOS application.
![Screenshot 2021-12-23 at 8.43.47 PM](assets/Screenshot%202021-12-23%20at%208.43.47%20PM.png) 


8:49 If you want to follow along, the code is at github.com:zw3rk/mobile-core-ios

8:52 Because we want to link against some C sources, we need some bridging header to get our C functions available in swift. The easiest way I’ve found to get this right is to go via Menubar > File > New > File...
And chose some Objective C file, call it Dummy (or anything really), and then delete it.
3 files 
![Screenshot 2021-12-23 at 8.50.46 PM](assets/Screenshot%202021-12-23%20at%208.50.46%20PM.png)
![Screenshot 2021-12-23 at 8.51.18 PM](assets/Screenshot%202021-12-23%20at%208.51.18%20PM.png)
![Screenshot 2021-12-23 at 8.51.27 PM](assets/Screenshot%202021-12-23%20at%208.51.27%20PM.png)

8:54 We now have a new file in the project called mobile-core-ios-Bridging-Header.h, and we can add our extern c functions to it:
```c
extern void hs_init(int argc, char ** argv[]);
extern char * hello();
```

8:58 Next, we’ll modify our mobile_core_iosApp swift file to include the hs_init call to initialize the haskell runtime.
from
```swift
@main
struct mobile_core_iosApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}
```
to
```swift
@main
struct mobile_core_iosApp: App {
    init() {
        hs_init(0, nil)
    }
    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}
```
8:59 in our ContentView, we change the ContentView struct from
```swift
struct ContentView: View {
    var body: some View {
        Text("Hello, world!")
            .padding()
    }
}
```
to
```swift
struct ContentView: View {
    var body: some View {
        Text(String.init(cString: hello()))
            .padding()
    }
}
```
9:01 At this point we now have an application that wants to call out to two haskell functions, but, doesn’t know about them (yet).

9:03 We’ll create a new Group Libraries and add the libgmp.a, libffi.a and libHSmobile-core...a in there.

9:05 Dropping libraries into xcode, makes it usually decide to automatically link those libraries against the target.
![Screenshot 2021-12-23 at 9.04.50 PM](assets/Screenshot%202021-12-23%20at%209.04.50%20PM.png) 

9:06 but, those libraries are built for aarch64-darwin, and I’m on x86_64-darwin, so there is no way for me to actually test this in the simulator. We could build fat libraries that contain multiple slices for aarch64 and x86_64, but for now, we’ll just restrict ourselves to actually testing this on a real device.

9:10 Thanks to my test device not having been update in a while, I’m greeted with the following information. My device’s iOS is too old. I’ll turn down the deployment target to iOS 14+.
![Screenshot 2021-12-23 at 9.07.24 PM](assets/Screenshot%202021-12-23%20at%209.07.24%20PM.png) 


9:10 Let’s try again...

9:10 Wonderful, we are greeted with the following errors, that didn’t go as planned:
```
Signing for "mobile-core-ios" requires a development team. Select a development team in the Signing & Capabilities editor.
/zw3rk/mobile-core-ios/mobile-core-ios.xcodeproj Building for iOS, but the linked library 'libffi.a' was built for macOS.
/zw3rk/mobile-core-ios/mobile-core-ios.xcodeproj Building for iOS, but the linked library 'libgmp.a' was built for macOS.
/zw3rk/mobile-core-ios/mobile-core-ios.xcodeproj Building for iOS, but the linked library 'libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a' was built for macOS.
```

9:11 Fixing the first one is fairly trivial, we can just use our own team (you’ll have to register your apple id with apple for this, but iirc it’s at no cost).

9:11 The other ones seem a bit more annoying, so let’s see what’s going on here?

9:20 PM alright, let’s create a trivial test.c file (int square(int n) { return n*n; }).

9:22 And compile it for ios and macos:
```shell
xcrun --sdk iphoneos clang -arch arm64 -c test.c -o test-ios.o
xcrun --sdk macosx clang -arch arm64 -c test.c -o test-mac.o
```

9:25
```shell
$ ls -la *.o
-rw-r--r--  1 angerman  staff  536 Dec 23 21:21 test-ios.o
-rw-r--r--  1 angerman  staff  536 Dec 23 21:21 test-mac.o
$ file test*.o
test-ios.o: Mach-O 64-bit object arm64
test-mac.o: Mach-O 64-bit object arm64
$ shasum *.o
b77f13779288fb49bc4c09b3b68e2ad287bf6f66  test-ios.o
0f62971f7eef9e39e1da9483e11ade98915340f5  test-mac.o
```
Huh, ok so both files are identical in length, and target the same architecture but are not the same.  Ok, time for some quick binary diffing, using our trusty hextump and diff tools.
```shell
$ hexdump -C test-ios.o > test-ios.o.hd
$ hexdump -C test-mac.o > test-mac.o.hd
$ diff -u test-ios.o.hd test-mac.o.hd
--- test-ios.o.hd	2021-12-23 21:24:14.000000000 +0800
+++ test-mac.o.hd	2021-12-23 21:24:20.000000000 +0800
@@ -15,7 +15,7 @@
 000000e0  20 00 00 00 00 00 00 00  a8 01 00 00 03 00 00 00  | ...............|
 000000f0  c8 01 00 00 01 00 00 00  00 00 00 02 00 00 00 00  |................|
 00000100  00 00 00 00 00 00 00 00  32 00 00 00 18 00 00 00  |........2.......|
-00000110  02 00 00 00 00 02 0f 00  00 02 0f 00 00 00 00 00  |................|
+00000110  01 00 00 00 00 00 0b 00  00 01 0c 00 00 00 00 00  |................|
 00000120  02 00 00 00 18 00 00 00  d0 01 00 00 03 00 00 00  |................|
 00000130  00 02 00 00 18 00 00 00  0b 00 00 00 50 00 00 00  |............P...|
 00000140  00 00 00 00 02 00 00 00  02 00 00 00 01 00 00 00  |................|
```

9:26 So fairly early in the file, there are some bytes with minimal differences. Ok, so it’s probably in the mach-o header, let’s find out with otool.

9:28
```shell
$ otool -l test-ios.o > test-ios.o.otool
$ otool -l test-mac.o > test-mac.o.otool
$ diff -u test-ios.o.otool test-mac.o.otool
--- test-ios.o.otool	2021-12-23 21:27:09.000000000 +0800
+++ test-mac.o.otool	2021-12-23 21:27:15.000000000 +0800
@@ -1,4 +1,4 @@
-test-ios.o:
+test-mac.o:
 Load command 0
       cmd LC_SEGMENT_64
   cmdsize 232
@@ -38,9 +38,9 @@
 Load command 1
       cmd LC_BUILD_VERSION
   cmdsize 24
- platform 2
-    minos 15.2
-      sdk 15.2
+ platform 1
+    minos 11.0
+      sdk 12.1
    ntools 0
 Load command 2
      cmd LC_SYMTAB
```
:facepalm:

9:35 PM We encode the platform in the mach-o headers. I guess this is a good safeguard, to prevent us from accidentally linking stuff. And this could be a problem if we linked against something that use features available only one one platform but not the other. I’m fairly hopeful that stock GHC will stick functions that are available across both platforms.

9:38 Well, nothing we can’t fix with a quick hack, let’s write a quick C program that patches up the mach-o load command that contains the BUILD_VERSION.
```c
int main(int argc, char ** argv) {

    FILE *ptr;

    ptr = fopen(argv[1],"rb+");
    fread(buffer,sizeof(buffer),1,ptr);

    mach_header_64 *hdr = (mach_header_64*)buffer;

    size_t offset = sizeof(mach_header_64);
    load_command *cmd;
    while(offset < sizeof(mach_header_64) + hdr->sizeofcmds) {
        cmd = (load_command*)(buffer+offset);
        switch(cmd->cmd) {
            case LC_BUILD_VERSION: {
                struct build_version_command* bvc = (struct build_version_command*)(buffer+offset);
                bvc->platform = PLATFORM_IOS;
                fseek(ptr, offset, SEEK_SET);
                fwrite(buffer+offset, sizeof(struct build_version_command), 1, ptr);
            }
            default: break;
        }
        offset += cmd->cmdsize;
    }
    fclose(ptr);
    return 0;
}
```
Definitions for the macho_header and the defines can be found in apples public `xnu` (that’s what apples calls their kernel) sources.

9:41 I’ve extended the tool a bit here: https://github.com/zw3rk/mobile-core-tools/blob/master/mac2ios.c, to iterate over archives (remember the discussion from before, that it’s basically just concatenated object files with a tine bit of metadata header.

9:45 After running
```shell
$ mac2ios Libraries/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a
$ mac2ios Libraries/libffi.a
$ mac2ios Libraries/libgmp.a
```
we are now left with only the following error when trying to build
```
Signing for "mobile-core-ios" requires a development team. Select a development team in the Signing & Capabilities editor.
```
Success? Let’s see.

9:48 After setting the Signing Team, it... builds?
![Screenshot 2021-12-23 at 9.46.18 PM](assets/Screenshot%202021-12-23%20at%209.46.18%20PM.png) 

9:49 Sadly not yet, it fails with
```
ld: '/zw3rk/mobile-core-ios/Libraries/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(Lib.o)' does not contain bitcode. You must rebuild it with bitcode enabled (Xcode setting ENABLE_BITCODE), obtain an updated library from the vendor, or disable bitcode for this target. for architecture arm64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
```
yea, I guess that’s correct, we can’t emit bitcode, or can we? Maybe we could force llvm to emit some for us? But with ghc’s NCG eventually this will become impossible. So let’s just disable this. I can go into details what bitcode is if anyone is interested. Conceptually it’s just LLVM’s intermediate representation in binary form.

9:52 Ohh no, yet another failure
```
Undefined symbols for architecture arm64:
  "_iconv", referenced from:
      _hs_iconv in libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(iconv.o)
     (maybe you meant: _base_GHCziIOziEncodingziIconv_iconvEncoding8_closure, _base_GHCziIOziEncodingziIconv_iconvEncoding11_info , _base_GHCziIOziEncodingziIconv_iconvEncoding12_info , _base_GHCziIOziEncodingziIconv_iconvEncoding11_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding1_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding3_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding4_info , _base_GHCziIOziEncodingziIconv_iconvEncoding7_info , _base_GHCziIOziEncodingziIconv_iconvEncoding6_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding6_info , _base_GHCziIOziEncodingziIconv_iconvEncoding6_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding14_bytes , _base_GHCziIOziEncodingziIconv_iconvEncoding8_info , _base_GHCziIOziEncodingziIconv_iconvEncoding12_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding13_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding10_bytes , _base_GHCziIOziEncodingziIconv_iconvEncoding9_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding11_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding13_info , _base_GHCziIOziEncodingziIconv_iconvEncoding12_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding1_closure , _hs_iconv_open , _base_GHCziIOziEncodingziIconv_iconvEncoding15_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding4_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding1_info , _base_GHCziIOziEncodingziIconv_iconvEncoding15_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding13_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding4_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding15_info , _base_GHCziIOziEncodingziIconv_iconvEncoding9_info , _hs_iconv , _base_GHCziIOziEncodingziIconv_iconvEncoding5_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding8_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding2_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding2_info$def , _base_GHCziIOziEncodingziIconv_iconvEncoding7_info$def , _hs_iconv_close , _base_GHCziIOziEncodingziIconv_iconvEncoding7_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding9_closure , _base_GHCziIOziEncodingziIconv_iconvEncoding2_info , _base_GHCziIOziEncodingziIconv_iconvEncoding_info )
  "_iconv_open", referenced from:
      _hs_iconv_open in libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(iconv.o)
     (maybe you meant: _hs_iconv_open)
  "_iconv_close", referenced from:
      _hs_iconv_close in libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(iconv.o)
     (maybe you meant: _hs_iconv_close)
  "_locale_charset", referenced from:
      _localeEncoding in libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(PrelIOUtils.o)
ld: symbol(s) not found for architecture arm64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
```
So, we are missing libiconv, that’s not so bad, apple provides that.

9:54 After disabling Bitcode, and adding libiconv, the build suceeds! :tada:

2 files
![Screenshot 2021-12-23 at 9.51.04 PM](assets/Screenshot%202021-12-23%20at%209.51.04%20PM.png)
![Screenshot 2021-12-23 at 9.53.03 PM](assets/Screenshot%202021-12-23%20at%209.53.03%20PM.png)

9:56 It actually runs on the device. (An iPhone 7+ here).
![Image from iOS 1.png](assets/Image%20from%20iOS%201.jpg)


9:58 This seems like a good point to leave it here for today. Guess we “solved” the basic task of getting a haskell library running on an iPhone.


9:02 AM Wow. This is really cool! No support for Mach-o yet, but it might be useful going forward. https://github.com/wader/fq

9:08 AM For those who followed along and wonder if we could just use the same approach we used for iOS and translate it to Android? That is a very good idea however it hinges on a readily available aarch64-android aka aarch64-linux-bionic. Outside of Android Open Source Project (AOSP), this does not seem like a common setup. It might also be mostly hindered by googles build system and the AOSP. Building singular components from that project in isolation is hard and for anything but integrated in Android is just not supported well. It's certainly a quite interesting idea that would be fun to explore; likely just very time consuming.

9:10 I think I'd rather try to see if we can find some middle ground and try to go for the same as with musl. We build a linux-bionic executable with the cross compiler on linux-gnu and try to see if we can make it run on linux-gnu.

9:15 While we have a few interesting avenues to explore, I'm going to focus on avenues that should have some a chance of working in the future as well. That is as few modifications, and customizations as possible and only as thin as possible veneers over upstream.

2:49 PM Let’s try to continue with our Journey to build an Android mobile application. The library we have so far does not do anything fancy. We don’t use any TH, so this should be trivially buildable with a “stock” cross compiler. We’ll hence (for now) focus on build an actual android application. For this we’ll use Android Studio.

2:54 PM After launching Android Studio, we’ll create a new project, with an empty activity. Select kotlin as language (default) and API 21 as the minimum SDK. Neither the language nor the minimum SDK should pose any issues (I hope).
4 files 
![Screenshot 2021-12-25 at 2.49.49 PM](assets/Screenshot%202021-12-25%20at%202.49.49%20PM.png)
![Screenshot 2021-12-25 at 2.50.48 PM](assets/Screenshot%202021-12-25%20at%202.50.48%20PM.png)
![Screenshot 2021-12-25 at 2.51.00 PM](assets/Screenshot%202021-12-25%20at%202.51.00%20PM.png)
![Screenshot 2021-12-25 at 2.52.09 PM](assets/Screenshot%202021-12-25%20at%202.52.09%20PM.png)


2:56 We can use our hydra build of the mobile-core library for aarch64-android to obtain the libraries we need.  This build was only successful because we didn’t use any TH, and as such could simply cross compile our trivial library.

2:57 Android Studio should now look something like this.
![Screenshot 2021-12-25 at 2.56.58 PM](assets/Screenshot%202021-12-25%20at%202.56.58%20PM.png) 


3:00 Now, to add our haskell library to an android application the easies seem to be to right-click on the “app” in the project tree on the left, and select “Add C++ to module”.

3:01 That will add some CMake and C++ file to our project, both files contain some guidance, notes on how to use it. But we do need to wire our haskell lib up via JNI.
3 files 
![Screenshot 2021-12-25 at 2.58.33 PM](assets/Screenshot%202021-12-25%20at%202.58.33%20PM.png)
![Screenshot 2021-12-25 at 2.58.40 PM](assets/Screenshot%202021-12-25%20at%202.58.40%20PM.png)
![Screenshot 2021-12-25 at 2.59.30 PM](assets/Screenshot%202021-12-25%20at%202.59.30%20PM.png)

3:08 PM We’ll drop the library files from the `pkg.zip` from hydra, into `libs/arm64-a8v` into the `cpp` folder next to the pre-generated `CMakeLists.txt`.
3:09 Again, you can follow along with the source at github.com:zw3rk/mobile-core-android
![Screenshot 2021-12-25 at 3.08.45 PM](assets/Screenshot%202021-12-25%20at%203.08.45%20PM.png) 


3:11 If we launch the application (on a device) right, now, nothing much will happen. We’ll just be greeted with a “Hello World!” text. After all, we didn’t link the libraries, nor do provide any integration just yet.

3:14 To get android studio to link our lirbaries into our build target, we need to add the following boilerplate to the CMakeLists.txt to make it aware of the libraries, and have it find them:
```
add_library( mobile-core STATIC IMPORTED )
set_target_properties( mobile-core PROPERTIES IMPORTED_LOCATION
        ${CMAKE_SOURCE_DIR}/libs/${ANDROID_ABI}/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a)

add_library( ffi STATIC IMPORTED )
set_target_properties( ffi PROPERTIES IMPORTED_LOCATION
        ${CMAKE_SOURCE_DIR}/libs/${ANDROID_ABI}/libffi.a)

add_library( gmp STATIC IMPORTED )
set_target_properties( gmp PROPERTIES IMPORTED_LOCATION
        ${CMAKE_SOURCE_DIR}/libs/${ANDROID_ABI}/libgmp.a)
and adjust the target_link_libraries section to include them in the final product:
target_link_libraries( # Specifies the target library.
                       mobile_core_android

                       # Our Haskell library, as well as ffi and gmp,
                       # on which our haskell ibrary depends.
                       mobile-core
                       ffi
                       gmp
        
                       # Links the target library to the log library
                       # included in the NDK.
                       ${log-lib} )
```

3:15 So far, we have an empty .cpp file, we want to use for JNI to bridge the functions, and our haskell library all wired up.
3:18 In our MainActivity.kt we need a companion object to load our library (the linked product of our haskell library with our wrapping code).  The mobile_core_android.cpp had kindly provided this as a guidance. All we need is to also somehow initialize our Hasekll runtime. Let’s assume that function is called initHS() for now. So your companion object is
```kotlin
    companion object {
      init {
         System.loadLibrary("mobile_core_test")
         initHS()
      }
    }
```

3:21 We also need an external declaration, otherwise it will stick to trying to find the function in the kotlin module. So we add
```
external fun initHS()
```
prior to the
```
class MainActivity
```

3:22 Now we can use the helpful tooling in Android Studio and click on the red exclamation mark next to the external fun statement.
![Screenshot 2021-12-25 at 3.20.35 PM](assets/Screenshot%202021-12-25%20at%203.20.35%20PM.png)

3:24 That generates the following code in mobile_core_android.cpp
```cpp
extern "C"
JNIEXPORT void JNICALL
Java_com_zw3rk_mobile_1core_1android_MainActivityKt_initHS(JNIEnv *env, jclass clazz) {
    // TODO: implement initHS()
} 
```
which we’ll need to make aware of the extern (it lives in our library) hs_init function, and call it.

3:45 PM So, we’ll extend it a bit
```cpp
extern void hs_init(int argc, char **argv[]);

extern "C"
JNIEXPORT void JNICALL
Java_com_zw3rk_mobile_1core_1android_MainActivityKt_initHS(JNIEnv *env, jclass clazz) {
    hs_init(NULL, NULL);
}
```
and are greeted with the following helpful note upon compilation:
```
ninja: error: '[...]/armeabi-v7a/libmobile_core_android.so', missing and no known rule to make it
```
so, what happened here? Clearly we don’t have aarch32 libraries, but somehow android tried to build one.

3:49 This is where I find android studio just very confusing every time again. We need to set the following in the app/build.gradle file.
```
ndk { abiFilters 'arm64-v8a' }
```
under `android > defaultConfig` , and then select sync now.

3:49 Once we have native libraries for other architectures, we can add more here, and target more than just aarch64 based android devices.

3:56 PM After fixing that up, we are greeted with the following great error message
```
mobile_core_android.cpp:26: undefined reference to `hs_init(int, char**)'
```
this is due to our extern declaration not explicitly stating `extern "C"` :facepalm: , so let’s fix that for hs_init.

3:57 Huh, this is ... somewhat unexpected
```
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(GetTime.o): In function `getCurrentThreadCPUTime':
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/posix/GetTime.c:96: undefined reference to `clock_getcpuclockid'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(Signals.o): In function `backtrace_handler':
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/posix/Signals.c:547: undefined reference to `stderr'
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/posix/Signals.c:547: undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(PrelIOUtils.o): In function `localeEncoding':
PrelIOUtils.c:(.text.localeEncoding+0x0): undefined reference to `locale_charset'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(iconv.o): In function `hs_iconv_open':
iconv.c:(.text.hs_iconv_open+0x0): undefined reference to `libiconv_open'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(iconv.o): In function `hs_iconv':
iconv.c:(.text.hs_iconv+0x0): undefined reference to `libiconv'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(iconv.o): In function `hs_iconv_close':
iconv.c:(.text.hs_iconv_close+0x0): undefined reference to `libiconv_close'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(Heap.o): In function `heap_view_closurePtrs':
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/Heap.c:226: undefined reference to `stderr'
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/Heap.c:226: undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(Hpc.o): In function `readTix':
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/Hpc.c:(.text.startupHpc+0x660): undefined reference to `stderr'
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/Hpc.c:(.text.startupHpc+0x664): undefined reference to `stderr'
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/Hpc.c:(.text.startupHpc+0x678): undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(Hpc.o):/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/Hpc.c:(.text.startupHpc+0x67c): more undefined references to `stderr' follow
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(RtsFlags.o): In function `procRtsOpts':
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsFlags.c:800: undefined reference to `stdout'
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsFlags.c:(.text.procRtsOpts+0x8c): undefined reference to `stdout'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(RtsFlags.o): In function `errorUsage':
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsFlags.c:1785: undefined reference to `stdout'
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsFlags.c:1785: undefined reference to `stdout'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(RtsMessages.o): In function `rtsFatalInternalErrorFn':
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsMessages.c:(.text.rtsFatalInternalErrorFn+0x14): undefined reference to `stderr'
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsMessages.c:(.text.rtsFatalInternalErrorFn+0x24): undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(RtsMessages.o): In function `rtsDebugMsgFn':
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsMessages.c:308: undefined reference to `stderr'
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsMessages.c:308: undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(RtsMessages.o): In function `rtsErrorMsgFn':
/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsMessages.c:214: undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(RtsMessages.o):/build/aarch64-unknown-linux-android-ghc-8.10.7-configured-src/rts/RtsMessages.c:214: more undefined references to `stderr' follow
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libffi.a(closures.o): In function `allocate_space':
closures.c:(.text+0x2884): undefined reference to `__write_chk'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libffi.a(closures.o): In function `open_temp_exec_file_memfd':
closures.c:(.text+0x28d0): undefined reference to `memfd_create'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libffi.a(closures.o): In function `open_temp_exec_file_dir':
closures.c:(.text+0x29b0): undefined reference to `mkostemp'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libgmp.a(realloc.o): In function `__gmpz_realloc':
(.text+0x8c): undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libgmp.a(realloc.o): In function `__gmpz_realloc':
(.text+0x90): undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libgmp.a(assert.o): In function `__gmp_assert_header':
(.text+0x28): undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libgmp.a(assert.o): In function `__gmp_assert_header':
(.text+0x2c): undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libgmp.a(assert.o): In function `__gmp_assert_fail':
(.text+0x7c): undefined reference to `stderr'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libgmp.a(assert.o):(.text+0x80): more undefined references to `stderr' follow
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
ninja: build stopped: subcommand failed.
```

3:58 We are apparently missing libiconv (again...; we were missing that on iOS as well). But a bunch of stdout and other symbols are missing?

4:02 I guess we might have to explicilty link libc  (in androids case this is bionic).

4:03 So again edit the CMakeLists.txt file, and add
```
# find libc
find_library( c-lib
              c )
```
to give CMake a library to look for (c) and a name to refer back to it c-lib. And then throw in ${c-lib} into the target_link_libraries as well.

4:04 Sure enough we are now left with only
```
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(PrelIOUtils.o): In function `localeEncoding':
PrelIOUtils.c:(.text.localeEncoding+0x0): undefined reference to `locale_charset'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(iconv.o): In function `hs_iconv_open':
iconv.c:(.text.hs_iconv_open+0x0): undefined reference to `libiconv_open'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(iconv.o): In function `hs_iconv':
iconv.c:(.text.hs_iconv+0x0): undefined reference to `libiconv'
mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a(iconv.o): In function `hs_iconv_close':
iconv.c:(.text.hs_iconv_close+0x0): undefined reference to `libiconv_close'
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
ninja: build stopped: subcommand failed.
```
which are all symbols from libiconv I believe.

4:06 unlike Apple, Google doesn’t ship libiconv with android. But we can build an grab a static one for android.

4:10 I’ve added libiconv to the packaging, and now the pkg.zip from a more recent build of aarch64-android:lib:mobile-core:smallAddressSpace.x86_64-linux contains libiconv.a as expected.

4:15 After adding libiconv to the android project the build now succeeds, but the app won’t lauch, and crashes right away. Cool.

4:15 Android Studio has a debugger, so let’s try that, and see what comes up...

4:23 PM well, it just keeps crashing wihtout providing any form of output. This is exiting.

4:33 PM So debugger won’t come up, app won’t start, I guess we are looking for breadcrumbs in that brutal logcat event log... and we find?
```
    --------- beginning of crash
2021-12-25 16:30:58.217 25280-25280/com.zw3rk.mobile_core_android E/AndroidRuntime: FATAL EXCEPTION: main
    Process: com.zw3rk.mobile_core_android, PID: 25280
    java.lang.UnsatisfiedLinkError: dalvik.system.PathClassLoader[DexPathList[[zip file "/data/app/com.zw3rk.mobile_core_android-1/base.apk"],nativeLibraryDirectories=[/data/app/com.zw3rk.mobile_core_android-1/lib/arm64, /data/app/com.zw3rk.mobile_core_android-1/base.apk!/lib/arm64-v8a, /vendor/lib64, /system/lib64]]] couldn't find "libmobile_core_test.so"
        at java.lang.Runtime.loadLibrary(Runtime.java:367)
        at java.lang.System.loadLibrary(System.java:1076)
        at com.zw3rk.mobile_core_android.MainActivity.<clinit>(MainActivity.kt:15)
        at java.lang.Class.newInstance(Native Method)
        at android.app.Instrumentation.newActivity(Instrumentation.java:1068)
        at android.app.ActivityThread.performLaunchActivity(ActivityThread.java:2335)
        at android.app.ActivityThread.handleLaunchActivity(ActivityThread.java:2494)
        at android.app.ActivityThread.access$900(ActivityThread.java:153)
        at android.app.ActivityThread$H.handleMessage(ActivityThread.java:1347)
        at android.os.Handler.dispatchMessage(Handler.java:102)
        at android.os.Looper.loop(Looper.java:148)
        at android.app.ActivityThread.main(ActivityThread.java:5451)
        at java.lang.reflect.Method.invoke(Native Method)
        at com.android.internal.os.ZygoteInit$MethodAndArgsCaller.run(ZygoteInit.java:726)
        at com.android.internal.os.ZygoteInit.main(ZygoteInit.java:616)
```

4:34 loadLibrary crashes, couldn't find "libmobile_core_test.so"
4:34 that does indeed sound reasonable, out lirbary is called libmobile_core_android.so, :facepalm:
4:35 after fixing that, the app still crashes. But at least we can get a debugger attached this time.
4:39
```
* thread #1, name = 'le_core_android', stop reason = signal SIGSEGV: invalid address (fault address: 0x0)
  * frame #0: 0x0000007f72a8aec8 libmobile_core_android.so`::getauxval(unsigned long) at getauxval.cpp:41:60
    frame #1: 0x0000007f72a8aec0 libmobile_core_android.so`::getauxval(type=6) at getauxval.cpp:53
    frame #2: 0x0000007f72a25e74 libmobile_core_android.so`::sysconf(name=<unavailable>) at sysconf.cpp:0
    frame #3: 0x0000007f72a79868 libmobile_core_android.so`je_pages_boot [inlined] os_page_detect at pages.c:410:16
    frame #4: 0x0000007f72a7985c libmobile_core_android.so`je_pages_boot at pages.c:577
    frame #5: 0x0000007f72a56784 libmobile_core_android.so`malloc_init_hard_a0_locked at jemalloc.c:1298:6
    frame #6: 0x0000007f72a56f8c libmobile_core_android.so`malloc_init_hard at jemalloc.c:1533:6
    frame #7: 0x0000007f8b068c2c linker64`__dl__ZN6soinfo13call_functionEPKcPFvvE + 108
    frame #8: 0x0000007f8b068d80 linker64`__dl__ZN6soinfo10call_arrayEPKcPPFvvEmb + 252
    frame #9: 0x0000007f8b06f148 linker64`__dl__Z9do_dlopenPKciPK17android_dlextinfo + 440
    frame #10: 0x0000007f8b0683b0 linker64`__dl_dlopen + 48
    frame #11: 0x0000007f87523330 libart.so`art::JavaVMExt::LoadNativeLibrary(_JNIEnv*, std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> > const&, _jobject*, std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >*) + 884
    frame #12: 0x0000007f875c3a78 libart.so`art::Runtime_nativeLoad(_JNIEnv*, _jclass*, _jstring*, _jobject*, _jstring*) + 300
    frame #13: 0x0000000073d5eb30
    frame #14: 0x0000007f873411c8 libart.so`art::ArtMethod::Invoke(art::Thread*, unsigned int*, unsigned int, art::JValue*, char const*) + 348
    frame #15: 0x0000007f876e33f4 libart.so`artInterpreterToCompiledCodeBridge + 216
    frame #16: 0x0000007f874bcb04 libart.so`bool art::interpreter::DoCall<false, false>(art::ArtMethod*, art::Thread*, art::ShadowFrame&, art::Instruction const*, unsigned short, art::JValue*) + 484
    frame #17: 0x0000007f872ee54c libart.so`art::JValue art::interpreter::ExecuteGotoImpl<false, false>(art::Thread*, art::DexFile::CodeItem const*, art::ShadowFrame&, art::JValue) + 22204
    frame #18: 0x0000007f8749a514 libart.so`art::interpreter::EnterInterpreterFromEntryPoint(art::Thread*, art::DexFile::CodeItem const*, art::ShadowFrame*) + 100
    frame #19: 0x0000007f8774ff04 libart.so`artQuickToInterpreterBridge + 636
    frame #20: 0x0000007f8733afe8 libart.so`art_quick_to_interpreter_bridge + 104
    frame #21: 0x0000007f8733151c libart.so`art_quick_invoke_static_stub + 604
    frame #22: 0x0000007f873411c8 libart.so`art::ArtMethod::Invoke(art::Thread*, unsigned int*, unsigned int, art::JValue*, char const*) + 348
    frame #23: 0x0000007f873ad290 libart.so`art::ClassLinker::InitializeClass(art::Thread*, art::Handle<art::mirror::Class>, bool, bool) (.part.608) + 856
    frame #24: 0x0000007f873ae234 libart.so`art::ClassLinker::EnsureInitialized(art::Thread*, art::Handle<art::mirror::Class>, bool, bool) + 120
    frame #25: 0x0000007f875bdb40 libart.so`art::Class_newInstance(_JNIEnv*, _jobject*) + 2840
    frame #26: 0x0000000073a6e140
```
huh? :confused: We crash in jemalloc’s initialization?

8:07 PM What’s going on here? So Android applications are fundamentally Java (or Kotlin) applications. As such we can’t fully statically link them, and any JNI library we provide has to be a shared object. This is then loaded (and initialized) once the Application launched. We can see a lot of Android Runtime (ART) calls, that eventually end in a call to nativeLoad, at which point we are trying to use the loader (linker64) to load the shared object. During the opening of the dynamic library (dlopen), we process initialization routines. Yes, dynamic libraries can have functionality that’s executed when they are loaded and when they are unloaded. This is mostly used to set some global state. We all love state! For some reason malloc_init_hard from the jemalloc library is in there, and as the ART trying to initialize the dynamic library it hits jemallocs initialization code, which tries to obtain the page size. Page size is something that’s dependent on the Memory Management Unit (MMU), and which values it permits. We use page sizes to chunk up address space. The most common page size value is probably 4096 byte (4kb). And some software just hardcodes this, but we can ask the kernel for this via sysconf.  Relatedly this is something that’s different on apples platforms where pagesizes default to 16kb now on M1s; and some software tripped over this change.

8:07 But, jemalloc should be provided by the bionic libc, so why is it in our shared object? I guess we need to investigate the construction of our libHSmobile-core.a

8:20 PM I assume it might be helpful to outline how I usually try to investigate this and figure out what’s wrong? We have a hypothesis as to what’s wrong (we uinitentionally link libjemalloc into our libHSmoblle-core.a).  To validate this, we’ll most likely want to look at the step where we built that actual library. Because use nix, this is luckily fairly easy, we pulled our packaged zip file with the library from https://ci.zw3rk.com/build/427476, (and we know that the packaging is in the postInstall phase of that build), if we click on Details, we find the “derivation store path”, which is /nix/store/i891wvgpydi78wn32ijfqq5b3q74kyrs-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv

8:21 The details page also tells us that that derivation was built on an x86_64-linux machine. So we head on over to one. And run
```shell
$ nix-shell --pure /nix/store/i891wvgpydi78wn32ijfqq5b3q74kyrs-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv
```
this should drop us into the environment in which we can build said derivation, and follow the build steps. We use --pure, so that we don’t leak in external envrionement variable (and as such we don’t leak in PATH either).

8:23 First thing I usually do is, ensure I’m in a temoprary directory: cd $(mktemp -d), and then use the genericBuild function. Sometimes it throws me out of the shell, in which case I follow unpackPhase, patchPhase, configurePhase, buildPhase. to narrow it down.

8:25 As the genericBuild runs though just fine, I’m interested in re-running the buildPhase to inspect that closer. echo "$buildPhase" will show us the commands it would have run, and we grab the $SETUP_HS one, add -v and re-run it.

8:29 In this specific case it’s
```
SSETUP_HS build lib:mobile-core -j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) --ghc-option=-fexternal-interpreter --ghc-option=-pgmi --ghc-option=/nix/store/ngzya50p174a82xw21y1i1gc3kr247h4-iserv-wrapper/bin/iserv-wrapper --ghc-option=-L/nix/store/pb2bzxmmv5mkmakxg6ywrbaxdpy3yhpl-gmp-6.2.1-aarch64-unknown-linux-android/lib --ghc-option=-fPIC --gcc-option=-fPIC -v
from which we find the last tool invocation as
/nix/store/6lq7v08pv08l43miy80pvxq0w0amcs3j-mobile-core-lib-mobile-core-0.1.0.0-aarch64-unknown-linux-android-ghc-8.10.7-env/bin/aarch64-unknown-linux-android-ghc -staticlib -this-unit-id mobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf -hide-all-packages -no-auto-link-packages -clear-package-db -package-db /nix/store/zzpwdbaz0hrnhzrj9671s1y738pijvf6-aarch64-unknown-linux-android-mobile-core-lib-mobile-core-0.1.0.0-config/lib/aarch64-unknown-linux-android-ghc-8.10.7/package.conf.d -package-db dist/package.conf.inplace -package-id base-4.14.3.0 dist/build/Lib.o -o dist/build/libHSmobile-core-0.1.0.0-HfUuggbqw4DC9ci8Blc8Tf-ghc8.10.7.a -fPIC -staticlib -fexternal-interpreter -pgmi /nix/store/ngzya50p174a82xw21y1i1gc3kr247h4-iserv-wrapper/bin/iserv-wrapper -L/nix/store/pb2bzxmmv5mkmakxg6ywrbaxdpy3yhpl-gmp-6.2.1-aarch64-unknown-linux-android/lib -fPIC
```

8:37 PM running nm over the produces archive, it looks like malloc_init_hard or other symbols are not in that library. So where are they from? The search continues.

10:25 PM if we look for malloc_init in the produced .so from android studio, we see
```
0000000000d7d99c t je_malloc_initialized
0000000000d89de0 t malloc_init_hard
0000000000d889d0 t malloc_init_hard_a0_locked
000000000105dcdc d malloc_init_state
0000000001067070 b malloc_initializer
```
which I find confusing, as that implies that malloc_init_hard is a local text symbol (e.g. a symbol pointing at machine code).

11:23 PM It appears that cmake decids to statically link the libc we try to link. And getting proper debug output of that andorid buildsystem with gradle driving cmake, driving ninja is rather painful.

11:39 PM So turns out the problem is that CMake’s
```
find_library( c-lib
              c )
```
returns the full path to the static archive (.../libc.a). I’ve yet to find out how to force it to pick up the dynamic library.

10:20 AM The best way to debug anything in cmake through android studio seems to be some rather brutal printf style debugging. We can use message(FATAL_ERROR "..."), to have cmake exit, and give us the output. This way we can see that find_library, finds
```
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/libc.a
```
for the static library. The shared ones are in the API subfolder. e.g.
```
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/21/libc.so
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/22/libc.so
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/23/libc.so
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/24/libc.so
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/26/libc.so
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/27/libc.so
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/28/libc.so
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/29/libc.so
~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot/usr/lib/aarch64-linux-android/30/libc.so
```
:thinking_face:

10:32 AM The android.toolchain.cmake from the shipped sdk has this beautiful section
```
# find_library searches a handful of paths as described by
# https://cmake.org/cmake/help/v3.6/command/find_library.html. CMake doesn't
# understand the Android sysroot layout, so we need to give the direct path to
# the libraries rather than just the sysroot. Set up CMAKE_SYSTEM_LIBRARY_PATH
# (https://cmake.org/cmake/help/v3.6/variable/CMAKE_SYSTEM_LIBRARY_PATH.html)
# instead.

# NB: This variable causes CMake to automatically pass --sysroot to the
# toolchain. Studio currently relies on this to recognize Android builds. If
# this variable is removed, ensure that flag is still passed.
# TODO: Teach Studio to recognize Android builds based on --target.
set(CMAKE_SYSROOT "${ANDROID_TOOLCHAIN_ROOT}/sysroot")

# Allows CMake to find headers in the architecture-specific include directories.
set(CMAKE_LIBRARY_ARCHITECTURE "${ANDROID_TOOLCHAIN_NAME}")

# Instructs CMake to search the correct API level for libraries.
list(APPEND CMAKE_SYSTEM_LIBRARY_PATH
  "/usr/lib/${ANDROID_TOOLCHAIN_NAME}/${ANDROID_PLATFORM_LEVEL}")
```

10:34 CMAKE_SYSROOT ends up being correctly set to ~/Library/Android/sdk/ndk/21.4.7075529/toolchains/llvm/prebuilt/darwin-x86_64/sysroot, and CMAKE_SYSTEM_LIBRARY_PATH is /usr/lib/aarch64-linux-android/21;/usr/lib/X11, by that logic, it should be able to find libc.so; debugging this find_library function is not the easiest it seems :disappointed:

11:01 AM sigh, you need a newer cmake, than the one android studio ships to enable find_library debugging.

11:39 AM Turns out, I’ve been going down the wrong rabbit hole. Even if we link libc.so, the necessary symbols are not exported. The primary issue is that I had the minSdk set to 21. Which is not what we built our software against. We built it against r23. Bump that leaves us with three remaining problems:
```
/mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libffi.a(closures.o): In function `allocate_space':
closures.c:(.text+0x2884): undefined reference to `__write_chk'
/mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libffi.a(closures.o): In function `open_temp_exec_file_memfd':
closures.c:(.text+0x28d0): undefined reference to `memfd_create'
/zw3rk/mobile-core-android/app/src/main/cpp/libs/arm64-v8a/libiconv.a(localcharset.o): In function `locale_charset':
(.text+0xc): undefined reference to `nl_langinfo'
```
nl_langinfo, and _write_chk would be available from sdk 26 onwards. But my test android device only has r23. So at least I will need to work around this somehow. memfd_create isn’t available (exposed) in sdk 26 either. Hmm...

2:08 PM So if we don’t have that functionality in the SDK, why did we end up building it that way?

2:08 libiconv and libffi are C libraries. Short detour about autotools.

2:13 While most of us work on a daily basis with languages that come with their own respective build system (rust has cargo, haskell has cabal, javascript has npm, scala has sbt, ...) around the time when this all started an we had mostly C, assembly, pascal, and a few other languages, programs were smaller, and operating systems more diverse. So were C compilers, at different versions, operating systems, ... and the major question became: how do I build my software across multiple architectures on different operating systems against different toolchains? The solution still sticks with us to this day, in the form of autotools.
The idea is fairly simple: what if we rely on the bare minimum we can everyone expect to agree on? Some very basic shell? And then we provide routines to (a) guess some values, but more importantly (b) interrogate tools to derive information?

2:15 While we can use Makefiles to fairly easily define a set of build rules, and make being available with almost every toolchain, we do not yet have a way for feature detection. Does this system support function xyz? How does it expect to link library lib? How large are machine words? 4byes? 8bytes? Or even something different? What are the sizes of c types? ints, longs, ...

2:16 Thus autotools provides a way to describe in a fairly obscure language how we want to detect features in a set of files (configure.ac, aclocal.m4, ...) m4 being yet another macro processing language.

2:16 So we write a simple configure.ac, and run auto(re)conf on it. It will produce a configure shell script for us, that does all the feature detection we asked for.

2:19 As it turns out libffi and libiconv both use configure scripts to detect features. libiconv checks for to see if it can use langinfo, and will write #define HAVE_LANGINFO_CODESET 1 into a header file if it thinks it can. Similarly libffi, will check if it can use memfd_create, and if it deems so write #define HAVE_MEMFD_CREATE 1 into a header file.

2:20 _write_chk is the result of a fortification c compiler flag. (And nix will by default enable fortification in its attempt to produce hardened libraries and applications).

2:21 The SDK we are targeting doesn’t support fortification, if my recollection from the online research is correct, fortification should be available from sdk 24 onwards. We are at 23.

2:23 Now, having nix as our base to describe our libraries comes in handy. While I have no idea (yet?) why the configure detection script seemingly detects the wrong availability for the android cross compiler, we can fix this up fairly easily. Every build product (library or executable) is defined by a so call derivation, which is a set of build instructions that nix evaluates to build the library, executable or anything really.

2:25 For the android cross compiled libffi this is pkgsCross.aarch64-android.libffi. And we want to “fix up” this derivation a little. We can just override some values.

2:28
```
libffi.overrideAttrs (old: {
  dontDisableStatic = true;
  hardeningDisable = [ "fortify" ];
  postConfigure = ''
    echo "#undef HAVE_MEMFD_CREATE" >> aarch64-unknown-linux-android/fficonfig.h
  '';
});
```
This will take the libffi derivation, and enable producing static (.a) lirbaries, disable the fortify hardening feature, and ... quite unorthodox, force HAVE_MEMFD_CREATE  to be undefined. This effectively reverse any #define HAVE_MEMFD_CREATE that the configure script would have written into the header file.
As this is run in postConfigure, we are effectively patching up the value we didn’t want configure to produce. :facepalm:

2:29 We can do the same for libiconv to disable HAVE_LANGINFO_CODESET.

2:29 With both libraries patched up, we should then not run into any further linking complications...

2:29 ... and we don’t. It builds; and doesn’t crash on load.

2:30 So let’s fix up that hello() string.

2:32 Our Main Activity has a TextView with an id called example. This is the text, we want to replace with the string from our haskell library.
![Screenshot 2021-12-26 at 2.30.43 PM](assets/Screenshot%202021-12-26%20at%202.30.43%20PM.png) 

2:33 In our MainActivity.kt file we need to first tell the compiler that there is an external function we want to call, and it returns a String.

2:34 We add
```kotlin
external fun hello() : String
```

2:34 We can then use that function in the onCreate method, to set the TextViews’ content:
```kotlin
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
        val tv = findViewById<TextView>(R.id.example)
        tv.text = hello()
    }
```

2:36 Android studio will complain that there is no hello external function, and provide us with a quick fix.
![Screenshot 2021-12-26 at 2.35.55 PM](assets/Screenshot%202021-12-26%20at%202.35.55%20PM.png) 

2:37 This will take us to the mobile_core_android.cpp file and it will also have generated the proper function name for us, all we need to do is fill our the logic.
2:37 We come up with the following solution:
```cpp
extern "C"
JNIEXPORT jstring JNICALL
Java_com_zw3rk_mobile_1core_1android_MainActivityKt_hello(JNIEnv *env, jclass clazz) {
    return env->NewStringUTF(hello());
}
``

2:38 hello here is the external c function we exported from haskell.
2:38 Thus our final complete mobile_core_android.cpp file looks like this
```cpp
#include <jni.h>

// Imports from our haskell library
extern "C" void hs_init(int * argc, char **argv[]);
extern "C" char *hello(void);

// JNI Exports for our Kotlin application.
extern "C"
JNIEXPORT void JNICALL
Java_com_zw3rk_mobile_1core_1android_MainActivityKt_initHS(JNIEnv *env, jclass clazz) {
    hs_init(NULL, NULL);
}
extern "C"
JNIEXPORT jstring JNICALL
Java_com_zw3rk_mobile_1core_1android_MainActivityKt_hello(JNIEnv *env, jclass clazz) {
    return env->NewStringUTF(hello());
}
```

2:42 And it compiles... and runs. Even on actual android hardware.
![Image from iOS 2](assets/Image%20from%20iOS%202.jpg)

:tada:

4:13 PM We’ll next focus on doing something slightly more complicated than exporting a c string from haskell. At this point we have the basic pipelines down, and bare minimum applications for both platforms.

8:44 PM I’ve tagged the relevant repositories (zw3rk/mobile-core, zw3rk/mobile-core-ios, and zw3rk/mobile-core-android) with milestone-1 tags. Maybe it’s a good time to summarise what we’ve got so far.

9:09 PM Summary (up to Milestone 1)
- We’ve discussed system fundamentals, including file formats, dynamic and static linking, kernels, libcs and autotools.
- We’ve discussed the specific complications that GHC has with cross compilation.
- We’ve build a (really) simple haskell library (zw3rk/mobile-core)
- We’ve setup a nix build description for the haskell library using haskell.nix, including cross compilation targets using modern nix formulation (flakes) (zw3rk/mobile-core - flake.nix)
- We’ve added packaging and minor derivation modifications to our nix build description.
- We’ve started building a trivial modern iOS application using Swift
- We’ve integrated a foreign library with a C interface into our iOS application. (The haskell lirbary we built).
- We’ve started building a trivial modern Android application using Kotlin
- We’ve integrated a foreign library with a C interface into our Android application.

We sadly had to deal with some complications along the way (that mostly had to do with linking)
- Making macOS aarch64 built object files, compatible with iOS (rewriting the platform embedded in the object files)
- Dealing with library expectations and the android SDK, and working around limitations.

From a project management perspective, we could now comfortably start with three (or more) teams. If the teams can agree on API functionality for the next release, the library team can focus on implementing the business logic; they can also use all the testing tools at their disposal for the library. And each of the iOS and Android teams can preliminarily mock their APIs, and just replace the mocks with the library once delivered. We could add additional teams if we wanted a web based solution now. We might also want a service (as in JSON api, ...) solution. We could also want a CLI solution. Or a desktop solution (natively or via Electron, ...). This can all be similarly split across the same boundary.

Challenges ahead:
- We haven’t used any Template Haskell yet. (And I’m certain it will break for our Android pipeline; as I’ve alluded to in the discussion around Template Haskell)
- We haven’t really done any interesting computation; and maybe we’ll run into bugs.
- We haven’t had to deal with large dependency trees and obscure dependencies yet.
- We haven’t looked at size of the final executable.
- We don’t support anything but aarch64 targets yet.

For milestone 2, we’ll look at the first three bullet points: TH, some interesting computation, and some non-trivial dependencies.

5:47 PM Quite busy today, so I didn’t get around doing much. But we can break the android pipeline in Lib.hs fairly easy by adding an ANN annotation. This will cause GHC to run the same as the Template Haskell pipeline and shell out to iserv. We can see this failing now.
The key to note is qemu-aarch64: Could not open '/system/bin/linker64': No such file or directory, which tells us (as we’ve outlined a few days ago), that the ghc-iserv application we try to run, is a dynamic executable, that refers to android’s bionic linker.
5:50 Let’s run an illustrative experiment, building a simple executable against the android sdk.
5:52 Using nix with nixpkgs, the easiest to get hold of an android cross compiler, with the associated toolchain is to enter a shell like so:
```shell
$ nix-shell -p pkgsCross.aarch64-android.stdenv.cc
```
This will drop us into a [nix-shell:~] that has aarch64-unknown-linux-android-cc available.

5:53 Next, let’s create a very trivial c program for experimentation:
```shell
$ cat > test.c << EOF
> #include <stdio.h>
> int main(int argc, char ** argv) {
>   printf("Hi from aarch64-bionic\n");
> }
> EOF
```

5:55 We still have our system native c compiler around, so let’s first try that one:
```shell
$ gcc test.c -o test-native
```
This will compiler test.c  into an executable called test-native that we should be able to just execute.  But for now let’s try to understand what this executable is actually made of. The file tool is as always quite helpful for some basic inspection:
```shell
$ file test-native
test-native: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), dynamically linked, interpreter /nix/store/z34r7j0ni01ggc7y243114mdyag345xy-glibc-2.32-46/lib/ld-linux-aarch64.so.1, for GNU/Linux 2.6.32, with debug_info, not stripped
```
so, be default we got a dynamically linked (against glibc) executable. The dynamic loader is ld-linux-aarch64.so.1  (I built this on an aarch64 native host).

5:59 Next, let’s look what the list dynamic dependencies (ldd) tool tells us
```shell
$ ldd test-native
	linux-vdso.so.1 (0x0000ffffaf722000)
	libc.so.6 => /nix/store/z34r7j0ni01ggc7y243114mdyag345xy-glibc-2.32-46/lib/libc.so.6 (0x0000ffffaf584000)
	/nix/store/z34r7j0ni01ggc7y243114mdyag345xy-glibc-2.32-46/lib/ld-linux-aarch64.so.1 (0x0000ffffaf6f1000)
```
so, we depend on the kernels vDSO, libc from glibc, and the loader. Nothing too surprising here.

5:59 Alright, now let’s try the same with the android cross compiler.

6:00 We compile test.c into a android native test executable
```shell
$ aarch64-unknown-linux-android-cc test.c -o test
```

and inspect the result again:

```shell
$ file test
test: ELF 64-bit LSB pie executable, ARM aarch64, version 1 (SYSV), dynamically linked, interpreter /system/bin/linker64, not stripped
$ ldd test
 	linux-vdso.so.1 (0x0000ffff9d9a1000)
	libunwind.so.1 => /nix/store/wwcpf7298pvyn3min351jd13i6a72k1z-libunwind-aarch64-unknown-linux-android-12.0.0/lib/libunwind.so.1 (0x0000ffff9d95f000)
	libc.so => /nix/store/a8n267l7b6wprb8vaspg37bygs162fch-bionic-prebuilt-ndk-release-r23/lib/libc.so (0x0000ffff9d947000)
	libdl.so => /nix/store/a8n267l7b6wprb8vaspg37bygs162fch-bionic-prebuilt-ndk-release-r23/lib/libdl.so (0x0000ffff9d996000)
```

6:01 We see that the c compiler decided to put references to the kernels vDSO, and the unwind, c, and dl lirbaries. But notably the interpreter (the dynamic loader) is /system/bin/linker64; which we don’t have on our aarch64-linux-gnu system.

6:01 What happens if we try to just load it with glibc’s loader? Let’s find out
```shell
$ /nix/store/z34r7j0ni01ggc7y243114mdyag345xy-glibc-2.32-46/lib/ld-linux-aarch64.so.1 $PWD/test
Bus error (core dumped)
```

6:02 doesn’t look so promising. What we really want is a static library, so we don’t have to deal with the linking nonsense. Let’s try to ask the compiler for a static executable:
```shell
$ aarch64-unknown-linux-android-cc test.c -o test -static
aarch64-unknown-linux-android-ld: error: unable to find library -lc
clang-12: error: linker command failed with exit code 1 (use -v to see invocation)
```

6:04 lovely, the static libc isn’t part of the shipped toolchain, why would it... people might do stupid things. But wait, didn’t we have android studio provide us with a static libc.a? Maybe nixpkgs is messing something up here, or the download it pulls in from which it extracts the libc, just doesn’t contain the static one. This would be another rabbit hole to fall into right now, but we can just (for experimentation and validation purposes) take the one we found in the android studio sdk.

6:05 We we drop the libc.a into a folder called lib, and try again
```shell
$ aarch64-unknown-linux-android-cc test.c -o test -static -L./lib
this time around the compiler didn’t complain. That seems fairly promising. Let’s inspect the build product again.
```

6:06
```shell
$ file test
test: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), statically linked, with debug_info, not stripped
$ ldd test
	not a dynamic executable
```
good riddence.  Dynamic keeps spelling trouble.  But does it work?
```shell
$ ./test
Hi from aarch64-bionic
```
:tada:

6:07 So, we’ll next try to statically link the ghc-iserv binary we build for android. (probably in a few hours, or maybe tomorrow).  
6:12 One quick note for now though. If we look at the CI build results for the change to the Lib.hs, which triggered the failure for the android build (aarch64-android:lib:mobile-core:smallAddressSpace.x86_64-linux), we see that the darwin build (lib:mobile-core:smallAddressSpace:static.aarch64-darwin) succeeded. Why is that? The key here is that for darwin we build natively on aarch64-macos. This means we don’t cross compile and have a full stage2 compiler at our disposal, for android however, we don’t have a full stage2 compiler running on aarch64-linux-bionic, as we don’t have such a system readily available; and thus have to resort to cross compilation.

8:46 PM So we can see remote-iserv failing, let’s try to get an idea what that executable that fails actually is. We’ll use nix-shell again.
8:47 From the failing job we can see that the derivation that failed is `/nix/store/9rawpvxc2xbw2r52bfkwqich4wvq7shy-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv`, so we will `nix-shell --pure /nix/store/9rawpvxc2xbw2r52bfkwqich4wvq7shy-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv`. To be put into the same build environment.

8:48 If we ask the environment for iserv, we’ll find:
```shell
$ env |grep iserv
$SETUP_HS build lib:mobile-core -j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) --ghc-option=-fexternal-interpreter --ghc-option=-pgmi --ghc-option=/nix/store/ngzya50p174a82xw21y1i1gc3kr247h4-iserv-wrapper/bin/iserv-wrapper --ghc-option=-L/nix/store/pb2bzxmmv5mkmakxg6ywrbaxdpy3yhpl-gmp-6.2.1-aarch64-unknown-linux-android/lib --ghc-option=-fPIC --gcc-option=-fPIC
```
8:51 that iserv-wrapper is some logic from haskell.nix to help setup the slave process. If we cat the wrapper file we find:
```shell
$ cat /nix/store/ngzya50p174a82xw21y1i1gc3kr247h4-iserv-wrapper/bin/iserv-wrapper
#!/nix/store/rfbw2ni8wbbiq6dda4xm2y4plflm7m27-bash-5.1-p8/bin/bash
set -euo pipefail
# Unset configure flags as configure should have run already
unset configureFlags
PORT=$((5000 + $RANDOM % 5000))
(>&2 echo "---> Starting remote-iserv on port $PORT")
/nix/store/cpsz6wyi4xx8scfir55brzkcvndg1yaw-qemu-6.1.0/bin/qemu-aarch64 /nix/store/rfj6bzfzq33fg2md8ycvq5120z2d5gn4-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv tmp $PORT &
(>&2 echo "---| remote-iserv should have started on $PORT")
RISERV_PID="$!"
/nix/store/mic3s83phylvm2p570ycp6ailmd63an2-iserv-proxy-exe-iserv-proxy-8.10.7/bin/iserv-proxy $@ 127.0.0.1 "$PORT"
(>&2 echo "---> killing remote-iserve...")
kill $RISERV_PID
```
so, the wrapper actually launches remote-iserv inside of qemu-aarch64 (we are translating aarch64 to x86_64 via qemu’s user mode emulation; this let’s us run foreign binaries like native ones).  And from the failed job, we know this somehow failed. The rest of the script essentially sets up a network bridge, so that we have a local (native) executable called iserv-proxy, that communicates via a network socket with the remote-iserv executable we are emulating via qemu. (For windows cross compilation, we don’t use qemu, but WINE).

8:53 If we interrogate the executable like we did before we find:
```shell
$ file /nix/store/rfj6bzfzq33fg2md8ycvq5120z2d5gn4-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv
/nix/store/rfj6bzfzq33fg2md8ycvq5120z2d5gn4-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), dynamically linked, interpreter /system/bin/linker64, with debug_info, not stripped
```
and
```shell
$ ldd /nix/store/rfj6bzfzq33fg2md8ycvq5120z2d5gn4-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv
	not a dynamic executable
```

8:53 so... it’s not a dynamic executable, but still has the interpreter set? That seems odd...

9:03 PM If we inspect the executable on an aarch64-linux machine we see
```shell
$ file remote-iserv
remote-iserv: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), dynamically linked, interpreter /system/bin/linker64, with debug_info, not stripped
```
and
```shell
$ ldd remote-iserv
./remote-iserv: error while loading shared libraries: /nix/store/z34r7j0ni01ggc7y243114mdyag345xy-glibc-2.32-46/lib/libc.so: invalid ELF header
```
If we try to execute it via the glibc loader on aarch64, we see
```shell
$ /nix/store/z34r7j0ni01ggc7y243114mdyag345xy-glibc-2.32-46/lib/ld-linux-aarch64.so.1 remote-iserv
remote-iserv: error while loading shared libraries: remote-iserv: cannot open shared object file: No such file or directory
```
this looks like some fairly frankensteined executable.

9:07 Let’s see how that remote-iserv is actually built. The CI (hydra) allows us to inspect the “Build dependencies” quickly. (nix-store -q --tree /path/to/drv should do the same on the command line).  
9:12 From there we’ll find the following derivation /nix/store/6hfk11qxd52gkcimnz923cjx7ibgfvyq-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7.drv, and corresponding build log. The build log is fairly sparse, so we’ll (again) use nix-shell to get more insight into what’s actually happening.
9:13 From the env again we grab (grep) the build command, and re-run it with added verbosity:
```shell
$ $SETUP_HS build exe:remote-iserv -v
```
This shows us the following:
```
Component build order: executable 'remote-iserv'
/nix/store/f7ampv2wm7zndxggmkjpc9cp7bayxvdi-remote-iserv-exe-remote-iserv-8.10.7-aarch64-unknown-linux-android-ghc-8.10.7-env/bin/aarch64-unknown-linux-android-ghc-pkg init dist/package.conf.inplace
creating dist/build/remote-iserv
creating dist/build/remote-iserv/autogen
creating dist/build/remote-iserv/autogen
Preprocessing executable 'remote-iserv' for remote-iserv-8.10.7..
Building executable 'remote-iserv' for remote-iserv-8.10.7..
creating dist/build/remote-iserv
creating dist/build/remote-iserv/remote-iserv-tmp
/nix/store/f7ampv2wm7zndxggmkjpc9cp7bayxvdi-remote-iserv-exe-remote-iserv-8.10.7-aarch64-unknown-linux-android-ghc-8.10.7-env/bin/aarch64-unknown-linux-android-ghc --make -no-link -fbuilding-cabal-package -O -split-sections -static -outputdir dist/build/remote-iserv/remote-iserv-tmp -odir dist/build/remote-iserv/remote-iserv-tmp -hidir dist/build/remote-iserv/remote-iserv-tmp -stubdir dist/build/remote-iserv/remote-iserv-tmp -i -idist/build/remote-iserv/remote-iserv-tmp -isrc -idist/build/remote-iserv/autogen -idist/build/global-autogen -Idist/build/remote-iserv/autogen -Idist/build/global-autogen -Idist/build/remote-iserv/remote-iserv-tmp -optP-include -optPdist/build/remote-iserv/autogen/cabal_macros.h -hide-all-packages -Wmissing-home-modules -clear-package-db -package-db /nix/store/vnw4a65kd8hms8bqnmsa61pb31g3a3q2-aarch64-unknown-linux-android-remote-iserv-exe-remote-iserv-8.10.7-config/lib/aarch64-unknown-linux-android-ghc-8.10.7/package.conf.d -package-db dist/package.conf.inplace -package-id base-4.14.3.0 -package-id libiserv-8.10.7-2MDjPwYrjU87yxghQqcqJs -XHaskell2010 src/Cli.hs -fPIC
Linking...
/nix/store/f7ampv2wm7zndxggmkjpc9cp7bayxvdi-remote-iserv-exe-remote-iserv-8.10.7-aarch64-unknown-linux-android-ghc-8.10.7-env/bin/aarch64-unknown-linux-android-ghc --make -fbuilding-cabal-package -O -split-sections -static -outputdir dist/build/remote-iserv/remote-iserv-tmp -odir dist/build/remote-iserv/remote-iserv-tmp -hidir dist/build/remote-iserv/remote-iserv-tmp -stubdir dist/build/remote-iserv/remote-iserv-tmp -i -idist/build/remote-iserv/remote-iserv-tmp -isrc -idist/build/remote-iserv/autogen -idist/build/global-autogen -Idist/build/remote-iserv/autogen -Idist/build/global-autogen -Idist/build/remote-iserv/remote-iserv-tmp -optP-include -optPdist/build/remote-iserv/autogen/cabal_macros.h -hide-all-packages -Wmissing-home-modules -clear-package-db -package-db /nix/store/vnw4a65kd8hms8bqnmsa61pb31g3a3q2-aarch64-unknown-linux-android-remote-iserv-exe-remote-iserv-8.10.7-config/lib/aarch64-unknown-linux-android-ghc-8.10.7/package.conf.d -package-db dist/package.conf.inplace -package-id base-4.14.3.0 -package-id libiserv-8.10.7-2MDjPwYrjU87yxghQqcqJs -XHaskell2010 src/Cli.hs -o dist/build/remote-iserv/remote-iserv -fPIC
Linking dist/build/remote-iserv/remote-iserv ...
```

9:15 We are mostly interested in the linking phase, as that is where we conceptually want to build a static binary, instead of this weird dynamic one. So we re-run that with verbosity added
```
/nix/store/f7ampv2wm7zndxggmkjpc9cp7bayxvdi-remote-iserv-exe-remote-iserv-8.10.7-aarch64-unknown-linux-android-ghc-8.10.7-env/bin/aarch64-unknown-linux-android-ghc --make -fbuilding-cabal-package -O -split-sections -static -outputdir dist/build/remote-iserv/remote-iserv-tmp -odir dist/build/remote-iserv/remote-iserv-tmp -hidir dist/build/remote-iserv/remote-iserv-tmp -stubdir dist/build/remote-iserv/remote-iserv-tmp -i -idist/build/remote-iserv/remote-iserv-tmp -isrc -idist/build/remote-iserv/autogen -idist/build/global-autogen -Idist/build/remote-iserv/autogen -Idist/build/global-autogen -Idist/build/remote-iserv/remote-iserv-tmp -optP-include -optPdist/build/remote-iserv/autogen/cabal_macros.h -hide-all-packages -Wmissing-home-modules -clear-package-db -package-db /nix/store/vnw4a65kd8hms8bqnmsa61pb31g3a3q2-aarch64-unknown-linux-android-remote-iserv-exe-remote-iserv-8.10.7-config/lib/aarch64-unknown-linux-android-ghc-8.10.7/package.conf.d -package-db dist/package.conf.inplace -package-id base-4.14.3.0 -package-id libiserv-8.10.7-2MDjPwYrjU87yxghQqcqJs -XHaskell2010 src/Cli.hs -o dist/build/remote-iserv/remote-iserv -fPIC -v
```

9:15 That will output a lot, but at the end we find this
```
*** Linker:
/nix/store/r0yvyhzsbya4by3rglf03s3ij4dipw2c-aarch64-unknown-linux-android-clang-wrapper-12.0.1/bin/aarch64-unknown-linux-android-cc -Wl,-z,noexecstack -Wl,--no-as-needed -o dist/build/remote-iserv/remote-iserv -no-pie -fPIC -U__PIC__ -D__PIC__ -Wl,--gc-sections dist/build/remote-iserv/remote-iserv-tmp/Main.o -L/nix/store/xcblnpbni05fx6c55bvdcd6sa2l1f27y-libiserv-lib-libiserv-aarch64-unknown-linux-android-8.10.7/lib/aarch64-android-ghc-8.10.7/libiserv-8.10.7-2MDjPwYrjU87yxghQqcqJs -L/nix/store/h2wsjmgg2p1g91dd63004b5437bc363n-network-lib-network-aarch64-unknown-linux-android-2.8.0.1/lib/aarch64-android-ghc-8.10.7/network-2.8.0.1-CNTBASghOn9I0zEulNtnZF -L/nix/store/dm9akz06799ipdqpg3msangkcs5yzhql-ghci-lib-ghci-aarch64-unknown-linux-android-8.10.7/lib/aarch64-android-ghc-8.10.7/ghci-8.10.7-CCbMD7xG6qa6GtFiyGV42R -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/transformers-0.5.6.2 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/template-haskell-2.16.0.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/pretty-1.1.3.6 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-heap-8.10.7 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-boot-8.10.7 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-boot-th-8.10.7 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/directory-1.3.6.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/unix-2.7.2.2 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/time-1.9.3 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/filepath-1.4.2.1 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/binary-0.8.8.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/containers-0.6.5.1 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/bytestring-0.10.12.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/deepseq-1.4.4.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/array-0.5.4.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/base-4.14.3.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/integer-gmp-1.0.3.0 -L/nix/store/pb2bzxmmv5mkmakxg6ywrbaxdpy3yhpl-gmp-6.2.1-aarch64-unknown-linux-android/lib -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/rts -L/nix/store/rkc9l0vx0i8dgwny1anwq8w0575sycp0-libffi-aarch64-unknown-linux-android-3.4.2/lib /run/user/1000/ghc2305410_0/ghc_2.o /run/user/1000/ghc2305410_0/ghc_5.o -Wl,-u,base_GHCziTopHandler_runIO_closure -Wl,-u,base_GHCziTopHandler_runNonIO_closure -Wl,-u,ghczmprim_GHCziTuple_Z0T_closure -Wl,-u,ghczmprim_GHCziTypes_True_closure -Wl,-u,ghczmprim_GHCziTypes_False_closure -Wl,-u,base_GHCziPack_unpackCString_closure -Wl,-u,base_GHCziWeak_runFinalizzerBatch_closure -Wl,-u,base_GHCziIOziException_stackOverflow_closure -Wl,-u,base_GHCziIOziException_heapOverflow_closure -Wl,-u,base_GHCziIOziException_allocationLimitExceeded_closure -Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnMVar_closure -Wl,-u,base_GHCziIOziException_blockedIndefinitelyOnSTM_closure -Wl,-u,base_GHCziIOziException_cannotCompactFunction_closure -Wl,-u,base_GHCziIOziException_cannotCompactPinned_closure -Wl,-u,base_GHCziIOziException_cannotCompactMutable_closure -Wl,-u,base_ControlziExceptionziBase_absentSumFieldError_closure -Wl,-u,base_ControlziExceptionziBase_nonTermination_closure -Wl,-u,base_ControlziExceptionziBase_nestedAtomically_closure -Wl,-u,base_GHCziEventziThread_blockedOnBadFD_closure -Wl,-u,base_GHCziConcziSync_runSparks_closure -Wl,-u,base_GHCziConcziIO_ensureIOManagerIsRunning_closure -Wl,-u,base_GHCziConcziIO_ioManagerCapabilitiesChanged_closure -Wl,-u,base_GHCziConcziSignal_runHandlersPtr_closure -Wl,-u,base_GHCziTopHandler_flushStdHandles_closure -Wl,-u,base_GHCziTopHandler_runMainIO_closure -Wl,-u,ghczmprim_GHCziTypes_Czh_con_info -Wl,-u,ghczmprim_GHCziTypes_Izh_con_info -Wl,-u,ghczmprim_GHCziTypes_Fzh_con_info -Wl,-u,ghczmprim_GHCziTypes_Dzh_con_info -Wl,-u,ghczmprim_GHCziTypes_Wzh_con_info -Wl,-u,base_GHCziPtr_Ptr_con_info -Wl,-u,base_GHCziPtr_FunPtr_con_info -Wl,-u,base_GHCziInt_I8zh_con_info -Wl,-u,base_GHCziInt_I16zh_con_info -Wl,-u,base_GHCziInt_I32zh_con_info -Wl,-u,base_GHCziInt_I64zh_con_info -Wl,-u,base_GHCziWord_W8zh_con_info -Wl,-u,base_GHCziWord_W16zh_con_info -Wl,-u,base_GHCziWord_W32zh_con_info -Wl,-u,base_GHCziWord_W64zh_con_info -Wl,-u,base_GHCziStable_StablePtr_con_info -Wl,-u,hs_atomic_add8 -Wl,-u,hs_atomic_add16 -Wl,-u,hs_atomic_add32 -Wl,-u,hs_atomic_add64 -Wl,-u,hs_atomic_sub8 -Wl,-u,hs_atomic_sub16 -Wl,-u,hs_atomic_sub32 -Wl,-u,hs_atomic_sub64 -Wl,-u,hs_atomic_and8 -Wl,-u,hs_atomic_and16 -Wl,-u,hs_atomic_and32 -Wl,-u,hs_atomic_and64 -Wl,-u,hs_atomic_nand8 -Wl,-u,hs_atomic_nand16 -Wl,-u,hs_atomic_nand32 -Wl,-u,hs_atomic_nand64 -Wl,-u,hs_atomic_or8 -Wl,-u,hs_atomic_or16 -Wl,-u,hs_atomic_or32 -Wl,-u,hs_atomic_or64 -Wl,-u,hs_atomic_xor8 -Wl,-u,hs_atomic_xor16 -Wl,-u,hs_atomic_xor32 -Wl,-u,hs_atomic_xor64 -Wl,-u,hs_cmpxchg8 -Wl,-u,hs_cmpxchg16 -Wl,-u,hs_cmpxchg32 -Wl,-u,hs_cmpxchg64 -Wl,-u,hs_atomicread8 -Wl,-u,hs_atomicread16 -Wl,-u,hs_atomicread32 -Wl,-u,hs_atomicread64 -Wl,-u,hs_atomicwrite8 -Wl,-u,hs_atomicwrite16 -Wl,-u,hs_atomicwrite32 -Wl,-u,hs_atomicwrite64 -lHSlibiserv-8.10.7-2MDjPwYrjU87yxghQqcqJs -lHSnetwork-2.8.0.1-CNTBASghOn9I0zEulNtnZF -lHSghci-8.10.7-CCbMD7xG6qa6GtFiyGV42R -lHStransformers-0.5.6.2 -lHStemplate-haskell-2.16.0.0 -lHSpretty-1.1.3.6 -lHSghc-heap-8.10.7 -lHSghc-boot-8.10.7 -lHSghc-boot-th-8.10.7 -lHSdirectory-1.3.6.0 -lHSunix-2.7.2.2 -lHStime-1.9.3 -lHSfilepath-1.4.2.1 -lHSbinary-0.8.8.0 -lHScontainers-0.6.5.1 -lHSbytestring-0.10.12.0 -lHSdeepseq-1.4.4.0 -lHSarray-0.5.4.0 -lHSbase-4.14.3.0 -lHSinteger-gmp-1.0.3.0 -lHSghc-prim-0.6.1 -lHSrts -ldl -liconv -lgmp -lm -ldl -lffi
```

9:17 we can’t trivally re-run that command, because it uses temporary files, and the linker will complain about not finding them:
```
clang-12: error: no such file or directory: '/run/user/1000/ghc2305410_0/ghc_2.o'
clang-12: error: no such file or directory: '/run/user/1000/ghc2305410_0/ghc_5.o'
```

9:18 We can re-run the ghc command from before with an additional flag -keep-tmp-files, that will leave us with the temporary files and skip the cleanup step. After that we can run the linking step in isolation.

9:19 That in turn will expand to:
```
 "/nix/store/r0yvyhzsbya4by3rglf03s3ij4dipw2c-aarch64-unknown-linux-android-clang-wrapper-12.0.1/bin/aarch64-unknown-linux-android-ld" -z noexecstack -EL --fix-cortex-a53-843419 --warn-shared-textrel -z now -z relro -z max-page-size=4096 --hash-style=both --enable-new-dtags --eh-frame-hdr -m aarch64linux -o dist/build/remote-iserv/remote-iserv /nix/store/3bgmf62qn1km52yqnrzmln8w0g9pbbd2-bionic-prebuilt-ndk-release-r23/lib/crtbegin_dynamic.o -L/nix/store/xcblnpbni05fx6c55bvdcd6sa2l1f27y-libiserv-lib-libiserv-aarch64-unknown-linux-android-8.10.7/lib/aarch64-android-ghc-8.10.7/libiserv-8.10.7-2MDjPwYrjU87yxghQqcqJs -L/nix/store/h2wsjmgg2p1g91dd63004b5437bc363n-network-lib-network-aarch64-unknown-linux-android-2.8.0.1/lib/aarch64-android-ghc-8.10.7/network-2.8.0.1-CNTBASghOn9I0zEulNtnZF -L/nix/store/dm9akz06799ipdqpg3msangkcs5yzhql-ghci-lib-ghci-aarch64-unknown-linux-android-8.10.7/lib/aarch64-android-ghc-8.10.7/ghci-8.10.7-CCbMD7xG6qa6GtFiyGV42R -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/transformers-0.5.6.2 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/template-haskell-2.16.0.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/pretty-1.1.3.6 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-heap-8.10.7 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-boot-8.10.7 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-boot-th-8.10.7 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/directory-1.3.6.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/unix-2.7.2.2 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/time-1.9.3 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/filepath-1.4.2.1 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/binary-0.8.8.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/containers-0.6.5.1 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/bytestring-0.10.12.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/deepseq-1.4.4.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/array-0.5.4.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/base-4.14.3.0 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/integer-gmp-1.0.3.0 -L/nix/store/pb2bzxmmv5mkmakxg6ywrbaxdpy3yhpl-gmp-6.2.1-aarch64-unknown-linux-android/lib -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1 -L/nix/store/hm1a083wradvx2lgimac1pspncsswr09-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/rts -L/nix/store/rkc9l0vx0i8dgwny1anwq8w0575sycp0-libffi-aarch64-unknown-linux-android-3.4.2/lib -L/nix/store/nprzjj56cwq9h06rd7gj4j5c2k47ifj0-libcxx-aarch64-unknown-linux-android-12.0.1/lib -L/nix/store/7bfqyckz3qc04l99zkkbhcd8ljj6n8yg-libcxxabi-aarch64-unknown-linux-android-12.0.1/lib -L/nix/store/13kjd1p990rj7i903772mjq9bkinrrm4-libunwind-aarch64-unknown-linux-android-12.0.1/lib -L/nix/store/67vs680y95xms5fr5qcxs6l03slavkmh-ncurses-6.2/lib -L/nix/store/rkc9l0vx0i8dgwny1anwq8w0575sycp0-libffi-aarch64-unknown-linux-android-3.4.2/lib -L/nix/store/pb2bzxmmv5mkmakxg6ywrbaxdpy3yhpl-gmp-6.2.1-aarch64-unknown-linux-android/lib -L/nix/store/1mb9bb3493jd0q0r3rxva9y8y1l3fc91-libiconv-aarch64-unknown-linux-android-1.16/lib -L/nix/store/3bgmf62qn1km52yqnrzmln8w0g9pbbd2-bionic-prebuilt-ndk-release-r23/lib -L/nix/store/7w3k58grhjwpg8gmpn829fp77k5c69d1-clang-12.0.1-lib/aarch64-unknown-linux-android/lib -dynamic-linker=/system/bin/linker64 -z noexecstack --no-as-needed --gc-sections dist/build/remote-iserv/remote-iserv-tmp/Main.o /run/user/1000/ghc2307948_0/ghc_2.o /run/user/1000/ghc2307948_0/ghc_5.o -u base_GHCziTopHandler_runIO_closure -u base_GHCziTopHandler_runNonIO_closure -u ghczmprim_GHCziTuple_Z0T_closure -u ghczmprim_GHCziTypes_True_closure -u ghczmprim_GHCziTypes_False_closure -u base_GHCziPack_unpackCString_closure -u base_GHCziWeak_runFinalizzerBatch_closure -u base_GHCziIOziException_stackOverflow_closure -u base_GHCziIOziException_heapOverflow_closure -u base_GHCziIOziException_allocationLimitExceeded_closure -u base_GHCziIOziException_blockedIndefinitelyOnMVar_closure -u base_GHCziIOziException_blockedIndefinitelyOnSTM_closure -u base_GHCziIOziException_cannotCompactFunction_closure -u base_GHCziIOziException_cannotCompactPinned_closure -u base_GHCziIOziException_cannotCompactMutable_closure -u base_ControlziExceptionziBase_absentSumFieldError_closure -u base_ControlziExceptionziBase_nonTermination_closure -u base_ControlziExceptionziBase_nestedAtomically_closure -u base_GHCziEventziThread_blockedOnBadFD_closure -u base_GHCziConcziSync_runSparks_closure -u base_GHCziConcziIO_ensureIOManagerIsRunning_closure -u base_GHCziConcziIO_ioManagerCapabilitiesChanged_closure -u base_GHCziConcziSignal_runHandlersPtr_closure -u base_GHCziTopHandler_flushStdHandles_closure -u base_GHCziTopHandler_runMainIO_closure -u ghczmprim_GHCziTypes_Czh_con_info -u ghczmprim_GHCziTypes_Izh_con_info -u ghczmprim_GHCziTypes_Fzh_con_info -u ghczmprim_GHCziTypes_Dzh_con_info -u ghczmprim_GHCziTypes_Wzh_con_info -u base_GHCziPtr_Ptr_con_info -u base_GHCziPtr_FunPtr_con_info -u base_GHCziInt_I8zh_con_info -u base_GHCziInt_I16zh_con_info -u base_GHCziInt_I32zh_con_info -u base_GHCziInt_I64zh_con_info -u base_GHCziWord_W8zh_con_info -u base_GHCziWord_W16zh_con_info -u base_GHCziWord_W32zh_con_info -u base_GHCziWord_W64zh_con_info -u base_GHCziStable_StablePtr_con_info -u hs_atomic_add8 -u hs_atomic_add16 -u hs_atomic_add32 -u hs_atomic_add64 -u hs_atomic_sub8 -u hs_atomic_sub16 -u hs_atomic_sub32 -u hs_atomic_sub64 -u hs_atomic_and8 -u hs_atomic_and16 -u hs_atomic_and32 -u hs_atomic_and64 -u hs_atomic_nand8 -u hs_atomic_nand16 -u hs_atomic_nand32 -u hs_atomic_nand64 -u hs_atomic_or8 -u hs_atomic_or16 -u hs_atomic_or32 -u hs_atomic_or64 -u hs_atomic_xor8 -u hs_atomic_xor16 -u hs_atomic_xor32 -u hs_atomic_xor64 -u hs_cmpxchg8 -u hs_cmpxchg16 -u hs_cmpxchg32 -u hs_cmpxchg64 -u hs_atomicread8 -u hs_atomicread16 -u hs_atomicread32 -u hs_atomicread64 -u hs_atomicwrite8 -u hs_atomicwrite16 -u hs_atomicwrite32 -u hs_atomicwrite64 -lHSlibiserv-8.10.7-2MDjPwYrjU87yxghQqcqJs -lHSnetwork-2.8.0.1-CNTBASghOn9I0zEulNtnZF -lHSghci-8.10.7-CCbMD7xG6qa6GtFiyGV42R -lHStransformers-0.5.6.2 -lHStemplate-haskell-2.16.0.0 -lHSpretty-1.1.3.6 -lHSghc-heap-8.10.7 -lHSghc-boot-8.10.7 -lHSghc-boot-th-8.10.7 -lHSdirectory-1.3.6.0 -lHSunix-2.7.2.2 -lHStime-1.9.3 -lHSfilepath-1.4.2.1 -lHSbinary-0.8.8.0 -lHScontainers-0.6.5.1 -lHSbytestring-0.10.12.0 -lHSdeepseq-1.4.4.0 -lHSarray-0.5.4.0 -lHSbase-4.14.3.0 -lHSinteger-gmp-1.0.3.0 -lHSghc-prim-0.6.1 -lHSrts -ldl -liconv -lgmp -lm -ldl -lffi -rpath /nix/store/rfj6bzfzq33fg2md8ycvq5120z2d5gn4-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/lib64 -rpath /nix/store/rfj6bzfzq33fg2md8ycvq5120z2d5gn4-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/lib -liconv -lunwind /nix/store/r0yvyhzsbya4by3rglf03s3ij4dipw2c-aarch64-unknown-linux-android-clang-wrapper-12.0.1/resource-root/lib/linux/libclang_rt.builtins-aarch64-android.a -lc /nix/store/r0yvyhzsbya4by3rglf03s3ij4dipw2c-aarch64-unknown-linux-android-clang-wrapper-12.0.1/resource-root/lib/linux/libclang_rt.builtins-aarch64-android.a /nix/store/3bgmf62qn1km52yqnrzmln8w0g9pbbd2-bionic-prebuilt-ndk-release-r23/lib/crtend_android.o
```
and we see the -lc being injected a the very end.
9:20 what might happen if we pass an extra -static to the linker (aarch64-unknown-linux-android-ld)?
9:21 It ends up complaining:
```
aarch64-unknown-linux-android-ld: error: unable to find library -ldl
aarch64-unknown-linux-android-ld: error: unable to find library -lgmp
aarch64-unknown-linux-android-ld: error: unable to find library -lm
aarch64-unknown-linux-android-ld: error: unable to find library -ldl
aarch64-unknown-linux-android-ld: error: unable to find library -lffi
aarch64-unknown-linux-android-ld: error: unable to find library -lc
clang-12: error: linker command failed with exit code 1 (use -v to see invocation)
```

9:22 so it can’t find dl, gmp, m, ffi and c. dl, m, and c are from android, ffi and gmp we provide. How did we fail to bring in the static built ones?

9:23 If we look at the corresponding gmp folder from the library search paths (-L), we see

9:23 ```shell
$ ls /nix/store/pb2bzxmmv5mkmakxg6ywrbaxdpy3yhpl-gmp-6.2.1-aarch64-unknown-linux-android/lib
libgmp.la  libgmp.so  libgmpxx.la  libgmpxx.so
```

9:23 no static libgmp.a to be found.

9:24 same for libffi
```shell
$ ls /nix/store/rkc9l0vx0i8dgwny1anwq8w0575sycp0-libffi-aarch64-unknown-linux-android-3.4.2/lib
libffi.la  libffi.so
```

9:25 if we look at the ndk folder, we see
```shell
$ ls /nix/store/3bgmf62qn1km52yqnrzmln8w0g9pbbd2-bionic-prebuilt-ndk-release-r23/lib
crtbegin_dynamic.o  crtbeginS.o    crtbegin_static.o  crtend_android.o  crtendS.o    libc.so   libgcc.a   libm.so
crtbegin.o          crtbegin_so.o  crtbeginT.o        crtend.o          crtend_so.o  libdl.so  liblog.so
```
so that’s where dl, m and c are. Alright.

9:25 So how can we force the -static from from for remote-iserv? We want to pass a flag specifically to the linker. Luckily GHC has an option for that: -optl  

9:31 PM Thus if we add an extra configuration flag as --ghc-option=-optl-static to configure, this should force it to be static.

9:39 PM The setup logic for remote-iserv  <-> iserv-proxy communication on linux (and android is technically linux here) is in haskell.nix/overlays/linux-cross.nix; which is called from haskell.nix/overlays/armv6l-linux.nix for hysterical raisins. And we can see remote-iserv is just a regular haskell executable component. As such I think we should be able to override it :slightly_smiling_face:

10:27 PM After overriding the setupBuildFlags in case of android we now see this fail as expected. I’ve also changed the android overlay in haskell.nix to always contain the static libraries for libffi and gmp. This should then leave us only with the failing library from the android sdk. (ld,m, and c), which we’ll try to fix up tomorrow.

10:43 PM Let’s see if this nixpgks patch will give us access to the ndk’s libc, libm, and libdl. The build will take a while, so maybe we can see some success on hydra tomorrow.

9:25 AM Alright, a new day with new challenges, let’s do this!

9:28 Looking at the built from we kicked off yesterday night we find that the mobile-core library for android is still failing. But it’s not failing to actually build/link the remote-iserv applciation anymore, it’s failing because there still seems to be the /system/bin/linker64 embedded, and the application still seems to appear partially dynamic.

9:28 We’ll again grab the derivation nix/store/85a4kkaz6adml3pimypxml8g8asxbz93-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv from the top, and debug this in a nix-shell.

9:33 Using ldd the last time didn’t give us much info, but nixpkgs has this nice patchelf tool, which has a --print-needed flag.

9:33 If we check this for the last remote-iserv  (the one we inspected yesterday), we see:
```shell
$ patchelf --print-needed /nix/store/rfj6bzfzq33fg2md8ycvq5120z2d5gn4-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv
libdl.so
libgmp.so
libm.so
libffi.so
libunwind.so.1
libc.so
```
which aligns with the missing libraries that -static also complained about.

9:34 If we do the same for the remote-iserv that we have in the new derivation we are currently looking at, we see
```shell
$ patchelf --print-needed /nix/store/va5w3cyfgdilf04m4ba3w49p4bhlfi0n-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv
patchelf: cannot find section '.dynamic'. The input file is most likely statically linked
```

9:36 patchelf also has the ability to not only print the loader/interpreter, but also change it. As /nix/store is write protected (so we can’t mess up our installed/built software), we just copy remote-iserv for experimentation purposes out of the store into a temporary location.
9:36
```shell
$ chmod +w remote-iserv
```
should make it writable.

9:39 Sadly we’ll learn that patchelf doesn’t allow us to remove the interpreter. We can set it to something else only.
9:41 if we can’t fix it, maybe we can set the approrpiate aarch64-linux loader for now. We should later try to figure out why we inject the loader into the executable anyway if it’s dynamic.
9:46 To obtain a aarch64-linux cross compiler, we run
```shell
$ nix-shell -p pkgsCross.aarch64-multiplatform.stdenv.cc
```
and then we can compile a trivial main.c file as follows
```shell
$ aarch64-unknown-linux-gnu-cc main.c -o main-gnu
```
that will give us
```shell
$ file main-gnu
main-gnu: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), dynamically linked, interpreter /nix/store/vr82r47gl514vsbxnmb282hhsxj54jd9-glibc-aarch64-unknown-linux-gnu-2.33-49/lib/ld-linux-aarch64.so.1, for GNU/Linux 2.6.32, with debug_info, not stripped
```

9:47 We can then change the interpreter in remote-iserv to match that of main-gnu as follows
```shell
$ patchelf --set-interpreter $(patchelf --print-interpreter main-gnu) remote-iserv
$ patchelf --print-interpreter remote-iserv
/nix/store/vr82r47gl514vsbxnmb282hhsxj54jd9-glibc-aarch64-unknown-linux-gnu-2.33-49/lib/ld-linux-aarch64.so.1
```

9:48 The iserv-wrapper was executing
```shell
$ /nix/store/hm1fz57xqzhnvrc8hy2il8n81l3nj12l-qemu-6.1.0/bin/qemu-aarch64 /nix/store/va5w3cyfgdilf04m4ba3w49p4bhlfi0n-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv tmp $PORT &
so, let’s see if we can run our patched remote-iserv via qemu-aarch64 to validate it’s only the interpreter.
```
9:51
```shell
$ /nix/store/hm1fz57xqzhnvrc8hy2il8n81l3nj12l-qemu-6.1.0/bin/qemu-aarch64 remote-iserv
Segmentation fault (core dumped)
```
:sad: I don’t think we can call this success. So patching the interpreter is not working, but we do know that we can compile static aarch64-linux-bionic executables and execute them just fine. Let’s dig a bit into how we built remote-iserv and try to have it built without the interpreter.

9:53 From the build dependencies of the failed mobile-core library built, we can find the derivation for remote-iserv: /nix/store/s08g7p1fsxkadfdahpik885camz9849z-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7.drv; and again we’ll drop into a nix-shell for it.

9:57 This is getting a bit odd. In our nix-shell, after running genericBuild, we have dist/build/remote-iserv/remote-iserv, which reports as follows:
```shell
$ file dist/build/remote-iserv/remote-iserv
dist/build/remote-iserv/remote-iserv: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), statically linked, with debug_info, not stripped
```
and even works in qemu...
```shell
/nix/store/hm1fz57xqzhnvrc8hy2il8n81l3nj12l-qemu-6.1.0/bin/qemu-aarch64 dist/build/remote-iserv/remote-iserv
usage: remote-iserv /path/to/storage PORT [-v]
```

what’s going on?

9:58 Looking at the build log we see
```
Linking dist/build/remote-iserv/remote-iserv ...
installing
Installing executable remote-iserv in /nix/store/va5w3cyfgdilf04m4ba3w49p4bhlfi0n-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin
Warning: The directory
/nix/store/va5w3cyfgdilf04m4ba3w49p4bhlfi0n-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin
is not in the system search path.
post-installation fixup
patching script interpreter paths in /nix/store/va5w3cyfgdilf04m4ba3w49p4bhlfi0n-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7
checking for references to /build/ in /nix/store/va5w3cyfgdilf04m4ba3w49p4bhlfi0n-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7...
patchelf: cannot find section '.dynamic'. The input file is most likely statically linked
```

9:58 Huh, what’s that postInstall fixup stuff?

10:06 AM we can ask the shell to show us
```shell
$ type fixupPhase
type fixupPhase
fixupPhase is a function
fixupPhase ()
{
    local output;
    for output in $outputs;
    do
        if [ -e "${!output}" ]; then
            chmod -R u+w "${!output}";
        fi;
    done;
    runHook preFixup;
    local output;
    for output in $outputs;
    do
        prefix="${!output}" runHook fixupOutput;
    done;
    declare -ra flatVars=(depsBuildBuildPropagated propagatedNativeBuildInputs depsBuildTargetPropagated depsHostHostPropagated propagatedBuildInputs depsTargetTargetPropagated);
    declare -ra flatFiles=("${propagatedBuildDepFiles[@]}" "${propagatedHostDepFiles[@]}" "${propagatedTargetDepFiles[@]}");
    local propagatedInputsIndex;
    for propagatedInputsIndex in "${!flatVars[@]}";
    do
        local propagatedInputsSlice="${flatVars[$propagatedInputsIndex]}[@]";
        local propagatedInputsFile="${flatFiles[$propagatedInputsIndex]}";
        [[ -n "${!propagatedInputsSlice}" ]] || continue;
        mkdir -p "${!outputDev}/nix-support";
        printWords ${!propagatedInputsSlice} > "${!outputDev}/nix-support/$propagatedInputsFile";
    done;
    if [ -n "${setupHook:-}" ]; then
        mkdir -p "${!outputDev}/nix-support";
        substituteAll "$setupHook" "${!outputDev}/nix-support/setup-hook";
    fi;
    if [ -n "${setupHooks:-}" ]; then
        mkdir -p "${!outputDev}/nix-support";
        local hook;
        for hook in $setupHooks;
        do
            local content;
            consumeEntire content < "$hook";
            substituteAllStream content "file '$hook'" >> "${!outputDev}/nix-support/setup-hook";
            unset -v content;
        done;
        unset -v hook;
    fi;
    if [ -n "${propagatedUserEnvPkgs:-}" ]; then
        mkdir -p "${!outputBin}/nix-support";
        printWords $propagatedUserEnvPkgs > "${!outputBin}/nix-support/propagated-user-env-packages";
    fi;
    runHook postFixup
}
```

10:13 AM :confused: so this doesn’t really tell me where the “patching script interpreter paths” comes from.
10:14 An alternative strategy is to look for the string in a nixpkgs clone:
```
pkgs/build-support/setup-hooks/patch-shebangs.sh
35:    echo "patching script interpreter paths in $@"
```

10:18 From the helpful documentation at the header of that file
```
# This setup hook causes the fixup phase to rewrite all script
# interpreter file names (`#!  /path') to paths found in $PATH.  E.g.,
# /bin/sh will be rewritten to /nix/store/<hash>-some-bash/bin/sh.
# /usr/bin/env gets special treatment so that ".../bin/env python" is
# rewritten to /nix/store/<hash>/bin/python.  Interpreters that are
# already in the store are left untouched.
# A script file must be marked as executable, otherwise it will not be
# considered.
```
we can deduce that this is likely not the issue we are looking at. We don’t care about #!/... interpreter lines, we actually care about the patchelf --set-interpreter call that must be happening somewhere.

10:21 If we search nixpkgs for linker64, we find
```
pkgs/build-support/bintools-wrapper/default.nix
74:    else if (targetPlatform.libc == "bionic" && targetPlatform.is64bit) then "/system/bin/linker64"
```

10:23 Which ends up in a variable called dynamicLinker, if we follow that, we find the following snippet
```
      if [ -n "''${dynamicLinker-}" ]; then
        echo $dynamicLinker > $out/nix-support/dynamic-linker

        ${if targetPlatform.isDarwin then ''
          printf "export LD_DYLD_PATH=%q\n" "$dynamicLinker" >> $out/nix-support/setup-hook
        '' else lib.optionalString (sharedLibraryLoader != null) ''
          if [ -e ${sharedLibraryLoader}/lib/32/ld-linux.so.2 ]; then
            echo ${sharedLibraryLoader}/lib/32/ld-linux.so.2 > $out/nix-support/dynamic-linker-m32
          fi
          touch $out/nix-support/ld-set-dynamic-linker
        ''}
      fi
```
so, we write the linker into a nix-support/dynamic-linker file, and create nix-support/ld-set-dynamic-linker file. Let’s see if that’s where this is coming from, who reads ld-set-dynamic-linker?

10:31 AM impressive, so it is set by nix in an attempt to always set the proper dynamic-linker, even though this flag should not be present if we link -static. The logic is in binutils, so this will be a bit tricker to fix.

10:52 AM After some grep hunting through the nixpgks code, it appears as if there is a new checkLinkType feature that tries to decide the needed link flags based on flags passed to cc. If it finds -static or -static-pie, it sets the the respective type value to those. But if not, it default to dynamic.

10:53 The issue now appears to be that ghc uses response files to pass to cc, and the checkLinkType function does not operate on response file, but on arguments.

10:53 Ok, short response-file detour.

10:54 If we pass argument to executables, we usually talke about executable --arg1 --arg2 x y z and so on. Now the space we have available for that string or arguments we can pass to an executable (this is what ends up in the argv and argc values) can be system dependent, and is not infinite. So how do we get around this if we need to pass a lot of arguments? The idea is to pass a file instead of the arguments with a special syntax. The syntax for response files is @/path/to/file, and the program will then read that file, and parse the arguments from that file. This of course means the program actually needs to support this response file feature and it’s up to the developer to support it. gcc and clang as well as most of the toolchains do support this, simply because passing library paths, library names, include paths, ... can become very long very fast, and thus they are running into maximum argument length limits fast.  

10:58 There is a tiny amusement here: while gcc does support response files, it internally passes arguments as non-response files to subprocesses (e.g. collect2), this has the brilliant effect of gcc support arbitrary long arguments via response files, but really being limited by the maximum argument size internally.  Thus one has to be very careful not to pass too many arguments (or work around their length) to gcc, that it would internally forward to collect2.  :facepalm:

10:59 So, how do we fix this? I guess, we have to fix this proper in pkgs/build-support/wrapper-common/utils.bash  in nixpgks :disappointed:

11:00 this is annoying as it means we’ll potentially have to rebuild the world, as this change is so fundamental that it might just change every package.

4:48 PM (... still waiting for CI to rebuild everything after making those changes to checkLinkType which are at the base of the toolchains and thus cause every package to need to be rebuilt ...)

9:16 PM It **did** finally [build](https://ci.zw3rk.com/build/427638/nixlog/1/tail) and all the basics look good
```shell
$ file /nix/store/4i9vj2yvnl095jb9l7mczvdnnz1ksmp2-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv
/nix/store/4i9vj2yvnl095jb9l7mczvdnnz1ksmp2-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), statically linked, with debug_info, not stripped

$ ldd /nix/store/4i9vj2yvnl095jb9l7mczvdnnz1ksmp2-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv
	not a dynamic executable

$ /nix/store/5ca9ws9hj9isc7x5iq542szbzdyvrnyg-qemu-6.1.0/bin/qemu-aarch64 /nix/store/4i9vj2yvnl095jb9l7mczvdnnz1ksmp2-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv
usage: remote-iserv /path/to/storage PORT [-v]
```
... but it segfaults
```shell
---> Starting remote-iserv on port 6721
---| remote-iserv should have started on 6721
Listening on port 6721
qemu: uncaught target signal 11 (Segmentation fault) - core dumped
iserv-proxy: <socket: 7>: hGetBufSome: resource vanished (Connection reset by peer)
/nix/store/44zjyq5i0l7ad54j89b720yb21f16n0y-iserv-wrapper/bin/iserv-wrapper: line 10:   186 Segmentation fault      (core dumped) /nix/store/5ca9ws9hj9isc7x5iq542szbzdyvrnyg-qemu-6.1.0/bin/qemu-aarch64 /nix/store/4i9vj2yvnl095jb9l7mczvdnnz1ksmp2-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv tmp $PORT

<no location info>: error: ghc: ghc-iserv terminated (1)
```
9:18 Thus, I guess we’ll have to debug `remote-iserv`.

9:32 PM As we know the failing `.drv`, we do the same dance again, and enter a `nix-shell` for it. If we run `genericBuild` we find ourselves being ejected from the nix shell. That’s not so cool, so we try a second time and this time run the steps by hand: `unpackPhase`, `patchPhase`. `configurePhase` doesn’t work for some reason, so we `echo "$configurePhase"` and run the command that starts with `$SETUP_HS`. Same for `$buildPhase`. We notice that it takes quite a bit of time before it crashes. Interesting. So how do we get more verbosity out of this?

9:33 `remote-iserv` has a `-v` flag, and so does `iserv-proxy`. But they are driven by `iserv-wrapper`, and that’s in the nix store, so we can’t modify it? Well, let’s just copy it to `$PWD` and make it writable. We can then pass `-pgmi $PWD/iserv-wrapper`.

9:36 This yields the following:
```shell
$ $SETUP_HS build lib:mobile-core -j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) --ghc-option=-fexternal-interpreter --ghc-option=-pgmi --ghc-option=$PWD/iserv-wrapper --ghc-option=-L/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib --ghc-option=-fPIC --gcc-option=-fPIC
Preprocessing library for mobile-core-0.1.0.0..
Building library for mobile-core-0.1.0.0..
[1 of 1] Compiling Lib              ( lib/Lib.hs, dist/build/Lib.o )
---> Starting remote-iserv on port 8827
---| remote-iserv should have started on 8827
[        remote-iserv] Opening socket
Listening on port 8827
[        remote-iserv] Starting serv
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: MallocStrings ["This will trigger Template Haskell. Hooray!"]
[        remote-iserv] writing pipe: [RemotePtr 12970367291930780032]
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: InitLinker
[        remote-iserv] writing pipe: ()
[        remote-iserv] reading pipe...
Need Path: tmp/nix/store/irqmifkzz75ai9x9sdg171760ngsd5cz-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: AddLibrarySearchPath "tmp/nix/store/irqmifkzz75ai9x9sdg171760ngsd5cz-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1"
[        remote-iserv] writing pipe: RemotePtr 0
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: LoadArchive "tmp/nix/store/irqmifkzz75ai9x9sdg171760ngsd5cz-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1/libHSghc-prim-0.6.1.a"
iserv-proxy: <socket: 6>: hGetBufSome: resource vanished (Connection reset by peer)
/run/user/1000/tmp.AxXerUa7eo/mobile-core-root-lib-mobile-core/iserv-wrapper: line 10: 600416 Segmentation fault      (core dumped) /nix/store/5ca9ws9hj9isc7x5iq542szbzdyvrnyg-qemu-6.1.0/bin/qemu-aarch64 /nix/store/4i9vj2yvnl095jb9l7mczvdnnz1ksmp2-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv tmp $PORT -v
```
my assumption now is that we might have some bug in ghc’s in-memory linker.

9:39 Detour: what is ghc’s in-memory linker?
As I’ve explained a while back, GHC needs to run a function to evaluate a template haskell splice. But if this function is part of a library (libHS...a), how do we run it? The archive only contains objects. And usually we’d throw them at a linker program (e.g. `ld`) to produce an executable or shared library. But we only have object files. Ok, so we could maybe turn this into a shared object library, and then ask the system linker to load it into our process with `dlopen`,... BUT, system linkers are finicky and buggy, and potentially unable to unload code... and not even available on some platforms. So GHC comes with it’s own linker for ELF and Mach-O object files for x86_64, and aarch64, and some (probably incomplete aarch32), and ppc?,...

9:41 This linker can parse object files (or object files contained in archives), and load them into memory. It then has to do a very important step: relocation. Relocation is in essence the wiring of different objects together. If object A references symbol S (could be a function on data) in object B, we need to have that reference in A point to where we loaded B into memory.

9:42 So our object files contain a relocation section, and once we have loaded all dependencies into memory we walk over the relocation table. The relocation table tells us where (at what offset) from the data or text section we mapped into memory of Object A, we need to embed the memory location to the symbol S in object B.

9:43 There are a few different relocation types, depending on the instruction at that offset that we patch up. E.g. it could be a LOAD instruction, or a JUMP instruction, ... and there might be some value encoded in the instruction already. We might want to add that to the symbol value, etc. Lots of possibilities. Of course relocations are object file dependent, ELF has other relocations that Mach-O, and they are also different per architecture.

9:45 Another fun observation is: aarch64 has at most a 4GB range of relative addressable memory (otherwise you’d need to go through an offset table or procedure linking table; but that’s a topic for another day); so if A and B end up too far apart in memory (and we all use address randomization, of course), we may have trouble linking them together.

9:46 anyway, so we have this piece of code in GHC that performs almost the same logic as the `ld` program when linking executable. But it’s lazy, becuase this is haskell :slightly_smiling_face:

9:46 And this is how GHC can with just a bunch of object files (and archives) load pretty much any function it compiled into memory and execute it from there.

9:46 And this piece of code is buggy (or at least that’s my current assumption) for aarch64-android.

10:13 PM guess, I’ll be doing a gdb session tomorrow.

9:13 AM Alright, so let’s do try this. First we’ll have to get back into our shell that broke.
```shell
# enter the nix shell for the broken derivation
$ nix-shell /nix/store/r73j18zy5wdsblgvjsfy306n66bzcwa9-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv
# change into a temporary directory, so we don't pollute what ever we started.
$ cd $(mktemp -d)
# create a temporary $out directory (this is where the build process might want to install build products)
$ mkdir tmp-out
$ export out=$PWD/tmp-out
# unpack the sources
$ unpackPhase
# change into the source (now unpacked) source directory
$ cd mobile-core-root-lib-mobile-core
# run the patchPhase, just in case we have patches (we don't this time, but in general we might)
$ patchPhase
# grab the configure line
$ echo "$configurePhase"
$ $SETUP_HS configure --prefix=$out lib:mobile-core $(cat /nix/store/g7l31ha678rdc4mn4fdhfz53ag7smp5z-aarch64-unknown-linux-android-mobile-core-lib-mobile-core-0.1.0.0-config/configure-flags) --with-ghc=aarch64-unknown-linux-android-ghc --with-ghc-pkg=aarch64-unknown-linux-android-ghc-pkg --with-hsc2hs=aarch64-unknown-linux-android-hsc2hs --with-gcc=aarch64-unknown-linux-android-cc --with-ld=aarch64-unknown-linux-android-ld --with-ar=aarch64-unknown-linux-android-ar --with-strip=aarch64-unknown-linux-android-strip --disable-executable-stripping --disable-library-stripping --disable-library-profiling --disable-executable-profiling --enable-static --disable-shared --disable-coverage --enable-library-for-ghci --enable-split-sections --hsc2hs-option=--cross-compile --ghc-option=-fPIC --gcc-option=-fPIC
# grab the build line
$ echo "$buildPhase"
$ $SETUP_HS build lib:mobile-core -j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) --ghc-option=-fexternal-interpreter --ghc-option=-pgmi --ghc-option=/nix/store/44zjyq5i0l7ad54j89b720yb21f16n0y-iserv-wrapper/bin/iserv-wrapper --ghc-option=-L/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib --ghc-option=-fPIC --gcc-option=-fPIC
```
alright, so this fails predictably.
Let’s patch up the iserv-wrapper, to be more verbose and have qemu listen for a gdb session.
```shell
$ cp /nix/store/44zjyq5i0l7ad54j89b720yb21f16n0y-iserv-wrapper/bin/iserv-wrapper .
$ chmod +w iserv-wrapper
```
and then change the qmeu line to
```shell
/nix/store/5ca9ws9hj9isc7x5iq542szbzdyvrnyg-qemu-6.1.0/bin/qemu-aarch64 -g 1234 /nix/store/4i9vj2yvnl095jb9l7mczvdnnz1ksmp2-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv tmp $PORT -v &
```
and then start the build command (again) but this time pointing at our customized iserv-wrapper.

9:16
```shell
$ $SETUP_HS build lib:mobile-core -j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) --ghc-option=-fexternal-interpreter --ghc-option=-pgmi --ghc-option=$PWD/iserv-wrapper --ghc-option=-L/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib --ghc-option=-fPIC --gcc-option=-fPIC
Now, we just need to start a gdb session (in another terminal).
$ nix-shell -p gdb
$ gdb /nix/store/4i9vj2yvnl095jb9l7mczvdnnz1ksmp2-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv
(gdb) target remote localhost:1234
(gdb) c
```
and we’ll see it (slowly) progressing in the oother terminal.

9:18 a little while late, our (full) gdb session looks like this:
```shell
GNU gdb (GDB) 10.2
Copyright (C) 2021 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
Type "show copying" and "show warranty" for details.
This GDB was configured as "x86_64-unknown-linux-gnu".
Type "show configuration" for configuration details.
For bug reporting instructions, please see:
<https://www.gnu.org/software/gdb/bugs/>.
Find the GDB manual and other documentation resources online at:
    <http://www.gnu.org/software/gdb/documentation/>.

For help, type "help".
Type "apropos word" to search for commands related to "word"...
Reading symbols from /nix/store/4i9vj2yvnl095jb9l7mczvdnnz1ksmp2-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv...
(gdb) target remote localhost:1234
Remote debugging using localhost:1234
0x0000000000305500 in _start ()
(gdb) c
Continuing.

Program received signal SIGSEGV, Segmentation fault.
ocGetNames_ELF (oc=oc@entry=0xb400005501e54500) at rts/linker/Elf.c:812
812	rts/linker/Elf.c: No such file or directory.
(gdb)
```

9:18 so, we did crash in the linker. Let’s try to locate the correct source line.

9:19 I think this would be the 8.10.7 source, we used to build iserv-remote with the same version (I think), but we can again ask nix for this.

9:22 The `remote-iserv` binary has to be a dependency of `/nix/store/r73j18zy5wdsblgvjsfy306n66bzcwa9-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv`, if we ask nix for the dependencies:
```shell
$ nix-store -q --tree /nix/store/r73j18zy5wdsblgvjsfy306n66bzcwa9-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv
```
we find
```
/nix/store/z7dqc2cph2p816h5g01g2zf46l55lmfr-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7.drv
```
from there we can look for the configured-src (haskell.nix splits building ghc, into (a) configuring the source, (b) using that to build ghc) and we find
```
/nix/store/477ygzwv7vbsmr3dlpr1qzcb6ab6a4wa-aarch64-unknown-linux-android-ghc-8.10.7-configured-src.drv
```
9:24 we can now use nix-build to get us that source:
```shell
$ nix-build /nix/store/477ygzwv7vbsmr3dlpr1qzcb6ab6a4wa-aarch64-unknown-linux-android-ghc-8.10.7-configured-src.drv
```
this will (by default, we could have passed `--out-link` for a different name) provide us with a symbolic link named `result`, that points to the build artifacts. The build didn’t take any time at all really, because we have that already built and nix just needed to create the result link to the path int he nix store.

9:24 Thus we now have the source from which ghc was built in ./result.

9:26 the relevant code from Elf.c around 812 looks like this:
```
    808           addSection(&sections[i], kind, alloc, start, size,
    809                      mapped_offset, mapped_start, mapped_size);
    810
    811 #if defined(NEED_PLT)
    812           oc->sections[i].info->nstubs = 0;
    813           oc->sections[i].info->stub_offset = (uint8_t*)mem + size;
    814           oc->sections[i].info->stub_size = stub_space;
    815           oc->sections[i].info->stubs = NULL;
    816 #else
    817           oc->sections[i].info->nstubs = 0;
    818           oc->sections[i].info->stub_offset = NULL;
    819           oc->sections[i].info->stub_size = 0;
    820           oc->sections[i].info->stubs = NULL;
    821 #endif
    822
```

9:28 if we add the result directory to `gdb` with
```
(gdb) directory /path/to/result/
we also get the source info in gdb
(gdb) list
807	#endif
808	          addSection(&sections[i], kind, alloc, start, size,
809	                     mapped_offset, mapped_start, mapped_size);
810
811	#if defined(NEED_PLT)
812	          oc->sections[i].info->nstubs = 0;
813	          oc->sections[i].info->stub_offset = (uint8_t*)mem + size;
814	          oc->sections[i].info->stub_size = stub_space;
815	          oc->sections[i].info->stubs = NULL;
816	#else
```

9:34 AM I’m not quite sure how this caused a segfault. `oc` seems valid, `oc->sections[i]`  looks valid. And info as well. :confused:  that i is 9598 feels rather brutal though. That’s a lot of sections.

9:44 AM we can ask gdb for he acual disassembly, so le’s see what that is.
```
(gdb) disassemble $pc-32,+64
Dump of assembler code from 0x639800 to 0x639840:
   0x0000000000639800 <ocGetNames_ELF+680>:	bl	0x61b444 <addSection>
   0x0000000000639804 <ocGetNames_ELF+684>:	ldr	x8, [x19, #88]
   0x0000000000639808 <ocGetNames_ELF+688>:	add	x9, x27, x25
   0x000000000063980c <ocGetNames_ELF+692>:	mov	x0, x19
   0x0000000000639810 <ocGetNames_ELF+696>:	mov	x1, x27
   0x0000000000639814 <ocGetNames_ELF+700>:	add	x8, x8, x23
   0x0000000000639818 <ocGetNames_ELF+704>:	ldr	x8, [x8, #48]
   0x000000000063981c <ocGetNames_ELF+708>:	mov	w2, w25
=> 0x0000000000639820 <ocGetNames_ELF+712>:	str	xzr, [x8, #16]
   0x0000000000639824 <ocGetNames_ELF+716>:	ldr	x8, [x19, #88]
   0x0000000000639828 <ocGetNames_ELF+720>:	add	x8, x8, x23
   0x000000000063982c <ocGetNames_ELF+724>:	ldr	x8, [x8, #48]
   0x0000000000639830 <ocGetNames_ELF+728>:	str	x9, [x8]
   0x0000000000639834 <ocGetNames_ELF+732>:	ldr	x8, [x19, #88]
   0x0000000000639838 <ocGetNames_ELF+736>:	add	x8, x8, x23
   0x000000000063983c <ocGetNames_ELF+740>:	ldr	x8, [x8, #48]
End of assembler dump.
```

9:45 so we are trying to store a zero word into x8+16.

9:46 so...
```
(gdb) p oc->sections[i].info
$22 = (struct SectionFormatInfo *) 0x209fbf
```
ok, the info struct is at `0x209fbf`, we’d expect his to be `$x8`.

9:46
```
(gdb) p/x $x8
$24 = 0x209fbf
```
seems to line up.

9:48 if we print info (and this is uninitialised memory)
```
(gdb) p *oc->sections[i].info
$21 = {stub_offset = 0x29656d616e6f6e28, stub_size = 8390045715513156864, nstubs = 7165064715131969824, stubs = 0x6f20656c62617461,
  name = 0x2e28207463656a62 <error: Cannot access memory at address 0x2e28207463656a62>, sectionHeader = 0x656c696620296f}
```
we see that nstubs is the third element. So an offset of 16 makes sense. (if se assume stub_offset, and stub_size  both hold some 8byte value (int64 like or ptr).

9:50 I’m still quite puzzled why we can’t write to that memory location though.

9:57 AM That’s odd..., so this is addSection which should allocate the info struct.
```
/* -----------------------------------------------------------------------------
 * Section management.
 */
void
addSection (Section *s, SectionKind kind, SectionAlloc alloc,
            void* start, StgWord size,
            StgWord mapped_offset, void* mapped_start, StgWord mapped_size)
{
   s->start        = start;     /* actual start of section in memory */
   s->size         = size;      /* actual size of section in memory */
   s->kind         = kind;
   s->alloc        = alloc;
   s->mapped_offset = mapped_offset; /* offset from the image of mapped_start */

   s->mapped_start = mapped_start; /* start of mmap() block */
   s->mapped_size  = mapped_size;  /* size of mmap() block */

   if (!s->info)
     s->info
       = (struct SectionFormatInfo*)stgCallocBytes(1, sizeof *s->info,
                                            "addSection(SectionFormatInfo)");

   IF_DEBUG(linker,
            debugBelch("addSection: %p-%p (size %" FMT_Word "), kind %d\n",
                       start, (void*)((StgWord)start + size),
                       size, kind ));
}
```
and
```
void *
stgCallocBytes (size_t count, size_t size, char *msg)
{
    void *space;

    if ((space = calloc(count, size)) == NULL) {
      /* don't fflush(stdout); WORKAROUND bug in Linux glibc */
      rtsConfig.mallocFailHook((W_) count*size, msg);
      stg_exit(EXIT_INTERNAL_ERROR);
    }
    return space;
}
```
so, we call `calloc` on it, that should return zeroed memory, why are we seeing non-zero memory, when the man page tells us this?

> The calloc() function allocates memory for an array of nmemb elements of size bytes each and returns a pointer to the allocated memory. The memory is set to zero. If nmemb or size is 0, then calloc() returns either NULL, or a unique pointer value that can later be successfully passed to free().

9:58 One thing that bionic is different about is it’s use of jemalloc by default.

9:59 [jemallocs manpage](http://jemalloc.net/jemalloc.3.html) reads the same, and yes it would have been quite strange if it behaved differently.
> The calloc() function allocates space for number objects, each size bytes in length. The result is identical to calling malloc() with an argument of number * size, with the exception that the allocated memory is explicitly initialized to zero bytes.

10:00 We do know this consistenly happens for section 9598. So maybe we can step through `addSection` for that.

10:03 Let’s see if this works:
```
(gdb) target remote localhost:1234
Remote debugging using localhost:1234

Program received signal SIGTRAP, Trace/breakpoint trap.
0x0000000000305500 in _start ()
(gdb) b rts/linker/Elf.c:808 if i == 9598
Breakpoint 1 at 0x6397e0: file rts/linker/Elf.c, line 808.
(gdb) c
```

10:05 GDB in principle has this really amazing reverse debugging feature, where you can ask it to record, and then basically step back and forward, but it is really slow.
10:06 So once we have some location and still aren’t sure what happened, we might be able to break a bit before the location that is problematic and then start recording just the section we are concerned about. One caveat is that I don’t know if this remote gdb + qemu supports this well.

10:30 AM With breakpoints the run takes even longer. But eventually we’ll be greeted with:
```
Breakpoint 1, ocGetNames_ELF (oc=oc@entry=0xb400005501e54500) at rts/linker/Elf.c:808
808	          addSection(&sections[i], kind, alloc, start, size,
(gdb)
```

10:31 if we look at the `sections[i]`, we’ll see the following:
```
(gdb) p sections[i]
$27 = {start = 0x0, size = 12970367291944688592, kind = 2138047, alloc = SECTION_NOMEM, mapped_offset = 0, mapped_start = 0x0, mapped_size = 12970367291944688616, info = 0x209fbf}
```
10:32 this is not good. From the above addSection function we see that we will only allocate space for info if it’s `NULL`. So we will never allocate space for info.
10:33 so, where did we allocate sections then?
10:35
```
    667
    668    sections = (Section*)stgCallocBytes(sizeof(Section), shnum,
    669                                        "ocGetNames_ELF(sections)");
    670    oc->sections = sections;
    671    oc->n_sections = shnum;
    672
from Elf.c
(gdb) p oc->n_sections
$32 = 24960 
```
ok. This is confusing.

11:49 AM :exploding_head:

11:55 AM Alright, I’ll just keep my notes here, hopefully writing this down will help me understand what’s off.
We set a break point right after the sections assignment. First hit
```
Breakpoint 1, ocGetNames_ELF (oc=oc@entry=0xb400005501e54400) at rts/linker/Elf.c:673
673     in rts/linker/Elf.c
(gdb) p *oc
$32 = {status = OBJECT_LOADED,
  fileName = 0xb400005501e650c0 "tmp/nix/store/irqmifkzz75ai9x9sdg171760ngsd5cz-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1/libHSghc-prim-0.6.1.a", fileSize = 20480,
  formatName = 0x20a340 "ELF",
  archiveMemberName = 0xb400005501e65180 "tmp/nix/store/irqmifkzz75ai9x9sdg171760ngsd5cz-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1/libHSghc-prim-0.6.1.a(CString.o)",
  symbols = 0x0, n_symbols = 0, image = 0x7fe14d3eb000 "\177ELF\002\001\001", info = 0xb400005501e33050,
  imageMapped = 1, misalignment = 0, n_sections = 71, sections = 0x0, n_segments = 0, segments = 0x0, next = 0x0,
  prev = 0x0, next_loaded_object = 0x0, mark = 255, dependencies = 0xb400005501e68000, proddables = 0x0,
  symbol_extras = 0x7fe14d3f0000, first_symbol_extra = 0, n_symbol_extras = 125,
  bssBegin = 0x7fe14d3f0000 "PrimopWrappers.o/\n/", bssEnd = 0x7fe14d3f0000 "PrimopWrappers.o/\n/",
  foreign_exports = 0x0, extraInfos = 0x0, rw_m32 = 0xb400005501e6f000, rx_m32 = 0xb400005501e6f140}
```
looking at the n_sections value, this is not the object code we are interested in.

11:56 after continuing, we have a second hit
```
(gdb) c
Continuing.

Breakpoint 1, ocGetNames_ELF (oc=oc@entry=0xb400005501e54500) at rts/linker/Elf.c:673
673     in rts/linker/Elf.c
(gdb) p *oc
$33 = {status = OBJECT_LOADED,
  fileName = 0xb400005501e65240 "tmp/nix/store/irqmifkzz75ai9x9sdg171760ngsd5cz-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1/libHSghc-prim-0.6.1.a", fileSize = 6148096,
  formatName = 0x20a340 "ELF",
  archiveMemberName = 0xb400005501e65300 "tmp/nix/store/irqmifkzz75ai9x9sdg171760ngsd5cz-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1/libHSghc-prim-0.6.1.a(Classes.o)",
  symbols = 0x0, n_symbols = 0, image = 0x7fe14cdeb000 "\177ELF\002\001\001", info = 0xb400005501e330a0,
  imageMapped = 1, misalignment = 0, n_sections = 24960, sections = 0x0, n_segments = 0, segments = 0x0, next = 0x0,
  prev = 0x0, next_loaded_object = 0x0, mark = 255, dependencies = 0xb400005501e6a800, proddables = 0x0,
  symbol_extras = 0x7fe14d3c8000, first_symbol_extra = 0, n_symbol_extras = 40428, bssBegin = 0x7fe14d3c8000 "",
  bssEnd = 0x7fe14d3c8000 "", foreign_exports = 0x0, extraInfos = 0x0, rw_m32 = 0xb400005501e6f280,
  rx_m32 = 0xb400005501e6f3c0}
```
this looks like the one we were interested in.

11:59 If we look at the assembly:
```
(gdb) disassemble $pc-64,+128
Dump of assembler code from 0x6395a0 to 0x639620:
   0x00000000006395a0 <ocGetNames_ELF+72>:      add     x10, x10, x8
   0x00000000006395a4 <ocGetNames_ELF+76>:      add     x10, x10, #0x18
   0x00000000006395a8 <ocGetNames_ELF+80>:      ldur    w11, [x10, #-20]
   0x00000000006395ac <ocGetNames_ELF+84>:      cmp     w11, #0x12
   0x00000000006395b0 <ocGetNames_ELF+88>:      b.eq    0x639c58 <ocGetNames_ELF+1792>  // b.none
   0x00000000006395b4 <ocGetNames_ELF+92>:      subs    x9, x9, #0x1
   0x00000000006395b8 <ocGetNames_ELF+96>:      add     x10, x10, #0x40
   0x00000000006395bc <ocGetNames_ELF+100>:     b.ne    0x6395a8 <ocGetNames_ELF+80>  // b.any
   0x00000000006395c0 <ocGetNames_ELF+104>:     str     xzr, [sp, #40]
   0x00000000006395c4 <ocGetNames_ELF+108>:     cbnz    w20, 0x6395cc <ocGetNames_ELF+116>
   0x00000000006395c8 <ocGetNames_ELF+112>:     ldr     w20, [x22, #32]
   0x00000000006395cc <ocGetNames_ELF+116>:     adrp    x2, 0x206000
   0x00000000006395d0 <ocGetNames_ELF+120>:     add     x2, x2, #0x9
   0x00000000006395d4 <ocGetNames_ELF+124>:     mov     w0, #0x38                       // #56
   0x00000000006395d8 <ocGetNames_ELF+128>:     mov     x1, x20
   0x00000000006395dc <ocGetNames_ELF+132>:     bl      0x620478 <stgCallocBytes>
=> 0x00000000006395e0 <ocGetNames_ELF+136>:     ldr     w8, [x19, #72]
   0x00000000006395e4 <ocGetNames_ELF+140>:     str     x0, [sp, #24]
   0x00000000006395e8 <ocGetNames_ELF+144>:     str     x0, [x19, #88]
   0x00000000006395ec <ocGetNames_ELF+148>:     str     w20, [x19, #80]
   0x00000000006395f0 <ocGetNames_ELF+152>:     cbz     w8, 0x639614 <ocGetNames_ELF+188>
   0x00000000006395f4 <ocGetNames_ELF+156>:     ldr     x0, [x19, #8]
   0x00000000006395f8 <ocGetNames_ELF+160>:     mov     w1, wzr
   0x00000000006395fc <ocGetNames_ELF+164>:     bl      0x683e10 <open(char const*, int, ...)>
   0x0000000000639600 <ocGetNames_ELF+168>:     cmn     w0, #0x1
   0x0000000000639604 <ocGetNames_ELF+172>:     b.eq    0x639c6c <ocGetNames_ELF+1812>  // b.none
   0x0000000000639608 <ocGetNames_ELF+176>:     str     w0, [sp, #4]
   0x000000000063960c <ocGetNames_ELF+180>:     cbnz    w20, 0x639620 <ocGetNames_ELF+200>
   0x0000000000639610 <ocGetNames_ELF+184>:     b       0x639c24 <ocGetNames_ELF+1740>
   0x0000000000639614 <ocGetNames_ELF+188>:     mov     w0, #0xffffffff                 // #-1
   0x0000000000639618 <ocGetNames_ELF+192>:     str     w0, [sp, #4]
   0x000000000063961c <ocGetNames_ELF+196>:     cbz     w20, 0x639c24 <ocGetNames_ELF+1740>
```
we jsut got back from calling `stgCallocBytes`. The first argument to that function was `sizeof(Section)` = `0x38` = `56`, the second argument we pulled from register `x20`. Thst should be `24960`. Sure that lines up
```
(gdb) p $x20
$36 = 24960
```
so we should have 56*24960 zero bytes at the returned address.

12:02
```
(gdb) p/x $x0
$38 = 0xb400005503154f40
(gdb) p sections
$39 = (Section *) 0xb400005503154f40
```
We can read more about the general [procedure call standard for aarch64](https://developer.arm.com/documentation/ihi0055/latest), and it states
> The first eight registers, r0-r7, are used to pass argument values into a subroutine and to return result values from a function. They may also be used to hold intermediate values within a routine (but, in general, only between subroutine calls).

hence we can expect to get the result from that call back in `$x0`.

12:06 And this is where I’m losing my sanity a little... printing the first 32words from the sections offset.
```
(gdb) x/32x sections
0xb400005503154f40:     0x02e4ac02      0xb4000055      0x00000000      0x00000000
0xb400005503154f50:     0x00000000      0x00000000      0x02af3530      0xb4000055
0xb400005503154f60:     0x02e67fea      0xb4000055      0x00000000      0x00000000
0xb400005503154f70:     0x00000000      0x00000000      0x02af3548      0xb4000055
0xb400005503154f80:     0x02e60978      0xb4000055      0x00000000      0x00000000
0xb400005503154f90:     0x00000000      0x00000000      0x02af3560      0xb4000055
0xb400005503154fa0:     0x02e59306      0xb4000055      0x00000000      0x00000000
0xb400005503154fb0:     0x00000000      0x00000000      0x02af3578      0xb4000055
```
What is this? It should be 56*24960 zeroed bytes. Instead this is some odd sequence of pointers. `0xb400005503154f40`, clearly looks like a memory address (64bit pointer).

12:07 Running this by hand... (having gdb call `stgCallocBytes`) ...
```
(gdb) p stgCallocBytes(56, 24960, "")
$40 = (void *) 0xb4000055032d5d80
(gdb) x/32x $40
0xb4000055032d5d80:     0x00000000      0x00000000      0x00000000      0x00000000
0xb4000055032d5d90:     0x00000000      0x00000000      0x00000000      0x00000000
0xb4000055032d5da0:     0x00000000      0x00000000      0x00000000      0x00000000
0xb4000055032d5db0:     0x00000000      0x00000000      0x00000000      0x00000000
0xb4000055032d5dc0:     0x00000000      0x00000000      0x00000000      0x00000000
0xb4000055032d5dd0:     0x00000000      0x00000000      0x00000000      0x00000000
0xb4000055032d5de0:     0x00000000      0x00000000      0x00000000      0x00000000
0xb4000055032d5df0:     0x00000000      0x00000000      0x00000000      0x00000000
```
w.t.f?

12:13 PM omg? [jemalloc/jemalloc#1844](https://giters.com/jemalloc/jemalloc/issues/1844), [qemu-devel/2020-05/msg03119](https://lists.nongnu.org/archive/html/qemu-devel/2020-05/msg03119.html); guess we’ll need some explicit zeroing.

12:16 This is actually pretty bad, now we need to figure out a way around this.

12:17 luckily use is fairly isolated in the rts
```shell
$ rg calloc rts
rts/RtsUtils.c
104:    if ((space = calloc(count, size)) == NULL) {

rts/Hpc.c
140:    tmpModule -> tixArr = (StgWord64 *)calloc(tmpModule->tickCount,sizeof(StgWord64));

rts/linker/Elf.c
851:      // Note calloc: if we fail partway through initializing symbols, we need

rts/linker/elf_plt.c
54:    Stub * s = calloc(1, sizeof(Stub));
```

12:18 My idea for fixing this is as follows: we change all calls to calloc to `stgCallocBytes`, and then try to device a good fix for adding an additional `memzero` to `stgCallocBytes`, which we ideally just run if we (detect) we are running under qemu, or know we are using jemalloc.

12:29 PM Let’s see if we can for now just throw this diff into our ghc for android
```diff
diff --git a/rts/Hpc.c b/rts/Hpc.c
index abf8543..fd4a153 100644
--- a/rts/Hpc.c
+++ b/rts/Hpc.c
@@ -137,7 +137,7 @@ readTix(void) {
     tmpModule -> hashNo = (unsigned int)expectWord64();
     ws();
     tmpModule -> tickCount = (int)expectWord64();
-    tmpModule -> tixArr = (StgWord64 *)calloc(tmpModule->tickCount,sizeof(StgWord64));
+    tmpModule -> tixArr = (StgWord64 *)stgCallocBytes(tmpModule->tickCount,sizeof(StgWord64), "Hpc.readTix");
     ws();
     expect('[');
     ws();
diff --git a/rts/RtsUtils.c b/rts/RtsUtils.c
index b9ddb2a..c7a4a5a 100644
--- a/rts/RtsUtils.c
+++ b/rts/RtsUtils.c
@@ -106,6 +106,11 @@ stgCallocBytes (size_t count, size_t size, char *msg)
       rtsConfig.mallocFailHook((W_) count*size, msg);
       stg_exit(EXIT_INTERNAL_ERROR);
     }
+    // If we run under qemu with jemalloc, calloc is not guaranteed
+    // to zero memory.
+    // - https://giters.com/jemalloc/jemalloc/issues/1844
+    // - https://lists.nongnu.org/archive/html/qemu-devel/2020-05/msg03119.html
+    memset(space, 0, count*size);
     return space;
 }
 
diff --git a/rts/linker/elf_plt.c b/rts/linker/elf_plt.c
index 9cd42ef..70817d8 100644
--- a/rts/linker/elf_plt.c
+++ b/rts/linker/elf_plt.c
@@ -1,4 +1,5 @@
 #include "Rts.h"
+#include "RtsUtils.h"
 #include "elf_plt.h"
 
 #include <stdbool.h>
@@ -51,7 +52,7 @@ makeStub(Section * section,
           void* * addr,
           uint8_t flags) {
 
-    Stub * s = calloc(1, sizeof(Stub));
+    Stub * s = stgCallocBytes(1, sizeof(Stub), "makeStub");
     ASSERT(s != NULL);
     s->target = *addr;
     s->flags  = flags;
```

12:39 PM Looks like figuring qemu out at runtime is hard, same for jemalloc. For now we’ll thus stick to just condtionally compile the android rts with an addtional memset. Performance penalty will be there, but it’s hopefully negligible.

12:40 As CI is building this now for us, it’s probably a good time to grab lunch.

2:05 PM so this did go a bit further, but now we have:
```
unpacking sources
unpacking source archive /nix/store/plbjz49p51izx7qnkfh1ha6yrjpzr704-mobile-core-root-lib-mobile-core
source root is mobile-core-root-lib-mobile-core
patching sources
updateAutotoolsGnuConfigScriptsPhase
configuring
Configure flags:
--prefix=/nix/store/q0v2i8nvw0lj0635zhzvah4dr6p5mwxm-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0 lib:mobile-core --package-db=clear --package-db=/nix/store/4fcj2sf4zv7w2x5cwzr4ps70nprkdxch-aarch64-unknown-linux-android-mobile-core-lib-mobile-core-0.1.0.0-config/lib/aarch64-unknown-linux-android-ghc-8.10.7/package.conf.d --exact-configuration --dependency=rts=rts --dependency=ghc-heap=ghc-heap-8.10.7 --dependency=ghc-prim=ghc-prim-0.6.1 --dependency=integer-gmp=integer-gmp-1.0.3.0 --dependency=base=base-4.14.3.0 --dependency=deepseq=deepseq-1.4.4.0 --dependency=array=array-0.5.4.0 --dependency=ghc-boot-th=ghc-boot-th-8.10.7 --dependency=pretty=pretty-1.1.3.6 --dependency=template-haskell=template-haskell-2.16.0.0 --dependency=ghc-boot=ghc-boot-8.10.7 --dependency=Cabal=Cabal-3.2.1.0 --dependency=array=array-0.5.4.0 --dependency=binary=binary-0.8.8.0 --dependency=bytestring=bytestring-0.10.12.0 --dependency=containers=containers-0.6.5.1 --dependency=directory=directory-1.3.6.0 --dependency=filepath=filepath-1.4.2.1 --dependency=ghc-boot=ghc-boot-8.10.7 --dependency=ghc-compact=ghc-compact-0.1.0.0 --dependency=ghc-prim=ghc-prim-0.6.1 --dependency=hpc=hpc-0.6.1.0 --dependency=mtl=mtl-2.2.2 --dependency=parsec=parsec-3.1.14.0 --dependency=process=process-1.6.13.2 --dependency=text=text-1.2.4.1 --dependency=time=time-1.9.3 --dependency=transformers=transformers-0.5.6.2 --dependency=unix=unix-2.7.2.2 --with-ghc=aarch64-unknown-linux-android-ghc --with-ghc-pkg=aarch64-unknown-linux-android-ghc-pkg --with-hsc2hs=aarch64-unknown-linux-android-hsc2hs --with-gcc=aarch64-unknown-linux-android-cc --with-ld=aarch64-unknown-linux-android-ld --with-ar=aarch64-unknown-linux-android-ar --with-strip=aarch64-unknown-linux-android-strip --disable-executable-stripping --disable-library-stripping --disable-library-profiling --disable-profiling --enable-static --disable-shared --disable-coverage --enable-library-for-ghci --enable-split-sections --hsc2hs-option=--cross-compile --ghc-option=-fPIC --gcc-option=-fPIC
Configuring library for mobile-core-0.1.0.0..
Warning: 'hs-source-dirs: app' directory does not exist.
Warning: 'hs-source-dirs: c-app' directory does not exist.
Warning: 'include-dirs: stubs' directory does not exist.
building
Preprocessing library for mobile-core-0.1.0.0..
Building library for mobile-core-0.1.0.0..
[1 of 1] Compiling Lib              ( lib/Lib.hs, dist/build/Lib.o )
---> Starting remote-iserv on port 7977
---| remote-iserv should have started on 7977
Listening on port 7977

<no location info>: error:
    <command line>: libdl.a is a stub --- use libdl.so instead
---> killing remote-iserve...
builder for '/nix/store/p0zfan4gbzjzxy3bwr76y76rlix45ixs-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv' failed with exit code 1
```
so libdl is a stub, fair enough. I guess the bigger question is if we really need it.

2:06 huh, so ... this is `libdl_static.cpp`
```
/*
 * Copyright (C) 2007 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include <dlfcn.h>
#include <link.h>
#include <stdlib.h>
void* dlopen(const char* /*filename*/, int /*flag*/) {
  return nullptr;
}
char* dlerror() {
  return const_cast<char*>("libdl.a is a stub --- use libdl.so instead");
}
void* dlsym(void* /*handle*/, const char* /*symbol*/) {
  return nullptr;
}
void* dlvsym(void* /*handle*/, const char* /*symbol*/, const char* /*version*/) {
  return nullptr;
}
int dladdr(const void* /*addr*/, Dl_info* /*info*/) {
  return 0;
}
int dlclose(void* /*handle*/) {
  return -1;
}
```
we are for some reason calling `dlerror`, ... but why?

2:23 PM The linker seems to be particularly taxing on aarch64-qemu. Loading objects takes impressively long. Something is special about this libc. even musl cross doesn’t take this long, and has similar contraints.

2:43 PM Ahh! so here’s the full log
```shell
$ $SETUP_HS build lib:mobile-core -j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) --ghc-option=-fexternal-interpreter --ghc-option=-pgmi --ghc-option=$PWD/iserv-wrapper --ghc-option=-L/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib --ghc-option=-fPIC --gcc-option=-fPIC
Preprocessing library for mobile-core-0.1.0.0..
Building library for mobile-core-0.1.0.0..
[1 of 1] Compiling Lib              ( lib/Lib.hs, dist/build/Lib.o )
---> Starting remote-iserv on port 7981
---| remote-iserv should have started on 7981
[        remote-iserv] Opening socket
Listening on port 7981
[        remote-iserv] Starting serv
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: MallocStrings ["This will trigger Template Haskell. Hooray!"]
[        remote-iserv] writing pipe: [RemotePtr 12970367291930780032]
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: InitLinker
[        remote-iserv] writing pipe: ()
[        remote-iserv] reading pipe...
Need Path: tmp/nix/store/cldmain5pj40f1yk0prgqniim16c7md1-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: AddLibrarySearchPath "tmp/nix/store/cldmain5pj40f1yk0prgqniim16c7md1-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1"
[        remote-iserv] writing pipe: RemotePtr 0
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: LoadArchive "tmp/nix/store/cldmain5pj40f1yk0prgqniim16c7md1-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1/libHSghc-prim-0.6.1.a"
[        remote-iserv] writing pipe: ()
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: ResolveObjs
[        remote-iserv] writing pipe: True
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: RemoveLibrarySearchPath (RemotePtr 0)
[        remote-iserv] writing pipe: False
[        remote-iserv] reading pipe...
Need Path: tmp/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: AddLibrarySearchPath "tmp/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib"
[        remote-iserv] writing pipe: RemotePtr 0
[        remote-iserv] reading pipe...
Need Path: tmp/nix/store/cldmain5pj40f1yk0prgqniim16c7md1-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/integer-gmp-1.0.3.0
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: AddLibrarySearchPath "tmp/nix/store/cldmain5pj40f1yk0prgqniim16c7md1-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/integer-gmp-1.0.3.0"
[        remote-iserv] writing pipe: RemotePtr 0
[        remote-iserv] reading pipe...
[        remote-iserv] Need DLL: tmp/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib/libgmp.so
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: LoadDLL "tmp/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib/libgmp.so"
[        remote-iserv] writing pipe: Just "libdl.a is a stub --- use libdl.so instead"
[        remote-iserv] reading pipe...

<no location info>: error:
    <command line>: libdl.a is a stub --- use libdl.so instead
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: Shutdown
[        remote-iserv] serv ended
[        remote-iserv] Opening socket
---> killing remote-iserve...
```
So we compiled `remote-iserv` into a static binary and as such have the statically linked libdl in there, which does not have support for loading dynamic libraries, which is perfectly fine. However GHC tries to be smart and sees we want to load libgmp, sees it can find `libgmp.so`, and prefers to load that.

2:44 We’ve got two ways to work around this:
- we could drop dynamic loading logic outright from the rts for android.
- not build the shared objects for libgmp, and other libraries, so GHC isn’t even tempted to load them.

2:45 While making changes, I’ll disable split-sections for android for now, hoping that will speed up iserv. We probably want to find a good solution for this though, as split sections is fairly essential to get good deadstripping.

2:54 PM Turns out, [I’ve implemented the first option](https://github.com/ghc/ghc/commit/6dae65484f9552239652f743e2303fa17aae953b) a while back already. And we have [a flag for this](https://github.com/input-output-hk/haskell.nix/blob/a57f4f77d811490ac1faf309295ed98d8e1ee6b9/compiler/ghc/default.nix#L122-L126) in haskell.nix.

4:01 Alright, so changing the split-sections has improved performance noticeably. It went down from 30+min to ~90s to failure.

4:02 We now see
```
unpacking sources
unpacking source archive /nix/store/plbjz49p51izx7qnkfh1ha6yrjpzr704-mobile-core-root-lib-mobile-core
source root is mobile-core-root-lib-mobile-core
patching sources
updateAutotoolsGnuConfigScriptsPhase
configuring
Configure flags:
--prefix=/nix/store/r4rfj2ai21lh6d1i383aks16a63vd5iq-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0 lib:mobile-core --package-db=clear --package-db=/nix/store/hsip6xq24w8zdlw5vxz5d6bh6yccpw17-aarch64-unknown-linux-android-mobile-core-lib-mobile-core-0.1.0.0-config/lib/aarch64-unknown-linux-android-ghc-8.10.7/package.conf.d --exact-configuration --dependency=rts=rts --dependency=ghc-heap=ghc-heap-8.10.7 --dependency=ghc-prim=ghc-prim-0.6.1 --dependency=integer-gmp=integer-gmp-1.0.3.0 --dependency=base=base-4.14.3.0 --dependency=deepseq=deepseq-1.4.4.0 --dependency=array=array-0.5.4.0 --dependency=ghc-boot-th=ghc-boot-th-8.10.7 --dependency=pretty=pretty-1.1.3.6 --dependency=template-haskell=template-haskell-2.16.0.0 --dependency=ghc-boot=ghc-boot-8.10.7 --dependency=Cabal=Cabal-3.2.1.0 --dependency=array=array-0.5.4.0 --dependency=binary=binary-0.8.8.0 --dependency=bytestring=bytestring-0.10.12.0 --dependency=containers=containers-0.6.5.1 --dependency=directory=directory-1.3.6.0 --dependency=filepath=filepath-1.4.2.1 --dependency=ghc-boot=ghc-boot-8.10.7 --dependency=ghc-compact=ghc-compact-0.1.0.0 --dependency=ghc-prim=ghc-prim-0.6.1 --dependency=hpc=hpc-0.6.1.0 --dependency=mtl=mtl-2.2.2 --dependency=parsec=parsec-3.1.14.0 --dependency=process=process-1.6.13.2 --dependency=text=text-1.2.4.1 --dependency=time=time-1.9.3 --dependency=transformers=transformers-0.5.6.2 --dependency=unix=unix-2.7.2.2 --with-ghc=aarch64-unknown-linux-android-ghc --with-ghc-pkg=aarch64-unknown-linux-android-ghc-pkg --with-hsc2hs=aarch64-unknown-linux-android-hsc2hs --with-gcc=aarch64-unknown-linux-android-cc --with-ld=aarch64-unknown-linux-android-ld --with-ar=aarch64-unknown-linux-android-ar --with-strip=aarch64-unknown-linux-android-strip --disable-executable-stripping --disable-library-stripping --disable-library-profiling --disable-profiling --enable-static --disable-shared --disable-coverage --enable-library-for-ghci --enable-split-sections --hsc2hs-option=--cross-compile --ghc-option=-fPIC --gcc-option=-fPIC
Configuring library for mobile-core-0.1.0.0..
Warning: 'hs-source-dirs: app' directory does not exist.
Warning: 'hs-source-dirs: c-app' directory does not exist.
Warning: 'include-dirs: stubs' directory does not exist.
building
Preprocessing library for mobile-core-0.1.0.0..
Building library for mobile-core-0.1.0.0..
[1 of 1] Compiling Lib              ( lib/Lib.hs, dist/build/Lib.o )
---> Starting remote-iserv on port 5255
---| remote-iserv should have started on 5255
Listening on port 5255
remote-iserv: Failed to lookup symbol: __stack_chk_fail
remote-iserv: Failed to lookup symbol: __gmpn_mod_1
remote-iserv: Failed to lookup symbol: base_GHCziNatural_minusNatural_closure
remote-iserv: Failed to lookup symbol: base_GHCziNum_fromInteger_info
remote-iserv: Failed to lookup symbol: base_GHCziList_any_info
remote-iserv: Failed to lookup symbol: base_DataziOldList_intercalatezuzdspolyzugo1_info
remote-iserv: Failed to lookup symbol: base_GHCziException_errorCallException_closure
remote-iserv: Failed to lookup symbol: base_GHCziErr_errorWithoutStackTrace_closure
remote-iserv: Failed to lookup symbol: base_DataziEither_Left_con_info
remote-iserv: Failed to lookup symbol: base_ControlziExceptionziBase_absentError_closure
remote-iserv: Failed to lookup symbol: base_GHCziBase_eqString_info
remote-iserv: Failed to lookup symbol: base_DataziTypeziCoercion_Coercion_con_info
remote-iserv: Failed to lookup symbol: base_ControlziCategory_CZCCategory_con_info
remote-iserv: Failed to lookup symbol: base_ControlziArrow_arr_info
remote-iserv: Failed to lookup symbol: base_ControlziApplicative_zdtcWrappedArrow1_closure
remote-iserv: ^^ Could not load 'base_DataziData_zdfDataChar_closure', dependency unresolved. See top entry above.

<no location info>: error:

ByteCodeLink.lookupCE
During interactive linking, GHCi couldn't find the following symbol:
  base_DataziData_zdfDataChar_closure
This may be due to you not asking GHCi to load extra object files,
archives or DLLs needed by your current session.  Restart GHCi, specifying
the missing library using the -L/path/to/object/dir and -lmissinglibname
flags, or simply by naming the relevant files on the GHCi command line.
Alternatively, this link failure might indicate a bug in GHCi.
If you suspect the latter, please report this as a GHC bug:
  https://www.haskell.org/ghc/reportabug

---> killing remote-iserve...
builder for '/nix/store/1w05hwycxn37arvf562x4q40imzzw473-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv' failed with exit code 1
```
As expected we are not loading libgmp from a static archive. And that one fails, due to the mising fortification. E.g. `_stack_chk_fail` is missing. 

4:03 The easiest will likely be to just disable fortification on the gmp build.

4:21 PM What exactly is happening here? If we again take the derivation and add the `-v` flag to `iserv-wrapper`, we get a more detailed picture:
```shell
$ $SETUP_HS build lib:mobile-core -j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) --ghc-option=-fexternal-interpreter --ghc-option=-pgmi --ghc-option=$PWD/iserv-wrapper --ghc-option=-L/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib --ghc-option=-fPIC --gcc-option=-fPIC
Preprocessing library for mobile-core-0.1.0.0..
Building library for mobile-core-0.1.0.0..
[1 of 1] Compiling Lib              ( lib/Lib.hs, dist/build/Lib.o )
---> Starting remote-iserv on port 9469
---| remote-iserv should have started on 9469
[        remote-iserv] Opening socket
Listening on port 9469
[        remote-iserv] Starting serv
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: MallocStrings ["This will trigger Template Haskell. Hooray!"]
[        remote-iserv] writing pipe: [RemotePtr 12970367291930780032]
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: InitLinker
[        remote-iserv] writing pipe: ()
[        remote-iserv] reading pipe...
Need Path: tmp/nix/store/1im00cavjaqzyyjjyxn2m0q077ivwsg4-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: AddLibrarySearchPath "tmp/nix/store/1im00cavjaqzyyjjyxn2m0q077ivwsg4-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1"
[        remote-iserv] writing pipe: RemotePtr 0
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: LoadArchive "tmp/nix/store/1im00cavjaqzyyjjyxn2m0q077ivwsg4-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/ghc-prim-0.6.1/libHSghc-prim-0.6.1.a"
[        remote-iserv] writing pipe: ()
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: ResolveObjs
[        remote-iserv] writing pipe: True
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: RemoveLibrarySearchPath (RemotePtr 0)
[        remote-iserv] writing pipe: False
[        remote-iserv] reading pipe...
Need Path: tmp/nix/store/1im00cavjaqzyyjjyxn2m0q077ivwsg4-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/integer-gmp-1.0.3.0
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: AddLibrarySearchPath "tmp/nix/store/1im00cavjaqzyyjjyxn2m0q077ivwsg4-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/integer-gmp-1.0.3.0"
[        remote-iserv] writing pipe: RemotePtr 0
[        remote-iserv] reading pipe...
Need Path: tmp/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: AddLibrarySearchPath "tmp/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib"
[        remote-iserv] writing pipe: RemotePtr 0
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: LoadArchive "tmp/nix/store/1im00cavjaqzyyjjyxn2m0q077ivwsg4-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/integer-gmp-1.0.3.0/libHSinteger-gmp-1.0.3.0.a"
[        remote-iserv] writing pipe: ()
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: LoadArchive "tmp/nix/store/nsx93cjkd11my4dikrqwhv6isxincdgx-gmp-6.2.1-aarch64-unknown-linux-android/lib/libgmp.a"
[        remote-iserv] writing pipe: ()
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: ResolveObjs
[        remote-iserv] writing pipe: True
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: RemoveLibrarySearchPath (RemotePtr 0)
[        remote-iserv] writing pipe: False
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: RemoveLibrarySearchPath (RemotePtr 0)
[        remote-iserv] writing pipe: False
[        remote-iserv] reading pipe...
Need Path: tmp/nix/store/1im00cavjaqzyyjjyxn2m0q077ivwsg4-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/base-4.14.3.0
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: AddLibrarySearchPath "tmp/nix/store/1im00cavjaqzyyjjyxn2m0q077ivwsg4-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/base-4.14.3.0"
[        remote-iserv] writing pipe: RemotePtr 0
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: LoadArchive "tmp/nix/store/1im00cavjaqzyyjjyxn2m0q077ivwsg4-aarch64-unknown-linux-android-ghc-8.10.7/lib/aarch64-unknown-linux-android-ghc-8.10.7/base-4.14.3.0/libHSbase-4.14.3.0.a"
[        remote-iserv] writing pipe: ()
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: ResolveObjs
[        remote-iserv] writing pipe: True
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: RemoveLibrarySearchPath (RemotePtr 0)
[        remote-iserv] writing pipe: False
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: ResolveObjs
[        remote-iserv] writing pipe: True
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: LookupSymbol "ghczmprim_GHCziCString_unpackCStringzh_closure"
[        remote-iserv] writing pipe: Just (RemotePtr 25010184)
[        remote-iserv] reading pipe...
[        remote-iserv] discardCtrlC
[        remote-iserv] msg: LookupSymbol "base_DataziData_zdfDataChar_closure"
remote-iserv: Failed to lookup symbol: __stack_chk_fail
remote-iserv: Failed to lookup symbol: __gmpn_mod_1
remote-iserv: Failed to lookup symbol: base_GHCziNatural_minusNatural_closure
remote-iserv: Failed to lookup symbol: base_GHCziNum_fromInteger_info
remote-iserv: Failed to lookup symbol: base_GHCziList_any_info
remote-iserv: Failed to lookup symbol: base_DataziOldList_intercalatezuzdspolyzugo1_info
remote-iserv: Failed to lookup symbol: base_GHCziException_errorCallException_closure
remote-iserv: Failed to lookup symbol: base_GHCziErr_errorWithoutStackTrace_closure
remote-iserv: Failed to lookup symbol: base_DataziEither_Left_con_info
remote-iserv: Failed to lookup symbol: base_ControlziExceptionziBase_absentError_closure
remote-iserv: Failed to lookup symbol: base_GHCziBase_eqString_info
remote-iserv: Failed to lookup symbol: base_DataziTypeziCoercion_Coercion_con_info
remote-iserv: Failed to lookup symbol: base_ControlziCategory_CZCCategory_con_info
remote-iserv: Failed to lookup symbol: base_ControlziArrow_arr_info
remote-iserv: Failed to lookup symbol: base_ControlziApplicative_zdtcWrappedArrow1_closure
remote-iserv: ^^ Could not load 'base_DataziData_zdfDataChar_closure', dependency unresolved. See top entry above.
[        remote-iserv] writing pipe: Nothing
[        remote-iserv] reading pipe...

<no location info>: error:

ByteCodeLink.lookupCE
During interactive linking, GHCi couldn't find the following symbol:
  base_DataziData_zdfDataChar_closure
This may be due to you not asking GHCi to load extra object files,
archives or DLLs needed by your current session.  Restart GHCi, specifying
the missing library using the -L/path/to/object/dir and -lmissinglibname
flags, or simply by naming the relevant files on the GHCi command line.
Alternatively, this link failure might indicate a bug in GHCi.
If you suspect the latter, please report this as a GHC bug:
  https://www.haskell.org/ghc/reportabug

[        remote-iserv] discardCtrlC
[        remote-iserv] msg: Shutdown
[        remote-iserv] serv ended
[        remote-iserv] Opening socket
---> killing remote-iserve...
```
from this we can see that we loaded `ghc-prim`, `integer-gmp`, `libgmp`. and then `base`. After that we try to get handles for `ghczmprim_GHCziCString_unpackCStringzh_closure`, which succeeds, next we try to get `base_DataziData_zdfDataChar_closure`, and we can see how that basically starts pulling a bunch of dependent symbols. This ultimately ends at `_gmpn_mod_1` which in turn needs `__stack_chk_fail`. And we simply don’t know where that symbol is. (It’s most likely in `libgcc` or a similar compiler builtins library).

4:23 Now a common theme in GHC (which is pretty fragile!) is to have the `rts` have a list of those symbols in a long list. During compilation the compiler will have to link that symbol (as the `rts` depends on it), and the `rts` now knows where that symbol is, and can pre-populate it’s symbol lookup cache with that set of symbols.

4:24 For the curious, this is the relevant file in the rts as of 8.10.7: [rts/RtsSymbols.c](https://github.com/ghc/ghc/blob/e28dba7ed6cd4a24f738ff009508e7823793c241/rts/RtsSymbols.c)

7:09 PM Now it's failing due to `abort()` something seems off. I think I'm a muppet. We've fixed this before. Why doesn't `prim` load `libc`

7:10 Maybe [libraries/ghc-prim/ghc-prim.cabal#L70-L73](https://github.com/ghc/ghc/blob/e28dba7ed6cd4a24f738ff009508e7823793c241/libraries/ghc-prim/ghc-prim.cabal#L70-L73) doesn't hold for android. Oh, I have a bad feeling about this.

9:36 PM After re-reading what I wrote. I need to make a correction: `__stack_chk_fail` is not fortify but stackprotector hardening.

10:35 PM Ohh my... and now another kind of crash, no segfault though...
```
unpacking sources
unpacking source archive /nix/store/plbjz49p51izx7qnkfh1ha6yrjpzr704-mobile-core-root-lib-mobile-core
source root is mobile-core-root-lib-mobile-core
patching sources
updateAutotoolsGnuConfigScriptsPhase
configuring
Configure flags:
--prefix=/nix/store/k2ivc7f9sdj3b20fx173qf5khq8w492q-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0 lib:mobile-core --package-db=clear --package-db=/nix/store/l57lh77dfxzlm8zlaxxvcl3p6xls453c-aarch64-unknown-linux-android-mobile-core-lib-mobile-core-0.1.0.0-config/lib/aarch64-unknown-linux-android-ghc-8.10.7/package.conf.d --exact-configuration --dependency=rts=rts --dependency=ghc-heap=ghc-heap-8.10.7 --dependency=ghc-prim=ghc-prim-0.6.1 --dependency=integer-gmp=integer-gmp-1.0.3.0 --dependency=base=base-4.14.3.0 --dependency=deepseq=deepseq-1.4.4.0 --dependency=array=array-0.5.4.0 --dependency=ghc-boot-th=ghc-boot-th-8.10.7 --dependency=pretty=pretty-1.1.3.6 --dependency=template-haskell=template-haskell-2.16.0.0 --dependency=ghc-boot=ghc-boot-8.10.7 --dependency=Cabal=Cabal-3.2.1.0 --dependency=array=array-0.5.4.0 --dependency=binary=binary-0.8.8.0 --dependency=bytestring=bytestring-0.10.12.0 --dependency=containers=containers-0.6.5.1 --dependency=directory=directory-1.3.6.0 --dependency=filepath=filepath-1.4.2.1 --dependency=ghc-boot=ghc-boot-8.10.7 --dependency=ghc-compact=ghc-compact-0.1.0.0 --dependency=ghc-prim=ghc-prim-0.6.1 --dependency=hpc=hpc-0.6.1.0 --dependency=mtl=mtl-2.2.2 --dependency=parsec=parsec-3.1.14.0 --dependency=process=process-1.6.13.2 --dependency=text=text-1.2.4.1 --dependency=time=time-1.9.3 --dependency=transformers=transformers-0.5.6.2 --dependency=unix=unix-2.7.2.2 --with-ghc=aarch64-unknown-linux-android-ghc --with-ghc-pkg=aarch64-unknown-linux-android-ghc-pkg --with-hsc2hs=aarch64-unknown-linux-android-hsc2hs --with-gcc=aarch64-unknown-linux-android-cc --with-ld=aarch64-unknown-linux-android-ld --with-ar=aarch64-unknown-linux-android-ar --with-strip=aarch64-unknown-linux-android-strip --disable-executable-stripping --disable-library-stripping --disable-library-profiling --disable-profiling --enable-static --disable-shared --disable-coverage --enable-library-for-ghci --enable-split-sections --hsc2hs-option=--cross-compile --ghc-option=-fPIC --gcc-option=-fPIC --ghc-options=-staticlib
Configuring library for mobile-core-0.1.0.0..
Warning: 'hs-source-dirs: app' directory does not exist.
Warning: 'hs-source-dirs: c-app' directory does not exist.
Warning: 'include-dirs: stubs' directory does not exist.
building
Preprocessing library for mobile-core-0.1.0.0..
Building library for mobile-core-0.1.0.0..
[1 of 1] Compiling Lib              ( lib/Lib.hs, dist/build/Lib.o )
---> Starting remote-iserv on port 8676
---| remote-iserv should have started on 8676
Listening on port 8676
qemu: uncaught target signal 6 (Aborted) - core dumped
iserv-proxy: {handle: <socket: 6>}: GHCi.Message.remoteCall: end of file
/nix/store/ijpjw26ypchx25bd09c5h52bybw70knj-iserv-wrapper/bin/iserv-wrapper: line 10:   170 Aborted                 (core dumped) /nix/store/5ca9ws9hj9isc7x5iq542szbzdyvrnyg-qemu-6.1.0/bin/qemu-aarch64 /nix/store/c2xag9rkbailg8n03xz8iqyfa0am95i4-remote-iserv-exe-remote-iserv-aarch64-unknown-linux-android-8.10.7/bin/remote-iserv tmp $PORT

<no location info>: error: ghc: ghc-iserv terminated (1)

builder for '/nix/store/gvbad9j5ci7mfwpac8lhinc1biaj9a7i-mobile-core-lib-mobile-core-aarch64-unknown-linux-android-0.1.0.0.drv' failed with exit code 1
```
10:39 ... looks like it happens somewhere during the symbol lookup. Guess we’ll be running a gdb session tomorrow again.

11:06 PM cool, we hit some relocation bug (remember we talked about relocations the other day?)
```
(gdb) bt
#0  abort () at bionic/libc/bionic/abort.cpp:50
#1  0x00000000012b3790 in relocateObjectCodeAarch64 (oc=oc@entry=0xb40000550428a500)
    at rts/linker/elf_reloc_aarch64.c:253
#2  0x00000000012b3028 in relocateObjectCode (oc=0x0, oc@entry=0xb40000550428a500) at rts/linker/elf_reloc.c:12
#3  0x00000000012b25e0 in ocResolve_ELF (oc=oc@entry=0xb40000550428a500) at rts/linker/Elf.c:1896
#4  0x0000000001291ab8 in ocTryLoad (oc=0xb40000550428a500) at rts/Linker.c:1744
#5  0x0000000001291964 in loadSymbol (lbl=0x3006417 "_exit", pinfo=0xb400005504f2af60) at rts/Linker.c:926
#6  lookupDependentSymbol (lbl=0x3006417 "_exit", dependent=dependent@entry=0xb400005504019900) at rts/Linker.c:906
#7  0x00000000012b2c58 in fillGot (oc=oc@entry=0xb400005504019900) at rts/linker/elf_got.c:94
#8  0x00000000012b25d4 in ocResolve_ELF (oc=oc@entry=0xb400005504019900) at rts/linker/Elf.c:1887
#9  0x0000000001291ab8 in ocTryLoad (oc=0xb400005504019900) at rts/Linker.c:1744
#10 0x0000000001291964 in loadSymbol (lbl=0x4b915bc "abort", pinfo=0xb40000550401c380) at rts/Linker.c:926
#11 lookupDependentSymbol (lbl=0x4b915bc "abort", dependent=dependent@entry=0xb400005505c12900) at rts/Linker.c:906
#12 0x00000000012b2c58 in fillGot (oc=oc@entry=0xb400005505c12900) at rts/linker/elf_got.c:94
#13 0x00000000012b25d4 in ocResolve_ELF (oc=oc@entry=0xb400005505c12900) at rts/linker/Elf.c:1887
#14 0x0000000001291ab8 in ocTryLoad (oc=0xb400005505c12900) at rts/Linker.c:1744
#15 0x0000000001291964 in loadSymbol (lbl=0x4bb7307 "__gmp_allocate_func", pinfo=0xb400005505d0cc00)
    at rts/Linker.c:926
#16 lookupDependentSymbol (lbl=0x4bb7307 "__gmp_allocate_func", dependent=dependent@entry=0xb400005505d53400)
    at rts/Linker.c:906
#17 0x00000000012b2c58 in fillGot (oc=oc@entry=0xb400005505d53400) at rts/linker/elf_got.c:94
#18 0x00000000012b25d4 in ocResolve_ELF (oc=oc@entry=0xb400005505d53400) at rts/linker/Elf.c:1887
#19 0x0000000001291ab8 in ocTryLoad (oc=0xb400005505d53400) at rts/Linker.c:1744
#20 0x0000000001291964 in loadSymbol (lbl=0x508d3cd "__gmp_tmp_reentrant_alloc", pinfo=0xb400005505d68040)
    at rts/Linker.c:926
I’m not so sure the line is fully correct
    224         case COMPAT_R_AARCH64_JUMP26:
    225         case COMPAT_R_AARCH64_CALL26: {
    226             // S+A-P
    227             int64_t V = S + A - P;
    228             /* note: we are encoding bits [27:2] */
    229             if(!isInt64(26+2, V)) {
    230                 // Note [PC bias aarch64]
    231                 // There is no PC bias to accommodate in the
    232                 // relocation of a place containing an instruction
    233                 // that formulates a PC-relative address. The program
    234                 // counter reflects the address of the currently
    235                 // executing instruction.
    236
    237                 /* need a stub */
    238                 /* check if we already have that stub */
    239                 if(findStub(section, (void**)&S, 0)) {
    240                     /* did not find it. Crete a new stub. */
    241                     if(makeStub(section, (void**)&S, 0)) {
    242                         abort(/* could not find or make stub */);
    243                     }
    244                 }
    245
    246                 assert(0 == (0xffff000000000000 & S));
    247                 V = S + A - P;
    248                 assert(isInt64(26+2, V)); /* X in range */
    249             }
    250             return V;
    251         }
    252         case COMPAT_R_AARCH64_LDST128_ABS_LO12_NC: assert(0 == ((S+A) & 0x0f));
    253         case COMPAT_R_AARCH64_LDST64_ABS_LO12_NC:  assert(0 == ((S+A) & 0x07));
    254         case COMPAT_R_AARCH64_LDST32_ABS_LO12_NC:  assert(0 == ((S+A) & 0x03));
    255         case COMPAT_R_AARCH64_LDST16_ABS_LO12_NC:  assert(0 == ((S+A) & 0x01));
    256         case COMPAT_R_AARCH64_LDST8_ABS_LO12_NC:
    257             /* type: static, class: aarch64, op: S + A */
    258             return (S + A) & 0xfff;
    259
```
11:07 It’s somewhat ironic that we run into abort while trying to relocate abort.

11:08 We have two ways around this:
we can simply implement abort as a known symbol in the rts. Which is probably the easier fix.
or we can fix the relocation, and figure out why our relocation logic is wrong. This is likely going to be more challenging.

11:08 I’ll kick of a build with the first solution over night. And then revise if I feel like going into relocations.

11:11 We’ll also need to figure out what why the macOS builds fail with a recent nixpkgs update; and upstream the linkType detection change to nixpkgs.


8:30 AM Alright, so adding abort still makes it fail. Let’s look at this relocation issue first and then look at the more recent failure. Not having to embed abort into ghc’s runtime system would be preferable to me. (In general I don’t link the RtsSymbols solution for it is tedious to work with).

8:31 In case anyone is wondering, this is how my debug session looks like. It’s on a vertical 27" display. The other vertical has Slack, a browser, emacs, ...
![Screenshot 2021-12-30 at 8.30.17 AM](assets/Screenshot%202021-12-30%20at%208.30.17%20AM.png) 

8:39 AM If we look at the assembly preceding our current crashing position, we find:
```
   0x00000000012b3744 <+880>:   mov     w1, #0xff                       // #255
   0x00000000012b3748 <+884>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b374c <+888>:   adrp    x0, 0x205000
   0x00000000012b3750 <+892>:   adrp    x2, 0x20f000
   0x00000000012b3754 <+896>:   adrp    x3, 0x206000
   0x00000000012b3758 <+900>:   add     x0, x0, #0x53f
   0x00000000012b375c <+904>:   add     x2, x2, #0x652
   0x00000000012b3760 <+908>:   add     x3, x3, #0x806
   0x00000000012b3764 <+912>:   mov     w1, #0xfe                       // #254
   0x00000000012b3768 <+916>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b376c <+920>:   adrp    x0, 0x205000
   0x00000000012b3770 <+924>:   adrp    x2, 0x20f000
   0x00000000012b3774 <+928>:   adrp    x3, 0x20a000
   0x00000000012b3778 <+932>:   add     x0, x0, #0x53f
   0x00000000012b377c <+936>:   add     x2, x2, #0x652
   0x00000000012b3780 <+940>:   add     x3, x3, #0xbcb
   0x00000000012b3784 <+944>:   mov     w1, #0xfd                       // #253
   0x00000000012b3788 <+948>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b378c <+952>:   bl      0x1301f7c <abort()>
=> 0x00000000012b3790 <+956>:   adrp    x0, 0x205000
```
Note, that we are really at the `abort()` call, not the next line, but the `$pc` will point to the next instruction to execute.

8:40 Now, where did we come from, this just a bunch of jump targets to call `__assert2` with the expected values. We’d have to have come from `<+920>`. maybe we can locate the the location from which we jumped here.

8:43 looking at the disassembled function we are currently in (`disassemble $pc`), we find the following segment
```
   0x00000000012b35d0 <+508>:   mov     w8, #0x8000000                  // #134217728
   0x00000000012b35d4 <+512>:   add     x8, x2, x8
   0x00000000012b35d8 <+516>:   lsr     x8, x8, #28
   0x00000000012b35dc <+520>:   cbz     x8, 0x12b34d4 <relocateObjectCodeAarch64+256>
   0x00000000012b35e0 <+524>:   b       0x12b37b0 <relocateObjectCodeAarch64+988>
   0x00000000012b35e4 <+528>:   add     x8, x24, x8
   0x00000000012b35e8 <+532>:   b       0x12b3648 <relocateObjectCodeAarch64+628>
   0x00000000012b35ec <+536>:   add     w8, w24, w8
   0x00000000012b35f0 <+540>:   and     x2, x8, #0xfff
   0x00000000012b35f4 <+544>:   b       0x12b34d4 <relocateObjectCodeAarch64+256>
   0x00000000012b35f8 <+548>:   add     x8, x24, x8
   0x00000000012b35fc <+552>:   b       0x12b3638 <relocateObjectCodeAarch64+612>
   0x00000000012b3600 <+556>:   add     x8, x24, x8
   0x00000000012b3604 <+560>:   b       0x12b3634 <relocateObjectCodeAarch64+608>
   0x00000000012b3608 <+564>:   add     x8, x24, x8
   0x00000000012b360c <+568>:   b       0x12b362c <relocateObjectCodeAarch64+600>
   0x00000000012b3610 <+572>:   add     x8, x24, x8
   0x00000000012b3614 <+576>:   b       0x12b3624 <relocateObjectCodeAarch64+592>
   0x00000000012b3618 <+580>:   add     x8, x24, x8
   0x00000000012b361c <+584>:   tst     x8, #0xf
   0x00000000012b3620 <+588>:   b.ne    0x12b37d0 <relocateObjectCodeAarch64+1020>  // b.any
   0x00000000012b3624 <+592>:   tst     x8, #0x7
   0x00000000012b3628 <+596>:   b.ne    0x12b376c <relocateObjectCodeAarch64+920>  // b.any
   0x00000000012b362c <+600>:   tst     x8, #0x3
   0x00000000012b3630 <+604>:   b.ne    0x12b374c <relocateObjectCodeAarch64+888>  // b.any
```
that looks quite a bit like our case statement (or a fragment there of). We also see the `0x7` value.

8:43 If we are lucky x8 still contains the `S+A` value. `S` is the address we want this location to point to, and `A` is the pre-filled addend from the instruction which we try to patch up.

8:50 AM The `tst` instruction will perform a bitwise and on `x8` and `0x7`, and store the boolean result for us to branch on. Thus `tst` + `b.ne` (branch not equal), checks if `x8 & 0x7`, and if that doesn’t hold, branches to `<+920>`. But $x8 is 240 (`0xf0`), that doesn’t look like what we were hoping for.

8:54 if we look at the assembly for `abort()`, we find that this is where we write `0xf0` into `x8`, so this is not helpful
```
   0x0000000001302018 <+156>:   mov     w8, #0xf0                       // #240
```

8:54 so back to breakpoints. Let’s see a breakpoint on abort, and run again.
8:56
I did look at the object code we were given:
```
(gdb) p *(ObjectCode*)0xb40000550428a500
$2 = {status = OBJECT_NEEDED,
  fileName = 0xb400005504d79aa0 "tmp/nix/store/g3n4xjhcnazz18zi0q57p1q904a3mia7-bionic-prebuilt-ndk-release-r23/lib/libc.a", fileSize = 45056, formatName = 0x20c28b "ELF",
  archiveMemberName = 0xb400005504e46420 "tmp/nix/store/g3n4xjhcnazz18zi0q57p1q904a3mia7-bionic-prebuilt-ndk-release-r23/lib/libc.a(syscalls-arm64.o)", symbols = 0xb4000055042a2000, n_symbols = 237,
  image = 0x3f11000 "\177ELF\002\001\001", info = 0xb400005504d0fae0, imageMapped = 1, misalignment = 0,
  n_sections = 16, sections = 0xb400005504771080, n_segments = 0, segments = 0x0, next = 0xb40000550428a400,
  prev = 0xb40000550428a600, next_loaded_object = 0xb40000550428a400, mark = 255, dependencies = 0xb40000550429a800,
  proddables = 0xb400005504f2a780, symbol_extras = 0x3f1c000, first_symbol_extra = 0, n_symbol_extras = 237,
  bssBegin = 0x3f1c000 "\250\033\200\322\001", bssEnd = 0x3f1c000 "\250\033\200\322\001", foreign_exports = 0x0,
  extraInfos = 0x0, rw_m32 = 0xb400005504f18840, rx_m32 = 0xb400005504f18980}
```
so we know this is some relocation entry from the `syscalls-arm64.o` from `libc`.

9:00 In a temporary directory, we can extract the archive
```shell
$ ar x /nix/store/g3n4xjhcnazz18zi0q57p1q904a3mia7-bionic-prebuilt-ndk-release-r23/lib/libc.a
```
and then run `readelf -r syscalls-arm64.o`; however, I don’t see LDST relocation entries in there :confused:

11:42 AM I’ve done a bit of assembly annotation for the function in question. It’s quite annoying that we basically merge a bunch of static functions into it. It makes it just harder to read... not because this is necessarily the most productive use of my time, but I’m on holidays and enjoy some assembly analysing from time to time.
```S
Dump of assembler code for function relocateObjectCodeAarch64:
   0x00000000012b33d4 <+0>:     sub     sp, sp, #0x70
   0x00000000012b33d8 <+4>:     stp     x29, x30, [sp, #16]
   0x00000000012b33dc <+8>:     stp     x28, x27, [sp, #32]
   0x00000000012b33e0 <+12>:    stp     x26, x25, [sp, #48]
   0x00000000012b33e4 <+16>:    stp     x24, x23, [sp, #64]
   0x00000000012b33e8 <+20>:    stp     x22, x21, [sp, #80]
   0x00000000012b33ec <+24>:    stp     x20, x19, [sp, #96]
   0x00000000012b33f0 <+28>:    ldr     x8, [x0, #64]                                            // x0 should be ObjectCode // x8 = x0->info, type: ObjectCodeFormatInfo
   0x00000000012b33f4 <+32>:    mov     x19, x0                                                  // x19 <- x0 // ObjectCode
   0x00000000012b33f8 <+36>:    ldr     x9, [x8, #40]                                            // x9 <- x0->info->relTable
   0x00000000012b33fc <+40>:    cbz     x9, 0x12b346c <relocateObjectCodeAarch64+152>            // x9 == NULL -> <+152>
   0x00000000012b3400 <+44>:    ldr     x10, [x19, #88]                                          // x10 <- x19->n_sections;
   0x00000000012b3404 <+48>:    mov     w11, #0x38                      // #56                   // x11 = 56
   0x00000000012b3408 <+52>:    b       0x12b3414 <relocateObjectCodeAarch64+64>                 // -> <+64>
   0x00000000012b340c <+56>:    ldr     x9, [x9, #32]                                            // x9 <- x9->next
   0x00000000012b3410 <+60>:    cbz     x9, 0x12b346c <relocateObjectCodeAarch64+152>            // x9 == NULL -> <+152>
   0x00000000012b3414 <+64>:    ldr     w12, [x9, #4]                                            // x12 <- x9->targetSectionIndex
   0x00000000012b3418 <+68>:    nop          
   0x00000000012b341c <+72>:    madd    x12, x12, x11, x10                                       // x12 <- x12 * x11 + x10
   0x00000000012b3420 <+76>:    ldr     w12, [x12, #16]                                          // load .kind for the section.
   0x00000000012b3424 <+80>:    cmp     w12, #0x4                                                // SECTIONKIND_OTHER
   0x00000000012b3428 <+84>:    b.eq    0x12b340c <relocateObjectCodeAarch64+56>  // b.none      // -> <+56> (continue statement)
   0x00000000012b342c <+88>:    ldr     x12, [x9, #24]                                           // x9->n_relocations
   0x00000000012b3430 <+92>:    cbz     x12, 0x12b340c <relocateObjectCodeAarch64+56>            // x12 == 0 -> <+56> -- we don't have any relocations.
   0x00000000012b3434 <+96>:    ldp     x8, x9, [x9, #8]                                         // (load pair) x8 <- x9->sectionHeader, x9 <- x9->relocations
   0x00000000012b3438 <+100>:   mov     x0, x19                                                  // x0 <- x19 (ObjectCode)
   0x00000000012b343c <+104>:   ldr     w1, [x8, #40]                                            // x1 <- x8(sectionHeader)->sh_link
   0x00000000012b3440 <+108>:   ldr     w2, [x9, #12]                                            // x2 <- x9(relocations)->r_info
   0x00000000012b3444 <+112>:   bl      0x12b385c <findSymbol>                           
   0x00000000012b3448 <+116>:   cbnz    x0, 0x12b378c <relocateObjectCodeAarch64+952>            // x0 != NULL -> <+952> (symbol found; but this will be abort(); beause we don't support decoding the addened.
   0x00000000012b344c <+120>:   adrp    x0, 0x205000                                             // 
   0x00000000012b3450 <+124>:   adrp    x2, 0x20a000
   0x00000000012b3454 <+128>:   adrp    x3, 0x204000
   0x00000000012b3458 <+132>:   add     x0, x0, #0x53f
   0x00000000012b345c <+136>:   add     x2, x2, #0x571
   0x00000000012b3460 <+140>:   add     x3, x3, #0xe90
   0x00000000012b3464 <+144>:   mov     w1, #0x132                      // #306
   0x00000000012b3468 <+148>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b346c <+152>:   ldr     x23, [x8, #48]                                           // so continue with RelA relocations; x8 is info, so x8->relaTable 
   0x00000000012b3470 <+156>:   cbz     x23, 0x12b3668 <relocateObjectCodeAarch64+660>           // x23 (ElfRelocationATable) != NULL
   0x00000000012b3474 <+160>:   adrp    x26, 0x236000                                            // load address relative
   0x00000000012b3478 <+164>:   mov     w25, #0x18                      // #24
   0x00000000012b347c <+168>:   add     x26, x26, #0x2a0                                         // add 0x2a0
   0x00000000012b3480 <+172>:   b       0x12b348c <relocateObjectCodeAarch64+184>
   0x00000000012b3484 <+176>:   ldr     x23, [x23, #32]                                          // x23 <- x23-> x23 (ElfRelocationATable)->next
   0x00000000012b3488 <+180>:   cbz     x23, 0x12b3668 <relocateObjectCodeAarch64+660>
   0x00000000012b348c <+184>:   ldr     x8, [x19, #88]                                           // oc->sections
   0x00000000012b3490 <+188>:   ldr     w9, [x23, #4]                                            // x23(ElfRelocationATable)->targetSectionIndex
   0x00000000012b3494 <+192>:   mov     w10, #0x38                      // #56
   0x00000000012b3498 <+196>:   madd    x20, x9, x10, x8                                         // x20 <- sectionIndex * (sizeof Section = 56) + oc->sections; -- this is the current section.
   0x00000000012b349c <+200>:   mov     x11, x20                                                 // x11 <- x20
   0x00000000012b34a0 <+204>:   ldr     w10, [x11, #16]!                                         // x10 <- kind; x11 <- x11+16
   0x00000000012b34a4 <+208>:   cmp     w10, #0x4                                                // is it SECTIONKIND_OTHER?
   0x00000000012b34a8 <+212>:   str     x11, [sp]                                                // store $sp in x11
   0x00000000012b34ac <+216>:   b.eq    0x12b3484 <relocateObjectCodeAarch64+176>  // b.none     // SECTIONKIND_OTHER -> <+176>
   0x00000000012b34b0 <+220>:   ldr     x10, [x23, #24]                                          // x10 <- x23(ElfRelocationATable)->n_relocations
   0x00000000012b34b4 <+224>:   cbz     x10, 0x12b3484 <relocateObjectCodeAarch64+176>           // 0 == n_relocations -> <+176>
   0x00000000012b34b8 <+228>:   mov     w10, #0x38                      // #56                   // store stride in x10
   0x00000000012b34bc <+232>:   madd    x8, x9, x10, x8                                          // x8 <- section (same we stored in x20); but now x8 doesn't point to `oc` anymore.
   0x00000000012b34c0 <+236>:   mov     x24, xzr                                                 // x24 <- 0
   0x00000000012b34c4 <+240>:   add     x28, x8, #0x8                                            // x28 <- x8(Section)->size
   0x00000000012b34c8 <+244>:   mov     w29, #0x1                       // #1                    // x29 <- 1
   0x00000000012b34cc <+248>:   b       0x12b34f4 <relocateObjectCodeAarch64+288>                -.
   0x00000000012b34d0 <+252>:   add     x2, x24, x8                                               |
   0x00000000012b34d4 <+256>:   mov     x0, x20                                                   |
   0x00000000012b34d8 <+260>:   mov     x1, x21                                                   |
   0x00000000012b34dc <+264>:   bl      0x12b30cc <encodeAddendAarch64>                           | 
   0x00000000012b34e0 <+268>:   ldr     x8, [x23, #24]                                            |
   0x00000000012b34e4 <+272>:   mov     w24, w29                                                  |
   0x00000000012b34e8 <+276>:   add     w29, w29, #0x1                                            |
   0x00000000012b34ec <+280>:   cmp     x8, x24                                                   |
   0x00000000012b34f0 <+284>:   b.ls    0x12b3484 <relocateObjectCodeAarch64+176>  // b.plast     |
   0x00000000012b34f4 <+288>:   ldp     x8, x22, [x23, #8]                                      <-' // load pair: x8 <- sectionHeader, x22 <- relocations
   0x00000000012b34f8 <+292>:   mov     x0, x19                                                  // x0 <- x19 (ObjectCode)
   0x00000000012b34fc <+296>:   madd    x21, x24, x25, x22                                       // x1 <- x24 * x25 + x22; We start with x24 = 0; and x25 is likely sizeof Elf_Rela
   0x00000000012b3500 <+300>:   ldr     w1, [x8, #40]                                            // sh_link
   0x00000000012b3504 <+304>:   ldr     w2, [x21, #12]                                           // rel->r_info
   0x00000000012b3508 <+308>:   bl      0x12b385c <findSymbol>
   0x00000000012b350c <+312>:   cbz     x0, 0x12b368c <relocateObjectCodeAarch64+696>      // symbol != NULL 
   0x00000000012b3510 <+316>:   ldr     x8, [x0, #8]
   0x00000000012b3514 <+320>:   cbz     x8, 0x12b36ac <relocateObjectCodeAarch64+728>      // symbol->addr != NULL
   0x00000000012b3518 <+324>:   ldr     x9, [x20]                                                // x9 <- P (section->start + rel->r_offset)
   0x00000000012b351c <+328>:   cbz     x9, 0x12b36cc <relocateObjectCodeAarch64+760>      // P != NULL
   0x00000000012b3520 <+332>:   ldr     x10, [x21]                                               // x10 <- rel->r_offset
   0x00000000012b3524 <+336>:   tbnz    x10, #63, 0x12b36ec <relocateObjectCodeAarch64+792> // section->start <= P
   0x00000000012b3528 <+340>:   ldr     x4, [x28]
   0x00000000012b352c <+344>:   add     x27, x9, x10
   0x00000000012b3530 <+348>:   add     x9, x4, x9
   0x00000000012b3534 <+352>:   cmp     x9, x27
   0x00000000012b3538 <+356>:   b.cc    0x12b370c <relocateObjectCodeAarch64+824>  // b.lo, b.ul, b.last // P <= section->start + section_size
   0x00000000012b353c <+360>:   madd    x10, x24, x25, x22                                       // x10 <- relocation x24 counter, x25 stride.
   0x00000000012b3540 <+364>:   add     x9, x21, #0x8                                            // x9 <- rel->r_info
   0x00000000012b3544 <+368>:   ldr     x24, [x10, #16]                                          // x24 <- 
   0x00000000012b3548 <+372>:   str     x8, [sp, #8]
   0x00000000012b354c <+376>:   ldr     w9, [x9]
   0x00000000012b3550 <+380>:   sub     w10, w9, #0x101                                          // R_AARCH64_ABS64 is 0x101, so the minimal value.
   0x00000000012b3554 <+384>:   cmp     w10, #0x37
   0x00000000012b3558 <+388>:   b.hi    0x12b378c <relocateObjectCodeAarch64+952>  // b.pmore    // > 0x101+0x37 ; we don't handle any relocations past R_AARCH64_LD64_GOT_LO12_NC
   0x00000000012b355c <+392>:   ldr     x9, [x0, #16]                                            // so now we can override x9
   0x00000000012b3560 <+396>:   adr     x11, 0x12b34d0 <relocateObjectCodeAarch64+252>           // 
   0x00000000012b3564 <+400>:   ldrb    w12, [x26, x10]                                          // use jumptable. ...2a0+(relocation-0x101) -> load byte x12
   0x00000000012b3568 <+404>:   add     x11, x11, x12, lsl #2                                    // x11 <- x11 (<+252>) + lookuptable << 2
   0x00000000012b356c <+408>:   br      x11                                                      // jump there.

(gdb) x/40b $x26
0x2362a0:       0x00    0x00    0x00    0x28    0x28    0x28    0xaf    0xaf
0x2362a8:       0xaf    0xaf    0xaf    0xaf    0xaf    0xaf    0xaf    0xaf
0x2362b0:       0xaf    0xaf    0x45    0xaf    0x47    0x4a    0xaf    0xaf
0x2362b8:       0xaf    0x2b    0x2b    0x4c    0x4e    0x50    0xaf    0xaf
0x2362c0:       0xaf    0xaf    0xaf    0xaf    0xaf    0xaf    0xaf    0xaf

   0x00000000012b3570 <+412>:   add     x8, x24, x8
   0x00000000012b3574 <+416>:   sub     x2, x8, x27
   0x00000000012b3578 <+420>:   b       0x12b34d4 <relocateObjectCodeAarch64+256>
   0x00000000012b357c <+424>:   add     x8, x24, x8
   0x00000000012b3580 <+428>:   sub     x2, x8, x27
   0x00000000012b3584 <+432>:   mov     w8, #0x8000000                  // #134217728
   0x00000000012b3588 <+436>:   add     x8, x2, x8
   0x00000000012b358c <+440>:   lsr     x8, x8, #28
   0x00000000012b3590 <+444>:   cbz     x8, 0x12b34d4 <relocateObjectCodeAarch64+256>
   0x00000000012b3594 <+448>:   add     x1, sp, #0x8
   0x00000000012b3598 <+452>:   mov     x0, x20
   0x00000000012b359c <+456>:   mov     w2, wzr
   0x00000000012b35a0 <+460>:   bl      0x12b2ec8 <findStub>
   0x00000000012b35a4 <+464>:   tbz     w0, #0, 0x12b35bc <relocateObjectCodeAarch64+488>
   0x00000000012b35a8 <+468>:   add     x1, sp, #0x8
   0x00000000012b35ac <+472>:   mov     x0, x20
   0x00000000012b35b0 <+476>:   mov     w2, wzr
   0x00000000012b35b4 <+480>:   bl      0x12b2f14 <makeStub>
   0x00000000012b35b8 <+484>:   tbnz    w0, #0, 0x12b378c <relocateObjectCodeAarch64+952>
   0x00000000012b35bc <+488>:   ldr     x8, [sp, #8]
   0x00000000012b35c0 <+492>:   lsr     x9, x8, #48
   0x00000000012b35c4 <+496>:   cbnz    x9, 0x12b3790 <relocateObjectCodeAarch64+956>
   0x00000000012b35c8 <+500>:   sub     x9, x24, x27
   0x00000000012b35cc <+504>:   add     x2, x8, x9
   0x00000000012b35d0 <+508>:   mov     w8, #0x8000000                  // #134217728
   0x00000000012b35d4 <+512>:   add     x8, x2, x8
   0x00000000012b35d8 <+516>:   lsr     x8, x8, #28
   0x00000000012b35dc <+520>:   cbz     x8, 0x12b34d4 <relocateObjectCodeAarch64+256>
   0x00000000012b35e0 <+524>:   b       0x12b37b0 <relocateObjectCodeAarch64+988>
   0x00000000012b35e4 <+528>:   add     x8, x24, x8
   0x00000000012b35e8 <+532>:   b       0x12b3648 <relocateObjectCodeAarch64+628>
   0x00000000012b35ec <+536>:   add     w8, w24, w8
   0x00000000012b35f0 <+540>:   and     x2, x8, #0xfff
   0x00000000012b35f4 <+544>:   b       0x12b34d4 <relocateObjectCodeAarch64+256>
   0x00000000012b35f8 <+548>:   add     x8, x24, x8
   0x00000000012b35fc <+552>:   b       0x12b3638 <relocateObjectCodeAarch64+612>
   0x00000000012b3600 <+556>:   add     x8, x24, x8
   0x00000000012b3604 <+560>:   b       0x12b3634 <relocateObjectCodeAarch64+608>
   0x00000000012b3608 <+564>:   add     x8, x24, x8
   0x00000000012b360c <+568>:   b       0x12b362c <relocateObjectCodeAarch64+600>             ---.
   0x00000000012b3610 <+572>:   add     x8, x24, x8                                              |
   0x00000000012b3614 <+576>:   b       0x12b3624 <relocateObjectCodeAarch64+592>             -. |
   0x00000000012b3618 <+580>:   add     x8, x24, x8                                            | |
   0x00000000012b361c <+584>:   tst     x8, #0xf                                               | |
   0x00000000012b3620 <+588>:   b.ne    0x12b37d0 <relocateObjectCodeAarch64+1020>  // b.any   | |
   0x00000000012b3624 <+592>:   tst     x8, #0x7                                             <-' |
-> 0x00000000012b3628 <+596>:   b.ne    0x12b376c <relocateObjectCodeAarch64+920>  // b.any      |
   0x00000000012b362c <+600>:   tst     x8, #0x3                                              <--'
   0x00000000012b3630 <+604>:   b.ne    0x12b374c <relocateObjectCodeAarch64+888>  // b.any
   0x00000000012b3634 <+608>:   tbnz    w8, #0, 0x12b372c <relocateObjectCodeAarch64+856>
   0x00000000012b3638 <+612>:   and     x2, x8, #0xfff
   0x00000000012b363c <+616>:   b       0x12b34d4 <relocateObjectCodeAarch64+256>
   0x00000000012b3640 <+620>:   cbz     x9, 0x12b37f0 <relocateObjectCodeAarch64+1052>
   0x00000000012b3644 <+624>:   add     x8, x24, x9
   0x00000000012b3648 <+628>:   and     x8, x8, #0xfffffffffffff000
   0x00000000012b364c <+632>:   and     x9, x27, #0xfffffffffffff000
   0x00000000012b3650 <+636>:   sub     x2, x8, x9
   0x00000000012b3654 <+640>:   b       0x12b34d4 <relocateObjectCodeAarch64+256>
   0x00000000012b3658 <+644>:   cbz     x9, 0x12b3814 <relocateObjectCodeAarch64+1088>
   0x00000000012b365c <+648>:   add     w8, w24, w9
   0x00000000012b3660 <+652>:   and     x2, x8, #0xfff
   0x00000000012b3664 <+656>:   b       0x12b34d4 <relocateObjectCodeAarch64+256>
   0x00000000012b3668 <+660>:   ldp     x20, x19, [sp, #96]
   0x00000000012b366c <+664>:   ldp     x22, x21, [sp, #80]
   0x00000000012b3670 <+668>:   ldp     x24, x23, [sp, #64]
   0x00000000012b3674 <+672>:   ldp     x26, x25, [sp, #48]
   0x00000000012b3678 <+676>:   ldp     x28, x27, [sp, #32]
   0x00000000012b367c <+680>:   ldp     x29, x30, [sp, #16]
   0x00000000012b3680 <+684>:   mov     w0, wzr
   0x00000000012b3684 <+688>:   add     sp, sp, #0x70
   0x00000000012b3688 <+692>:   ret
   0x00000000012b368c <+696>:   adrp    x0, 0x205000
   0x00000000012b3690 <+700>:   adrp    x2, 0x20a000
   0x00000000012b3694 <+704>:   adrp    x3, 0x204000
   0x00000000012b3698 <+708>:   add     x0, x0, #0x53f
   0x00000000012b369c <+712>:   add     x2, x2, #0x571
   0x00000000012b36a0 <+716>:   add     x3, x3, #0xe90
   0x00000000012b36a4 <+720>:   mov     w1, #0x14c                      // #332
   0x00000000012b36a8 <+724>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b36ac <+728>:   adrp    x0, 0x205000
   0x00000000012b36b0 <+732>:   adrp    x2, 0x20a000
   0x00000000012b36b4 <+736>:   adrp    x3, 0x205000
   0x00000000012b36b8 <+740>:   add     x0, x0, #0x53f
   0x00000000012b36bc <+744>:   add     x2, x2, #0x571
   0x00000000012b36c0 <+748>:   add     x3, x3, #0x580
   0x00000000012b36c4 <+752>:   mov     w1, #0x14d                      // #333
   0x00000000012b36c8 <+756>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b36cc <+760>:   adrp    x0, 0x205000
   0x00000000012b36d0 <+764>:   adrp    x2, 0x20f000
   0x00000000012b36d4 <+768>:   adrp    x3, 0x20f000
   0x00000000012b36d8 <+772>:   add     x0, x0, #0x53f
   0x00000000012b36dc <+776>:   add     x2, x2, #0x652
   0x00000000012b36e0 <+780>:   add     x3, x3, #0x6a4
   0x00000000012b36e4 <+784>:   mov     w1, #0xbf                       // #191
   0x00000000012b36e8 <+788>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b36ec <+792>:   adrp    x0, 0x205000
   0x00000000012b36f0 <+796>:   adrp    x2, 0x20f000
   0x00000000012b36f4 <+800>:   adrp    x3, 0x20e000
   0x00000000012b36f8 <+804>:   add     x0, x0, #0x53f
   0x00000000012b36fc <+808>:   add     x2, x2, #0x652
   0x00000000012b3700 <+812>:   add     x3, x3, #0x114
   0x00000000012b3704 <+816>:   mov     w1, #0xc0                       // #192
   0x00000000012b3708 <+820>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b370c <+824>:   adrp    x0, 0x205000
   0x00000000012b3710 <+828>:   adrp    x2, 0x20f000
   0x00000000012b3714 <+832>:   adrp    x3, 0x203000
   0x00000000012b3718 <+836>:   add     x0, x0, #0x53f
   0x00000000012b371c <+840>:   add     x2, x2, #0x652
   0x00000000012b3720 <+844>:   add     x3, x3, #0xcc4
   0x00000000012b3724 <+848>:   mov     w1, #0xc1                       // #193
   0x00000000012b3728 <+852>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b372c <+856>:   adrp    x0, 0x205000
   0x00000000012b3730 <+860>:   adrp    x2, 0x20f000
   0x00000000012b3734 <+864>:   adrp    x3, 0x203000
   0x00000000012b3738 <+868>:   add     x0, x0, #0x53f
   0x00000000012b373c <+872>:   add     x2, x2, #0x652
   0x00000000012b3740 <+876>:   add     x3, x3, #0xcf2
   0x00000000012b3744 <+880>:   mov     w1, #0xff                       // #255
   0x00000000012b3748 <+884>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b374c <+888>:   adrp    x0, 0x205000
   0x00000000012b3750 <+892>:   adrp    x2, 0x20f000
   0x00000000012b3754 <+896>:   adrp    x3, 0x206000
   0x00000000012b3758 <+900>:   add     x0, x0, #0x53f
   0x00000000012b375c <+904>:   add     x2, x2, #0x652
   0x00000000012b3760 <+908>:   add     x3, x3, #0x806
   0x00000000012b3764 <+912>:   mov     w1, #0xfe                       // #254
   0x00000000012b3768 <+916>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b376c <+920>:   adrp    x0, 0x205000
   0x00000000012b3770 <+924>:   adrp    x2, 0x20f000
   0x00000000012b3774 <+928>:   adrp    x3, 0x20a000
   0x00000000012b3778 <+932>:   add     x0, x0, #0x53f
   0x00000000012b377c <+936>:   add     x2, x2, #0x652
   0x00000000012b3780 <+940>:   add     x3, x3, #0xbcb
   0x00000000012b3784 <+944>:   mov     w1, #0xfd                       // #253
   0x00000000012b3788 <+948>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b378c <+952>:   bl      0x1301f7c <abort()>
=> 0x00000000012b3790 <+956>:   adrp    x0, 0x205000
   0x00000000012b3794 <+960>:   adrp    x2, 0x20f000
   0x00000000012b3798 <+964>:   adrp    x3, 0x206000
   0x00000000012b379c <+968>:   add     x0, x0, #0x53f
   0x00000000012b37a0 <+972>:   add     x2, x2, #0x652
   0x00000000012b37a4 <+976>:   add     x3, x3, #0x1f9
   0x00000000012b37a8 <+980>:   mov     w1, #0xf6                       // #246
   0x00000000012b37ac <+984>:   bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b37b0 <+988>:   adrp    x0, 0x205000
   0x00000000012b37b4 <+992>:   adrp    x2, 0x20f000
   0x00000000012b37b8 <+996>:   adrp    x3, 0x20d000
   0x00000000012b37bc <+1000>:  add     x0, x0, #0x53f
   0x00000000012b37c0 <+1004>:  add     x2, x2, #0x652
   0x00000000012b37c4 <+1008>:  add     x3, x3, #0x43d
   0x00000000012b37c8 <+1012>:  mov     w1, #0xf8                       // #248
   0x00000000012b37cc <+1016>:  bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b37d0 <+1020>:  adrp    x0, 0x205000
   0x00000000012b37d4 <+1024>:  adrp    x2, 0x20f000
   0x00000000012b37d8 <+1028>:  adrp    x3, 0x204000
   0x00000000012b37dc <+1032>:  add     x0, x0, #0x53f
   0x00000000012b37e0 <+1036>:  add     x2, x2, #0x652
   0x00000000012b37e4 <+1040>:  add     x3, x3, #0x899
   0x00000000012b37e8 <+1044>:  mov     w1, #0xfc                       // #252
   0x00000000012b37ec <+1048>:  bl      0x130211c <__assert2(char const*, int, char const*, char const*)>
   0x00000000012b37f0 <+1052>:  ldr     x2, [x19, #32]
   0x00000000012b37f4 <+1056>:  ldr     x1, [x0]
   0x00000000012b37f8 <+1060>:  cbnz    x2, 0x12b3800 <relocateObjectCodeAarch64+1068>
   0x00000000012b37fc <+1064>:  ldr     x2, [x19, #8]
   0x00000000012b3800 <+1068>:  ldr     x8, [sp]
   0x00000000012b3804 <+1072>:  adrp    x0, 0x204000
   0x00000000012b3808 <+1076>:  add     x0, x0, #0xe9e
   0x00000000012b380c <+1080>:  ldr     w3, [x8]
   0x00000000012b3810 <+1084>:  bl      0x1295834 <barf>
   0x00000000012b3814 <+1088>:  ldr     x2, [x19, #32]
   0x00000000012b3818 <+1092>:  ldr     x1, [x0]
   0x00000000012b381c <+1096>:  cbnz    x2, 0x12b3824 <relocateObjectCodeAarch64+1104>
   0x00000000012b3820 <+1100>:  ldr     x2, [x19, #8]
   0x00000000012b3824 <+1104>:  ldr     x8, [sp]
   0x00000000012b3828 <+1108>:  adrp    x0, 0x20a000
   0x00000000012b382c <+1112>:  add     x0, x0, #0xfc0
   0x00000000012b3830 <+1116>:  ldr     w3, [x8]
   0x00000000012b3834 <+1120>:  bl      0x1295834 <barf>
```

11:42 This gives us a bit more clues as to which registers might be of interest.

11:43 We find
```
(gdb) p/x *(*(ElfRelocationATable *)$x23)->sectionHeader
$44 = {sh_name = 0xeb, sh_type = 0x4, sh_flags = 0x0, sh_addr = 0x0, sh_offset = 0x5da8, sh_size = 0x1380,
  sh_link = 0xf, sh_info = 0x2, sh_addralign = 0x8, sh_entsize = 0x18}
```
and can correlate this with the header info we can extract with readelf
```shell
$ readelf -S syscalls-arm64.o
There are 16 section headers, starting at offset 0xa218:

Section Headers:
  [Nr] Name              Type             Address           Offset
       Size              EntSize          Flags  Link  Info  Align
  [ 0]                   NULL             0000000000000000  00000000
       0000000000000000  0000000000000000           0     0     0
  [ 1] .strtab           STRTAB           0000000000000000  000098d0
       0000000000000942  0000000000000000           0     0     1
  [ 2] .text             PROGBITS         0000000000000000  00000040
       00000000000019f8  0000000000000000  AX       0     0     16
  [ 3] .rela.text        RELA             0000000000000000  00005da8
       0000000000001380  0000000000000018          15     2     8
  [ 4] .note.GNU-stack   PROGBITS         0000000000000000  00001a38
       0000000000000000  0000000000000000           0     0     1
  [ 5] .note.gnu.pr[...] NOTE             0000000000000000  00001a38
       0000000000000020  0000000000000000   A       0     0     8
  [ 6] .eh_frame         PROGBITS         0000000000000000  00001a58
       0000000000001058  0000000000000000   A       0     0     8
  [ 7] .rela.eh_frame    RELA             0000000000000000  00007128
       0000000000001380  0000000000000018          15     6     8
  [ 8] .debug_info       PROGBITS         0000000000000000  00002ab0
       000000000000170e  0000000000000000           0     0     1
  [ 9] .rela.debug_info  RELA             0000000000000000  000084a8
       00000000000013e0  0000000000000018          15     8     8
  [10] .debug_abbrev     PROGBITS         0000000000000000  000041be
       000000000000001f  0000000000000000           0     0     1
  [11] .debug_aranges    PROGBITS         0000000000000000  000041dd
       0000000000000030  0000000000000000           0     0     1
  [12] .rela.debug_[...] RELA             0000000000000000  00009888
       0000000000000030  0000000000000018          15    11     8
  [13] .debug_line       PROGBITS         0000000000000000  0000420d
       000000000000055c  0000000000000000           0     0     1
  [14] .rela.debug_line  RELA             0000000000000000  000098b8
       0000000000000018  0000000000000018          15    13     8
  [15] .symtab           SYMTAB           0000000000000000  00004770
       0000000000001638  0000000000000018           1    12     8
Key to Flags:
  W (write), A (alloc), X (execute), M (merge), S (strings), I (info),
  L (link order), O (extra OS processing required), G (group), T (TLS),
  C (compressed), x (unknown), o (OS specific), E (exclude),
  p (processor specific)
```
looks like we are in .rela.eh_frame.

11:47 If we try to find out which relocations are used
```shell
$ readelf -r syscalls-arm64.o | awk  '{ print $3 }' |sort|uniq

R_AARCH64_ABS32
R_AARCH64_ABS64
R_AARCH64_CONDBR1
R_AARCH64_PREL32
'.rela.debug_aranges'
'.rela.debug_info'
'.rela.debug_line'
'.rela.eh_frame'
'.rela.text'
Type
```
and ignore the irrelevant bits, we see that we have `ABS32`, `ABS64`, `CONDBR1`, and `PREL32`. The relocation logic covers all cases except for `CONDBR1`.

11:51 Here I am being a muppet again. :man-facepalming: , we crash in a direct call to abort, basically in the default branch, not the assert. I got fooled by the assembly. And I’ve been banging my head against
```
(gdb) p/x $x10
$25 = 0x17
(gdb) p/x $x10+0x101
$27 = 0x118
```
for a while already. Wondering why we’d end up in that assert in the case statement, if the relocation is 0x118, which corresponds to the following in GHC
```
ELF_RELOC(R_AARCH64_CONDBR19,                        0x118)
```

11:51 Well, I guess mystery solved.

11:52 So we’ll need to implement `CONDBR19`.

11:54 [This SO question](https://stackoverflow.com/questions/38570495/aarch64-relocation-prefixes) has a handy table: 
```
Operator    | Relocation name | Operation | Inst | Immediate | Check
------------+-----------------+-----------+------+-----------+----------
[implicit]  | TSTBR14         | S + A - P | tbz  | X[15:2]   | |X|≤2^15
            |                 |           | tbnz |           |  
------------+-----------------+-----------+------+-----------+----------
[implicit]  | CONDBR19        | S + A - P | b.*  | X[20:2]   | |X|≤2^20
------------+-----------------+-----------+------+-----------+----------
[implicit]  | JUMP26          | S + A - P | b    | X[27:2]   | |X|≤2^27
------------+-----------------+-----------+------+-----------+----------
[implicit]  | CALL26          | S + A - P | bl   | X[27:2]   | |X|≤2^27
------------+-----------------+-----------+------+-----------+----------
```
So we should be able to implement it fairly similar to our `JUMP26` and `CALL26` relocations, we just need to make sure the range is checked.  

1:18 PM Alright, let’s do this. First we can construct the case fro `CONDBR19` similar to the `JUMP26` in computeAddend, by just ensuring we check the range of X. That is the offset we try to encode has to fit into 20bits.

1:19 We then just add `CONDBR19` to `needStubForRelAarch64`, and `needStubForRelaAarch64`. Why do we need this? We’ve use the same stub logic in `CONDBR19` in `computeAddend` as we did for `JUMP26`, but what are stubs?

1:21 When we relocate branch/jump/call targets, control flow is basically just going forward. But the instruction we encode the target in might not have enough bits to represent the full addressable range. To overcome this, we construct some executable code near to our object we want to relocate. Instead of relocating to the final target, we now relocate to the code near our object. And then encode a jump to the final address in that code.

1:22
```
     .---------.
0xa0 |         | <- a
     |         |
     '---------'

     .---------. 
0xe0 |         | <- b
0xe8 | a()     |
     '---------'
```
let’s assume we have to items in memory. One is the executable code for function `a` and one for function `b`, where `b` calls `a`. At compile time we don’t know where `a` will reside in memory, so we basically encode a `JUMP <a>`, which the compiler translates to (simplified)  `JUMP 0x0`  and an extra entry that tells us that we need to patch the instruction at `+0x08`, to point to the address where a resides.  

1:25 Now a might be too far away from the location where we want to patch up the instruction such that there is not enough space in the instruction to encode it.

1:30 In this case (for calls — not data lookups), we can do the following:
```
     .---------.
0xa0 |         | <- a
     |         |    ^
     '---------'    |
                    |
       .------------'
       |
0xd0 .-|-------.
0xd8 | o       |<-----.
     |---------|      |
0xe0 |         | <- b |
0xe8 | a()    -+------'
     '---------'
```
We encode a simple “load address” + “jump” instruction for a at 0xa0 at 0xd8, patch up the instruction at `0xe8` with `-0x10`,  If we now call `a`, the computer jumps to `0xd8`, where it finds instructions to load the address for a and then jump to that address. From the call site this looks indistinguishable.

1:30 So that are stub / jump islands.

1:31 So the only thing that’s left is to patch up the instruction. In `encodeAddendAarch64` we add an entry for `COMPAT_R_AARCH64_CONDBR19`.

1:34 The documentationf rom ARM for this AArch64 B.cond instruction is [here](https://developer.arm.com/documentation/ddi0596/2021-12/Base-Instructions/B-cond--Branch-conditionally-?lang=en)

1:37 The important part is to know the bit layout of the B.cond instruction to properly patch it up:
```
// 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
//  0  1  0  1  0  1  0  0 [ imm19 ...
//
// 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
// ...        imm19               ]  0  [  cond  ]
```

1:41 For comparison, this is the B ranch instruction:
```
// 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
//  0  0  0  1  0  1 [ imm26 ...
//
// 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
// ...        imm26                              ]
```
1:48 PM This should lead us to the following implementation:
```
        case COMPAT_R_AARCH64_CONDBR19: { /* relocate b.* ... */
            // 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16
            //  0  1  0  1  0  1  0  0 [ imm19 ...
            //
            // 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
            // ...        imm19               ]  0  [  cond  ]        
            assert(isInt64(19+2, addend));
            *(inst_t *)P = (*(inst_t *)P & 0xff00001f)
                         | ((uint32_t)(addend << (5-2)) & 0x00ffffe0)
            break;
        }  
```
we basically want to shift the addend by 5 to the left to leave the lower 5 bits untouched, but we also discard the lower two bits of the addened, as they are always 00.  The ARM page describes this as `bits(64) offset = SignExtend(imm19:'00', 64);`. Note the `:'00'` bit.
So we shift by 5-2 to the left, and then mask it to fit.

1:56 PM If we look through all the symbols in the `libc`, we find a few more relocations:
```
[ok] R_AARCH64_ABS32
[ok] R_AARCH64_ABS64
[ok] R_AARCH64_ADD_ABS_LO12_NC
[ok] R_AARCH64_ADR_GOT_PAGE
[ok] R_AARCH64_ADR_PREL_PG_HI21
[ok] R_AARCH64_CALL26
[**] R_AARCH64_CONDBR19
[ok] R_AARCH64_JUMP26
[ok] R_AARCH64_LD64_GOT_LO12_NC
[ok] R_AARCH64_LDST128_ABS_LO12_NC
[ok] R_AARCH64_LDST64_ABS_LO12_NC
[ok] R_AARCH64_LDST32_ABS_LO12_NC
[ok] R_AARCH64_LDST16_ABS_LO12_NC
[ok] R_AARCH64_LDST8_ABS_LO12_NC
[ok] R_AARCH64_PREL32
[XX] R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21
[XX] R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC
```
the TLSIE relocations seem to only be in address sanitizer objects.
```
guarded_pool_allocator.o
000000000000013c  000000560000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000140  000000560000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000000  000000560000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000004  000000560000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000084  000000560000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000088  000000560000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000018  000000560000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000020  000000560000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000028  000000560000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
000000000000002c  000000560000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000000  000000560000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000004  000000560000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000088  000000560000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
000000000000008c  000000560000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
guarded_pool_allocator_posix.o
000000000000001c  000000380000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000020  000000380000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
gwp_asan_wrappers.o
0000000000000010  000000620000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000014  000000620000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000010  000000620000021d R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
0000000000000014  000000620000021e R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC 0000000000000000 _ZZN8gwp_asan15getThreadLocalsEvE6Locals + 0
```
:crossed_fingers: that we won’t hit them.

1:59 let’s build a new set of ghcs with that patch.

2:32 PM While we wait for that build to complete, let’s look at the current eval failures: https://ci.zw3rk.com/eval/422#tabs-errors.
2:33 apparently we fail to build /nix/store/7ylqy380s52syc6vnfmlbywl861yrp5z-haskell-project-plan-to-nix-pkgs.drv, so let’s just try to build that path and see what happens.

2:42 PM after `nix-build`’ing that path, we find
```
...
builder for '/nix/store/gsmjcp4anyjvfyiwm2w96d97n21xnmmq-libredirect-0.drv' failed with exit code 134
```

2:42 alright, let’s see what that did specifically.
2:42 We are presented with this log:
```shell
$ nix-build /nix/store/gsmjcp4anyjvfyiwm2w96d97n21xnmmq-libredirect-0.drv
warning: unknown setting 'experimental-features'
warning: don't know how to open Nix store 'iohk.cachix.org'
these derivations will be built:
  /nix/store/gsmjcp4anyjvfyiwm2w96d97n21xnmmq-libredirect-0.drv
warning: unknown setting 'experimental-features'
building '/nix/store/gsmjcp4anyjvfyiwm2w96d97n21xnmmq-libredirect-0.drv' on 'ssh://builder@20.0.1.31'...
unpacking sources
patching sources
updateAutotoolsGnuConfigScriptsPhase
configuring
no configure script, doing nothing
building
libredirect.c:102:11: warning: incompatible function pointer types initializing 'int (*)(const char *, int, mode_t)' (aka 'int (*)(const char *, int, unsigned short)') with an expression of type 'int (*)(const char *, int, ...)' [-Wincompatible-function-pointer-types]
    int (*open_real) (const char *, int, mode_t) = LOOKUP_REAL(open);
          ^                                        ~~~~~~~~~~~~~~~~~
libredirect.c:107:27: warning: second argument to 'va_arg' is of promotable type 'mode_t' (aka 'unsigned short'); this va_arg has undefined behavior because arguments will be promoted to 'int' [-Wvarargs]
        mode = va_arg(ap, mode_t);
                          ^~~~~~
/nix/store/q0z24117022srlj2mxlr6npbnmgrnc4g-clang-wrapper-11.1.0/resource-root/include/stdarg.h:19:50: note: expanded from macro 'va_arg'
#define va_arg(ap, type)    __builtin_va_arg(ap, type)
                                                 ^~~~
libredirect.c:134:11: warning: incompatible function pointer types initializing 'int (*)(int, const char *, int, mode_t)' (aka 'int (*)(int, const char *, int, unsigned short)') with an expression of type 'int (*)(int, const char *, int, ...)' [-Wincompatible-function-pointer-types]
    int (*openat_real) (int, const char *, int, mode_t) = LOOKUP_REAL(openat);
          ^                                               ~~~~~~~~~~~~~~~~~~~
libredirect.c:139:27: warning: second argument to 'va_arg' is of promotable type 'mode_t' (aka 'unsigned short'); this va_arg has undefined behavior because arguments will be promoted to 'int' [-Wvarargs]
        mode = va_arg(ap, mode_t);
                          ^~~~~~
/nix/store/q0z24117022srlj2mxlr6npbnmgrnc4g-clang-wrapper-11.1.0/resource-root/include/stdarg.h:19:50: note: expanded from macro 'va_arg'
#define va_arg(ap, type)    __builtin_va_arg(ap, type)
                                                 ^~~~
4 warnings generated.
installing
install: creating directory '/nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0'
install: creating directory '/nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0/lib'
'libredirect.dylib' -> '/nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0/lib/libredirect.dylib'
post-installation fixup
patching script interpreter paths in /nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0
patching script interpreter paths in /nix/store/m2cwdlzrx89yj3isg887a3bqyg5a5ps9-libredirect-0-hook
running install tests
dyld: could not load inserted library '/nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0/lib/libredirect.dylib' because no suitable image found.  Did find:
        /nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0/lib/libredirect.dylib: mach-o, but wrong architecture
        /nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0/lib/libredirect.dylib: stat() failed with errno=1

Assertion failed: (system(TESTPATH) == 0), function test_system, file test.c, line 37.
/nix/store/sy85akb96r458i21dpk0jyzlyz2fhkk7-stdenv-darwin/setup: line 1355: 43568 Abort trap: 6           NIX_REDIRECTS="/foo/bar/test=/nix/store/5vdrla0lyph05a18c2i47rz5pr21z96a-coreutils-9.0/bin/true" ./test
builder for '/nix/store/gsmjcp4anyjvfyiwm2w96d97n21xnmmq-libredirect-0.drv' failed with exit code 134
error: build of '/nix/store/gsmjcp4anyjvfyiwm2w96d97n21xnmmq-libredirect-0.drv' on 'ssh://builder@20.0.1.31' failed: builder for '/nix/store/gsmjcp4anyjvfyiwm2w96d97n21xnmmq-libredirect-0.drv' failed with exit code 134
builder for '/nix/store/gsmjcp4anyjvfyiwm2w96d97n21xnmmq-libredirect-0.drv' failed with exit code 1
error: build of '/nix/store/gsmjcp4anyjvfyiwm2w96d97n21xnmmq-libredirect-0.drv' failed
```
2:42 What’s this?
```
dyld: could not load inserted library '/nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0/lib/libredirect.dylib' because no suitable image found.  Did find:
        /nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0/lib/libredirect.dylib: mach-o, but wrong architecture
        /nix/store/9bmni99v5ip81n9zv8m2223gc0a8w0dn-libredirect-0/lib/libredirect.dylib: stat() failed with errno=1
```

2:43 Again, nix-shell to the rescue.

2:45 (I should mention that I tried to find anything about this on github:nixos/nixpkgs first; the only somewhat related I could find is: https://github.com/NixOS/nixpkgs/issues/141811, but that’s not aarch64-darwin)  
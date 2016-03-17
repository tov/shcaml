(**
 

Objective Caml excels at "programming in the large," but for small or
write-once tasks, even the veteran functional programmer often prefers
a language that feels lighter. Throwaway scripts, however, often live
longer than expected, and what started as 14 lines of AWK may
metastasize into a 14-Kloc maintenance nightmare.

UNIX shells provide easy access to UNIX functionality such as pipes,
signals, file descriptor manipulation, and the file system. Shcaml
hopes to excel at these same tasks.

{3 Likely Modules}

Shcaml has a bunch of modules; these are the ones we think it's likely
you'll need.  All modules in the system are submodules of the {!Shcaml}
module.

{!modules:
  UsrBin 
  Adaptor
  Fitting
  Flags
  LineShtream
  Line
  Reader
  Channel
  Proc
}

{3 Getting Started}

Shcaml is available in opam. To install it, simply run:

> % opam install shcaml

Shcaml should now be installed.  Try the following:
> % ocaml
> # #use "topfind";;
> ...
> # #require "shcaml.top";;
>         Caml-Shcaml version 0.1.3 (Shmeer)
*)

(**
{[# let processes = LineShtream.string_list_of @@
    run_source (ps () -| cut Line.Ps.command);; ]}
*)
(**
> val processes : string list ... 

If all has gone well, you should have a list of all the process
invocations (whatever's in the "COMMAND" field when you call {i ps auxww})
currently running on your system.

{1 User Manual}

This manual is more tutorial style than straight ahead instruction
manual.  The API is (hopefully!) completely documented, so for
specific information on any particular bit of the library, check
there.  This document is here to demonstrate some of the concepts and
features of Shcaml.  

{2 Components}

Shcaml is composed of several major components that are the building
blocks of the library.  Let's start out by examining a few of them.  

Follow the instructions above in the "Getting Started" section to get
Shcaml installed and running.  We'll work in the toploop, with Shcaml
loaded.  So, run [ocaml], then:
*)

#use "topfind";;
(** 
> ...
*)
#require "shcaml.top";;
(**
> ...

{3 Lines}

{e For Shcaml versions <TODO> and greater, the implementation and interface of
{!Line} differs from what is described in the
{{:http://users.eecs.northwestern.edu/~jesse/pubs/caml-shcaml/} Shcaml paper}.
The [Line.t] having a phantom parameter with row polymorphism has been replaced
by a simpler heterogeneous map. This provides less static guarantees (which
fields are present or not is not statically known anymore), but improves the
maintenability of the library.}

A {!Line.t} represents structured data that might be found in a file
or in the output of a command.  A line might represent a record from
the passwd file, or the output of {i ps}.  Let's make one:
*)

let hello = Line.line "hello world, I'm a line!";;

(**
> val hello : Shcaml.Line.t = <line:"hello world, I'm a line!">
*)

(**
I know it looks like [hello] has our greeting in it, but at the moment
it doesn't contain any structured information. What gives?  Well, all
lines are constructed from a raw string, in this case ["hello world,
I'm a line!"].  But that doesn't actually tell us any useful
information about what kind of data is in that string.  Let's suppose
that [hello] were a line that came from a comma-delimited file.  Then
we would want to think of it as delimited input, rather than simply a
string.  Lines represent delimited input simply as a list of strings.
Let's turn our empty line into a more structured piece of data.  We'll
use [Pcre.asplit] to create to turn the string into an array.
*)

let hello_delim = 
  Line.Delim.create
    (Pcre.asplit ~pat:", " (Line.show hello)) 
    hello;;

(**
> val hello_delim : Shcaml.Line.t = <line:"hello world, I'm a line!">
*)

(** 
Let's now check and make sure you got what I promised you.  Try this:
*)

Line.Delim.fields hello_delim;;
(**
> - : string array = [|"hello world"; "I'm a line!"|]
*)

(**
We just added some structured information to the previously "empty"
line. Now consider, [hello] does not have a [delim] field.  What would
happen if we try to get the [Delim.fields] list from [hello]?
*)

Line.Delim.fields hello;;

(**
> Exception: Line.Field_not_found delim.
*)

(**
So we get an exception, because [hello] does not contain a [delim]
field; while we added one to [hello_delim] using {!Line.Delim.create}.

Now, suppose we wanted to uppercase the strings in the [Delim.fields]
list:
*)

let hello_DELIM = 
  Line.Delim.set_fields
    (Array.map String.uppercase (Line.Delim.fields hello_delim))
    hello_delim;;
(**
> val hello_DELIM : Shcaml.Line.t = <line:"hello world, I'm a line!">
*)

Line.Delim.fields hello_DELIM;;
(**
> - : string array = [|"Hello world"; "I'm a line!"|]
*)

(**
To wrap it up, we can define a function that does just that:
*)

let uppercase_delims ln =
      Line.Delim.set_fields
        (Array.map String.uppercase (Line.Delim.fields ln))
 	ln;;
(**
> val uppercase_delims : Shcaml.Line.t -> Shcaml.Line.t = <fun>
*)

(**
We've seen how lines can have generic delimited data attached.  Lines
can also have passwd data, data from {i ps}, data representing
key-value pairs, a record of its provenance ([source]), and several
others.  Functions for manipulating this data will often appear in
submodules of {!Line}, for instance, {!Line.Passwd}.  Let's try
another example, creating a line with data from the password file in
it.  (Don't worry, this is all built in, but we want to walk you through it.  
It builds character.)  We'll start by making a delimited list of the fields:
*)

let root = Line.line "root:x:0:0:Enoch Root:/root:/bin/shcaml";;
(**
> val root : Shcaml.Line.t =
>   <line:"root:x:0:0:Enoch Root:/root:/bin/shcaml">
*)

let root_delim = Line.Delim.create
  (Pcre.asplit ~pat:":" (Line.show root)) root;;
(**
> val root_delim : Shcaml.Line.t =
>   <line:"root:x:0:0:Enoch Root:/root:/bin/shcaml">
*)

(**
Then, we'll make a function that takes lines with delimited data to
lines with passwd data as well.
*)

let passwd_of_delim ln = 
  match Line.Delim.fields ln with
    | [|name;passwd;uid;gid;gecos;home;shell|] -> 
        Line.Passwd.create
          ~name ~passwd ~gecos ~home ~shell
          ~uid:(int_of_string uid) ~gid:(int_of_string gid)
          ln
    | _ -> Shtream.warn "Line didn't have 7 fields";;

(**
> val passwd_of_delim : Shcaml.Line.t -> Shcaml.Line.t = <fun>
*)

(**
Our function takes a line with a [delim] field, and returns one with
not just a [delim] field, but also a [passwd] field.  ({!Shtream.warn}
will be discussed below).  Let's try it out:
*)

let root_pw = passwd_of_delim root_delim;;
(**
> val root_pw : Shcaml.Line.t =
>   <line:"root:x:0:0:Enoch Root:/root:/bin/shcaml">
*)

Line.Passwd.uid root_pw;;
(**
> - : int = 0
*)

(**
You may have noticed that when we get the string a line was made
out of, we use {!Line.show}.  You can call [show] on any line, and
it will return a string representation of that line.  That does not
necessarily mean it will print out the exact value with which the line
was created.  In fact, you can change what [show] returns using
{!Line.select}.  Suppose that we wanted people to only see a username
when they tried to [show] [root_pw]:
*)

let root_un = Line.select Line.Passwd.name root_pw;;
(**
> val root_un : Shcaml.Line.t = <line:"root">
*)

Line.show root_un;;
(**
> - : string = "root"
*)

Line.show root_pw;;
(**
> - : string = "root:x:0:0:Enoch Root:/root:/bin/shcaml"
*)

(**
Using {!Line.show} and {!Line.select} becomes extremely important when 
we start working with external processes (that is, running UNIX programs 
from Ocaml).  When a line is to be piped into some external process, Shcaml
calls [show] on it and sends the string that results along.  Thus,
when it's important, you can change how your data is rendered when it
goes to UNIX.
*)

(**
{3 Shtreams}

{!Shtream}s are similar in intent and operation to Ocaml [Stream]s,
but unlike a [Stream], {!Shtream}s have an ['h'].  Additionally, shtreams
know about Ocaml channels; any shtream may be turned into an Ocaml
[in_channel], and vice-versa.  Shtreams have a richer interface than
streams, which may be explored in the API.  Let's try to make a
shtream
*)

let stdin_shtream = Shtream.of_channel input_line stdin;;

(**
> val stdin_shtream : string Shcaml.Shtream.t = <abstr>
*)

Shtream.next stdin_shtream;;

(** 
> hello, there. (you type this)
> 
> - : string = "  hello, there. (you type this)"
*)

(**
Here, we create a shtream from the [stdin] using
{!Shtream.of_channel}.  The first argument is a reader function, that
is, a function that tells the shtream how to produce a value from the
channel.  In this example, [stdin_shtream]
reads data a line at a time.  When we call {!Shtream.next} on
[stdin_shtream], it tries to produce another value, causing
[input_line] to be called on the [in_channel] with which the shtream
was created.

We can turn our shtream into an [in_channel] again with
{!Shtream.channel_of}:
*)

let newstdin = Shtream.channel_of print_endline stdin_shtream;;

(**
> val newstdin : in_channel = <in_channel:4>
*)
input_line newstdin;;
Hi again!
(**
> - : string = "  Hi again!"
*)

(**
To turn the shtream back into an [in_channel], we needed to give it a
writer function, here [print_endline].  The writer function should take
values in the shtream and print them on stdout.  (Bear in mind, shtreams
need not contain strings, so a writer function for an ['a Shtream.t] has
type ['a -> unit].

Shtreams can be generated programmatically using {!Shtream.from}.  For
instance, we could write a shtream that acted like the UNIX program
{b yes}(1), which prints a string to stdout until it's killed.  Our
version will be a function that takes a string and creates a shtream
that generates that string over and over again.
As with standard library streams, [from]
takes a function of type [int -> 'a option].
That function is called
with successive integers starting from 0, and is expected to return
either [Some value], meaning the next value in the shtream, or [None],
indicating that there is no more data to read from the shtream.
To demonstrate that the generating function is called for each element,
we'll include the argument to the function in each element.
*)

let yes s = 
  let builder n = Some (Printf.sprintf "%d: %s" n s) in
    Shtream.from builder;;
(**
> val yes : string -> string Shcaml.Shtream.t = <fun>
*)
let yes_shtr = yes "yes";;
(**
> val yes_shtr : string Shcaml.Shtream.t = <abstr>
*)
Shtream.next yes_shtr;;
(**
> - : string = "0: yes"
*)
Shtream.next yes_shtr;;
(**
> - : string = "1: yes"
*)
Shtream.next yes_shtr;;
(**
> - : string = "2: yes"
*)
Shtream.next yes_shtr;;
(**
> - : string = "3: yes"
*)
Shtream.next yes_shtr;;
(**
> - : string = "4: yes"
*)

(**
We can, of course, create a channel from this shtream, as well.
*)
let yes_chan = Shtream.channel_of print_endline yes_shtr;;
(**
> val yes_chan : in_channel = <in_channel:3>
*)
input_line yes_chan;;
(**
> - : string = "5: yes"
*)
input_line yes_chan;;
(**
> - : string = "6: yes"
*)
Channel.close_in yes_chan;;
(**
> - : unit = ()
*)


(**
What we've demonstrated here is a small portion of the functionality
of shtreams, but it's enough to give you an idea of how they work.  Many
more facilities for creating, observing, and manipulating shtreams
are described in the {!Shtream} API documentation.  However, from the
perspective of Shcaml, shtreams are relatively low-level constructs.
In addition to extending [Stream]s, Shcaml provides extensions to
standard Ocaml channels in a module called {!Channel}, and an
abstraction of processes (UNIX programs you run from Shcaml) in
{!Proc}.  Lines and shtreams combine their powers in {!Fitting}s,
which we discuss next.
*)

(**
{2 Fittings}

Fittings provide an embedded process control notation.  That's fancy
way of saying that we did our best to create some functions that make
it look (kinda, sorta) like you're writing snippets of shell scripts
in your Ocaml.  Let's try a simple one:
*)

run (command "echo a fitting!");;
(**
> a fitting!
> - : Shcaml.Proc.status = Unix.WEXITED 0
*)

(**
We've run the command ["echo a fitting!"].  We can see "a fitting!"
printed, and that it finished successfully ([Unix.WEXITED 0]).  When a
command doesn't exit successfully, we see that too:
*)

run (command "false");;
(**
> - : Shcaml.Proc.status = Unix.WEXITED 1
*)

(**
Let's look a little more closely at that.  There are two things
happening.  We construct a fitting with [command "false"].  There are
several different ways to create fittings: {!Fitting.command} takes a
string that will be run in the shell (e.g., [command "foo bar baz"] is like
{i sh -c "foo bar baz"}).  However, the fitting is not actually executed
until we call {!Fitting.run} on it.  For example,
*)

let goodbye = command "echo goodbye from unix" in
  print_endline "hello from caml";
  run goodbye;;
(**
> hello from caml
> goodbye from unix
> - : Shcaml.Proc.status = Unix.WEXITED 0
*)

(**
Notice that the "hello from caml" appeared before the "goodbye from
unix".  There are several kinds of "runners".  The one we've seen,
[run], executes a fitting with stdin as its input and stdout as its
output.  The type of [run] is [(Shcaml.Fitting.text -> 'a
Shcaml.Fitting.elem) Shcaml.Fitting.t -> Shcaml.Proc.status].  In
general, [('a -> 'b) Shcaml.Fitting.t] is a thing that consumes a
sequence of ['a]s and produces a sequence of ['b]s.  The type
{!Fitting.text} indicates data coming in over a channel; the type ['a
Shcaml.Fitting.elem] indicates generic data that can be sent over a
channel.  There are several kinds of fitting constructors provided in
the {!Fitting} module.  Let's look at a few of them.  All of the
following print the {i /etc/passwd} file to the standard out (we'll
elide the output here to save space):
*)

run (command "cat /etc/passwd");;
(**
> ...
*)
run (from_file "/etc/passwd");;
(**
> ...
*)
run (from_gen (`Filename "/etc/passwd"));;
(**
> ...
*)

(**
Rather than send the output from a fitting to stdout, we can
get it as a shtream:
*)

let passwd = run_source (from_file "/etc/passwd");;
(**
> val passwd : Shcaml.Fitting.text Shcaml.Fitting.shtream = <abstr>
*)

Shtream.next passwd;;
(**
> - : Shcaml.Fitting.text = <line:"root:x:0:0:root:/root:/bin/bash">
*)
Shtream.next passwd;;
(**
> - : Shcaml.Fitting.text = <line:"daemon:x:1:1:daemon:/usr/sbin:/bin/sh">
*)

(**
What good is that, you may ask?  Well, now that we have a shtream of
lines, we can start applying some of our line functions to them.
Here's one that we provide for parsing passwd files (these sorts of
functions are provided by the {!Adaptor} module).
*)

let pw_shtream = run_source
  (from_file "/etc/passwd" -| Adaptor.Passwd.fitting ());;
(**
> val pw_shtream :
>   <| passwd : Shcaml.Line.present; seq : Shcaml.Line.present;
>      source : Shcaml.Line.present >
>   Shcaml.Line.t Shcaml.Fitting.shtream = <abstr>
*)

Shtream.next pw_shtream;;
(**
> - : <| passwd : Shcaml.Line.present; seq : Shcaml.Line.present;
>        source : Shcaml.Line.present >
>     Shcaml.Line.t
> = <line:"root:x:0:0:root:/root:/bin/bash">
*)

(**
Now we have a shtream that has (take a careful look at those types)
lines with passwd data in them.  (They also have [source], which tells
you where data came from, and [seq], which tells you its line number
in the source.)

Can you guess what the [(-|)] operator does?  That's
right, it's a pipe!  (The [|] character is pretty meaningful in Ocaml
programs, as are most other shell operators, so we have decorated them
a little bit to give them the right precedence and to keep them from
clashing with other Ocaml syntax.)  

The type of [(-|)] will help us understand fittings a whole lot
better
*)

(-|);;
(**
> - : ('a -> 'b) Shcaml.Fitting.t ->
>     ('b -> 'c) Shcaml.Fitting.t -> ('a -> 'c) Shcaml.Fitting.t
> = <fun>
*)

(**
Typically, in the shell, when we want to pipe two processes together
({i foo | bar}), we think of [bar] as a program that takes whatever
kind of output [foo] produces and then generates its own output.  In
Shcaml, we think the same way.  The type of a fitting tells us what
kind of data it accepts as input and generates as output.  An [('a ->
'b) Shcaml.Fitting.t] takes values of type ['a] as input and outputs
values of type ['b].  So of course, you can only pipe together two
fittings if the first one produces data the second one consumes.  So
if the first fitting given to [(-|)] reads ['a]s and outputs ['b]s,
then the second must consume ['b]s, and output ['c]s.  When you put
them together, then, you'll get a new fitting that reads ['a]s, runs
them through the first fitting and back into the second, and then
produces the output of the second, ['c]s.  That is, we get an
[('a -> 'c) Shcaml.Fitting.t].

Fittings provide a general mechanism to pipe together data like this.
But they also know a whole lot about UNIX, and make it very easy to
intermix calls to the shell with Ocaml code.  Let's use the system's
[sort] command and our built-in [uniq] functions (we provide a Fitting
version of [sort] in {!UsrBin}) to get a list of the different shells
that are in use on the system.  
*)

let shells = LineShtream.string_list_of
  (run_source
     (from_file "/etc/passwd" 
      -| Adaptor.Passwd.fitting ()
      -| cut Line.Passwd.shell
      -| command "sort" 
      -| uniq ()));;
(**
> val shells : string list =
>   ["/bin/bash"; "/bin/false"; "/bin/sh"; "/bin/sync"; "/bin/zsh";
>    "/usr/lib/nx/nxserver"; "/usr/sbin/nologin"]
*)

(**
Your results may differ, of course; on the box this manual is currently
being written on, it appears that nobody uses C Shell.  That pipeline is
longer than the one we've seen, but the only new material is
{!UsrBin.cut}, which takes a function from [('a Shcaml.Line.t ->
string)] and produces an [('a Shcaml.Line.t -> 'a Shcaml.Line.t)
Shcaml.Fitting.t].  It's like {!Line.select} for fittings.  We start
the pipeline off with [from_file "/etc/passwd"], which will generate a
shtream of the lines out of the passwd file.  Then we adapt the
shtream into a shtream with passwd data attached
([Adaptor.Passwd.fitting ()]).  Next, we want to make our lines appear
to the outside world not as the full string read out of the passwd
file, but rather just the shell field.  So we call {!UsrBin.cut} to
select the {!Line.Passwd.shell} field as the [show] text for each
line.  That way, when the lines get passed to the external [sort]
command, it just sees the shell field, and not the whole passwd
record.  Then we use our internal {!UsrBin.uniq} to remove duplicates.
Because we pass our fitting to [run_source], it generates a shtream,
upon which we may finally call [LineShtream.string_list_of].  But the
code is much easier to understand than the prose, isn't it?

In addition to pipes, Shcaml provides analogues to the shell's [&&],
[||], and [;] sequencing operators.  Take a bit of structured playtime
and poke around with them.  They're in the fine
{{:FittingSig.S.html#2_FittingCombinators} manual}.

{3 I/O Redirection}

A difference between fittings and UNIX pipelines is that fittings only
have one input and one output, while UNIX processes may read or write
on many different file descriptors (for instance, [stdout] and
[stderr]).  Shcaml provides facilities for sophisticated I/O
redirection.  Let's start by taking a look at how redirection is
specified.  

A {!Channel.dup_spec} is a list of instructions for how I/O redirection
should be done for a given fitting.  There are a great many operators
provided in {!Channel.Dup} for specifying different sorts of
interconnections.  Here's a bunch of different examples, each of which
redirects the standard output to {i /dev/null}:
*)

run (command "echo hello" />/ [ stdout />* `Null ]);;
(**
> - : Shcaml.Proc.status = Unix.WEXITED 0
*)
run (command "echo hello" />/ [ 1 %>* `Filename "/dev/null" ]);;
(**
> - : Shcaml.Proc.status = Unix.WEXITED 0
*)
run (command "echo hello" />/ [ `OutFd 1 *>& `Null ]);;
(**
> - : Shcaml.Proc.status = Unix.WEXITED 0
*)
run (command "echo hello" />/ [ `OutChannel stdout *>& `Null ]);;
(**
> - : Shcaml.Proc.status = Unix.WEXITED 0
*)

(**
Why so many ways to say nothing at all?  Well, there are a few different
kinds of places you can send data (not all of them {i /dev/null}), and
several different names for the same places.  For instance, writing to
[stdout], file descriptor 1, or [gen_out_channel]s [`OutFd 1] or
[`OutChannel stdout].  Shcaml provides operators for dealing with each
of these cases.  ({!Channel.gen_channel}s are Shcaml's lower-level
generalized channels.)  In order to make it easier to remember which
operator is which, they're named systematically.  See {!Channel.Dup} for
an explanation of the myriad redirection operators.

The operators [(/>/)] and [(/</)] take a fitting on the left and a
list of redirections on the right, and apply the redirections in the
latter to the former.  For example,
*)
run (command "echo hello; echo world 1>&2"
       />/ [ 1 %> "file1"; 2 %> "file2" ]);;
(**
> - : Shcaml.Proc.status = Unix.WEXITED 0
*)

(**
Let's check that it worked:
*)

run (from_file "file1");;
(**
> hello
> - : Shcaml.Proc.status = Unix.WEXITED 0
*)

run (from_file "file2");;
(**
> world
> - : Shcaml.Proc.status = Unix.WEXITED 0

{3 Adaptors}

The {!Adaptor} module provides record readers and splitters for a
variety of file formats.  The readers and splitters for each format
are contained in a submodule named for the format (for instance, the
functions for {i /etc/mailcap} are in {!Adaptor.Mailcap}.  Record readers
read "raw data off the wire".  That is, a reader is a function from an
[in_channel] to a {!Reader.raw_line}, which is a record of string data,
possibly including some delimiter junk.
Splitters do field-splitting.  Given a line, they will use the {!Line.raw}
data in the line to produce a line the relevant fields.  In
addition to readers and splitters, each module exports an [adaptor]
function that is used to transform shtreams of lines by using the
[reader] and [splitter] functions (they all have these names by
convention) in the module; a function [fitting] is
provided as well, which (as one might expect) provides a version
of the adaptor as a fitting, so it might be used directly in a
pipeline.  

There are adaptor submodules for delimited text, simple flat files,
comma-separated text, key-value and sectioned key-value (ie, ssh
config files or .ini-style files), /etc/{group,fstab,passwd,mailcap}
files, and more.

{3 UsrBin}

{!UsrBin} contains a collection of miscellaneous useful functions.
Among these are fittings like [ls], [ps], [cut], [head], [sort] and
[uniq].  In addition, it provides some lower-level but still quite
useful functions, such as [cd], [mkdir], [mkpath] ({i mkdir -p}, as well
as a submodule {!UsrBin.Test} that contains functions
analogous to {b test}(1).

{2 Glossary}

It is an unfortunate necessity of the scope and intent of Shcaml that
many of the names of things in the library sound generic (for
instance: runner, reader, stash, line etc.).  In fact, in the API
documentation and the manual, we have striven to use such terms in a
more formalized sense.  This glossary documents Shcaml (and related)
"terms of art", hopefully eliminating ambiguity and confusion.

- {b adaptor}: A module that knows how to parse a particular file
  format, storing the results in line fields.  See {!Adaptor}.
- {b clobber}: When redirecting output to a file that already exists,
  {i clobber}ing is to truncate and overwrite the file.
  {!Channel.clobber} controls whether to clobber files by default.
- {b command}: A string that is passed to the UNIX command interpreter
  (shell) for processing.  For example, ["ls > /dev/null"].  This
  differs from a {i program}, which is one executable to launch.
- {b dup}: I/O redirection. To copy a file descriptor, or cause one
  file descriptor to behave as another.  In Shcaml, we also use {i dup}
  to refer to the specification of a redirection (e.g., [2 %>& 1]).
- {b line}: A structured record of type {!Line.t}, containing
  both raw text and, usually, metadata.
- {b program}: A single UNIX executable; contrast with {i command}.
  Usually in Shcaml, when we run a program, we specify both the name
  of the executable and a [string list] of already-parsed arguments.
- {b raw}: The unprocessed text from which a {!Line.t} was created.
  Accessed with {!Line.raw}.  Use {!Line.show} to get the current
  (possibly processed) text of a line.
- {b reader}: A function for splitting channel input into records,
  before analysing the contents of each record.  See {!Reader} for how
  {i readers} are defined.  Most {!Adaptor}s also come with a reader
  for their format.
- {b runner}: A function in {!Fitting} for executing a fitting, which
  may be previously constructed.  See {!Fitting.run},
  {!Fitting.run_source}, {!Fitting.run_shtream}, etc.
- {b show}: The textual value of a {i line}, accessed with
  {!Line.show}.  This is used, for example, to send the contents
  of a line to external processes.  If you want the [string] of a line,
  this is usually it.
- {b splitter}:  A function to analyze the {i raw} text of a {i line}
  and fill in more line fields.  Most {!Adaptor}s come with a splitter
  for their format.
- {b stash}:  To store the {!Proc.t} in an optional parameter, so
  that the caller of a function that may fork can find out the identity
  of the resulting child process.  Functions such as
  {!Channel.open_command_in} take an optional [Proc.t option ref]
  parameter in which they stash [Proc.t] of the process that they
  start.
- {b thunk}: A function with one argument of type [unit], used to
  specify an action to be performed later, or in another context.  Often
  in Shcaml, that other context is a child process.  For example,
  {!Fitting.thunk} takes a thunk, which it runs in a subprocess and
  splices into the pipeline.

{1 Toplevel modules}

List of all the direct submodules of the {!Shcaml} module:

{!modules:
  Abort
  Adaptor
  AnyShtream
  AnyShtreamSig
  Channel
  Delimited
  DepDAG
  Disposal
  Fitting
  FittingSig
  Flags
  IVar
  Line
  LineShtream
  PriorityQueue
  Proc
  Reader
  Shtream
  ShtreamSig
  Signal
  StringShtream
  Util
  UsrBin
  Version
  WeakPlus
}
*)

(**
{1 Indices}

{!indexlist}
*)

(*
 * This file is used to implement the #shcaml directive, which installs
 * printers and opens several Shcaml modules for interactive use.
 *)

open Shcaml;;
open Util;;
open UsrBin;;
open Fitting;;
open Channel.Dup;;

#install_printer Shtop.print_line;;
#install_printer Shtop.print_descr;;
#install_printer Shtop.print_proc;;
#install_printer Shtop.print_in_channel;;
#install_printer Shtop.print_out_channel;;

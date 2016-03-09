(*
 * Loaded by shcaml.top; installs printers and opens ShcamlCommon for
 * interactive use.  
 *)

open Shcaml;;
open Util;;
open UsrBin;;
open Fitting;;
open Channel.Dup;;

#install_printer Line.pp;;
#install_printer Channel.pp_descr;;
#install_printer Proc.pp;;
#install_printer Channel.pp_in_channel;;
#install_printer Channel.pp_out_channel;;

open Odoc_info
open Parameter
open Value
open Type
open Extension
open Exception
open Class
open Module

let bp = Printf.bprintf
let bs = Buffer.add_string

let print_concat b sep f =
  let rec iter = function
      [] -> ()
    | [c] -> f c
    | c :: q ->
        f c;
        bs b sep;
        iter q
  in
  iter

(** The functions used for naming files and html marks.*)
module Naming =
  struct
    (** The prefix for types marks. *)
    let mark_type = "TYPE"

    (** The prefix for types elements (record fields or constructors). *)
    let mark_type_elt = "TYPEELT"

    (** The prefix for functions marks. *)
    let mark_function = "FUN"

    (** The prefix for extensions marks. *)
    let mark_extension = "EXTENSION"

    (** The prefix for exceptions marks. *)
    let mark_exception = "EXCEPTION"

    (** The prefix for values marks. *)
    let mark_value = "VAL"

    (** The prefix for attributes marks. *)
    let mark_attribute = "ATT"

    (** The prefix for methods marks. *)
    let mark_method = "METHOD"

    (** The prefix for code files.. *)
    let code_prefix = "code_"

    (** The prefix for type files.. *)
    let type_prefix = "type_"

    (** Return the two html files names for the given module or class name.*)
    let html_files name =
      let qual =
        try
          let i = String.rindex name '.' in
          match name.[i + 1] with
          | 'A'..'Z' -> ""
          | _ -> "-c"
        with Not_found -> ""
      in
      let prefix = name^qual in
      let html_file = prefix^".html" in
      let html_frame_file = prefix^"-frame.html" in
      (html_file, html_frame_file)

    (** Return the target for the given prefix and simple name. *)
    let target pref simple_name = pref^simple_name

    (** Return the complete link target (file#target) for the given prefix string and complete name.*)
    let complete_target pref complete_name =
      let simple_name = Name.simple complete_name in
      let module_name =
        let s = Name.father complete_name in
        if s = "" then simple_name else s
      in
      let (html_file, _) = html_files module_name in
      html_file^"#"^(target pref simple_name)

    (** Return the link target for the given type. *)
    let type_target t = target mark_type (Name.simple t.ty_name)

    (** Return the link target for the given variant constructor. *)
    let const_target t f =
      let name = Printf.sprintf "%s.%s" (Name.simple t.ty_name) f.vc_name in
      target mark_type_elt name

    (** Return the link target for the given record field. *)
    let recfield_target t f = target mark_type_elt
      (Printf.sprintf "%s.%s" (Name.simple t.ty_name) f.rf_name)

    (** Return the link target for the given object field. *)
    let objfield_target t f = target mark_type_elt
      (Printf.sprintf "%s.%s" (Name.simple t.ty_name) f.of_name)

    (** Return the complete link target for the given type. *)
    let complete_type_target t = complete_target mark_type t.ty_name

    let complete_recfield_target name =
      let typ = Name.father name in
      let field = Name.simple name in
      Printf.sprintf "%s.%s" (complete_target mark_type_elt typ) field

    let complete_const_target = complete_recfield_target

    (** Return the link target for the given extension. *)
    let extension_target x = target mark_extension (Name.simple x.xt_name)

    (** Return the complete link target for the given extension. *)
    let complete_extension_target x = complete_target mark_extension x.xt_name

    (** Return the link target for the given exception. *)
    let exception_target e = target mark_exception (Name.simple e.ex_name)

    (** Return the complete link target for the given exception. *)
    let complete_exception_target e = complete_target mark_exception e.ex_name

    (** Return the link target for the given value. *)
    let value_target v = target mark_value (Name.simple v.val_name)

    (** Return the given value name where symbols accepted in infix values
       are replaced by strings, to avoid clashes with the filesystem.*)
    let subst_infix_symbols name =
      let len = String.length name in
      let buf = Buffer.create len in
      let ch c = Buffer.add_char buf c in
      let st s = Buffer.add_string buf s in
      for i = 0 to len - 1 do
        match name.[i] with
        | '|' -> st "_pipe_"
        | '<' -> st "_lt_"
        | '>' -> st "_gt_"
        | '@' -> st "_at_"
        | '^' -> st "_exp_"
        | '&' -> st "_amp_"
        | '+' -> st "_plus_"
        | '-' -> st "_minus_"
        | '*' -> st "_star_"
        | '/' -> st "_slash_"
        | '$' -> st "_dollar_"
        | '%' -> st "_percent_"
        | '=' -> st "_equal_"
        | ':' -> st "_column_"
        | '~' -> st "_tilde_"
        | '!' -> st "_bang_"
        | '?' -> st "_questionmark_"
        | c -> ch c
      done;
      Buffer.contents buf

    (** Return the complete link target for the given value. *)
    let complete_value_target v = complete_target mark_value v.val_name

    (** Return the complete filename for the code of the given value. *)
    let file_code_value_complete_target v =
      let f = code_prefix^mark_value^(subst_infix_symbols v.val_name)^".html" in
      f

    (** Return the link target for the given attribute. *)
    let attribute_target a = target mark_attribute (Name.simple a.att_value.val_name)

    (** Return the complete link target for the given attribute. *)
    let complete_attribute_target a = complete_target mark_attribute a.att_value.val_name

    (** Return the complete filename for the code of the given attribute. *)
    let file_code_attribute_complete_target a =
      let f = code_prefix^mark_attribute^a.att_value.val_name^".html" in
      f

    (** Return the link target for the given method. *)
    let method_target m = target mark_method (Name.simple m.met_value.val_name)

    (** Return the complete link target for the given method. *)
    let complete_method_target m = complete_target mark_method m.met_value.val_name

    (** Return the complete filename for the code of the given method. *)
    let file_code_method_complete_target m =
      let f = code_prefix^mark_method^m.met_value.val_name^".html" in
      f

    (** Return the link target for the given label section. *)
    let label_target l = target "" l

    (** Return the complete link target for the given section label. *)
    let complete_label_target l = complete_target "" l

    (** Return the complete filename for the code of the type of the
       given module or module type name. *)
    let file_type_module_complete_target name =
      let f = type_prefix^name^".html" in
      f

    (** Return the complete filename for the code of the
       given module name. *)
    let file_code_module_complete_target name =
      let f = code_prefix^name^".html" in
      f

    (** Return the complete filename for the code of the type of the
       given class or class type name. *)
    let file_type_class_complete_target name =
      let f = type_prefix^name^".html" in
      f
  end

module Generator (G: Odoc_html.Html_generator) =
struct
  class html =
    object(self)
      inherit G.html as html

      method keyword s =
        if s = "|" then
          "<span class=\"keyword pipe\">"^s^"</span>"
        else
          "<span class=\"keyword\">"^s^"</span>"

      method private html_of_variant_info ?(cls="") ?(indent=true) b info_opt =
        match info_opt with
          None ->
          ()
        | Some info ->
          let module M = Odoc_info in
          if indent then bs b ("<div class=\"variantinfo"^cls^"\">\n");
          (
            match info.M.i_deprecated with
              None -> ()
            | Some d ->
              bs b "<span class=\"warning\">";
              bs b Odoc_messages.deprecated ;
              bs b "</span>" ;
              self#html_of_text b d;
              bs b "<br>\n"
          );
          (
            match info.M.i_desc with
              None -> ()
            | Some d when d = [Odoc_info.Raw ""] -> ()
            | Some d -> self#html_of_text b d; bs b "<br>\n"
          );
          self#html_of_author_list b info.M.i_authors;
          self#html_of_version_opt b info.M.i_version;
          self#html_of_before b info.M.i_before;
          self#html_of_since_opt b info.M.i_since;
          self#html_of_raised_exceptions b info.M.i_raised_exceptions;
          self#html_of_return_opt b info.M.i_return_value;
          self#html_of_sees b info.M.i_sees;
          self#html_of_custom b info.M.i_custom;
          if indent then bs b "</div>\n"

    method html_of_type b t =
        match t.ty_kind with
        | Type_variant l ->
          Odoc_info.reset_type_names ();
          let father = Name.father t.ty_name in
          let print_field_prefix () =
            bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
            bs b "<code>&nbsp;&nbsp;</code>";
            bs b "</td>\n<td align=\"left\" valign=\"top\" >\n";
            bs b "<code>";
          in
          let print_field_comment = function
            | None -> ()
            | Some t ->
              bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
              bs b "<code>";
              bs b "(*";
              bs b "</code></td>";
              bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" >";
              self#html_of_info b (Some t);
              bs b "</td><td class=\"typefieldcomment\" align=\"left\" valign=\"bottom\" >";
              bs b "<code>*)</code></td>"
          in
          bs b
            (match t.ty_manifest with
             | None -> "\n<pre><code>"
             | Some _ -> "\n<pre>"
            );
          bp b "<span id=\"%s\">" (Naming.type_target t);
          bs b ((self#keyword "type")^" ");
          self#html_of_type_expr_param_list b father t;
          (match t.ty_parameters with [] -> () | _ -> bs b " ");
          bs b (Name.simple t.ty_name);
          bs b "</span> ";
          let priv = t.ty_private = Asttypes.Private in
          (
            match t.ty_manifest with
              None -> ()
            | Some (Object_type fields) ->
              bs b "= ";
              if priv then bs b "private ";
              bs b "&lt;</pre>";
              bs b "<table class=\"typetable\">\n" ;
              let print_one f =
                print_field_prefix () ;
                bp b "<span id=\"%s\">%s</span>&nbsp;: "
                  (Naming.objfield_target t f)
                  f.of_name;
                self#html_of_type_expr b father f.of_type;
                bs b ";</code></td>\n";
                print_field_comment f.of_text ;
                bs b "\n</tr>"
              in
              print_concat b "\n" print_one fields;
              bs b "</table>\n>\n";
              bs b " "
            | Some (Other typ) ->
              bs b "= ";
              if priv then bs b "private ";
              self#html_of_type_expr b father typ;
              bs b " "
          );
          (bs b "= ";
           if priv then bs b "private ";
           bs b
             (match t.ty_manifest with
                None -> "</code></pre>"
              | Some _ -> "</pre>"
             );
           self#html_of_variant_info b t.ty_info;
           bs b "\n";
           bs b "<table class=\"typetable varianttable\">\n";
           let print_one constr =
             bs b "<tr>\n<td align=\"left\" valign=\"top\" >\n";
             bs b "<code>";
             bs b (self#keyword "|");
             bs b "</code></td>\n<td align=\"left\" valign=\"top\" >\n";
             bs b "<code>";
             bp b "<span id=\"%s\">%s</span>"
               (Naming.const_target t constr)
               (self#constructor constr.vc_name);
             (
               match constr.vc_args, constr.vc_ret with
                 [], None -> ()
               | l,None ->
                 bs b (" " ^ (self#keyword "of") ^ " ");
                 self#html_of_type_expr_list ~par: false b father " * " l;
               | [],Some r ->
                 bs b (" " ^ (self#keyword ":") ^ " ");
                 self#html_of_type_expr b father r;
               | l,Some r ->
                 bs b (" " ^ (self#keyword ":") ^ " ");
                 self#html_of_type_expr_list ~par: false b father " * " l;
                 bs b (" " ^ (self#keyword "->") ^ " ");
                 self#html_of_type_expr b father r;
             );
             bs b "</code></td></tr>\n<tr>\n";
             (
               match constr.vc_text with
                 None -> ()
               | Some t ->
                 bs b "<td class=\"typefieldcomment\" align=\"left\" valign=\"top\" colspan=\"2\">";
                 self#html_of_info b (Some t);
                 bs b "</td>";
             );
             bs b "\n</tr>"
           in
           print_concat b "\n" print_one l;
           bs b "</table>\n"
          );
          bs b "\n";

        | _ ->
          html#html_of_type b t

    (* (\** Print html code for an included module. *\) *)
    (* method html_of_included_module b im = *)
    (*   bs b "\n<pre>"; *)
    (*   bs b ((self#keyword "include")^" "); *)
    (*   ( *)
    (*    match im.im_module with *)
    (*     | None -> *)
    (*       bs b im.im_name *)
    (*     | Some mmt -> *)
    (*       begin match mmt with *)
    (*         | Mod m -> *)
    (*           let (html_file, _) = Naming.html_files m.m_name in *)
    (*           bp b "<a href=\"%s\">%s</a>" html_file m.m_name *)
    (*         | Modtype mt -> *)
              
          
    (*        let (file, name) = *)
    (*          match mmt with *)
    (*            Mod m -> *)
    (*              let (html_file, _) = Naming.html_files m.m_name in *)
    (*              (html_file, m.m_name) *)
    (*          | Modtype mt -> *)
    (*              let (html_file, _) = Naming.html_files mt.mt_name in *)
    (*              (html_file, mt.mt_name) *)
    (*        in *)
    (*        bp b "<a href=\"%s\">%s</a>" file name *)
    (*   ); *)
    (*   bs b "</pre>\n"; *)
    (*   self#html_of_info b im.im_info *)
    end
end

let _ = Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor)

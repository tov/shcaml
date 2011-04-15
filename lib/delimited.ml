(* Implementation of delimited text formats. *)

type options = {
  field_sep:            char;
  record_sep:           char;
  trim_space:           bool;
  rec_backslash:        bool;
  rec_quotation:        bool;
  rec_double_double:    bool;
  rec_cr:               bool;
  rec_escapes:          bool;
  max_fields:           int;
}

let default_options = {
  field_sep               = ',';
  record_sep              = '\n';
  trim_space               = true;
  rec_backslash           = false;
  rec_quotation           = true;
  rec_double_double       = true;
  rec_cr     = true;
  rec_escapes     = false;
  max_fields                = 0;
}

(* Here begins the record reader state machine. *)
LOCAL
  (* Here we're about to handle any character that's not inside
   * quotes.  The current character is passed in, because other
   * parts of the machine may look ahead at it. *)
  let rec start options channel buf c =
    match c with
    | _ when c = options.record_sep ->
        Reader.raw_of_string (Buffer.contents buf)
    | '"' when options.rec_quotation ->
        Buffer.add_char buf '"';
        quotation options channel buf
    | '\\' when options.rec_backslash
             or options.rec_escapes ->
        backslash options channel buf;
        start options channel buf (input_char channel)
    | '\r' when options.rec_cr ->
        let c = try input_char channel
                with End_of_file -> ' ' in
        if c = options.record_sep
          then Reader.raw_of_string ~after:"\r\n" (Buffer.contents buf)
          else failwith "Csv.reader: got CR without LF"
    | _ ->
        Buffer.add_char buf c;
        start options channel buf (input_char channel)
  (* Here we're inside double quotes. *)
  and quotation options channel buf =
    let c = try input_char channel
            with End_of_file ->
              failwith "Csv.reader: EOF during quotation" in
    match c with
    | '\\' when options.rec_backslash
             or options.rec_escapes ->
        backslash options channel buf;
        quotation options channel buf
    | '"' ->
        Buffer.add_char buf '"';
        let c = input_char channel in
        if c = '"'
          then
            begin
              Buffer.add_char buf '"';
              quotation options channel buf
            end
          else start options channel buf c
    | _ ->
        Buffer.add_char buf c;
        quotation options channel buf
  (* We just saw a backslash and we're in backslash mode,
   * so there better be another character following it. *)
  and backslash options channel buf =
    try let c = input_char channel in
      Buffer.add_char buf '\\';
      Buffer.add_char buf c with
    | End_of_file ->
      if options.rec_backslash then
        failwith "Csv.reader: recognized backslash at EOF"
      else
        Buffer.add_char buf '\\'
IN               
  (* The above code guards against end_of_file only in places that
   * require special treatment.  The other case is that we catch
   * it out here and wrap things up. *)
  let reader ?(options = default_options) channel =
    let buf = Buffer.create 80 in
    try
      start options channel buf (input_char channel)
    with
      End_of_file when Buffer.length buf > 0 ->
        Reader.raw_of_string ~after:"" (Buffer.contents buf)
END

(* Support code for \xHH and \OOO. *)
let hex_digit = function
  | '0' .. '9' as c -> int_of_char c - int_of_char '0'
  | 'a' .. 'f' as c -> int_of_char c - int_of_char 'a' + 10
  | 'A' .. 'F' as c -> int_of_char c - int_of_char 'A' + 10
  | c               -> failwith (Printf.sprintf
                                   "Got `%c' where hex digit expected" c)
let oct_digit = function
  | '0' .. '7' as c -> int_of_char c - int_of_char '0'
  | c               -> failwith (Printf.sprintf
                                   "Got `%c' where octal digit expected" c)

(*
 * The field-splitting state machine commences.
 *)
let splitter ?(options = default_options) record =
  let buf   = Buffer.create 80 in
  let limit = String.length record in

  (* We just saw a backslash.  We may be in rec_backslash mode,
   * escapes mode, or both.  If we're in rec_backslash
   * mode, we require another character to work with, but in
   * escapes mode, that's optional because we treat it as a code
   * we don't recognize and return the backslash. *)
  let backslash i =
    if i + 1 >= limit then
      if options.rec_backslash then
        failwith "Csv.splitter: backslash at end of record"
      else '\\', 1
    else
      match record.[i + 1] with
      (* If we're not recognizing backslash codes, we must be
       * recognizing backslashes, and we're done. *)
      | c when not options.rec_escapes ->
          c, 2
      (* Translate codes. *)
      | '"'  -> '"', 2
      | '\\' -> '\\', 2
      | 'a'  -> '\007', 2
      | 'b'  -> '\008', 2
      | 'f'  -> '\012', 2
      | 'n'  -> '\010', 2
      | 'r'  -> '\013', 2
      | 't'  -> '\009', 2
      | 'v'  -> '\011', 2
      | 'x'  -> if i + 3 >= limit then
                  failwith "Csv.splitter: unfinished \\xHH code"
                else
                  char_of_int(16 * hex_digit record.[i + 2] +
                              hex_digit record.[i + 3]), 4
      | ('0' .. '7') -> 
          if i + 3 >= limit then
            failwith "Csv.splitter: unfinished \\xHH code"
          else
            char_of_int(64 * oct_digit record.[i + 1] +
                        8 * oct_digit record.[i + 2] +
                        oct_digit record.[i + 3]), 4
      (* If we're in both modes and nothing to translate. . . *)
      | c when options.rec_backslash ->
          c, 2
      (* We're only recognizing codes, and there wasn't one there. *)
      | _ -> '\\', 1 in

  (* We're at the start of a field.  If we're trimming whitespace, then
   * do that thing. *)
  let rec start n fields i =
    if i >= limit
      then continue n fields i i
      else match record.[i] with
      | ' ' | '\t' | '\r' | '\n' when options.trim_space ->
          start n fields (i + 1)
      | _ ->
          continue n fields i i

  (* Traversing a fields, outside of quotation.  If we're trimming
   * whitespace, we use [mark] to track the beginning of the must recent
   * run of space.  That way, we can discard it if we hit the end of
   * the field or include it if we hit anything else. *)
  and continue n fields i mark =
    if i >= limit
    then List.rev (Buffer.contents buf :: fields)
    else match record.[i] with
    | c when c = options.field_sep && n <> 1 ->
        let fields = Buffer.contents buf :: fields in
        Buffer.clear buf;
        start (n - 1) fields (i + 1)
    | ' ' | '\t' | '\r' | '\n' when options.trim_space ->
        continue n fields (i + 1) mark
    | '"' when options.rec_quotation ->
        for j = mark to i - 1 do
          Buffer.add_char buf record.[j]
        done;
        quotation n fields (i + 1)
    | '\\' when options.rec_escapes
             or options.rec_backslash ->
        for j = mark to i - 1 do
          Buffer.add_char buf record.[j]
        done;
        let c, next = backslash i in
        Buffer.add_char buf c;
        continue n fields (i + next) (i + next)
    | c ->
        for j = mark to i - 1 do
          Buffer.add_char buf record.[j]
        done;
        Buffer.add_char buf c;
        continue n fields (i + 1) (i + 1)

  (* We're inside quotation marks. *)
  and quotation n fields i =
    if i >= limit
      then failwith "Csv.splitter: end of record during quotation"
      else match record.[i] with
      | '"' when i + 1 < limit
              && record.[i + 1] = '"'
              && options.rec_double_double ->
          Buffer.add_char buf '"';
          quotation n fields (i + 2)
      | '"' ->
          continue n fields (i + 1) (i + 1)
      | '\\' when options.rec_escapes
               or options.rec_backslash ->
          let c, next = backslash i in
          Buffer.add_char buf c;
          quotation n fields (i + next)
      | c ->
          Buffer.add_char buf c;
          quotation n fields (i + 1) in
  Array.of_list (start options.max_fields [] 0)

(* Here is how we output fields. *)
LOCAL
  (* Determine whether quoting (of some sort) is required.  Quoting
   * is required iff one of:
   * - There's leading or trailing space and we trim space
   * - We have a double-quote or CR and we recognize those
   * - We have a field delimiter or record separator
   *)
  let must_quote options field limit =
    if limit = 0 then
      if options.trim_space &&
         String.contains " \t\r\n" options.field_sep then
       if options.rec_quotation then
         true
       else
         failwith "sheeit"
      else
        false
    else try
      match field.[0], field.[limit - 1] with
      | (' '|'\t'),_ | _,(' '|'\t') when options.trim_space ->
          raise Exit
      | _ ->
          for i = 0 to limit - 1 do
            match field.[i] with
            | '"' when options.rec_quotation ->
                raise Exit
            | '\r' when options.rec_cr ->
                raise Exit
            | c when c = options.field_sep
                  or c = options.record_sep ->
                raise Exit
            | _ -> ()
          done;
          false
    with Exit -> true

  (* Here we've decided to use double-quotes as our quotation device.
   * This isn't guaranteed to succeed, because if we see quote but aren't
   * allowed to use backslash or doublingthen we can't do it.  The
   * preferred method is always in closest accordence with CSV. *)
  let quotation options field channel limit =
    output_char channel '"';
    for i = 0 to limit - 1 do
      match field.[i] with
      | '"' when options.rec_double_double ->
          output_string channel "\"\""
      | '"'|'\\' as c when options.rec_backslash
                        or options.rec_escapes ->
          output_char channel '\\';
          output_char channel c;
      | '"' ->
          failwith "Csv.output_field: can't quote double quote"
      | c ->
          output_char channel c
    done;
    output_char channel '"'

  (* If we choose backslash output and trimming, then we keep track of
   * runs of whitespace -- we'll then choose whether to output them
   * or without backslashes when we hit either the end of the field
   * or non-space characters. *)
  let flush_backslash field channel mark i limit plus =
    if mark = 0 or i = limit then
      for j = mark to i - 1 do
        output_char channel '\\';
        output_char channel field.[j]
      done
    else
      for j = mark to i - 1 do
        output_char channel field.[j]
      done;
    for j = i to i + plus - 1 do
      output_char channel '\\';
      output_char channel field.[j]
    done

  let rec backslash options field channel i mark limit =
    if i >= limit then
      flush_backslash field channel mark i limit 0
    else begin
      match field.[i] with
      | c when c = options.field_sep
            or c = options.record_sep ->
          flush_backslash field channel mark i limit 1;
          backslash options field channel (i + 1) (i + 1) limit
      | '"' when options.rec_quotation ->
          flush_backslash field channel mark i limit 1;
          backslash options field channel (i + 1) (i + 1) limit
      | '\\' ->
          flush_backslash field channel mark i limit 1;
          backslash options field channel (i + 1) (i + 1) limit
      | '\r' when options.rec_cr ->
          if i + 1 >= limit or field.[i] <> options.record_sep then
            failwith "Csv.output_field: got CR without LF"
          else begin
            flush_backslash field channel mark i limit 2;
            backslash options field channel (i + 2) (i + 2) limit
          end
      | ' ' | '\t' | '\r' | '\n' when options.trim_space ->
          backslash options field channel (i + 1) mark limit
      | c ->
          flush_backslash field channel mark i limit 0;
          output_char channel c;
          backslash options field channel (i + 1) (i + 1) limit
    end

  let escapes options field channel limit =
    for i = 0 to limit - 1 do
      let str = match field.[i] with
      | '\n' -> Some "\\n"
      | '\r' -> Some "\\r"
      | '\t' -> Some "\\t"
      | '"'  -> Some "\\\""
      | '\\' -> Some "\\\\"
      | '\000'..'\032' | '\128'..'\255' as c ->
          Some (Printf.sprintf "\\x%02x" (int_of_char c))
      | c when c = options.record_sep
            or c = options.field_sep ->
          Some (Printf.sprintf "\\x%02x" (int_of_char c))
      | _    -> None in
      match str with
      | Some s -> output_string channel s
      | _      -> output_char channel field.[i]
    done
IN
  let output_field ?(options = default_options) channel field =
    let limit = String.length field in
    if must_quote options field limit then
      if options.rec_quotation then
        quotation options field channel limit
      else if options.rec_escapes then
        escapes options field channel limit
      else if options.rec_backslash then
        backslash options field channel 0 0 limit
      else 
        failwith "Csv.output_field: options provide insufficient quoting"
    else if options.rec_backslash or
            options.rec_escapes then
      backslash options field channel 0 0 limit
    else
      output_string channel field
END

let output_record ?(options = default_options) channel record =
  let limit = Array.length record in
  if limit > 0 then begin
    output_field ~options channel record.(0);
    for i = 1 to limit - 1 do
      output_char channel options.field_sep;
      output_field ~options channel record.(i)
    done;
    output_char channel options.record_sep;
    flush channel
  end

(* vim: set ft=ocaml : *)
open Util
open Channel
open Channel.Dup

module Make(Shtream : AnyShtream.S) = struct
  type initial      = Shtream.initial
  type 'a elem      = 'a Shtream.elem
  type 'a shtream   = 'a Shtream.t
  type 'a coshtream = 'a Shtream.co_t

  type text = initial elem

  type procref = Proc.t option ref

  (*
   * Representation of Fittings
   *)

  type 'a t =
    (* Ground constructors *)
    | Source   of 'o source
    | Sink     of 'i sink
    | Trans    of ('i shtream ->'o shtream)
    | Ext      of ('i, 'o) ext
    | Status   of Proc.status
    (* Combinations *)
    | DupIn    of ('i, 'o) dupin
    | DupOut   of ('i, 'o) dupout
    | Pipe     of ('i, 'o) pipe
    | Par      of ('i, 'o) par
    | Seq      of ('i, 'o) seq
    constraint 'a = 'i -> 'o
    and 'a in_witness  = text shtream -> 'a shtream
    and ('a, 'r) out_witness =
      { out_witness :     'b . ('a shtream -> 'b elem shtream) -> 'r; }
    and 'o source =
      { source_witness :  'o in_witness;
        source_gen :      dup_in_source; }
    and 'i sink =
      { sink_witness :    'r . ('i, 'r) out_witness -> 'r;
        sink_gen :        dup_out_source; }
    and ('i, 'o) ext =
      { ext_in_witness :  'o in_witness;
        ext_out_witness : 'r . ('i, 'r) out_witness -> 'r;
        ext_c_c :         unit -> unit;
      }
    and ('i, 'o) dupin =
      { dupin_witness :   'i in_witness;
        dupin_dups :      dup_spec;
        dupin_sub :       ('i -> 'o) t; }
    and ('i, 'o) dupout =
      { dupout_witness :  'r . ('o, 'r) out_witness -> 'r;
        dupout_dups :     dup_spec;
        dupout_sub :      ('i -> 'o) t; }
    and ('i, 'o) pipe =
      { pipe :            'r . ('i, 'o, 'r) depipe -> 'r; }
    and ('i, 'o, 'r) depipe =
      { depipe :          'm . ('i -> 'm) t -> ('m -> 'o) t -> 'r; }
    and ('i, 'o) par =
      { par_bg :          'r . 'r depar -> 'r;
        par_fg :          Proc.t -> ('i -> 'o) t; }
    and 'r depar =
      { depar :           'o . (text -> 'o elem) t -> 'r; }
    and ('i, 'o) seq =
      { seq_fst :         ('i -> 'o) t;
        seq_snd :         Proc.status -> ('i -> 'o) t; }

  let out_witness_id f = f.out_witness id

  (*
   * Helpers
   *)

  (* This isn't reentrant. *)
  let copy_in_out =
    let buflen = 4096 in
    let buf    = Bytes.make buflen ' ' in
    let rec loop () =
      let count = Unix.read Unix.stdin buf 0 buflen in
      if count > 0 then begin
        ignore @@ Unix.write Unix.stdout buf 0 count;
        loop ()
      end in
    loop

  (*
   * Interpretation of Fittings
   *)

  let has_descr d dups =
    List.exists (fun (_, gen) -> descr_of_gen (gen :> gen_channel) = d) dups

  (* Apply a shtream transducer to a shtream, so that when the result of
   * the transducer is closed, so is the input shtream. *)
  let shtream_apply f s =
    try
      let it = f s in
      Shtream.add_cleanup (fun _ -> Shtream.close s) it;
      it
    with e -> Shtream.close s; raise e

  let dup_any = function
    | #dup_in_source as g  -> `InChannel (dup_in g)
    | #dup_out_source as g -> `OutChannel (dup_out g)
  let dup_protect dups thunk =
    let saved     = List.map (dup_any % fst) dups in
    let real_dups = List.map2 (fun (_, d) s -> s, d) dups saved in
    let protector = { protector = fun f -> with_dups real_dups f; } in
    let cleaner   = fun _ -> List.iter close_gen saved in
    let s = with_dups real_dups thunk in
    Shtream.add_protection protector s;
    Shtream.add_cleanup cleaner s;
    s

  (* OCaml allows polymorphic recursion if you tell it the types ahead
   * of time using a record.  Whee! *)
  type 'o unit_witness = ('o, unit) out_witness -> unit

  type run_low_record = {
    s_s: 'i 'o . ('i -> 'o) t -> 'i shtream -> 'o shtream;
    s_c: 'i 'o . ('i -> 'o) t -> 'i shtream -> 'o unit_witness -> unit;
    c_c: 'i 'o . ('i -> 'o) t -> 'i in_witness -> 'o unit_witness -> unit;
  }

  type pipe_low_record = {
    p_s_s: 'i 'o . ('i, 'o, 'i shtream -> 'o shtream) depipe;
    p_s_c: 'i 'o . ('i, 'o, 'i shtream -> 'o unit_witness -> unit) depipe;
    p_c_c: 'i 'o . ('i, 'o, 'i in_witness -> 'o unit_witness -> unit) depipe;
  }

  let rec run_low = {
    s_s = (fun fitting s -> match fitting with
      | Source r   ->
        r.source_witness @@
        with_dups [ 0 %<* r.source_gen ] (fun _ ->
          Shtream.of_channel stdin
        )
      | Sink r     ->
        with_dups [ 1 %>* r.sink_gen ] (fun _ ->
          r.sink_witness {
            out_witness = fun out_w ->
              Shtream.output (out_w s)
          }
        );
        Shtream.nil ()
      | Trans f    -> f s
      | Ext r      ->
        r.ext_in_witness @@ Shtream.of_thunk (fun _ ->
          r.ext_out_witness { out_witness = fun out_w ->
            let s' = out_w s in
            with_dups [ 0 %</ Shtream.channel_of s' ] (fun _ ->
              r.ext_c_c ()
            )
          }
        )
      | Status n   -> Shtream.from (fun _ -> Shtream.fail_with n)
      | DupIn r    ->
        dup_protect (r.dupin_dups :> dup_spec) (fun _ ->
          run_low.s_s r.dupin_sub
            (if has_descr Unix.stdin r.dupin_dups
             then r.dupin_witness (Shtream.of_channel stdin)
             else s)
        )
      | DupOut r   ->
        dup_protect (r.dupout_dups :> dup_spec) (fun _ ->
          if has_descr Unix.stdout r.dupout_dups
          then r.dupout_witness {
            out_witness = fun out_w ->
              Shtream.output @@ out_w @@
              run_low.s_s r.dupout_sub s;
              Shtream.nil ()
          }
          else run_low.s_s r.dupout_sub s
        )
      | Pipe r     -> r.pipe pipe_low.p_s_s s
      | Par r      ->
        let proc, _ = open_thunk (fun _ ->
          r.par_bg { depar = fun bg ->
            run_low.s_c bg (Shtream.nil ()) out_witness_id
          }
        ) in
        run_low.s_s (r.par_fg proc) s
      | Seq r      ->
        let s1  = run_low.s_s r.seq_fst s in
        let snd = ref None in
        Shtream.from_low
          ~close:(fun _ -> Shtream.close (maybe !snd (fun _ -> s1) id))
          (fun _ -> maybe !snd
              (fun _ -> maybe (Shtream.next' s1)
                  (fun _ ->
                     let status = maybe (Shtream.status s1)
                         (fun _ -> Proc.WEXITED 0) id in
                     let s2 = run_low.s_s (r.seq_snd status) s in
                     snd := Some s2;
                     Shtream.next s2
                  )
                  id)
              Shtream.next
          )
    );
    s_c = (fun fitting s out_w -> match fitting with
      | Source r   ->
        with_dups [ 0 %<* r.source_gen ]
          copy_in_out
      | Sink r     ->
        with_dups [ 1 %>* r.sink_gen ] (fun _ ->
          r.sink_witness {
            out_witness = fun out_w ->
              Shtream.output (out_w s)
          }
        )
      | Trans f    ->
        out_w { out_witness = fun out_w ->
          Shtream.output (out_w (f s))
        }
      | Ext r      ->
        r.ext_out_witness { out_witness = fun out_w ->
          let s' = out_w s in
          with_dups [ 0 %</ Shtream.channel_of s' ] (fun _ ->
            r.ext_c_c ()
          )
        }
      | Status n   ->
        Proc.exit_with_status n
      | DupIn r    ->
        with_dups (r.dupin_dups :> dup_spec) (fun _ ->
          run_low.s_c r.dupin_sub
            (if has_descr Unix.stdin r.dupin_dups
             then r.dupin_witness (Shtream.of_channel stdin)
             else s)
            out_w
        )
      | DupOut r   ->
        with_dups (r.dupout_dups :> dup_spec) (fun _ ->
          run_low.s_c r.dupout_sub s out_w
        )
      | Pipe r     -> r.pipe pipe_low.p_s_c s out_w
      | Par r      ->
        let proc, _ = open_thunk (fun _ ->
          r.par_bg { depar = fun bg ->
            run_low.s_c bg (Shtream.nil ()) out_witness_id
          }
        ) in
        run_low.s_c (r.par_fg proc) s out_w
      | Seq r      ->
        let proc, _ = open_thunk (fun _ ->
          run_low.s_c r.seq_fst s out_w
        ) in
        let status = Proc.wait proc in
        run_low.s_c (r.seq_snd status) s out_w
    );
    c_c = (fun fitting in_w out_w -> match fitting with
      | Source r   ->
        with_dups [ 0 %<* r.source_gen ]
          copy_in_out
      | Sink r     ->
        with_dups [ 1 %>* r.sink_gen ]
          copy_in_out
      | Trans f    ->
        out_w { out_witness = fun out_w ->
          Shtream.output @@ out_w @@
          f @@ in_w @@
          Shtream.of_channel stdin
        }
      | Ext r      -> r.ext_c_c ()
      | Status n   -> Proc.exit_with_status n
      | DupIn r    ->
        with_dups (r.dupin_dups :> dup_spec) (fun _ ->
          run_low.c_c r.dupin_sub in_w out_w
        )
      | DupOut r   ->
        with_dups (r.dupout_dups :> dup_spec) (fun _ ->
          run_low.c_c r.dupout_sub in_w out_w
        )
      | Pipe r     -> r.pipe pipe_low.p_c_c in_w out_w
      | Par r      ->
        let proc, _ = open_thunk (fun _ ->
          r.par_bg { depar = fun bg ->
            run_low.s_c bg (Shtream.nil ()) out_witness_id
          }
        ) in
        run_low.c_c (r.par_fg proc) in_w out_w
      | Seq r      ->
        let proc, _ = open_thunk (fun _ ->
          run_low.c_c r.seq_fst in_w out_w
        ) in
        let status = Proc.wait proc in
        run_low.c_c (r.seq_snd status) in_w out_w
    );
  } and pipe_low = {
  p_s_s = {
    depipe = fun a b s -> match a, b with
      | Source s1, t2 ->
        dup_protect [ 0 %<* s1.source_gen ] (fun _ ->
          shtream_apply
            (run_low.s_s t2)
            (s1.source_witness (Shtream.of_channel stdin))
        )
      | t1, Sink s2 ->
        with_dups [ 1 %>* s2.sink_gen ] (fun _ ->
          run_low.s_c t1 s s2.sink_witness
        );
        Shtream.nil ()
      | t1, t2 ->
        let s1 = run_low.s_s t1 s in
        shtream_apply (run_low.s_s t2) s1
  };
  p_s_c = {
    depipe = fun a b s out_w -> match a, b with
      | Source s1, t2 ->
        with_dups [ 0 %<* s1.source_gen ] (fun _ ->
          let s = s1.source_witness (Shtream.of_channel stdin) in
          run_low.s_c t2 s out_w
        )
      | t1, Sink s2 ->
        with_dups [ 1 %>* s2.sink_gen ] (fun _ ->
          run_low.s_c t1 s s2.sink_witness
        );
      | t1, t2 ->
        let s1 = run_low.s_s t1 s in
        unwind_protect
          (fun _ -> run_low.s_c t2 s1 out_w)
          (fun _ -> Shtream.close s1)
  };
  p_c_c = {
    depipe = fun a b in_w out_w -> match a, b with
      | Source s1, t2 ->
        with_dups [ 0 %<* s1.source_gen ] (fun _ ->
          run_low.c_c t2 s1.source_witness out_w
        )
      | t1, Sink s2 ->
        with_dups [ 1 %>* s2.sink_gen ] (fun _ ->
          run_low.c_c t1 in_w s2.sink_witness
        )
      | t1, t2 ->
        let to_close = ref None in
        unwind_protect (fun _ ->
          let f s =
            let it =
              shtream_apply (run_low.s_s t2) (run_low.s_s t1 s) in
            to_close := Some it;
            it in
          run_low.c_c (Trans f) in_w out_w
        ) (fun _ -> maybe !to_close ignore Shtream.close)
  };
}

  (*
   * Construction of Fittings
   *)

  let from_gen g    = Source {
    source_witness = id;
    source_gen     = g;
  }

  let from_file s = from_gen (`Filename s)

  let from_stdin  = Source {
    source_witness = id;
    source_gen     = `InFd 0;
  }

  let from_null = Source {
    source_witness = id;
    source_gen     = `Null;
  }

  let to_gen g = Sink {
    sink_witness = out_witness_id;
    sink_gen     = g;
  }

  let to_stdout = Sink {
    sink_witness = out_witness_id;
    sink_gen     = `OutFd 1;
  }

  let to_stderr = Sink {
    sink_witness = out_witness_id;
    sink_gen     = `OutFd 2;
  }

  let to_null   = Sink {
    sink_witness = out_witness_id;
    sink_gen     = `Null;
  }


  let to_file ?clobber s = to_gen @@ match clobber with
  | None      -> `Filename s
  | Some clob -> `Filespec (s, clob)

  let make_ext kont = Ext {
    ext_in_witness  = id;
    ext_out_witness = out_witness_id;
    ext_c_c         = kont;
  }

  let command cmd =
    make_ext (fun _ -> Proc.exec cmd)

  let program ?path prog ?argv0 args =
    make_ext (fun _ -> Proc.exec_program ?path prog ?argv0 args)

  let thunk next =
    make_ext (fun _ -> Abort.abort next)

  let sed f          = Trans (Shtream.map f)
  let grep p         = Trans (Shtream.filter p)
  let trans f        = Trans f

  let grep_string p  = let string_of = Shtream.Elem.string_of () in
    grep (p % string_of)
  let sed_string f   = let string_of = Shtream.Elem.string_of () in
    sed (f % string_of)

  let from_shtream s =
    Trans (fun _ -> s)
  let to_coshtream s =
    Trans (fun s' -> Shtream.annihilate s' s; Shtream.nil ())

  let redirect_in dups fitting = DupIn {
    dupin_witness = id;
    dupin_dups    = dups;
    dupin_sub     = fitting;
  }
  let ( /</ ) fitting dups = redirect_in dups fitting

  let redirect_out dups fitting = DupOut {
    dupout_witness = out_witness_id;
    dupout_dups    = dups;
    dupout_sub     = fitting;
  }
  let ( />/ ) fitting dups = redirect_out dups fitting

  let mk_pipe a b = Pipe { pipe = fun f -> f.depipe a b }
  let pipe = fun a b -> match a, b with
    | Trans f, Trans g -> Trans (g % f)
    | Trans f, Pipe p  -> p.pipe {
      depipe = fun c d -> match c, d with
        | Trans g, h -> mk_pipe (Trans (g % f)) h
        | _          -> mk_pipe a b
    }
    | Pipe p, Trans h  -> p.pipe {
      depipe = fun c d -> match c, d with
        | f, Trans g -> mk_pipe f (Trans (h % g))
        | _          -> mk_pipe a b
    }
    | Pipe p, Pipe q  -> p.pipe {
      depipe = fun c d -> match c, d with
        | f, Trans g -> q.pipe {
          depipe = fun c d -> match c, d with
            | Trans h, i -> mk_pipe (mk_pipe f (Trans (h % g))) i
            | _          -> mk_pipe a b
        }
        | _          -> mk_pipe a b
    }
    | _               -> mk_pipe a b

  let ( -| )   = pipe

  let yield n = Status n

  let seq fst snd =
    Seq { seq_fst = fst; seq_snd = snd; }

  let (^>>=)    = seq
  let (^>>) a b = a ^>>= (fun _ -> b)
  let (&&^) a b = a ^>>= function Proc.WEXITED 0 -> b | n -> yield n
  let (||^) a b = a ^>>= function Proc.WEXITED 0 as n -> yield n | _ -> b
  let (~>>) = function
    | x::xs -> List.fold_left (^>>) x xs
    | []    -> yield (Proc.WEXITED 0)
  let (~&&) = function
    | x::xs -> List.fold_left (&&^) x xs
    | []    -> yield (Proc.WEXITED 0)
  let (~||) = function
    | x::xs -> List.fold_left (||^) x xs
    | []    -> yield (Proc.WEXITED 1)
  let commands =
    (function
      | x::xs -> List.fold_left (-|) x xs
      | []    -> ~>>[]) % (List.map command)

  let caml f  = ~>>[] ^>>= (fun _ -> f ())

  let par a b = Par {
    par_bg      = (fun d -> d.depar a);
    par_fg      = b;
  }

  let (^&=)    = par
  let (^&) a b = a ^&= (fun _ -> b)

  (*
   * High-level Fitting Runners
   *)

  let run_source fitting =
    shtream_apply (run_low.s_s fitting) (Shtream.of_channel stdin)

  let run_sink fitting =
    Shtream.coshtream_of @@ fun s ->
      run_low.s_c fitting s out_witness_id

  let run_shtream = run_low.s_s

  let run_list fitting = Shtream.list_of (run_source fitting)

  let run_in ?procref fitting =
    open_thunk_in ?procref (fun _ ->
      run_low.c_c fitting id out_witness_id
    )

  let run_out ?procref fitting =
    open_thunk_out ?procref (fun _ ->
      run_low.c_c fitting id out_witness_id
    )

  let run_bg fitting =
    fst @@ open_thunk (fun _ ->
      run_low.c_c fitting id out_witness_id
    )

  let run_backquote ?procref fitting =
    let c = run_in ?procref fitting in
    unwind_protect
      (fun _ -> string_of_channel c)
      (fun _ -> close_in c)

  let run fitting = Proc.wait (run_bg fitting)

  let string_of_elem d  = Shtream.Elem.string_of () d
  let elem_of_string d  = Shtream.Elem.of_string () d
  let int_of_elem d     = int_of_string (string_of_elem d)
  let elem_of_int d     = elem_of_string (string_of_int d)
  let char_of_elem d    = (string_of_elem d).[0]
  let elem_of_char d    = elem_of_string (String.make 1 d)
  let float_of_elem d   = float_of_string (string_of_elem d)
  let elem_of_float d   = elem_of_string (string_of_float d)
  let bool_of_elem d    = bool_of_string (string_of_elem d)
  let elem_of_bool d    = elem_of_string (string_of_bool d)
end

include Make(LineShtream)

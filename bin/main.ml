let () = Printexc.print_backtrace stderr
let () = Printexc.record_backtrace true

open Util

let keys = Queue.create ()
let texts = Queue.create ()
let double = ref false
let debug = ref false
let bitslice = ref false
let fn_name = ref None

let spec =
  Arg.align
    [
      ("-2", Arg.Set double, " Enable double processing");
      ("-d", Arg.Set debug, " Debug mode");
      ("-b", Arg.Set bitslice, " Bitslice bytes");
      ( "-s",
        Arg.String (fun s -> fn_name := Some s),
        "<fn-name> function to evaluate" );
      ("-k", Arg.String (Fun.flip Queue.add keys), "<keyfile> path to the key");
    ]

let pos_args = Fun.flip Queue.add texts

let usage =
  Printf.sprintf
    "%s [-2] [-d] [-b] -s <fn-name> [-k <keyfile>]... FILE PLAINTEXT..."
    Sys.argv.(0)

let () = Arg.parse spec pos_args usage

module Double = Ua0.Value.Naperian (struct
  let n = 2
end)

module Rows = Ua0.Value.Naperian (struct
  let n = 4
end)

module Cols = Rows
module Slice = Rows
module ColsRows = Ua0.Value.NaperianCompose (Cols) (Rows)
module ColsRowsSlice = Ua0.Value.NaperianCompose (ColsRows) (Slice)

let vbitslice value =
  Ua0.Value.reindex_lr (module ColsRows) (module Slice) value

let invbitslice value =
  Ua0.Value.reindex_lr (module Slice) (module ColsRows) value

let eval ~bitslice ~double ast symbole keys texts =
  let () = assert (Queue.length keys = Queue.length texts) in
  let size = if double then 2 else 1 in
  let kps =
    Seq.map2
      (fun k t ->
        let k = Common.file_to_bools k in
        let t = Common.file_to_bools t in
        (k, t))
      (Queue.to_seq keys) (Queue.to_seq texts)
  in

  let kps =
    Seq.map
      (fun (k, t) ->
        let state = Gift.spec_tabulate t in
        let keys = Gift.uvconsts k in
        (state, keys))
      kps
  in

  let kps = Seq.take size kps in

  let kps =
    match bitslice with
    | false -> kps
    | true ->
        Seq.map
          (fun (state, keys) ->
            let state = vbitslice state in
            let keys = List.map vbitslice keys in
            (state, keys))
          kps
  in

  match double with
  | false ->
      Seq.find_map
        (fun (state, keys) ->
          let keys = Array.of_list keys in
          let () = assert (Array.length keys = 28) in
          Ua0.Eval.eval ast symbole None [ state; VArray keys ])
        kps
  | true ->
      let ( let* ) = Option.bind in
      let* (c1, ks1), kps = Seq.uncons kps in
      let* (c2, ks2), _ = Seq.uncons kps in
      let state =
        Ua0.Value.map2 (fun lhs rhs -> Ua0.Value.VArray [| lhs; rhs |]) c1 c2
      in
      let ks =
        List.map2
          (Ua0.Value.map2 (fun lhs rhs -> Ua0.Value.VArray [| lhs; rhs |]))
          ks1 ks2
      in
      let keys = Array.of_list ks in
      Ua0.Eval.eval ast symbole None [ state; VArray keys ]

(*let print module' = Format.printf "%a\n" Ua0.Pp.pp_module module'*)

let ast_raw file =
  In_channel.with_open_bin file (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      let () = Lexing.set_filename lexbuf file in
      Ua0.Parser.module_ Ua0.Lexer.token lexbuf)

let ast symbole file =
  let ast = ast_raw file in
  let env, ast = Ua0.Pass.Idents.of_string_ast_env ast in
  let symbole = Ua0.Pass.Idents.Env.find_fn_ident symbole env in
  (ast, symbole)

let pp_value value =
  let () = Format.(fprintf err_formatter "%a\n" Ua0.Value.pp value) in
  let slices = Gift.to_slice value in
  let chars = Gift.transpose_inverse slices in
  Gift.pp Format.err_formatter chars

let main () =
  match Queue.is_empty texts with
  | true -> (*print Ua0.Gift.gift*) ()
  | false -> (
      let file =
        match Queue.take_opt texts with
        | None -> raise @@ Arg.Bad "Missing ua file"
        | Some file -> file
      in
      match !fn_name with
      | None -> ignore (ast_raw file)
      | Some fn_name -> (
          let ast, symbole = ast fn_name file in
          match
            eval ~bitslice:!bitslice ~double:!double ast symbole keys texts
          with
          | None -> Printf.eprintf "evaluation None\n"
          | Some (_, (value, _)) ->
              let values =
                match !double with
                | true ->
                    let lhs, rhs = Ua0.Value.split2 value in
                    [ lhs; rhs ]
                | false -> [ value ]
              in
              let values =
                if !bitslice then List.map invbitslice values else values
              in
              List.iter pp_value values))

let () = main ()

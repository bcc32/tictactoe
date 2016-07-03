open Core.Std

module Mark = struct
  type t =
    | X
    | O
  [@@deriving bin_io, sexp]

  let to_string t =
    match t with
    | X -> "X"
    | O -> "O"

  let next t =
    match t with
    | X -> O
    | O -> X
end

type t = Mark.t option array array
[@@deriving bin_io, sexp]

exception More_than_one_winner

let create n =
  Array.make_matrix None ~dimx:n ~dimy:n

let get t ~x ~y =
  t.(x).(y)

let already_occupied =
  Or_error.errorf "Cell (%d, %d) already occupied"

let set t ~x ~y mark =
  match t.(x).(y) with
  | Some _ -> already_occupied x y
  | None ->
    t.(x).(y) <- Some mark;
    Ok ()

let fset t ~x ~y mark =
  match t.(x).(y) with
  | Some _ -> already_occupied x y
  | None ->
    let rows = Array.copy t in
    let row = Array.copy t.(x) in
    rows.(x) <- row;
    row.(y) <- Some mark;
    Ok rows

let chain_length line ~player =
  let opponent_marks =
    Array.exists line ~f:(Option.value_map ~default:false ~f:((<>) player))
  in
  if opponent_marks
  then 0
  else Array.count line ~f:(Option.value_map ~default:false ~f:((=) player))

let chain_lengths rows ~player =
  Array.fold rows ~init:0 ~f:(fun acc row -> acc + chain_length row ~player)

let lr_diag t =
  Array.mapi t ~f:(fun i r -> r.(i))

let rl_diag t =
  Array.mapi t ~f:(fun i r -> r.(Array.length r - i - 1))

let eval t ~player =
  let t' = Array.transpose_exn t in
  let row_wise = chain_lengths ~player t           in
  let col_wise = chain_lengths ~player t'          in
  let lr_chain = chain_length  ~player (lr_diag t) in
  let rl_chain = chain_length  ~player (rl_diag t) in
  row_wise + col_wise + lr_chain + rl_chain

let to_string t =
  let width = 2 * Array.length t - 1 in
  let hline = String.init width ~f:(Fn.const '-') in
  Array.map t ~f:(fun r ->
    Array.map r ~f:(Option.value_map ~default:" " ~f:Mark.to_string)
    |> String.concat_array ~sep:"|"
  )
  |> String.concat_array ~sep:("\n" ^ hline ^ "\n")

let winner t =
  let line_winner line =
    if Array.for_all line ~f:Option.is_some
    then line.(0)
    else None
  in
  let t' = Array.transpose_exn t in
  let winners =
    Array.concat
      [ Array.filter_map ~f:line_winner t
      ; Array.filter_map ~f:line_winner t'
      ; Array.filter_map ~f:line_winner [| lr_diag t |]
      ; Array.filter_map ~f:line_winner [| rl_diag t |]
      ]
    |> Array.to_list
  in
  match List.dedup winners with
  | [w] -> Some w
  | []  -> None
  | _   -> raise More_than_one_winner

let is_end t =
  Option.is_some (winner t)

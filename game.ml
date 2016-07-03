open Core.Std

let in_bounds size x y =
  0 <= x && x < size
  &&
  0 <= y && y < size

let get_valid_xy prompt size : int * int =
  let rec loop () =
    printf !"%s%!" prompt;
    let line = read_line () in
    Or_error.try_with (fun () ->
      Scanf.sscanf line "%d%_[, ]%d" Tuple2.create
    )
    |> function
    | Ok (x, y) when in_bounds size x y -> x, y
    | Ok (x, y) ->
      eprintf "(%d, %d) is out of bounds.\n" x y;
      loop ()
    | Error e ->
      eprintf "%s\n" (Error.to_string_hum e);
      loop ()
  in
  loop ()

let two_player =
  Command.basic' ~summary:"Play against another human" begin
    let open Command.Let_syntax in
    let%map_open size =
      flag "-size" (optional_with_default 3 int)
        ~doc:"n use a board size of n x n"
    in
    fun () ->
      let rec loop board player =
        printf !"%{Board}\n" board;
        if Board.is_end board
        then (
          match Board.winner board with
          | Some winner ->
            printf !"%{Board.Mark} wins!\n" winner
          | None ->
            printf "The only winning move is not to play...\n"
        ) else (
          let prompt = sprintf !"%{Board.Mark}'s move: " player in
          let x, y = get_valid_xy prompt size in
          Board.set board ~x ~y player |> Or_error.ok_exn; (* FIXME *)
          loop board (Board.Mark.next player)
        )
      in
      loop (Board.create size) Board.Mark.X
  end

let versus_cpu =
  Command.basic' ~summary:"Play against the computer" begin
    let open Command.Let_syntax in
    let%map_open () = Command.Param.return () in
    fun () ->
      failwith "unimplemented"
  end

let play =
  Command.group ~summary:"Play tic-tac-toe"
    [ "2p", two_player
    ; "1p", versus_cpu
    ]

let () =
  Command.run play

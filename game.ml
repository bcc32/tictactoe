open Core.Std

let two_player =
  Command.basic' ~summary:"Play against another human" begin
    let open Command.Let_syntax in
    let%map_open () = Command.Param.return () in
    fun () ->
      failwith "unimplemented"
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

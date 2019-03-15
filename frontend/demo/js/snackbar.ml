open Js_of_ocaml
open Components

let section () =
  let show = new Button.t ~label:"show snackbar" () in
  let snackbar =
    Snackbar.make
      ~label:"Can't send photo. Retry in 5 seconds."
      ~action:(Label "retry")
      ~dismiss:True
      () in
  show#listen_click_lwt' (fun _ _ -> snackbar#open_ ());
  snackbar#listen_lwt' Snackbar.Event.closing (fun e _ ->
      print_endline "closing";
      Js.Unsafe.global##.console##log e |> ignore;
      Lwt.return_unit);
  snackbar#listen_lwt' Snackbar.Event.closed (fun e _ ->
      print_endline "closed";
      Js.Unsafe.global##.console##log e |> ignore;
      Lwt.return_unit);
  Widget.create_div ~widgets:[snackbar#widget; show#widget] ()

open Js_of_ocaml
open Components

let section () =
  let snackbar =
    Snackbar.make
      ~label:"Can't send photo. Retry in 5 seconds."
      ~action:(Label "retry")
      ~dismiss:True
      () in
  let show =
    Button.make
      ~label:"show snackbar"
      ~on_click:(fun _ -> snackbar#open_ ())
      () in
  Lwt.ignore_result
  @@ Events.listen_lwt snackbar#root Snackbar.Event.closing (fun e _ ->
         print_endline "closing";
         Js.Unsafe.global##.console##log e |> ignore;
         Lwt.return_unit);
  Lwt.ignore_result
  @@ Events.listen_lwt snackbar#root Snackbar.Event.closed (fun e _ ->
         print_endline "closed";
         Js.Unsafe.global##.console##log e |> ignore;
         Lwt.return_unit);
  Widget.create_div ~widgets:[snackbar#widget; show#widget] ()

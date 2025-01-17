open Components
open Application_types
open Netlib.Uri
open Js_of_ocaml

open Lwt.Infix

module Api_http = Api_js.Http.Make(Body)

let make_card user =
  let _username = match user with
    | `Root -> "администратора"
    | `Operator -> "оператора"
    | `Guest -> "гостя" in

  (* TODO add https flag and permission check *)

  let key_field =
    Dom_html.createInput ~_type:(Js.string "file") Dom_html.document
  in
  let key_but = Button.make
      ~label:"Сохранить ключ"
      ~on_click:(fun _ _ _ ->
          match Js.Optdef.to_option @@ key_field##.files with
          | None -> Lwt.return @@ print_endline "no file 1"
          | Some files ->
            match Js.Opt.to_option @@ files##item 0 with
            | None -> Lwt.return @@ print_endline "no file 2"
            | Some file ->
              let freader = new%js File.fileReader in
              freader##readAsBinaryString file;
              freader##.onload :=
                Dom.handler (fun _ ->
                    let file_blob = Obj.magic freader##.result in
                    Js.Unsafe.global##.console##log file_blob |> ignore;
                    Api_js.Http.perform_file
                      ~file:file_blob
                      ~path:Path.Format.("api/server/config/key" @/ String ^/empty)
                      ~query:Query.empty
                      (Js.to_string file##.name)
                      (fun _ _ -> Lwt.return @@ print_endline "Done")
                    |> ignore;
                    Js._true);
              Lwt.return_unit)
      ()
  in

  let cert_field =
    Dom_html.createInput ~_type:(Js.string "file") Dom_html.document
  in
  let cert_but = Button.make
      ~label:"Сохранить сертификат"
      ~on_click:(fun _ _ _ ->
          match Js.Optdef.to_option @@ cert_field##.files with
          | None -> Lwt.return @@ print_endline "no file"
          | Some files ->
            match Js.Opt.to_option @@ files##item 0 with
            | None -> Lwt.return @@ print_endline "no file"
            | Some file ->
              let freader = new%js File.fileReader in
              freader##readAsBinaryString file;
              freader##.onload :=
                Dom.handler (fun _ ->
                    let file_blob = Obj.magic freader##.result in
                    Js.Unsafe.global##.console##log file_blob |> ignore;
                    Api_js.Http.perform_file
                      ~file:file_blob
                      ~path:Path.Format.("api/server/config/cert" @/ String ^/empty)
                      ~query:Query.empty
                      (Js.to_string file##.name)
                      (fun _ _ -> Lwt.return @@ print_endline "Done")
                    |> ignore;
                    Js._true);
              Lwt.return_unit)
      () in

  let reset = Button.make
      ~label:"Перезапустить сервер"
      ~on_click:(fun _ _ _ ->
          Api_http.perform_unit
            ~meth:`POST
            ~path:Path.Format.("api/server/config/server_restart" @/ empty)
            ~query:Query.empty
            (fun _ _ -> Lwt.return_unit))
      () in

  let cont =
    Box.make
      ~dir:`Column
      [ Widget.create key_field
      ; key_but#widget
      ; Widget.create cert_field
      ; cert_but#widget
      ; reset#widget
      ] in
  cont

let page user =
  (make_card user)#widget

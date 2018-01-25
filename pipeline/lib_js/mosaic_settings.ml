open Components
open Requests
open Lwt_result.Infix

class t () =

  let elt = Dom_html.createDiv Dom_html.document in

  object(self)

    val mutable in_dom = false
    val mutable observer = None 
    val mutable sock : WebSockets.webSocket Js.t option = None

    inherit Widget.widget elt ()

    method private observe =
      MutationObserver.observe
        ~node:Dom_html.document
        ~f:(fun _ _ ->
          let in_dom_new = (Js.Unsafe.coerce Dom_html.document)##contains self#root in
          if in_dom && (not in_dom_new)
          then CCOpt.iter (fun x -> x##close; sock <- None) sock
          else if (not in_dom) && in_dom_new
          then (Requests.get_wm ()
                >>= (fun wm ->
                  let e_wm,wm_sock = Requests.get_wm_socket () in
                  let open Lwt.Infix in
                  let wm_el = Ui.Wm.create ~init:wm ~events:e_wm
                                           ~post:(fun w -> Requests.post_wm w
                                                           >|= (function
                                                                | Ok () -> ()
                                                                | Error e -> print_endline @@ "error post wm" ^ e)
                                                           |> Lwt.ignore_result)
                  in
                  sock <- Some wm_sock;
                  Dom.appendChild self#root wm_el;
                  Lwt_result.return ())
                |> ignore);
          in_dom <- in_dom_new)
        ~child_list:true
        ~subtree:true
        ()
      |> (fun o -> observer <- Some o)

    initializer
      self#observe

  end

let page () = new t ()
module type S = sig

  type user

  type script = Src of string
              | Raw of string

  type side_sheet_props =
    { clipped : bool
    ; opened : bool
    ; typ : [`Modal | `Dismissible | `Permanent]
    ; content : Tyxml.Xml.elt list
    }

  type template_props =
    { title : string option
    ; top_app_bar_leading : Tyxml.Xml.elt option
    ; top_app_bar_content : Tyxml.Xml.elt list
    ; top_app_bar_bottom : Tyxml.Xml.elt option
    ; side_sheet : side_sheet_props option
    ; pre_scripts : script list
    ; post_scripts : script list
    ; stylesheets : string list
    ; content : Tyxml.Xml.elt list
    }

  type topmost
   
  type inner

  type priority = [ `Index of int | `None ]

  type 'a item

  val make_side_sheet_props : ?clipped:bool
    -> ?opened:bool
    -> ?typ:[ `Dismissible | `Modal | `Permanent ]
    -> ?content:Tyxml.Xml.elt list
    -> unit
    -> side_sheet_props

  val make_template_props : ?title:string
    -> ?top_app_bar_leading:Tyxml.Xml.elt
    -> ?top_app_bar_content:Tyxml.Xml.elt list
    -> ?top_app_bar_bottom:Tyxml.Xml.elt
    -> ?side_sheet:side_sheet_props
    -> ?pre_scripts:script list
    -> ?post_scripts:script list
    -> ?stylesheets:string list
    -> ?content:Tyxml.Xml.elt list
    -> unit
    -> template_props

  val reference : ?restrict:user list
                  -> ?priority:priority
                  -> title:string
                  -> href:Netlib.Uri.t
                  -> 'a item list

  val simple : ?restrict:user list
               -> ?priority:priority
               -> title:string
               -> ?icon:Tyxml.Xml.elt
               -> path:Netlib.Uri.Path.t
               -> template_props
               -> 'a item list

  val parametric : ?restrict:user list
                   -> path:('a, user -> Mustache.Json.value option) Netlib.Uri.Path.Format.t
                   -> template_props
                   -> 'c item list
    
  val subtree : ?restrict:user list
                -> ?priority:priority
                -> title:string
                -> ?icon:Tyxml.Xml.elt
                -> inner item list
                -> topmost item list

  val make : template:string
             -> topmost item list
             -> (Cohttp.Code.meth
                 * (user -> 'a -> 'b -> 'c ->
                    [> `Instant of  Cohttp_lwt_unix.Server.response_action Lwt.t
                    |  `Error of string ] Lwt.t) Netlib.Uri.Dispatcher.node) list

end
